      module IO_vectorBase_mod
      use IO_tools_mod
      use IO_tecplotHeaders_mod
      ! use hdf5 ! this module contains all necessary modules

      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private

      public :: readFromFile,writeToFile

      logical,parameter :: headerTecplot = .true.

      interface readFromFile;    module procedure readVec;                   end interface
      interface writeToFile;     module procedure writeVec3D;                end interface
      interface writeToFile;     module procedure writeVec2D;                end interface
      interface writeToFile;     module procedure write2CompVec2DTransient;  end interface
      ! interface writeToFile;     module procedure write1CompVec2DTransient;  end interface

      contains

      subroutine readVec(x,y,z,u,v,w,dir,namex,namey,namez,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,namez
        real(cp),dimension(:),intent(inout) :: x,y,z
        real(cp),dimension(:,:,:),intent(inout) :: u,v,w
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,k,sx,sy,sz
        integer,dimension(3) :: s
        s = shape(u)
        sx = size(x); sy = size(y); sz = size(z)
        if (s(1).ne.sx.or.s(2).ne.sy.or.s(3).ne.sz) then
          write(*,*) 'Mismatch of sizes in ' // trim(adjustl(namex)); stop
        endif

        un = openToRead(dir,trim(adjustl(namex))//','&
        //trim(adjustl(namey))//','//trim(adjustl(namez)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) then
            read(un,*);read(un,*);read(un,*)
          endif
        else
          if (headerTecplot) then
            read(un,*);read(un,*);read(un,*)
          endif
        endif

         do k = 1,sz
           do j = 1,sy
             do i = 1,sx
               read(un,'(6'//rarrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
             enddo
           enddo
         enddo

        call closeExisting(un,trim(adjustl(namex))//','&
        //trim(adjustl(namey))//','//trim(adjustl(namez)),dir)
      end subroutine

      subroutine writeVec3D(x,y,z,u,v,w,dir,namex,namey,namez,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,namez
        real(cp),dimension(:),intent(in) :: x,y,z
        real(cp),dimension(:,:,:),intent(in) :: u,v,w
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,k,sx,sy,sz
        sx = size(x); sy = size(y); sz = size(z)
        un = newAndOpen(dir,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(un,namex,namey,namez,sx,sy,sz)
        else
          if (headerTecplot) call writeTecPlotHeader(un,namex,namey,namez,sx,sy,sz)
        endif

         do k = 1,sz
           do j = 1,sy
             do i = 1,sx
               write(un,'(6'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
             enddo
           enddo
         enddo
        call closeAndMessage(un,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)),dir)
      end subroutine

      subroutine writeVec2D(x,y,u,v,dir,namex,namey,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey
        real(cp),dimension(:),intent(in) :: x,y
        real(cp),dimension(:,:),intent(in) :: u,v
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,sx,sy
        sx = size(x); sy = size(y)
        un = newAndOpen(dir,trim(adjustl(namex))//','//trim(adjustl(namey)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(un,namex,namey,sx,sy)
        else
          if (headerTecplot) call writeTecPlotHeader(un,namex,namey,sx,sy)
        endif

        do j = 1,sy
          do i = 1,sx
            write(un,'(4'//arrfmt//')') x(i),y(j),u(i,j),v(i,j)
          enddo
        enddo
        call closeAndMessage(un,trim(adjustl(namex))//','//trim(adjustl(namey)),dir)
      end subroutine

      subroutine write2CompVec2DTransient(x,y,u,v,dir,namex,namey,ext,n,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,ext
        real(cp),dimension(:),intent(in) :: x,y
        real(cp),dimension(:,:),intent(in) :: u,v
        integer,intent(in) :: n
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,sx,sy
        sx = size(x); sy = size(y)
        un = newAndOpen(dir,trim(adjustl(namex))//trim(adjustl(ext))//&
                       ','//trim(adjustl(namey))//trim(adjustl(ext)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeaderTransient(un,namex,namey,sx,sy,n)
        else
          if (headerTecplot) call writeTecPlotHeaderTransient(un,namex,namey,sx,sy,n)
        endif

        do j = 1,sy
          do i = 1,sx
            write(un,'(4'//arrfmt//')') x(i),y(j),u(i,j),v(i,j)
          enddo
        enddo
        call closeAndMessage(un,trim(adjustl(namex))//trim(adjustl(ext))//&
                           ','//trim(adjustl(namey))//trim(adjustl(ext)),dir)
      end subroutine

      ! subroutine write1CompVec2DTransient(x,y,u,dir,namex,ext,n,headerTecplotTemp)
      !   implicit none
      !   character(len=*),intent(in) :: dir,namex,ext
      !   real(cp),dimension(:),intent(in) :: x,y
      !   real(cp),dimension(:,:),intent(in) :: u
      !   integer,intent(in) :: n
      !   logical,intent(in),optional :: headerTecplotTemp
      !   integer un,i,j,sx,sy
      !   sx = size(x); sy = size(y)
      !   un = newAndOpen(dir,trim(adjustl(namex))//trim(adjustl(ext)))
      !   if (present(headerTecplotTemp)) then
      !     if (headerTecplotTemp) call writeTecPlotHeaderTransient(un,namex,sx,sy,n)
      !   else
      !     if (headerTecplot) call writeTecPlotHeaderTransient(un,namex,sx,sy,n)
      !   endif
      !   do j = 1,sy
      !     do i = 1,sx
      !       write(un,'(3'//arrfmt//')') x(i),y(j),u(i,j)
      !     enddo
      !   enddo
      !   call closeAndMessage(un,trim(adjustl(namex))//trim(adjustl(ext)),dir)
      ! end subroutine

      end module