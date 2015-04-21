      module IO_vectorFields_mod
      use IO_vectorBase_mod
      use grid_mod
      use vectorField_mod

      implicit none

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

      public :: writeToFile,writeVecPhysical

      logical,parameter :: headerTecplot = .true.

      interface writeToFile;     module procedure writeVecGrid;      end interface
      interface writeToFile;     module procedure writeVecFieldGrid; end interface
      ! interface writeToFile;     module procedure writeVecPhysical;  end interface
        
      contains

      subroutine writeVecGrid(g,u,v,w,dir,namex,namey,namez)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,namez
        type(grid),intent(in) :: g
        real(cp),dimension(:,:,:),intent(in) :: u,v,w
        integer,dimension(3) :: s
        integer :: i
        s = shape(u)
        if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then
        ! Node data
        call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,u,v,w,dir,namex,namey,namez)
        ! CC data
        elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then
        call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,u,v,w,dir,namex,namey,namez)
        else
          stop 'Error: grid size compared to input field in writeVecGrid in IO_vectorFields.f90.'
        endif
      end subroutine

      subroutine writeVecFieldPhysical(g,u,v,w,dir,namex,namey,namez)
        ! This routine writes the interior of the field to a file
        ! so the ghost points are not exported. This is helpful for
        ! visualization so that data is not distorted by unphysical scales.
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,namez
        type(grid),intent(in) :: g
        real(cp),dimension(:,:,:),intent(in) :: u,v,w
        integer,dimension(3) :: s
        integer :: i
        s = shape(u)
        if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then
        ! Node data
        call writeToFile(g%c(1)%hn(2:s(1)-1),&
                         g%c(2)%hn(2:s(2)-1),&
                         g%c(3)%hn(2:s(3)-1),&
                         u(2:s(1)-1,2:s(2)-1,2:s(3)-1),&
                         v(2:s(1)-1,2:s(2)-1,2:s(3)-1),&
                         w(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,namex,namey,namez)
        ! CC data
        elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then
        call writeToFile(g%c(1)%hc(2:s(1)-1),&
                         g%c(2)%hc(2:s(2)-1),&
                         g%c(3)%hc(2:s(3)-1),&
                         u(2:s(1)-1,2:s(2)-1,2:s(3)-1),&
                         v(2:s(1)-1,2:s(2)-1,2:s(3)-1),&
                         w(2:s(1)-1,2:s(2)-1,2:s(3)-1),dir,namex,namey,namez)
        else
          write(*,*) 'Error in exporting '//namex//namey//namez
          write(*,*) 's = ',s
          write(*,*) 's(u,v,w) = ',shape(u),shape(v),shape(w)
          stop 'Error: bad grid size compared to input field in writeVecFieldPhysical in IO_vectorFields.f90.'
        endif
      end subroutine

      subroutine writeVecFieldGrid(g,U,dir,namex,namey,namez)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,namez
        type(grid),intent(in) :: g
        type(vectorField),intent(in) :: U
        integer,dimension(3) :: s
        integer :: i
        s = shape(U%x)
        if (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then
        ! Node data
        call writeToFile(g,U%x,U%y,U%z,dir,namex,namey,namez)
        ! CC data
        elseif (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then
        call writeToFile(g,U%x,U%y,U%z,dir,namex,namey,namez)
        else
          stop 'Error: bad grid size compared to input field in IO_vectorFields.f90.'
        endif
      end subroutine

      subroutine writeVecPhysical(g,U,dir,namex,namey,namez)
        implicit none
        character(len=*),intent(in) :: dir,namex,namey,namez
        type(grid),intent(in) :: g
        type(vectorField),intent(in) :: U
        call writeVecFieldPhysical(g,U%x,U%y,U%z,dir,namex,namey,namez)
      end subroutine


      end module