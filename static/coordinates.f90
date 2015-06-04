       module coordinates_mod
       use IO_tools_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: coordinates
       public :: init,delete
       public :: print,export
       public :: addToFile
       public :: restrict ! For multi-grid

#ifdef _CHECK_GRID_
       public :: checkCoordinates
#endif

       type coordinates
         integer :: N                              ! Number of cells
         real(cp) :: hmin,hmax                     ! Min/Max value of domain
         real(cp) :: dhMin,dhMax,maxRange                ! Smallest spatial step/Maximum range
         integer :: sn,sc                          ! size of hn/hc
         real(cp),dimension(:),allocatable :: hn   ! Cell corner coordinates
         real(cp),dimension(:),allocatable :: hc   ! Cell center coordinates
         real(cp),dimension(:),allocatable :: dhn  ! Difference in cell corner coordinates
         real(cp),dimension(:),allocatable :: dhc  ! Difference in cell center coordinates
       end type

       interface init;      module procedure initCoordinates;        end interface
       interface init;      module procedure initCopy;               end interface
       interface delete;    module procedure deleteCoordinates;      end interface

       interface print;     module procedure printCoordinates;       end interface
       interface export;    module procedure exportCoordinates;      end interface
       interface addToFile; module procedure addToFileCoordinates;   end interface

       interface restrict;  module procedure restrictCoordinates;    end interface
       
       contains

       subroutine deleteCoordinates(c)
         implicit none
         type(coordinates),intent(inout) :: c
         if (allocated(c%hn)) deallocate(c%hn)
         if (allocated(c%hc)) deallocate(c%hc)
         if (allocated(c%dhn)) deallocate(c%dhn)
         if (allocated(c%dhc)) deallocate(c%dhc)
       end subroutine

       subroutine initCoordinates(c,h,gridType)
         implicit none
         type(coordinates),intent(inout) :: c
          real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: gridType
         select case (gridType)
         case (1); call initCellCenter(c,h)
         case (2); call initNodes(c,h)
         end select
       end subroutine

       subroutine initNodes(c,hn)
         implicit none
         type(coordinates),intent(inout) :: c
         real(cp),dimension(:),intent(in) :: hn
         integer :: i
         call delete(c)
         ! Node grid
         c%sn = size(hn)
         allocate(c%hn(c%sn))
         allocate(c%dhn(c%sn-1))
         c%hn = hn
         c%dhn = (/(hn(i+1)-hn(i),i=1,c%sn-1)/)

         ! Cell center grid
         c%sc = c%sn-1
         allocate(c%hc(c%sc))
         allocate(c%dhc(c%sc-1))

         c%hc = (/ ((hn(i+1)+hn(i))/real(2.0,cp),i=1,c%sn-1) /)
         c%dhc = (/(c%hc(i+1)-c%hc(i),i=1,c%sc-1)/)

         ! Additional information
         call initProps(c)
       end subroutine

       subroutine initCopy(c,d)
         implicit none
         type(coordinates),intent(inout) :: c
         type(coordinates),intent(in) :: d
         call delete(c)
         ! Node grid
         c%sn = d%sn
         allocate(c%hn(c%sn))
         allocate(c%dhn(c%sn-1))
         c%hn = d%hn
         c%dhn = d%dhn

         ! Cell center grid
         c%sc = d%sc
         allocate(c%hc(c%sc))
         allocate(c%dhc(c%sc-1))
         c%hc = d%hc
         c%dhc = d%dhc

         ! Additional information
         call initProps(c)
       end subroutine

       subroutine initCellCenter(c,hc)
         ! hc must includes the ghost cell.
         implicit none
         type(coordinates),intent(inout) :: c
         real(cp),dimension(:),intent(in) :: hc
         integer :: i
         call delete(c)
         ! Node grid
         stop 'Coordinates are being init. via cell centers!'

         ! Cell center grid
         c%sc = size(hc)
         allocate(c%hc(c%sc))
         allocate(c%dhc(c%sc-1))
         c%hc = hc
         c%dhc = (/(c%hc(i+1)-c%hc(i),i=1,c%sc-1)/)

         c%sn = c%sc-1
         allocate(c%hn(c%sn))
         allocate(c%dhn(c%sn-1))
         c%hn(1) = c%hc(1) + c%dhc(1)/real(2.0,cp)

         do i = 1,c%sn-1
          c%hn(i+1) = real(2.0,cp)*c%hc(i+1) - c%hn(i)
         enddo

         ! Differences
         c%dhn = (/(c%hn(i+1)-c%hn(i),i=1,c%sn-1)/)

         ! Additional information
         call initProps(c)
       end subroutine

       subroutine initProps(c)
        implicit none
        type(coordinates),intent(inout) :: c
         ! Additional information
         c%dhMin = minval(c%dhn)
         c%dhMax = maxval(c%dhn)
         c%hmin = c%hn(2); c%hmax = c%hn(c%sn-1) ! To account for ghost node
         c%maxRange = c%hmax-c%hmin
         c%N = size(c%hc)-2
       end subroutine

       subroutine restrictCoordinates(r,c)
         ! Restriction for bad cases should only be allowed ~1 time
         ! Multiple restrictions of this type can make the cells
         ! blow up in size and overlap.
         ! Consider making a flag to only allow 1 time or
         ! use warnings to the user about the grid
         implicit none
         type(coordinates),intent(in) :: c
         type(coordinates),intent(inout) :: r
         integer :: i
         if (c%sc.gt.3) then
           ! if (mod(c%sc,2).eq.0) call init(r,(/(c%hn(2*i),i=1,c%sc/2)/),2) ! good case
           if (mod(c%sc,2).eq.0) then
             call init(r,(/(c%hn(2*i),i=1,c%sc/2)/),2)
             call addGhostNodes(r)
           endif

           if (mod(c%sc,2).ne.0) call init(r,(/c%hc(1),(/(c%hc(2*i),i=1,(c%sc-1)/2)/),c%hc(c%sc)/),1) ! bad case
         else; call init(r,c) ! return c
         endif
       end subroutine

       subroutine addGhostNodes(c)
         implicit none
         type(coordinates),intent(inout) :: c
         call init(c,(/c%hn(1)-c%dhn(1),c%hn,c%hn(c%sn)+(c%dhn(c%sn-1))/),2)
       end subroutine

#ifdef _CHECK_GRID_
       subroutine checkCoordinates(c)
         implicit none
         type(coordinates),intent(in) :: c
         integer :: i
         real(cp) :: tol
         ! Check if consectutive
         do i=1,c%sn-1
           if (c%hn(i+1)-c%hn(i).lt.c%dhMin/real(2.0,cp)) then
              write(*,*) 'i,dh',i,c%hn(i+1)-c%hn(i)
              write(*,*) 'hn = ',c%hn
              stop 'Error: coordinates are not consecutive.'
           endif
         enddo
         ! Check if cell centeres are in cell center
         tol = c%dhMin*real(1.0**(-6.0),cp)
         do i=1,c%sn-1
           if (abs((c%hc(i)-c%hn(i))-(c%hn(i+1)-c%hc(i))).gt.tol) then
              write(*,*) 'Cell centers are not centered'
              write(*,*) 'i = ',i
              write(*,*) 'hn = ',c%hn
              write(*,*) 'hc = ',c%hc
              stop 'Error: cell centeres are not in cell centers.'
           endif
         enddo
       end subroutine
#endif

       subroutine exportCoordinates(c,dir,name,u)
         implicit none
         type(coordinates), intent(in) :: c
         character(len=*),intent(in) :: dir,name
         integer,intent(in),optional :: u
         integer :: newU
         if (present(u)) then
           call addToFile(c,u)
         else
           newU = newAndOpen(dir,name)
           call addToFile(c,newU)
           close(newU)
         endif
       end subroutine

       subroutine printCoordinates(c)
         implicit none
         type(coordinates),intent(in) :: c
         call addToFile(c,6)
       end subroutine

       subroutine addToFileCoordinates(c,u)
         implicit none
         type(coordinates), intent(in) :: c
         integer,intent(in) :: u
         write(u,*) '------- coordinates -------'
         write(u,*) 'sn = ',c%sn
         write(u,*) 'sc = ',c%sc
         write(u,*) 'hn = ',c%hn
         write(u,*) 'hc = ',c%hc
         write(u,*) 'dhn = ',c%dhn
         write(u,*) 'dhc = ',c%dhc
       end subroutine

       end module