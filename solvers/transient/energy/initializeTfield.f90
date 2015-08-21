       module initializeTfield_mod
       use SF_mod
       use grid_mod
       implicit none

       private
       public :: initTfield
       public :: restartT

       logical,parameter :: restartT = .false. ! (induced field)

       integer,parameter :: preDefinedT_ICs = 1 ! NOTE: All cases use B_induced = 0
       !                                      0 : User-defined case (no override)
       !                                      1 : Uniform


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine initTfield(T,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         type(SF),intent(inout) :: T
         integer :: i
         do i=1,T%s
           call initTfield_RF(T%RF(i)%f,g,dir)
         enddo
       end subroutine

       subroutine initTfield_RF(T,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: T
         if (restartT) then
           call initRestartT(T,g,dir)
         elseif (preDefinedT_ICs.ne.0) then
           call initPreDefinedT(T)
         else
           call initUserTfield(T)
         endif
       end subroutine

       subroutine initRestartT(T,g,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: T
         real(cp),dimension(:),allocatable :: xc,yc,zc
         allocate(xc(g%c(1)%sc),yc(g%c(2)%sc),zc(g%c(3)%sc))
         xc = g%c(1)%hc; yc = g%c(2)%hc; zc = g%c(3)%hc
         ! call readFromFile(xc,yc,zc,T,dir//'Tfield/','T')
         deallocate(xc,yc,zc)
       end subroutine

       subroutine initPreDefinedT(T)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: T
         select case (preDefinedT_ICs)
         case (1); call uniformTfield(T)
         case default
           write(*,*) 'Incorrect preDefinedT_ICs case in initTfield.'; stop
         end select
       end subroutine

       subroutine uniformTfield(T)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: T
         T = real(0.0,cp)
       end subroutine

       subroutine initUserTfield(T)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: T
         call uniformTfield(T)
       end subroutine

       end module
