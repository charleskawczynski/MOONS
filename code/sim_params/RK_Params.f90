       module RK_Params_mod
       ! Pre-processor directives: (_DEBUG_RK_PARAMS_)
       use current_precision_mod
       implicit none

       private
       public :: RK_Params
       public :: init,delete,export,import,display,print
       public :: update_stage
       public :: update_time

       type RK_Params
         integer :: RK_order = 0
         real(cp),dimension(:),allocatable :: a
         real(cp),dimension(:),allocatable :: b
         real(cp),dimension(:),allocatable :: c
         real(cp),dimension(:),allocatable :: d
         integer :: n = 0
       end type

       interface init;          module procedure init_RKP;              end interface
       interface init;          module procedure init_copy_RKP;         end interface
       interface delete;        module procedure delete_RKP;            end interface
       interface export;        module procedure export_RKP;            end interface
       interface import;        module procedure import_RKP;            end interface
       interface display;       module procedure display_RKP;           end interface
       interface print;         module procedure print_RKP;             end interface
       interface update_stage;  module procedure update_stage_RKP;      end interface
       interface update_time;   module procedure update_time_RKP;       end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_RKP(RKP,RK_order)
         ! Coefficients taken from:
         ! Lundbladh, Anders, et al. "An efficient spectral method
         ! for simulation of incompressible flow over a flat plate."
         ! Trita-mek. Tech. Rep 11 (1999).
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer,intent(in) :: RK_order
         call delete(RKP)

         call verify_valid_order(RK_order,'init_RKP')
         RKP%n = 1
         RKP%RK_order = RK_order

         allocate(RKP%a(RK_order))
         allocate(RKP%b(RK_order))
         allocate(RKP%c(RK_order))
         allocate(RKP%d(RK_order))

         if (RK_order.eq.3) then
         RKP%a(1) = 8.0_cp/15.0_cp;  RKP%b(1) =  0.0_cp        ; RKP%c(1) = 0.0_cp
         RKP%a(2) = 5.0_cp/12.0_cp;  RKP%b(2) =-17.0_cp/60.0_cp; RKP%c(2) = 8.0_cp/15.0_cp
         RKP%a(3) = 3.0_cp/ 4.0_cp;  RKP%b(3) =- 5.0_cp/12.0_cp; RKP%c(3) = 2.0_cp/ 3.0_cp
         elseif (RK_order.eq.4) then
         RKP%a(1) =  8.0_cp/17.0_cp; RKP%b(1) =  0.0_cp        ; RKP%c(1) = 0.0_cp
         RKP%a(2) = 17.0_cp/60.0_cp; RKP%b(2) =-15.0_cp/68.0_cp; RKP%c(2) = 8.0_cp/17.0_cp
         RKP%a(3) =  5.0_cp/12.0_cp; RKP%b(3) =-17.0_cp/60.0_cp; RKP%c(3) = 8.0_cp/15.0_cp
         RKP%a(4) =  3.0_cp/ 4.0_cp; RKP%b(4) =- 5.0_cp/12.0_cp; RKP%c(4) = 2.0_cp/ 3.0_cp
         endif
         RKP%d = RKP%a+RKP%b
       end subroutine

       subroutine init_copy_RKP(RKP,RKP_in)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         type(RK_Params),intent(in) :: RKP_in
         call delete(RKP)
         RKP%n = RKP_in%n
         RKP%RK_order = RKP_in%RK_order
         if (allocated(RKP_in%a)) then
           allocate(RKP%a(RKP_in%RK_order))
           RKP%a = RKP_in%a
         endif
         if (allocated(RKP_in%b)) then
           allocate(RKP%b(RKP_in%RK_order))
           RKP%b = RKP_in%b
         endif
         if (allocated(RKP_in%c)) then
           allocate(RKP%c(RKP_in%RK_order))
           RKP%c = RKP_in%c
         endif
         if (allocated(RKP_in%d)) then
           allocate(RKP%d(RKP_in%RK_order))
           RKP%d = RKP_in%d
         endif
       end subroutine

       subroutine delete_RKP(RKP)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         if (allocated(RKP%a)) deallocate(RKP%a)
         if (allocated(RKP%b)) deallocate(RKP%b)
         if (allocated(RKP%c)) deallocate(RKP%c)
         if (allocated(RKP%d)) deallocate(RKP%d)
         RKP%n = 0
         RKP%RK_order = 0
       end subroutine

       subroutine export_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(in) :: RKP
         integer,intent(in) :: un
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         write(un,*) ' ------------- RK_Params ------------- '
         write(un,*) 'a = '; write(un,*) RKP%a
         write(un,*) 'b = '; write(un,*) RKP%b
         write(un,*) 'c = '; write(un,*) RKP%c
         write(un,*) 'd = '; write(un,*) RKP%d
         write(un,*) 'n = '; write(un,*) RKP%n
         write(un,*) 'RK_order = '; write(un,*) RKP%RK_order
         write(un,*) ' ------------------------------------------------ '
       end subroutine

       subroutine import_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer,intent(in) :: un
         call delete(RKP)
         read(un,*);
         read(un,*); read(un,*) RKP%a
         read(un,*); read(un,*) RKP%b
         read(un,*); read(un,*) RKP%c
         read(un,*); read(un,*) RKP%d
         read(un,*); read(un,*) RKP%n
         read(un,*); read(un,*) RKP%RK_order
         read(un,*);
       end subroutine

       subroutine display_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(in) :: RKP
         integer,intent(in) :: un
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         write(un,*) 'a = ',RKP%a
         write(un,*) 'b = ',RKP%b
         write(un,*) 'c = ',RKP%c
         write(un,*) 'd = ',RKP%d
         write(un,*) 'n = ',RKP%n
         write(un,*) 'RK_order = ',RKP%RK_order
       end subroutine

       subroutine print_RKP(RKP)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         call display(RKP,6)
       end subroutine

       subroutine update_time_RKP(RKP,t,dt)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         real(cp),intent(inout) :: t
         real(cp),intent(in) :: dt
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         t = t + RKP%c(RKP%n)*dt
       end subroutine

       subroutine update_stage_RKP(RKP,n_step,first_step)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer(li),intent(in) :: n_step
         integer(li),intent(in) :: first_step ! (0 or 1)
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         RKP%n = mod(int(n_step)-int(first_step),RKP%RK_order)+1
       end subroutine

#ifdef _DEBUG_RK_PARAMS_
       subroutine insist_allocated(RKP)
         implicit none
         type(RK_Params),intent(in) :: RKP
         if (.not.allocated(RKP%a)) stop 'a not allocated in RKP in RK_Params.f90'
         if (.not.allocated(RKP%b)) stop 'b not allocated in RKP in RK_Params.f90'
         if (.not.allocated(RKP%c)) stop 'c not allocated in RKP in RK_Params.f90'
         if (.not.allocated(RKP%d)) stop 'd not allocated in RKP in RK_Params.f90'
       end subroutine
#endif

       subroutine verify_valid_order(RK_order,location)
         implicit none
         character(len=*),intent(in) :: location
         integer,intent(in) :: RK_order
         if ((RK_order.ne.3).and.(RK_order.ne.4)) then
           write(*,*) 'Error: bad RK_order in '//location
           write(*,*) 'Done in RK_Params.f90'
         endif
       end subroutine

       end module