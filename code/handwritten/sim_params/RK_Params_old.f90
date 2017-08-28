       module RK_Params_mod
       ! Pre-processor directives: (_DEBUG_RK_PARAMS_)
       use current_precision_mod
       implicit none

       private
       public :: RK_Params
       public :: init,delete,export,import,display,print
       public :: assign_stage
       public :: update_time

       type RK_Params
         logical :: active = .false.
         integer :: n_stages = 0
         integer :: n = 0
         real(cp),dimension(:),allocatable :: a
         real(cp),dimension(:),allocatable :: b
         real(cp),dimension(:),allocatable :: c
         real(cp),dimension(:),allocatable :: d
       end type

       interface init;               module procedure init_RKP;               end interface
       interface init;               module procedure init_copy_RKP;          end interface
       interface delete;             module procedure delete_RKP;             end interface
       interface export;             module procedure export_RKP;             end interface
       interface import;             module procedure import_RKP;             end interface
       interface display;            module procedure display_RKP;            end interface
       interface print;              module procedure print_RKP;              end interface
       interface print;              module procedure print_RKP_location;     end interface
       interface assign_stage;       module procedure assign_stage_RKP;       end interface
       interface update_time;        module procedure update_time_RKP;        end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_RKP(RKP,n_stages,active)
         ! Coefficients taken from:
         ! Lundbladh, Anders, et al. "An efficient spectral method
         ! for simulation of incompressible flow over a flat plate."
         ! Trita-mek. Tech. Rep 11 (1999).
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer,intent(in) :: n_stages
         logical,intent(in) :: active
         call delete(RKP)
         RKP%active = active
         if (active) then; RKP%n_stages = n_stages
         else;             RKP%n_stages = 1
         endif
         call verify_valid_order(n_stages,'init_RKP')
         RKP%n = 1
         allocate(RKP%a(RKP%n_stages))
         allocate(RKP%b(RKP%n_stages))
         allocate(RKP%c(RKP%n_stages))
         allocate(RKP%d(RKP%n_stages))
         if (.not.active) then
         RKP%a = 1.0_cp
         RKP%b = 1.0_cp
         RKP%c = 1.0_cp
         RKP%d = 1.0_cp
         elseif (RKP%n_stages.eq.3) then
         RKP%a(1) = 8.0_cp/15.0_cp;  RKP%b(1) =  0.0_cp        ; RKP%c(1) = 0.0_cp
         RKP%a(2) = 5.0_cp/12.0_cp;  RKP%b(2) =-17.0_cp/60.0_cp; RKP%c(2) = 8.0_cp/15.0_cp
         RKP%a(3) = 3.0_cp/ 4.0_cp;  RKP%b(3) =- 5.0_cp/12.0_cp; RKP%c(3) = 2.0_cp/ 3.0_cp
         RKP%d = RKP%a+RKP%b
         elseif (RKP%n_stages.eq.4) then
         RKP%a(1) =  8.0_cp/17.0_cp; RKP%b(1) =  0.0_cp        ; RKP%c(1) = 0.0_cp
         RKP%a(2) = 17.0_cp/60.0_cp; RKP%b(2) =-15.0_cp/68.0_cp; RKP%c(2) = 8.0_cp/17.0_cp
         RKP%a(3) =  5.0_cp/12.0_cp; RKP%b(3) =-17.0_cp/60.0_cp; RKP%c(3) = 8.0_cp/15.0_cp
         RKP%a(4) =  3.0_cp/ 4.0_cp; RKP%b(4) =- 5.0_cp/12.0_cp; RKP%c(4) = 2.0_cp/ 3.0_cp
         RKP%d = RKP%a+RKP%b
         else; stop 'Error: bad input to init_RKP in RK_Params.f90'
         endif
       end subroutine

       subroutine init_copy_RKP(RKP,RKP_in)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         type(RK_Params),intent(in) :: RKP_in
         call delete(RKP)
         RKP%active = RKP_in%active
         RKP%n = RKP_in%n
         RKP%n_stages = RKP_in%n_stages
         if (allocated(RKP_in%a)) then
           allocate(RKP%a(RKP_in%n_stages))
           RKP%a = RKP_in%a
         endif
         if (allocated(RKP_in%b)) then
           allocate(RKP%b(RKP_in%n_stages))
           RKP%b = RKP_in%b
         endif
         if (allocated(RKP_in%c)) then
           allocate(RKP%c(RKP_in%n_stages))
           RKP%c = RKP_in%c
         endif
         if (allocated(RKP_in%d)) then
           allocate(RKP%d(RKP_in%n_stages))
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
         RKP%active = .false.
         RKP%n = 0
         RKP%n_stages = 0
       end subroutine

       subroutine export_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(in) :: RKP
         integer,intent(in) :: un
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         write(un,*) ' ------------- RK_Params ------------- '
         write(un,*) 'n_stages      = '; write(un,*) RKP%n_stages
         write(un,*) 'n             = '; write(un,*) RKP%n
         write(un,*) 'active        = '; write(un,*) RKP%active
         write(un,*) 'a             = '; write(un,*) RKP%a
         write(un,*) 'b             = '; write(un,*) RKP%b
         write(un,*) 'c             = '; write(un,*) RKP%c
         write(un,*) 'd             = '; write(un,*) RKP%d
         write(un,*) ' ------------------------------------------------ '
       end subroutine

       subroutine import_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer,intent(in) :: un
         call delete(RKP)
         read(un,*);
         read(un,*); read(un,*) RKP%n_stages
         read(un,*); read(un,*) RKP%n
         allocate(RKP%a(RKP%n_stages))
         allocate(RKP%b(RKP%n_stages))
         allocate(RKP%c(RKP%n_stages))
         allocate(RKP%d(RKP%n_stages))
         read(un,*); read(un,*) RKP%active
         read(un,*); read(un,*) RKP%a
         read(un,*); read(un,*) RKP%b
         read(un,*); read(un,*) RKP%c
         read(un,*); read(un,*) RKP%d
         read(un,*);
       end subroutine

       subroutine display_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(in) :: RKP
         integer,intent(in) :: un
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         write(un,*) 'active = ',RKP%active
         write(un,*) 'a = ',RKP%a
         write(un,*) 'b = ',RKP%b
         write(un,*) 'c = ',RKP%c
         write(un,*) 'd = ',RKP%d
         write(un,*) 'n = ',RKP%n
         write(un,*) 'n_stages = ',RKP%n_stages
       end subroutine

       subroutine print_RKP(RKP)
         implicit none
         type(RK_Params),intent(in) :: RKP
         call display(RKP,6)
       end subroutine

       subroutine print_RKP_location(RKP,message)
         implicit none
         character(len=*),intent(in) :: message
         type(RK_Params),intent(in) :: RKP
         write(*,*) message
         call display(RKP,6)
       end subroutine

       subroutine update_time_RKP(RKP,t,dt)
         implicit none
         type(RK_Params),intent(in) :: RKP
         real(cp),intent(inout) :: t
         real(cp),intent(in) :: dt
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         if (RKP%active) then; t = t + RKP%c(RKP%n)*dt
         else;                 t = t + dt
         endif
       end subroutine

       subroutine assign_stage_RKP(RKP,n)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer,intent(in) :: n
         RKP%n = n
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

       subroutine verify_valid_order(n_stages,location)
         implicit none
         character(len=*),intent(in) :: location
         integer,intent(in) :: n_stages
         if ((n_stages.ne.3).and.(n_stages.ne.4)) then
           write(*,*) 'Error: bad n_stages in '//location
           write(*,*) 'Done in RK_Params.f90'
         endif
       end subroutine

       end module