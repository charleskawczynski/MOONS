       module RK_Params_mod
       ! Pre-processor directives: (_DEBUG_RK_PARAMS_)
       use array_mod
       use current_precision_mod
       implicit none

       private
       public :: RK_Params
       public :: init,delete,export,import,display,print
       public :: assign_stage
       public :: update_time

       type RK_Params
         logical :: RK_active = .false.
         integer :: n_stages = 0
         integer :: n = 0
         type(array) :: a
         type(array) :: b
         type(array) :: c
         type(array) :: d
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

#ifdef _DEBUG_RK_PARAMS_
       interface insist_allocated;   module procedure insist_allocated_RKP;   end interface
#endif

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_RKP(RKP,n_stages,RK_active)
         ! Coefficients taken from:
         ! Lundbladh, Anders, et al. "An efficient spectral method
         ! for simulation of incompressible flow over a flat plate."
         ! Trita-mek. Tech. Rep 11 (1999).
         implicit none
         type(RK_Params),intent(inout) :: RKP
         integer,intent(in) :: n_stages
         logical,intent(in) :: RK_active
         call delete(RKP)
         RKP%RK_active = RK_active
         if (RK_active) then; RKP%n_stages = n_stages
         else;                RKP%n_stages = 1
         endif
         call verify_valid_order(n_stages,'init_RKP')
         RKP%n = 1
         call init(RKP%a,RKP%n_stages)
         call init(RKP%b,RKP%n_stages)
         call init(RKP%c,RKP%n_stages)
         call init(RKP%d,RKP%n_stages)
         if (.not.RK_active) then
         RKP%a%f = 1.0_cp
         RKP%b%f = 1.0_cp
         RKP%c%f = 1.0_cp
         RKP%d%f = 1.0_cp
         elseif (RKP%n_stages.eq.3) then
         RKP%a%f(1) = 8.0_cp/15.0_cp;  RKP%b%f(1) =  0.0_cp        ; RKP%c%f(1) = 0.0_cp
         RKP%a%f(2) = 5.0_cp/12.0_cp;  RKP%b%f(2) =-17.0_cp/60.0_cp; RKP%c%f(2) = 8.0_cp/15.0_cp
         RKP%a%f(3) = 3.0_cp/ 4.0_cp;  RKP%b%f(3) =- 5.0_cp/12.0_cp; RKP%c%f(3) = 2.0_cp/ 3.0_cp
         RKP%d%f = RKP%a%f+RKP%b%f
         elseif (RKP%n_stages.eq.4) then
         RKP%a%f(1) =  8.0_cp/17.0_cp; RKP%b%f(1) =  0.0_cp        ; RKP%c%f(1) = 0.0_cp
         RKP%a%f(2) = 17.0_cp/60.0_cp; RKP%b%f(2) =-15.0_cp/68.0_cp; RKP%c%f(2) = 8.0_cp/17.0_cp
         RKP%a%f(3) =  5.0_cp/12.0_cp; RKP%b%f(3) =-17.0_cp/60.0_cp; RKP%c%f(3) = 8.0_cp/15.0_cp
         RKP%a%f(4) =  3.0_cp/ 4.0_cp; RKP%b%f(4) =- 5.0_cp/12.0_cp; RKP%c%f(4) = 2.0_cp/ 3.0_cp
         RKP%d%f = RKP%a%f+RKP%b%f
         else; stop 'Error: bad input to init_RKP in RK_Params.f90'
         endif
       end subroutine

       subroutine init_copy_RKP(RKP,RKP_in)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         type(RK_Params),intent(in) :: RKP_in
         call delete(RKP)
         RKP%RK_active = RKP_in%RK_active
         RKP%n = RKP_in%n
         RKP%n_stages = RKP_in%n_stages
         call init(RKP%a,RKP_in%a)
         call init(RKP%b,RKP_in%b)
         call init(RKP%c,RKP_in%c)
         call init(RKP%d,RKP_in%d)
       end subroutine

       subroutine delete_RKP(RKP)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         call delete(RKP%a)
         call delete(RKP%b)
         call delete(RKP%c)
         call delete(RKP%d)
         RKP%RK_active = .false.
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
         write(un,*) 'RK_active     = '; write(un,*) RKP%RK_active
         call export(RKP%a,un)
         call export(RKP%b,un)
         call export(RKP%c,un)
         call export(RKP%d,un)
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
         read(un,*); read(un,*) RKP%RK_active
         call import(RKP%a,un)
         call import(RKP%b,un)
         call import(RKP%c,un)
         call import(RKP%d,un)
         read(un,*);
       end subroutine

       subroutine display_RKP(RKP,un)
         implicit none
         type(RK_Params),intent(in) :: RKP
         integer,intent(in) :: un
#ifdef _DEBUG_RK_PARAMS_
         call insist_allocated(RKP)
#endif
         write(un,*) 'RK_active = ',RKP%RK_active
         write(un,*) 'n = ',RKP%n
         write(un,*) 'n_stages = ',RKP%n_stages
         call display(RKP%a,un)
         call display(RKP%b,un)
         call display(RKP%c,un)
         call display(RKP%d,un)
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
         if (RKP%RK_active) then; t = t + RKP%c%f(RKP%n)*dt
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
       subroutine insist_allocated_RKP(RKP)
         implicit none
         type(RK_Params),intent(in) :: RKP
         call insist_allocated(RKP%a,'insist_allocated_RKP')
         call insist_allocated(RKP%b,'insist_allocated_RKP')
         call insist_allocated(RKP%c,'insist_allocated_RKP')
         call insist_allocated(RKP%d,'insist_allocated_RKP')
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