       module RK_Params_mod
       ! Pre-processor directives: (_DEBUG_RK_PARAMS_)
       use array_mod
       use array_extend_mod
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
         type(array) :: gamma
         type(array) :: zeta
         type(array) :: alpha
         type(array) :: beta
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
         call init(RKP%gamma,RKP%n_stages)
         call init(RKP%zeta,RKP%n_stages)
         call init(RKP%alpha,RKP%n_stages)
         call init(RKP%beta,RKP%n_stages)
         if (.not.RK_active) then
         RKP%gamma%f = 1.0_cp
         RKP%zeta%f = 1.0_cp
         RKP%alpha%f = 1.0_cp
         RKP%beta%f = 1.0_cp
         elseif (RKP%n_stages.eq.3) then
         RKP%gamma%f(1) = 8.0_cp/15.0_cp;  RKP%zeta%f(1) =  0.0_cp        ; RKP%alpha%f(1) = 4.0_cp/15.0_cp
         RKP%gamma%f(2) = 5.0_cp/12.0_cp;  RKP%zeta%f(2) =-17.0_cp/60.0_cp; RKP%alpha%f(2) = 1.0_cp/15.0_cp
         RKP%gamma%f(3) = 3.0_cp/ 4.0_cp;  RKP%zeta%f(3) =- 5.0_cp/12.0_cp; RKP%alpha%f(3) = 1.0_cp/ 6.0_cp
         RKP%beta%f = RKP%alpha%f
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
         call init(RKP%gamma,RKP_in%gamma)
         call init(RKP%zeta,RKP_in%zeta)
         call init(RKP%alpha,RKP_in%alpha)
         call init(RKP%beta,RKP_in%beta)
       end subroutine

       subroutine delete_RKP(RKP)
         implicit none
         type(RK_Params),intent(inout) :: RKP
         call delete(RKP%gamma)
         call delete(RKP%zeta)
         call delete(RKP%alpha)
         call delete(RKP%beta)
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
         call export(RKP%gamma,un)
         call export(RKP%zeta,un)
         call export(RKP%alpha,un)
         call export(RKP%beta,un)
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
         call import(RKP%gamma,un)
         call import(RKP%zeta,un)
         call import(RKP%alpha,un)
         call import(RKP%beta,un)
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
         call display(RKP%gamma,un)
         call display(RKP%zeta,un)
         call display(RKP%alpha,un)
         call display(RKP%beta,un)
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
         if (RKP%RK_active) then; t = t + RKP%alpha%f(RKP%n)*dt
         else;                    t = t + dt
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
         call insist_allocated(RKP%gamma,'insist_allocated_RKP')
         call insist_allocated(RKP%zeta,'insist_allocated_RKP')
         call insist_allocated(RKP%alpha,'insist_allocated_RKP')
         call insist_allocated(RKP%beta,'insist_allocated_RKP')
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