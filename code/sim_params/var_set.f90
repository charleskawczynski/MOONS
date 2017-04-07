       module var_set_mod
       use current_precision_mod
       use var_mod
       use time_marching_params_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       implicit none

       private
       public :: var_set
       public :: init,delete,export,import,display,print

       public :: export_import_SS
       public :: couple_time_step

       public :: assign_beta
       public :: assign_coeff_implicit
       public :: assign_coeff_explicit

       public :: sanity_check

       public :: export_TMP
       public :: import_TMP

       type var_set
         type(var) :: T,U,p,B,B0,phi,rho
       end type

       interface init;                  module procedure init_copy_VS;             end interface
       interface delete;                module procedure delete_VS;                end interface
       interface export;                module procedure export_VS;                end interface
       interface import;                module procedure import_VS;                end interface
       interface display;               module procedure display_VS;               end interface
       interface print;                 module procedure print_VS;                 end interface

       interface export_import_SS;      module procedure export_import_SS_VS;      end interface

       interface couple_time_step;      module procedure couple_time_step_VS;      end interface
       interface assign_beta;           module procedure assign_beta_VS;           end interface
       interface assign_coeff_implicit; module procedure assign_coeff_implicit_VS; end interface
       interface assign_coeff_explicit; module procedure assign_coeff_explicit_VS; end interface

       interface sanity_check;          module procedure sanity_check_VS;          end interface
       interface export_TMP;            module procedure export_TMP_VS;            end interface
       interface import_TMP;            module procedure import_TMP_VS;            end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_copy_VS(VS,VS_in)
         implicit none
         type(var_set),intent(inout) :: VS
         type(var_set),intent(in) :: VS_in
         call init(VS%T,VS_in%T)
         call init(VS%U,VS_in%U)
         call init(VS%P,VS_in%P)
         call init(VS%B,VS_in%B)
         call init(VS%B0,VS_in%B0)
         call init(VS%phi,VS_in%phi)
         call init(VS%rho,VS_in%rho)
       end subroutine

       subroutine delete_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         call delete(VS%T)
         call delete(VS%U)
         call delete(VS%P)
         call delete(VS%B)
         call delete(VS%B0)
         call delete(VS%phi)
         call delete(VS%rho)
       end subroutine

       subroutine export_VS(VS,un)
         implicit none
         type(var_set),intent(in) :: VS
         integer,intent(in) :: un
         write(un,*) ' ---------------- VAR T   ---------------- '; call export(VS%T,un)
         write(un,*) ' ---------------- VAR U   ---------------- '; call export(VS%U,un)
         write(un,*) ' ---------------- VAR P   ---------------- '; call export(VS%P,un)
         write(un,*) ' ---------------- VAR B   ---------------- '; call export(VS%B,un)
         write(un,*) ' ---------------- VAR B0  ---------------- '; call export(VS%B0,un)
         write(un,*) ' ---------------- VAR phi ---------------- '; call export(VS%phi,un)
         write(un,*) ' ---------------- VAR rho ---------------- '; call export(VS%rho,un)
         write(un,*) ' ----------------------------------------- '
       end subroutine

       subroutine import_VS(VS,un)
         implicit none
         type(var_set),intent(inout) :: VS
         integer,intent(in) :: un
         read(un,*); call import(VS%T,un)
         read(un,*); call import(VS%U,un)
         read(un,*); call import(VS%P,un)
         read(un,*); call import(VS%B,un)
         read(un,*); call import(VS%B0,un)
         read(un,*); call import(VS%phi,un)
         read(un,*); call import(VS%rho,un)
         read(un,*);
       end subroutine

       subroutine display_VS(VS,un)
         implicit none
         type(var_set),intent(in) :: VS
         integer,intent(in) :: un
         write(un,*) ' ---------------- VAR T   ---------------- '; call display(VS%T,un)
         write(un,*) ' ---------------- VAR U   ---------------- '; call display(VS%U,un)
         write(un,*) ' ---------------- VAR P   ---------------- '; call display(VS%P,un)
         write(un,*) ' ---------------- VAR B   ---------------- '; call display(VS%B,un)
         write(un,*) ' ---------------- VAR B0  ---------------- '; call display(VS%B0,un)
         write(un,*) ' ---------------- VAR phi ---------------- '; call display(VS%phi,un)
         write(un,*) ' ---------------- VAR rho ---------------- '; call display(VS%rho,un)
         write(un,*) ' ----------------------------------------- '
       end subroutine

       subroutine print_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         call display(VS,6)
       end subroutine

       subroutine export_import_SS_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         if(VS%T%SS%restart) then;  call import(VS%T%ISP);  else;call export(VS%T%ISP);  endif
         if(VS%U%SS%restart) then;  call import(VS%U%ISP);  else;call export(VS%U%ISP);  endif
         if(VS%P%SS%restart) then;  call import(VS%P%ISP);  else;call export(VS%P%ISP);  endif
         if(VS%B%SS%restart) then;  call import(VS%B%ISP);  else;call export(VS%B%ISP);  endif
         if(VS%B0%SS%restart) then; call import(VS%B0%ISP); else;call export(VS%B0%ISP); endif
         if(VS%phi%SS%restart) then;call import(VS%phi%ISP);else;call export(VS%phi%ISP);endif
         if(VS%rho%SS%restart) then;call import(VS%rho%ISP);else;call export(VS%rho%ISP);endif

         if(VS%T%SS%restart) then;  call import(VS%T%TMP);  else;call export(VS%T%TMP);  endif
         if(VS%U%SS%restart) then;  call import(VS%U%TMP);  else;call export(VS%U%TMP);  endif
         if(VS%P%SS%restart) then;  call import(VS%P%TMP);  else;call export(VS%P%TMP);  endif
         if(VS%B%SS%restart) then;  call import(VS%B%TMP);  else;call export(VS%B%TMP);  endif
         if(VS%B0%SS%restart) then; call import(VS%B0%TMP); else;call export(VS%B0%TMP); endif
         if(VS%phi%SS%restart) then;call import(VS%phi%TMP);else;call export(VS%phi%TMP);endif
         if(VS%rho%SS%restart) then;call import(VS%rho%TMP);else;call export(VS%rho%TMP);endif
       end subroutine

       subroutine couple_time_step_VS(VS,coupled)
         implicit none
         type(var_set),intent(inout) :: VS
         type(time_marching_params),intent(in) :: coupled
         call couple_time_step(VS%T%TMP  ,coupled)
         call couple_time_step(VS%U%TMP  ,coupled)
         call couple_time_step(VS%P%TMP  ,coupled)
         call couple_time_step(VS%B%TMP  ,coupled)
         call couple_time_step(VS%B0%TMP ,coupled)
         call couple_time_step(VS%phi%TMP,coupled)
         call couple_time_step(VS%rho%TMP,coupled)
       end subroutine

       subroutine assign_beta_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         VS%B%MFP%beta   = 1.0_cp - VS%B%MFP%alpha ! weight of explicit treatment
         VS%U%MFP%beta   = 1.0_cp - VS%U%MFP%alpha ! weight of explicit treatment
         VS%T%MFP%beta   = 1.0_cp - VS%T%MFP%alpha ! weight of explicit treatment
         VS%phi%MFP%beta = 0.0_cp                  ! Poisson, coefficient unused
         VS%p%MFP%beta   = 0.0_cp                  ! Poisson, coefficient unused
         VS%rho%MFP%beta = 0.0_cp                  ! Poisson, coefficient unused
       end subroutine

       subroutine assign_coeff_explicit_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         VS%B%MFP%coeff_explicit   = VS%B%MFP%coeff_natural*VS%B%MFP%beta ! RHS diffusion coefficient
         VS%U%MFP%coeff_explicit   = VS%U%MFP%coeff_natural*VS%U%MFP%beta ! RHS diffusion coefficient
         VS%T%MFP%coeff_explicit   = VS%T%MFP%coeff_natural*VS%T%MFP%beta ! RHS diffusion coefficient
         VS%phi%MFP%coeff_explicit = 0.0_cp ! Poisson, coefficient unused
         VS%p%MFP%coeff_explicit   = 0.0_cp ! Poisson, coefficient unused
         VS%rho%MFP%coeff_explicit = 0.0_cp ! Poisson, coefficient unused
       end subroutine

       subroutine assign_coeff_implicit_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         VS%B%MFP%coeff_implicit   = -VS%B%MFP%coeff_natural*VS%B%MFP%alpha ! LHS diffusion coefficient
         VS%U%MFP%coeff_implicit   = -VS%U%MFP%coeff_natural*VS%U%MFP%alpha ! LHS diffusion coefficient
         VS%T%MFP%coeff_implicit   = -VS%T%MFP%coeff_natural*VS%T%MFP%alpha ! LHS diffusion coefficient
         VS%phi%MFP%coeff_implicit = 0.0_cp ! Poisson, coefficient unused
         VS%p%MFP%coeff_implicit   = 0.0_cp ! Poisson, coefficient unused
         VS%rho%MFP%coeff_implicit = 0.0_cp ! Poisson, coefficient unused
       end subroutine

       subroutine sanity_check_VS(VS)
         implicit none
         type(var_set),intent(in) :: VS
         if (VS%T%SS%solve  .and.(.not.VS%T%SS%initialize))   stop 'Error: solve but not init? T in var_set.f90'
         if (VS%U%SS%solve  .and.(.not.VS%U%SS%initialize))   stop 'Error: solve but not init? U in var_set.f90'
         if (VS%P%SS%solve  .and.(.not.VS%P%SS%initialize))   stop 'Error: solve but not init? P in var_set.f90'
         if (VS%B%SS%solve  .and.(.not.VS%B%SS%initialize))   stop 'Error: solve but not init? B in var_set.f90'
         if (VS%B0%SS%solve .and.(.not.VS%B0%SS%initialize))  stop 'Error: solve but not init? B0 in var_set.f90'
         if (VS%phi%SS%solve.and.(.not.VS%phi%SS%initialize)) stop 'Error: solve but not init? phi in var_set.f90'
         if (VS%rho%SS%solve.and.(.not.VS%rho%SS%initialize)) stop 'Error: solve but not init? rho in var_set.f90'
         if (window(VS%T%MFP%alpha,0.0_cp,1.0_cp)) stop 'Error: 0<alpha(T)<1 not true in var_set.f90'
         if (window(VS%U%MFP%alpha,0.0_cp,1.0_cp)) stop 'Error: 0<alpha(U)<1 not true in var_set.f90'
         if (window(VS%B%MFP%alpha,0.0_cp,1.0_cp)) stop 'Error: 0<alpha(B)<1 not true in var_set.f90'
       end subroutine

       function window(alpha,min_val,max_val) result(L)
         implicit none
         real(cp),intent(in) :: alpha,min_val,max_val
         logical :: L
         L = (alpha.lt.min_val).or.(alpha.gt.max_val)
       end function

       subroutine export_TMP_VS(VS)
         implicit none
         type(var_set),intent(in) :: VS
         if (VS%T%SS%initialize)   call export(VS%T%TMP)
         if (VS%U%SS%initialize)   call export(VS%U%TMP)
         if (VS%P%SS%initialize)   call export(VS%P%TMP)
         if (VS%B%SS%initialize)   call export(VS%B%TMP)
         if (VS%B0%SS%initialize)  call export(VS%B0%TMP)
         if (VS%phi%SS%initialize) call export(VS%phi%TMP)
         if (VS%rho%SS%initialize) call export(VS%rho%TMP)
       end subroutine

       subroutine import_TMP_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         if (VS%T%SS%initialize)   call import(VS%T%TMP)
         if (VS%U%SS%initialize)   call import(VS%U%TMP)
         if (VS%P%SS%initialize)   call import(VS%P%TMP)
         if (VS%B%SS%initialize)   call import(VS%B%TMP)
         if (VS%B0%SS%initialize)  call import(VS%B0%TMP)
         if (VS%phi%SS%initialize) call import(VS%phi%TMP)
         if (VS%rho%SS%initialize) call import(VS%rho%TMP)
       end subroutine


       end module