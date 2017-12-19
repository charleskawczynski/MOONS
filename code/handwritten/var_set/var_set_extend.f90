       module var_set_extend_mod
       use var_set_mod
       use var_mod
       use var_extend_mod
       use iter_solver_params_mod
       use iter_solver_params_extend_mod
       use time_marching_params_mod
       use time_marching_params_extend_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use current_precision_mod
       implicit none

       private
       public :: print_info
       public :: couple_time_step

       public :: assign_beta
       public :: assign_coeff_implicit
       public :: assign_coeff_explicit

       public :: sanity_check

       public :: import_TMP_dt
       public :: import_exit_criteria

       interface print_info;            module procedure print_info_VS;            end interface

       interface couple_time_step;      module procedure couple_time_step_VS;      end interface
       interface assign_beta;           module procedure assign_beta_VS;           end interface
       interface assign_coeff_implicit; module procedure assign_coeff_implicit_VS; end interface
       interface assign_coeff_explicit; module procedure assign_coeff_explicit_VS; end interface

       interface sanity_check;          module procedure sanity_check_VS;          end interface

       interface import_exit_criteria;  module procedure import_exit_criteria_VS;  end interface
       interface import_TMP_dt;         module procedure import_TMP_dt_VS;         end interface

       contains

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

       subroutine import_TMP_dt_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         if (VS%T%SS%initialize)   call import_structured(VS%T%TMP%TS)
         if (VS%U%SS%initialize)   call import_structured(VS%U%TMP%TS)
         if (VS%P%SS%initialize)   call import_structured(VS%P%TMP%TS)
         if (VS%B%SS%initialize)   call import_structured(VS%B%TMP%TS)
         if (VS%B0%SS%initialize)  call import_structured(VS%B0%TMP%TS)
         if (VS%phi%SS%initialize) call import_structured(VS%phi%TMP%TS)
         if (VS%rho%SS%initialize) call import_structured(VS%rho%TMP%TS)
       end subroutine

       subroutine import_exit_criteria_VS(VS)
         implicit none
         type(var_set),intent(inout) :: VS
         if(VS%T%SS%initialize) call import_structured(VS%T%ISP%EC)
         if(VS%U%SS%initialize) call import_structured(VS%U%ISP%EC)
         if(VS%P%SS%initialize) call import_structured(VS%P%ISP%EC)
         if(VS%B%SS%initialize) call import_structured(VS%B%ISP%EC)
         if(VS%B0%SS%initialize) call import_structured(VS%B0%ISP%EC)
         if(VS%phi%SS%initialize) call import_structured(VS%phi%ISP%EC)
         if(VS%rho%SS%initialize) call import_structured(VS%rho%ISP%EC)
       end subroutine

       subroutine print_info_VS(VS)
         implicit none
         type(var_set),intent(in) :: VS
         if (VS%T%SS%initialize)   call print_info(VS%T)
         if (VS%U%SS%initialize)   call print_info(VS%U)
         if (VS%P%SS%initialize)   call print_info(VS%P)
         if (VS%B%SS%initialize)   call print_info(VS%B)
         if (VS%B0%SS%initialize)  call print_info(VS%B0)
         if (VS%phi%SS%initialize) call print_info(VS%phi)
         if (VS%rho%SS%initialize) call print_info(VS%rho)
       end subroutine

       end module