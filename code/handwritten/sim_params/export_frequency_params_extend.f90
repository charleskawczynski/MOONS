       module export_frequency_params_extend_mod
       use export_frequency_params_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none
       private
       public :: init
       public :: update

       integer,parameter :: frequency_coeff_default = 1
       integer,parameter :: frequency_base_default = 10
       integer,parameter :: frequency_exp_default = 6

       interface init;    module procedure init_EFP;      end interface
       interface update;  module procedure update_EFP;    end interface

       contains

       subroutine init_EFP(EFP,export_ever,export_first_step,frequency_coeff,frequency_base,frequency_exp)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         logical,intent(in) :: export_ever,export_first_step
         integer,intent(in) :: frequency_coeff,frequency_base,frequency_exp
         EFP%export_ever       = export_ever
         EFP%export_now        = .false.
         EFP%export_first_step = export_first_step
         EFP%frequency_coeff   = frequency_coeff
         EFP%frequency_base    = frequency_base
         EFP%frequency_exp     = frequency_exp
       end subroutine

       subroutine update_EFP(EFP,n_step,substep)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         integer(li),intent(in) :: n_step
         logical,intent(in) :: substep
         logical :: first_step,past_first_step
         if (EFP%export_ever) then
           first_step = n_step.eq.0
           past_first_step = n_step.gt.0
           EFP%export_now = mod(n_step,EFP%frequency_coeff*EFP%frequency_base**EFP%frequency_exp).eq.1
           if (EFP%export_first_step) then; EFP%export_now = EFP%export_now.or.first_step
           else;                            EFP%export_now = EFP%export_now.and.past_first_step
           endif
         else; EFP%export_now = .false.
         endif
         if (substep) EFP%export_now = .false.
       end subroutine

       end module