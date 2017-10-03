       module export_frequency_extend_mod
       use export_frequency_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       use export_frequency_params_mod
       use export_frequency_params_extend_mod
       implicit none

       private
       public :: update
       interface update;  module procedure update_EF;         end interface

       contains

       subroutine update_EF(EF,n_step,substep)
         implicit none
         type(export_frequency),intent(inout) :: EF
         integer(li),intent(in) :: n_step
         logical,intent(in) :: substep
         call update(EF%info,n_step,substep)
         call update(EF%unsteady_0D,n_step,substep)
         call update(EF%unsteady_1D,n_step,substep)
         call update(EF%unsteady_2D,n_step,substep)
         call update(EF%unsteady_3D,n_step,substep)
         call update(EF%final_solution,n_step,substep)
         call update(EF%restart_files,n_step,substep)
       end subroutine

       end module