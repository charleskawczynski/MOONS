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

       subroutine update_EF(EF,t,n_step,substep)
         implicit none
         type(export_frequency),intent(inout) :: EF
         integer(li),intent(in) :: n_step
         real(cp),intent(in) :: t
         logical,intent(in) :: substep
         call update(EF%info,t,n_step,substep)
         call update(EF%unsteady_0D,t,n_step,substep)
         call update(EF%unsteady_1D,t,n_step,substep)
         call update(EF%unsteady_2D,t,n_step,substep)
         call update(EF%unsteady_3D,t,n_step,substep)
         call update(EF%final_solution,t,n_step,substep)
         call update(EF%restart_files,t,n_step,substep)
       end subroutine

       end module