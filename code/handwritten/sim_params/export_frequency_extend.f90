       module export_frequency_extend_mod
       use export_frequency_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       use time_marching_params_mod
       use export_frequency_params_mod
       use export_frequency_params_extend_mod
       implicit none

       private
       public :: update
       interface update;  module procedure update_EF;         end interface

       contains

       subroutine update_EF(EF,TMP,substep)
         implicit none
         type(export_frequency),intent(inout) :: EF
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: substep
         call update(EF%info,TMP,substep)
         call update(EF%unsteady_0D,TMP,substep)
         call update(EF%unsteady_1D,TMP,substep)
         call update(EF%unsteady_2D,TMP,substep)
         call update(EF%unsteady_3D,TMP,substep)
         call update(EF%final_solution,TMP,substep)
         call update(EF%restart_files,TMP,substep)
       end subroutine

       end module