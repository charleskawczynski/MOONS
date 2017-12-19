       module init_Ustar_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_extend_mod
       use VF_extend_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_Ustar_field

       contains

       subroutine init_Ustar_field(Ustar,U)
         implicit none
         type(VF),intent(inout) :: Ustar
         type(VF),intent(in) :: U
         call assign(Ustar,U)
       end subroutine

       end module