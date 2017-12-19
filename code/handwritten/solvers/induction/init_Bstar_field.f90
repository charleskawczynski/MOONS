       module init_Bstar_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_extend_mod
       use VF_extend_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_Bstar_field

       contains

       subroutine init_Bstar_field(Bstar,B)
         implicit none
         type(VF),intent(inout) :: Bstar
         type(VF),intent(in) :: B
         call assign(Bstar,B)
       end subroutine

       end module