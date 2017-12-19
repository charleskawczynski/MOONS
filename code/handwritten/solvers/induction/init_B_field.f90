       module init_B_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_extend_mod
       use VF_extend_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_B_field

       contains

       subroutine init_B_field(B,SP)
         implicit none
         type(VF),intent(inout) :: B
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call initZeroField(B)

         preset_ID = SP%VS%B%IC
         ! preset_ID = 0 ! manual override

         select case(preset_ID)
         case (0)
         case default; stop 'Error: bad preset_ID in init_B_field.f90'
         end select
       end subroutine

       subroutine initZeroField(B)
         implicit none
         type(VF),intent(inout) :: B
         call assign(B,0.0_cp)
       end subroutine

       end module