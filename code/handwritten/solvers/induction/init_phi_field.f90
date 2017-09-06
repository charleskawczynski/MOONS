       module init_phi_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_extend_mod
       use SF_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_phi_field

       contains

       subroutine init_phi_field(phi,SP)
         implicit none
         type(SF),intent(inout) :: phi
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call initZeroField(phi)

         preset_ID = SP%VS%phi%IC
         ! preset_ID = 0 ! manual override

         select case(preset_ID)
         case (0)
         case default; stop 'Error: bad preset_ID in init_phi_field.f90'
         end select
       end subroutine

       subroutine initZeroField(phi)
         implicit none
         type(SF),intent(inout) :: phi
         call assign(phi,0.0_cp)
       end subroutine

       end module