       module init_P_field_mod
       use current_precision_mod
       use IO_import_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use boundary_conditions_mod
       use sim_params_mod
       implicit none

       private
       public :: init_P_field

       contains

       subroutine init_P_field(p,SP)
         implicit none
         type(SF),intent(inout) :: p
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call assign(P,0.0_cp)

         preset_ID = SP%VS%P%IC
         ! preset_ID = 0 ! manual override

         select case(preset_ID)
         case (0)
         case (1); call initUserPfield(p)
         case default; stop 'Error: bad preset_ID in init_P_field.f90'
         end select
       end subroutine

       subroutine initUserPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       end module