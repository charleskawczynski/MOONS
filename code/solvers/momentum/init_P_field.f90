       module init_P_field_mod
       use current_precision_mod
       use IO_import_mod
       use grid_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use GF_mod
       use boundary_conditions_mod
       use ops_discrete_mod
       use sim_params_mod
       implicit none

       private
       public :: init_P_field

       contains

       subroutine init_P_field(p,m,SP)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call assign(P,0.0_cp)

         preset_ID = SP%VS%P%IC
         ! preset_ID = 0 ! manual override

         select case(preset_ID)
         case (0)
         case (1); call initUserPfield(p)
         case (2); call Taylor_Green_Vortex(p,m,SP%DP%Re)
         case default; stop 'Error: bad preset_ID in init_P_field.f90'
         end select
       end subroutine

       subroutine initUserPfield(p)
         implicit none
         type(SF),intent(inout) :: p
         call assign(p,0.0_cp)
       end subroutine

       subroutine Taylor_Green_Vortex(p,m,Re)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re
         type(VF) :: temp_F
         type(SF) :: lapP
         call init(lapP,P)
         call init_Face(temp_F,m)
         call Taylor_Green_Vortex_P(P%BF(1)%GF  ,m%B(1)%g,P%DL  ,Re,0.0_cp)
         call lap_centered(lapP,P,m,temp_F)
         call multiply(lapP,0.5_cp) ! since adding derivatives in 2 directions
         call multiply(lapP,0.25_cp) ! for 2 in cos
         call assign(P,lapP)
         call delete(temp_F)
         call delete(lapP)
       end subroutine

       end module