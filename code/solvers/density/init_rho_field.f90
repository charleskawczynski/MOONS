       module init_rho_field_mod
       use current_precision_mod
       use SF_mod
       use IO_import_mod
       use mesh_mod
       use sim_params_mod
       implicit none

       private
       public :: init_rho_field

       contains

       subroutine init_rho_field(rho,m,SP,dir)
         implicit none
         type(SF),intent(inout) :: rho
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(sim_params),intent(in) :: SP
         integer :: preset_ID

         call assign(rho,1.0_cp)

         preset_ID = SP%VS%rho%IC
         if (SP%VS%rho%SS%restart) then
               call restart_rho(rho,m,dir)
         else
           select case(preset_ID)
           case default; stop 'Error: bad preset_ID in init_rho_field.f90'
           end select
         endif
       end subroutine

       subroutine restart_rho(rho,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: rho
         call import_3D_1C(m,rho,dir,'Tc',0)
       end subroutine

       end module
