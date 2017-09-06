       module init_gravity_field_mod
       use current_precision_mod
       use VF_mod
       use IO_import_mod
       use mesh_extend_mod
       use sim_params_mod
       implicit none

       private
       public :: init_gravity_field

       contains

       subroutine init_gravity_field(gravity,m,SP,dir)
         implicit none
         type(VF),intent(inout) :: gravity
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(sim_params),intent(in) :: SP

         call assign(gravity,0.0_cp)

         if (SP%VS%T%SS%restart) then
           call restart_gravity(gravity,m,dir)
         else
           call uniform_gravity_field(gravity,SP%SCP%uniform_gravity_dir)
         endif
       end subroutine

       subroutine restart_gravity(gravity,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: gravity
         call import_3D_1C(m,gravity%x,dir,'gravityf_x',0)
         call import_3D_1C(m,gravity%y,dir,'gravityf_y',0)
         call import_3D_1C(m,gravity%z,dir,'gravityf_z',0)
       end subroutine

       subroutine uniform_gravity_field(gravity,dir)
         implicit none
         type(VF),intent(inout) :: gravity
         integer,intent(in) :: dir
         select case(dir)
         case (1); call assign(gravity%x,1.0_cp)
         case (2); call assign(gravity%y,1.0_cp)
         case (3); call assign(gravity%z,1.0_cp)
         case default; stop 'Error: bad dir in uniform_gravity_field.f90'
         end select
       end subroutine

       end module