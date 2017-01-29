       module init_Ustar_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_Ustar_field

       contains

       subroutine init_Ustar_field(Ustar,m,U,SP,dir)
         implicit none
         type(VF),intent(inout) :: Ustar
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         if (SP%VS%U%SS%restart) then
               call restart_Ustar(Ustar,m,dir)
         else; call assign(Ustar,U)
         endif
       end subroutine

       subroutine restart_Ustar(Ustar,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: Ustar
         call import_3D_1C(m,Ustar%x,dir,'Ustarf_x',0)
         call import_3D_1C(m,Ustar%y,dir,'Ustarf_y',0)
         call import_3D_1C(m,Ustar%z,dir,'Ustarf_z',0)
       end subroutine

       end module