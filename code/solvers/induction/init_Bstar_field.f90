       module init_Bstar_field_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_import_mod
       use sim_params_mod
       implicit none

       private
       public :: init_Bstar_field

       contains

       subroutine init_Bstar_field(Bstar,m,B,SP,dir)
         implicit none
         type(VF),intent(inout) :: Bstar
         type(VF),intent(in) :: B
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         if (SP%VS%B%SS%restart) then
               call restart_Bstar(Bstar,m,dir)
         else; call assign(Bstar,B)
         endif
       end subroutine

       subroutine restart_Bstar(Bstar,m,dir)
         implicit none
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: Bstar
         call import_3D_1C(m,Bstar%x,dir,'Bstarf_x',0)
         call import_3D_1C(m,Bstar%y,dir,'Bstarf_y',0)
         call import_3D_1C(m,Bstar%z,dir,'Bstarf_z',0)
       end subroutine

       end module