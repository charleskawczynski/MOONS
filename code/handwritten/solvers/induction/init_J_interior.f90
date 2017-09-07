       module init_J_interior_mod
       use current_precision_mod
       use mesh_extend_mod
       use mesh_domain_mod
       use VF_extend_mod
       use IO_import_mod
       implicit none

       private
       public :: initJ_interior

       contains

       subroutine initJ_interior(J,m,MD,dir)
         implicit none
         type(VF),intent(inout) :: J
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         character(len=*),intent(in) :: dir
         type(mesh) :: temp
         call init_other(temp,m,MD)
         call import_3D_1C(temp,J%x,dir,'J_interior_e_x',0)
         call import_3D_1C(temp,J%y,dir,'J_interior_e_y',0)
         call import_3D_1C(temp,J%z,dir,'J_interior_e_z',0)
         call delete(temp)
       end subroutine

       end module
