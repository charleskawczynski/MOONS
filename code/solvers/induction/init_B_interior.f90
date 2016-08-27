       module init_B_interior_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use VF_mod
       use IO_VF_mod
       use IO_SF_mod
       implicit none

       private
       public :: initB_interior

       contains

       subroutine initB_interior(B,m,dir)
         implicit none
         type(VF),intent(inout) :: B
         character(len=*),intent(in) :: dir
         type(mesh),intent(in) :: m
         type(mesh) :: temp
         call init(temp,m)
         call import_3D_1C(temp,B%x,dir,'B_interior_f_x',0)
         call import_3D_1C(temp,B%y,dir,'B_interior_f_y',0)
         call import_3D_1C(temp,B%z,dir,'B_interior_f_z',0)
         call delete(temp)
       end subroutine

       end module
