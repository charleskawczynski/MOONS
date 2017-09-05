       module apply_face_BC_op_mod
       use current_precision_mod
       use grid_field_mod
       use face_SD_mod
       implicit none
       private
       public :: apply_face_BC_op
       abstract interface
         subroutine apply_face_BC_op(GF,surf,FSD,face)
           import grid_field,face_SD
           implicit none
           type(grid_field),intent(inout) :: GF
           type(grid_field),intent(in) :: surf
           type(face_SD),intent(in) :: FSD
           integer,intent(in) :: face
         end subroutine
       end interface
       end module