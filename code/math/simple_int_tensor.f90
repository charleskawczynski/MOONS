       module simple_int_tensor_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       implicit none

       private
       public :: init
       public :: simple_int_tensor

       type simple_int_tensor
         integer,dimension(3) :: eye
       end type

       interface init;   module procedure init_eye;   end interface

       contains

       subroutine init_eye(T,dir)
         implicit none
         type(simple_int_tensor),intent(inout) :: T
         integer,intent(in) :: dir
         T%eye = eye_given_dir(dir)
       end subroutine

       end module