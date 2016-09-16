       module simple_int_tensor_mod
       use current_precision_mod
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
         select case (dir)
         case (1); T%eye = (/1,0,0/)
         case (2); T%eye = (/0,1,0/)
         case (3); T%eye = (/0,0,1/)
         case default; stop 'Error: dir must = 1,2,3 in simple_int_tensor.f90'
         end select
       end subroutine

       end module