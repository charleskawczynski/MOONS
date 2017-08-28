module plane_op_mod
use current_precision_mod
use grid_field_mod
implicit none

private
public :: plane_op

abstract interface
  subroutine plane_op(GF,val)
    import grid_field,cp
    implicit none
    type(grid_field),intent(inout) :: GF
    real(cp),intent(in) :: val
  end subroutine
end interface

end module