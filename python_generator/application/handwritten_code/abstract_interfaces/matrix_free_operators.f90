module matrix_free_operators_mod
use current_precision_mod
use mesh_mod
use SF_mod
use VF_mod
use TF_mod
use matrix_free_params_mod
implicit none

private
public :: op_SF,op_SF_explicit
public :: op_VF,op_VF_explicit

abstract interface
  subroutine op_SF(Ax,x,k,m,MFP,tempk)
    import :: SF,TF,mesh,matrix_free_params
    implicit none
    type(SF),intent(inout) :: Ax,x
    type(TF),intent(in) :: k
    type(TF),intent(inout) :: tempk
    type(mesh),intent(in) :: m
    type(matrix_free_params),intent(in) :: MFP
  end subroutine
end interface

abstract interface
  subroutine op_SF_explicit(Ax,x,k,m,MFP,tempk)
    import :: SF,TF,mesh,matrix_free_params
    implicit none
    type(SF),intent(inout) :: Ax,x
    type(TF),intent(in) :: k
    type(TF),intent(inout) :: tempk
    type(mesh),intent(in) :: m
    type(matrix_free_params),intent(in) :: MFP
  end subroutine
end interface

abstract interface
  subroutine op_VF(Ax,x,k,m,MFP,tempk)
    import :: VF,TF,mesh,matrix_free_params
    implicit none
    type(VF),intent(inout) :: Ax,x
    type(TF),intent(in) :: k
    type(TF),intent(inout) :: tempk
    type(mesh),intent(in) :: m
    type(matrix_free_params),intent(in) :: MFP
  end subroutine
end interface

abstract interface
  subroutine op_VF_explicit(Ax,x,k,m,MFP,tempk)
    import :: VF,TF,mesh,matrix_free_params
    implicit none
    type(VF),intent(inout) :: Ax,x
    type(TF),intent(in) :: k
    type(TF),intent(inout) :: tempk
    type(mesh),intent(in) :: m
    type(matrix_free_params),intent(in) :: MFP
  end subroutine
end interface

end module