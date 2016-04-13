      module Jacobi_mod
      use mesh_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_SF_mod
      use IO_tools_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      use Jacobi_solver_mod
      use matrix_mod

      implicit none

      private
      public :: Jacobi
      public :: init,delete
      public :: solve

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type Jacobi
        type(mesh) :: m
        type(VF) :: Ax,res,Dinv,D,k,tempk,vol
        type(norms) :: norm
        integer :: un ! unit to export norm
        procedure(op_VF),pointer,nopass :: operator
      end type
      
      interface init;        module procedure init_Jacobi;       end interface
      interface solve;       module procedure solve_Jacobi;      end interface
      interface delete;      module procedure delete_Jacobi;     end interface

      contains

      subroutine init_Jacobi(JAC,operator,x,k,m,MFP,dir,name,vizualizeOperator)
        implicit none
        procedure(op_VF) :: operator
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: vizualizeOperator
        call init(JAC%m,m)
        call init(JAC%Ax,x)
        call init(JAC%res,x)
        call init(JAC%Dinv,x)
        call init(JAC%D,x)
        call init_Face(JAC%k,m)
        call init(JAC%tempk,JAC%k)
        call assign(JAC%k,k)
        call init(JAC%vol,x)
        call volume(JAC%vol,m)
        JAC%un = newAndOpen(dir,'norm_JAC_'//name)
        JAC%operator => operator
        call init(JAC%norm)

        call get_diagonal(operator,JAC%D,x,JAC%k,JAC%vol,m,MFP,JAC%tempk)

        if (vizualizeOperator) then
          call export_operator(operator,'JAC_VF_'//name,dir,x,JAC%k,JAC%vol,m,MFP,JAC%tempk)
          call export_matrix(JAC%D,dir,'JAC_VF_diag_'//name)
        endif

        call assign(JAC%Dinv,JAC%D)
        call invert(JAC%Dinv)
      end subroutine

      subroutine solve_Jacobi(JAC,x,f,m,MFP,n,compute_norm)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norm
        type(matrix_free_params),intent(in) :: MFP
        call solve(JAC%operator,x,f,JAC%vol,JAC%k,JAC%Dinv,&
        JAC%D,m,MFP,n,JAC%norm,compute_norm,JAC%un,JAC%Ax,JAC%res,JAC%tempk)
      end subroutine

      subroutine delete_Jacobi(JAC)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        call delete(JAC%m)
        call delete(JAC%vol)
        call delete(JAC%Ax)
        call delete(JAC%res)
        call delete(JAC%Dinv)
        call delete(JAC%D)
        call delete(JAC%k)
        call delete(JAC%tempk)
        call init(JAC%norm)
        JAC%un = 0
      end subroutine

      end module