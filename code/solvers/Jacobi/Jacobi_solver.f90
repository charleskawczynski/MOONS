      module Jacobi_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_Stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_SF_mod
      use matrix_free_params_mod

      implicit none

      private
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

      interface solve;   module procedure solve_SF;         end interface
      interface solve;   module procedure solve_VF;         end interface

      contains

      subroutine solve_SF(operator,x,f,vol,k,Dinv,D,m,MFP,n,norm,compute_norm,un,Ax,res,tempk)
        implicit none
        external :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: f,Dinv,D,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        integer,intent(in) :: n,un
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norm
        type(SF),intent(inout) :: Ax,res
        integer :: i
        call apply_BCs(x,m) ! Boundaries
        do i=1,n
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call subtract(Ax,D) ! LU = Ax - D
          call subtract(res,f,Ax)
          call multiply(x,Dinv,res)
          call apply_Stitches(x,m)
          call apply_BCs(x,m)
#ifdef _EXPORT_JAC_CONVERGENCE
          call zeroGhostPoints(res)
          call compute(norm,res,m)
          write(un,*) norm%L1,norm%L2,norm%Linf
#endif
        enddo
        if (x%all_neumann) call subtract(x,mean(x))
#ifndef _EXPORT_JAC_CONVERGENCE_
        if (compute_norm) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          write(*,*) 'Jacobi iterations = ',n
          call subtract(res,Ax,f)
          call zeroGhostPoints(res)
          call compute(norm,res,m)
          call print(norm,'Jacobi residuals')
          write(un,*) norm%L1,norm%L2,norm%Linf
        endif
#endif
      end subroutine

      subroutine solve_VF(operator,x,f,vol,k,Dinv,D,m,MFP,n,norm,compute_norm,un,Ax,res,tempk)
        implicit none
        external :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: f,k,Dinv,D,vol
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        integer,intent(in) :: n
        integer,dimension(3),intent(in) :: un
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norm
        type(VF),intent(inout) :: Ax,res
        integer :: i
        call apply_BCs(x,m) ! Boundaries
        do i=1,n
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call subtract(Ax,D) ! LU = Ax - D
          call subtract(res,f,Ax)
          call multiply(x,Dinv,res)
          call apply_Stitches(x,m)
          call apply_BCs(x,m)
#ifdef _EXPORT_JAC_CONVERGENCE_
          call zeroGhostPoints(res)
          call compute(norm,res%x,m); write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,res%y,m); write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,res%z,m); write(un(3),*) norm%L1,norm%L2,norm%Linf
#endif
        enddo

#ifndef _EXPORT_JAC_CONVERGENCE_
        if (compute_norm) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          write(*,*) 'Jacobi iterations = ',n
          call subtract(res,Ax,f)
          call zeroGhostPoints(res)
          call compute(norm,res%x,m); call print(norm,'Jacobi residuals (x)')
          write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,res%y,m); call print(norm,'Jacobi residuals (y)')
          write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,res%z,m); call print(norm,'Jacobi residuals (z)')
          write(un(3),*) norm%L1,norm%L2,norm%Linf
        endif
#endif
      end subroutine

      end module