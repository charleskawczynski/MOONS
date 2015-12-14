      module PSE_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_Stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use BCs_mod
      use SF_mod
      use VF_mod
      use matrix_free_params_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: solve_PSE
      interface solve_PSE;       module procedure solve_PSE_SF;    end interface
      interface solve_PSE;       module procedure solve_PSE_VF;    end interface

      real(cp),parameter :: tol = 10.0_cp**(-15.0_cp)

      contains

      subroutine solve_PSE_SF(operator,x,b,vol,k,m,MFP,n,ds,norm,compute_norms,un,tempk,Ax,r)
        implicit none
        external :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        real(cp),intent(in) :: ds
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: Ax,r
        integer :: i
        call apply_BCs(x,m)
        do i=1,n
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call subtract(r,Ax,b)
          call multiply(r,ds)
          call add(x,r)
          call apply_BCs(x,m)
#ifdef _EXPORT_PSE_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r,m); write(un,*) norm%L1,norm%L2,norm%Linf
#endif
        enddo
        if (x%all_Neumann) call subtract(x,mean(x))
#ifndef _EXPORT_PSE_CONVERGENCE_
        if (compute_norms) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call subtract(r,Ax,b)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r,m); call print(norm,'PSE Residuals')
          write(un,*) norm%L1,norm%L2,norm%Linf
          write(*,*) 'PSE iterations (executed/max) = ',i-1,n
        endif
#endif
      end subroutine
      
      subroutine solve_PSE_VF(operator,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempk,Ax,r)
        implicit none
        external :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        integer,dimension(3),intent(in) :: un
        logical,intent(in) :: compute_norms
        type(VF),intent(inout) :: Ax,r
        integer :: i
        call apply_BCs(x,m)
        do i=1,n
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call subtract(r,Ax,b)
          call multiply(r,ds)
          call add(x,r)
          call apply_BCs(x,m)
#ifdef _EXPORT_PSE_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x,m); write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y,m); write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z,m); write(un(3),*) norm%L1,norm%L2,norm%Linf
#endif
        enddo

        call apply_BCs(x,m)
#ifndef _EXPORT_PSE_CONVERGENCE_
        if (compute_norms) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call subtract(r,Ax,b)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x,m); call print(norm,'PSE Residuals (x)')
          write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y,m); call print(norm,'PSE Residuals (y)')
          write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z,m); call print(norm,'PSE Residuals (z)')
          write(un(3),*) norm%L1,norm%L2,norm%Linf
          write(*,*) 'PSE iterations (executed/max) = ',i-1,n
        endif
#endif
      end subroutine

      end module