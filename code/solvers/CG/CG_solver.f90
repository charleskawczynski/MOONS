      module CG_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_Stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use export_raw_processed_mod
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
      public :: solve_CG
      interface solve_CG;       module procedure solve_CG_SF;    end interface
      interface solve_CG;       module procedure solve_CG_VF;    end interface

      real(cp) :: tol = 10.0_cp**(-15.0_cp)

      contains

      subroutine solve_CG_SF(operator,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter)
        implicit none
        external :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        integer,intent(inout) :: N_iter
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew
        call apply_BCs(x,m)
        call operator(Ax,x,k,vol,m,MFP,tempk)
        call multiply(Ax,vol)
        call multiply(r,b,vol)
        if (x%all_Neumann) call subtract_physical_mean(r)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call assign(p,r)
        rsold = dot_product(r,r,m,x,tempx)
        do i=1,n
          call apply_BCs(p,m,x)
          call operator(Ax,p,k,vol,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rsold/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha)
          call apply_BCs(x,m)
          call add_product(r,Ax,-alpha)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          rsnew = dot_product(r,r,m,x,tempx)
          ! if (rsnew.lt.tol) then; exit; endif

#ifdef _EXPORT_CG_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r); write(un,*) N_iter,norm%L1,norm%L2,norm%Linf

#endif
          call multiply(p,rsnew/rsold)
          call add(p,r)
          call zeroGhostPoints(p)
          rsold = rsnew
          N_iter = N_iter + 1
        enddo

        if (compute_norms) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          if (x%all_Neumann) call subtract_physical_mean(r)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r); call print(norm,'CG Residuals')
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf
          write(*,*) 'CG iterations (executed/max) = ',i-1,n
        endif
      end subroutine
      
      subroutine solve_CG_VF(operator,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter)
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
        integer,intent(inout) :: N_iter
        integer,dimension(3),intent(in) :: un
        logical,intent(in) :: compute_norms
        type(VF),intent(inout) :: tempx,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew
        call apply_BCs(x,m)
        call operator(Ax,x,k,vol,m,MFP,tempk)
        call multiply(Ax,vol)
        call multiply(r,b,vol)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call assign(p,r)
        rsold = dot_product(r,r,m,x,tempx)
        do i=1,n
          call apply_BCs(p,m,x)
          call operator(Ax,p,k,vol,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rsold/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha)
          call apply_BCs(x,m)
          call add_product(r,Ax,-alpha)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          rsnew = dot_product(r,r,m,x,tempx)
          ! if (rsnew.lt.tol) then; exit; endif

#ifdef _EXPORT_CG_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x); write(un(1),*) N_iter,norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y); write(un(2),*) N_iter,norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z); write(un(3),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call multiply(p,rsnew/rsold)
          call add(p,r)
          call zeroGhostPoints(p)
          rsold = rsnew
          N_iter = N_iter + 1
        enddo

        if (compute_norms) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x); call print(norm,'CG Residuals (x)')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un(1),*) norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%y); call print(norm,'CG Residuals (y)')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un(2),*) norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%z); call print(norm,'CG Residuals (z)')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un(3),*) norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'CG iterations (executed/max) = ',i-1,n
        endif
      end subroutine

      end module