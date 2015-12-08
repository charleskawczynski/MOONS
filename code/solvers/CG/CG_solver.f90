      module CG_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_Stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use BCs_mod
      use SF_mod
      use VF_mod
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

      contains

      subroutine solve_CG_SF(operator,x,b,vol,c,k,m,n,norm,compute_norms,un,tempx,tempk,Ax,r,p)
        implicit none
        external :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol
        real(cp),intent(in) :: c
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        logical,intent(in) :: compute_norms
        type(SF),intent(inout) :: tempx,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)
        call operator(Ax,x,vol,m,tempk,c,k)
        call multiply(r,b,vol)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call assign(p,r)
        rsold = dot_product(r,r,m,x,tempx)
        do i=1,n
          call operator(Ax,p,vol,m,tempk,c,k)
          alpha_temp = dot_product(p,Ax,m,x,tempx)
          alpha = rsold/alpha_temp
          call assign(tempx,p)
          call multiply(tempx,alpha)
          call add(x,tempx)
          call apply_BCs(x,m)
          call assign(tempx,Ax)
          call multiply(tempx,alpha)
          call subtract(r,tempx)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          rsnew = dot_product(r,r,m,x,tempx)
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif
#ifdef _EXPORT_CG_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r,m); write(un,*) norm%L1,norm%L2,norm%Linf
#endif

          call assign(tempx,p)
          call multiply(tempx,rsnew/rsold)
          call add(p,r,tempx)
          call zeroGhostPoints(p)
          rsold = rsnew
        enddo
        if (x%all_Neumann) call subtract(x,mean(x))
        call apply_BCs(x,m)
#ifndef _EXPORT_CG_CONVERGENCE_
        if (compute_norms) then
          call operator(Ax,x,vol,m,tempk,c,k)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r,m); call print(norm,'CG Residuals')
          write(un,*) norm%L1,norm%L2,norm%Linf
          write(*,*) 'CG iterations (executed/max) = ',i-1,n
        endif
#endif
      end subroutine
      
      subroutine solve_CG_VF(operator,x,b,vol,c,k,m,n,norm,compute_norms,un,tempx,tempk,Ax,r,p)
        implicit none
        external :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol
        real(cp),intent(in) :: c
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        integer,dimension(3),intent(in) :: un
        logical,intent(in) :: compute_norms
        type(VF),intent(inout) :: tempx,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)
        call operator(Ax,x,vol,m,tempk,c,k)
        call multiply(r,b,vol)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call assign(p,r)
        rsold = dot_product(r,r,m,x,tempx)
        do i=1,n
          call operator(Ax,p,vol,m,tempk,c,k)
          alpha_temp = dot_product(p,Ax,m,x,tempx)
          alpha = rsold/alpha_temp
          call assign(tempx,p)
          call multiply(tempx,alpha)
          call add(x,tempx)
          call apply_BCs(x,m)
          call assign(tempx,Ax)
          call multiply(tempx,alpha)
          call subtract(r,tempx)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          rsnew = dot_product(r,r,m,x,tempx)   
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif
#ifdef _EXPORT_CG_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x,m); write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y,m); write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z,m); write(un(3),*) norm%L1,norm%L2,norm%Linf
#endif
          call assign(tempx,p)
          call multiply(tempx,rsnew/rsold)
          call add(p,r,tempx)
          call zeroGhostPoints(p)
          rsold = rsnew
        enddo

        call apply_BCs(x,m)
#ifndef _EXPORT_CG_CONVERGENCE_
        if (compute_norms) then
          call operator(Ax,x,vol,m,tempk,c,k)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x,m); call print(norm,'CG Residuals (x)')
          write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y,m); call print(norm,'CG Residuals (y)')
          write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z,m); call print(norm,'CG Residuals (z)')
          write(un(3),*) norm%L1,norm%L2,norm%Linf
          write(*,*) 'CG iterations (executed/max) = ',i-1,n
        endif
#endif
      end subroutine

      end module