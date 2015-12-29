      module CG_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_BCs_faces_mod
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

      interface modify_forcing1;             module procedure modify_forcing1_SF;              end interface
      interface modify_forcing1;             module procedure modify_forcing1_VF;              end interface

      interface zeroGhostPoints_conditional; module procedure zeroGhostPoints_conditional_SF;  end interface
      interface zeroGhostPoints_conditional; module procedure zeroGhostPoints_conditional_VF;  end interface

      contains

      subroutine solve_CG_SF(operator,operator_explicit,x,b,vol,k,m,MFP,n,&
        norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter)
        implicit none
        external :: operator,operator_explicit
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
        call multiply(r,b,vol)

        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        if (.not.x%is_CC) call modify_forcing1(r,m,x)
        call assign(tempx,0.0_cp)
        call apply_BCs(tempx,m,x)
        call zeroGhostPoints_conditional(tempx)
        call operator_explicit(Ax,tempx,k,m,MFP,tempk)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call zeroWall_conditional(Ax,m,x)
        call subtract(r,Ax)
        ! ----------------------------------------------------------

        call operator(Ax,x,k,m,MFP,tempk)
        call multiply(Ax,vol)
        if (x%all_Neumann) call subtract_physical_mean(r)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call assign(p,r)
        rsold = dot_product(r,r,m,x,tempx)
        do i=1,n
          call operator(Ax,p,k,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rsold/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha)
          call apply_BCs(x,m) ! Needed for PPE
          N_iter = N_iter + 1
          call add_product(r,Ax,-alpha)
          call zeroGhostPoints(r)
          rsnew = dot_product(r,r,m,x,tempx)
          if (sqrt(rsnew).lt.tol) then; exit; endif

#ifdef _EXPORT_CG_CONVERGENCE_
          call zeroGhostPoints(r)
          call compute(norm,r); write(un,*) N_iter,norm%L1,norm%L2,norm%Linf

#endif
          call multiply(p,rsnew/rsold)
          call add(p,r)
          call zeroGhostPoints(p)
          rsold = rsnew
        enddo

        if (compute_norms) then
          call operator_explicit(Ax,x,k,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          if (x%all_Neumann) call subtract_physical_mean(r)
          call subtract(r,Ax)
          call zeroWall_conditional(r,m,x)
          call zeroGhostPoints(r)
          call compute(norm,r); call print(norm,'CG Residuals')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'CG iterations (executed/max) = ',i-1,n
        endif
      end subroutine
      
      subroutine solve_CG_VF(operator,operator_explicit,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter)
        implicit none
        external :: operator,operator_explicit
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
        call multiply(r,b,vol)

        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        call modify_forcing1(r,m,x)
        call assign(tempx,0.0_cp)
        call apply_BCs(tempx,m,x)
        call zeroGhostPoints_conditional(tempx)
        call operator_explicit(Ax,tempx,k,m,MFP,tempk)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call zeroWall_conditional(Ax,m,x)
        call subtract(r,Ax)
        ! ----------------------------------------------------------

        call operator(Ax,x,k,m,MFP,tempk)
        call multiply(Ax,vol)
        ! if (x%all_Neumann) call subtract_physical_mean(r)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call assign(p,r)
        rsold = dot_product(r,r,m,x,tempx)
        do i=1,n
          call operator(Ax,p,k,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rsold/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha)
          call apply_BCs(x,m)
          N_iter = N_iter + 1
          call add_product(r,Ax,-alpha)
          call zeroGhostPoints(r)
          rsnew = dot_product(r,r,m,x,tempx)
          if (sqrt(rsnew).lt.tol) then; exit; endif

#ifdef _EXPORT_CG_CONVERGENCE_
          call zeroGhostPoints(r)
          call compute(norm,r%x); write(un(1),*) N_iter,norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y); write(un(2),*) N_iter,norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z); write(un(3),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call multiply(p,rsnew/rsold)
          call add(p,r)
          call zeroGhostPoints(p)
          rsold = rsnew
        enddo

        if (compute_norms) then
          call operator_explicit(Ax,x,k,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x); call print(norm,'CG Residuals (x)')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un(1),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%y); call print(norm,'CG Residuals (y)')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un(2),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%z); call print(norm,'CG Residuals (z)')
#ifndef _EXPORT_CG_CONVERGENCE_
          write(un(3),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'CG iterations (executed/max) = ',i-1,n
        endif
      end subroutine

      subroutine modify_forcing1_SF(f,m,x)
        implicit none
        type(SF),intent(inout) :: f
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        integer :: i
        if (Node_along(f,1)) then
          do i=1,f%s
            if (m%g(i)%c(1)%N.gt.1) then
              if (x%RF(i)%b%f(1)%b%Neumann) then
                f%RF(i)%f(2,:,:) = f%RF(i)%f(2,:,:)/2.0_cp
              elseif (x%RF(i)%b%f(1)%b%Dirichlet) then
                f%RF(i)%f(2,:,:) = 0.0_cp
              endif
              if (x%RF(i)%b%f(2)%b%Neumann) then
                f%RF(i)%f(f%RF(i)%s(1)-1,:,:) = f%RF(i)%f(f%RF(i)%s(1)-1,:,:)/2.0_cp
              elseif (x%RF(i)%b%f(2)%b%Dirichlet) then
                f%RF(i)%f(f%RF(i)%s(1)-1,:,:) = 0.0_cp
              endif
            endif
          enddo
        endif
        if (Node_along(f,2)) then
          do i=1,f%s
            if (m%g(i)%c(2)%N.gt.1) then
              if (x%RF(i)%b%f(3)%b%Neumann) then
                f%RF(i)%f(:,2,:) = f%RF(i)%f(:,2,:)/2.0_cp
              elseif (x%RF(i)%b%f(3)%b%Dirichlet) then
                f%RF(i)%f(:,2,:) = 0.0_cp
              endif
              if (x%RF(i)%b%f(4)%b%Neumann) then
                f%RF(i)%f(:,f%RF(i)%s(2)-1,:) = f%RF(i)%f(:,f%RF(i)%s(2)-1,:)/2.0_cp
              elseif (x%RF(i)%b%f(4)%b%Dirichlet) then
                f%RF(i)%f(:,f%RF(i)%s(2)-1,:) = 0.0_cp
              endif
            endif
          enddo
        endif
        if (Node_along(f,3)) then
          do i=1,f%s
            if (m%g(i)%c(3)%N.gt.1) then
              if (x%RF(i)%b%f(5)%b%Neumann) then
                f%RF(i)%f(:,:,2) = f%RF(i)%f(:,:,2)/2.0_cp
              elseif (x%RF(i)%b%f(5)%b%Dirichlet) then
                f%RF(i)%f(:,:,2) = 0.0_cp
              endif
              if (x%RF(i)%b%f(6)%b%Neumann) then
                f%RF(i)%f(:,:,f%RF(i)%s(3)-1) = f%RF(i)%f(:,:,f%RF(i)%s(3)-1)/2.0_cp
              elseif (x%RF(i)%b%f(6)%b%Dirichlet) then
                f%RF(i)%f(:,:,f%RF(i)%s(3)-1) = 0.0_cp
              endif
            endif
          enddo
        endif
      end subroutine

      subroutine modify_forcing1_VF(f,m,x)
        implicit none
        type(VF),intent(inout) :: f
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x
        call modify_forcing1_SF(f%x,m,x%x)
        call modify_forcing1_SF(f%y,m,x%y)
        call modify_forcing1_SF(f%z,m,x%z)
      end subroutine

      subroutine zeroGhostPoints_conditional_SF(x)
        implicit none
        type(SF),intent(inout) :: x
        integer :: i
        if (Node_along(x,1)) then
          do i=1,x%s
            x%RF(i)%f(1,:,:) = 0.0_cp; x%RF(i)%f(x%RF(i)%s(1),:,:) = 0.0_cp
          enddo
        endif
        if (Node_along(x,2)) then
          do i=1,x%s
            x%RF(i)%f(:,1,:) = 0.0_cp; x%RF(i)%f(:,x%RF(i)%s(2),:) = 0.0_cp
          enddo
        endif
        if (Node_along(x,3)) then
          do i=1,x%s
            x%RF(i)%f(:,:,1) = 0.0_cp; x%RF(i)%f(:,:,x%RF(i)%s(3)) = 0.0_cp
          enddo
        endif
      end subroutine

      subroutine zeroGhostPoints_conditional_VF(x)
        implicit none
        type(VF),intent(inout) :: x
        call zeroGhostPoints_conditional_SF(x%x)
        call zeroGhostPoints_conditional_SF(x%y)
        call zeroGhostPoints_conditional_SF(x%z)
      end subroutine

      end module