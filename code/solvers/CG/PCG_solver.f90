      module PCG_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_Stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use BCs_mod
      use CG_aux_mod
      use export_raw_processed_mod
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
      public :: solve_PCG
      interface solve_PCG;      module procedure solve_PCG_SF;   end interface
      interface solve_PCG;      module procedure solve_PCG_VF;   end interface

      real(cp) :: tol = 10.0_cp**(-15.0_cp)

      contains

      subroutine solve_PCG_SF(operator,operator_explicit,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        external :: operator,operator_explicit
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        integer,intent(inout) :: N_iter
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        call multiply(r,b,vol)
        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        if (.not.x%is_CC) then
          call assign(tempx,r)
          call modify_forcing1(r,tempx,m,x)
        endif
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

        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx); res_norm = rhok
        do i=1,n
          call operator(Ax,p,k,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha) ! x = x + alpha p
          call apply_BCs(x,m) ! Needed for PPE
          N_iter = N_iter + 1
          call add_product(r,Ax,-alpha) ! x = x - alpha Ap
          call zeroGhostPoints(r)
          res_norm = dot_product(r,r,m,x,tempx)
          if (sqrt(res_norm).lt.tol) then; exit; endif

#ifdef _EXPORT_PCG_CONVERGENCE_
          call zeroGhostPoints(r)
          call compute(norm,r)
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call multiply(p,rhokp1/rhok) ! p = z + beta p
          call add(p,z)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo
        
        if (compute_norms) then
          call operator_explicit(Ax,x,k,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          if (x%all_Neumann) call subtract_physical_mean(r)
          call subtract(r,Ax)
          call zeroWall_conditional(r,m,x)
          call zeroGhostPoints(r)
          call compute(norm,r); call print(norm,'PCG Residuals')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'PCG iterations (executed/max) = ',i-1,n
          write(*,*) 'PCG sqrt(res_norm) = ',sqrt(res_norm)
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,operator_explicit,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        external :: operator,operator_explicit
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        integer,intent(inout) :: N_iter
        integer,dimension(3),intent(in) :: un
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        call multiply(r,b,vol)
        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        call assign(tempx,r)
        call modify_forcing1(r,tempx,m,x)
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
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx); res_norm = rhok
        do i=1,n
          call operator(Ax,p,k,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha) ! x = x + alpha p
          call apply_BCs(x,m) ! Needed for PPE
          N_iter = N_iter + 1
          call add_product(r,Ax,-alpha) ! x = x - alpha Ap
          call zeroGhostPoints(r)
          res_norm = dot_product(r,r,m,x,tempx)
          if (sqrt(res_norm).lt.tol) then; exit; endif

#ifdef _EXPORT_PCG_CONVERGENCE_
          call zeroGhostPoints(r)
          call compute(norm,r%x); write(un(1),*) N_iter,norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y); write(un(2),*) N_iter,norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z); write(un(3),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif

          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call multiply(p,rhokp1/rhok) ! p = z + beta p
          call add(p,z)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo
        
        if (compute_norms) then
          call operator_explicit(Ax,x,k,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          ! if (x%all_Neumann) call subtract_physical_mean(r)
          call subtract(r,Ax)
          call zeroWall_conditional(r,m,x)
          call zeroGhostPoints(r)
          call compute(norm,r%x); call print(norm,'PCG Residuals (x)')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un(1),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%y); call print(norm,'PCG Residuals (y)')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un(2),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%z); call print(norm,'PCG Residuals (z)')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un(3),*) N_iter,norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'PCG iterations (executed/max) = ',i-1,n
          write(*,*) 'PCG sqrt(res_norm) = ',sqrt(res_norm)
        endif
      end subroutine

      end module