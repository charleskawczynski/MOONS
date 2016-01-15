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

      contains

      subroutine solve_PCG_SF(operator,operator_explicit,x,b,vol,k,m,&
        MFP,n,tol,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        external :: operator,operator_explicit
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        real(cp),intent(in) :: tol
        integer,intent(inout) :: N_iter
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p,z
        type(norms) :: norm_b
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
        call compute(norm_b,r)

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
          call add_product(r,Ax,-alpha) ! r = r - alpha Ap
          call zeroGhostPoints(r)
          res_norm = dot_product(r,r,m,x,tempx)

#ifdef _EXPORT_PCG_CONVERGENCE_
          call compute(norm,r)
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,norm_b%L1,norm_b%L2,norm_b%Linf
#endif
          if (sqrt(res_norm)/norm_b%L2.lt.tol) then; exit; endif
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
          call compute(norm,r); call print(norm,'PCG_SF Residuals')
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,norm_b%L1,norm_b%L2,norm_b%Linf
          write(*,*) 'PCG_SF iterations (executed/max) = ',i-1,n
          write(*,*) 'PCG_SF exit condition = ',sqrt(res_norm)/norm_b%L2
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,operator_explicit,x,b,vol,k,m,&
        MFP,n,tol,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        external :: operator,operator_explicit
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        real(cp),intent(in) :: tol
        integer,intent(inout) :: N_iter
        integer,intent(in) :: un
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        type(norms) :: norm_b
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
        call compute(norm_b,r)

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

#ifdef _EXPORT_PCG_CONVERGENCE_
          call zeroGhostPoints(r)
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,norm_b%L1,norm_b%L2,norm_b%Linf
#endif
          if (sqrt(res_norm)/norm_b%L2.lt.tol) then; exit; endif

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
          call compute(norm,r); call print(norm,'PCG_VF Residuals')
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,norm_b%L1,norm_b%L2,norm_b%Linf
          write(*,*) 'PCG_VF iterations (executed/max) = ',i-1,n
          write(*,*) 'PCG_VF exit condition = ',sqrt(res_norm)/norm_b%L2
        endif
      end subroutine

      end module