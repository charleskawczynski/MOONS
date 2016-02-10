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
      use ops_norms_mod
      use omp_lib

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

      real(cp),parameter :: tol_abs = 10.0_cp**(-10.0_cp)

      contains

      subroutine solve_PCG_SF(operator,operator_explicit,name,x,b,vol,k,m,&
        MFP,n,tol,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        external :: operator,operator_explicit
        character(len=1),intent(in) :: name
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
        type(norms) :: norm_res0
        integer :: i,i_earlyExit
        real(cp) :: temp_Ln
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        ! call Ln(temp_Ln,b,1.0_cp); write(*,*) 'L1(b) input = ',temp_Ln
        call multiply(r,b,vol)
        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! RHS MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        if (.not.x%is_CC) then
          call assign(p,r)
          call modify_forcing1(r,p,m,x)
        endif
        ! call Ln(temp_Ln,r,1.0_cp); write(*,*) 'L1(r) post modify = ',temp_Ln
        call assign(p,0.0_cp)
        call apply_BCs(p,m) ! p has BCs for x
        call zeroGhostPoints_conditional(p,m)
        call operator_explicit(Ax,p,k,m,MFP,tempk)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call zeroWall_conditional(Ax,m,x)
        call subtract(r,Ax)
        ! ----------------------------------------------------------
        ! call Ln(temp_Ln,r,1.0_cp); write(*,*) 'L1(r) post modify RHS = ',temp_Ln

        call operator(Ax,x,k,m,MFP,tempk)
        call multiply(Ax,vol)
        if (x%all_Neumann) call subtract_physical_mean(r)
        call subtract(r,Ax)
        call compute(norm_res0,r)
#ifdef _EXPORT_PCG_SF_CONVERGENCE_
          call compute(norm,r)
          res_norm = dot_product(r,r,m,x,tempx)
          write(un,*) N_iter,sqrt(res_norm)/norm_res0%L2,norm%L1,norm%L2,norm%Linf,&
                                            norm_res0%L1,norm_res0%L2,norm_res0%Linf,0
#endif

        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx); res_norm = rhok; i_earlyExit = 0
        ! call Ln(temp_Ln,r,1.0_cp); write(*,*) 'L1(r) before loop = ',temp_Ln
        ! call Ln(temp_Ln,x,1.0_cp); write(*,*) 'L1(x) before loop = ',temp_Ln
        do i=1,n
          call apply_stitches(p,m)
          call operator(Ax,p,k,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha) ! x = x + alpha p
          call apply_BCs(x,m) ! Needed for PPE
          N_iter = N_iter + 1
          call add_product(r,Ax,-alpha) ! r = r - alpha Ap
          call zeroGhostPoints(r)
          res_norm = dot_product(r,r,m,x,tempx)
          ! call Ln(temp_Ln,r,1.0_cp); write(*,*) 'L1(r) in loop = ',temp_Ln

#ifdef _EXPORT_PCG_SF_CONVERGENCE_
          call compute(norm,r)
          write(un,*) N_iter,sqrt(res_norm)/norm_res0%L2,norm%L1,norm%L2,norm%Linf,&
                                            norm_res0%L1,norm_res0%L2,norm_res0%Linf,i
#endif
          if ((sqrt(res_norm)/norm_res0%L2.lt.tol).or.(sqrt(res_norm).lt.tol_abs)) then; i_earlyExit=1; exit; endif
          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call multiply(p,rhokp1/rhok) ! p = z + beta p
          call add(p,z)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo
        ! call Ln(temp_Ln,r,1.0_cp); write(*,*) 'L1(r) after loop = ',temp_Ln
        ! call Ln(temp_Ln,x,1.0_cp); write(*,*) 'L1(x) after loop = ',temp_Ln

#ifdef _EXPORT_PCG_SF_CONVERGENCE_
        flush(un)
#endif
        ! call export_processed(m,x,'out/LDC/','x_solution',1)

        if (compute_norms) then
          call operator_explicit(Ax,x,k,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          if (x%all_Neumann) call subtract_physical_mean(r)
          call subtract(r,Ax)
          call zeroWall_conditional(r,m,x)
          call zeroGhostPoints(r)
          call compute(norm,r); call print(norm,'PCG_SF Residuals for '//name)
          write(un,*) N_iter,sqrt(res_norm)/norm_res0%L2,norm%L1,norm%L2,norm%Linf,&
                                            norm_res0%L1,norm_res0%L2,norm_res0%Linf,i-1+i_earlyExit
          flush(un)
          write(*,*) 'PCG_SF iterations (executed/max) = ',i-1+i_earlyExit,n
          write(*,*) 'PCG_SF exit condition = ',sqrt(res_norm)/norm_res0%L2
          write(*,*) ''
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,operator_explicit,name,x,b,vol,k,m,&
        MFP,n,tol,norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        external :: operator,operator_explicit
        character(len=1),intent(in) :: name
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
        integer :: i,i_earlyExit
        type(norms) :: norm_res0
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        call multiply(r,b,vol)
        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        call assign(p,r)
        call modify_forcing1(r,p,m,x)
        call assign(p,0.0_cp)
        call apply_BCs(p,m) ! p has BCs for x
        call zeroGhostPoints_conditional(p,m)
        call operator_explicit(Ax,p,k,m,MFP,tempk)
        call multiply(Ax,vol)
        call zeroGhostPoints(Ax)
        call zeroWall_conditional(Ax,m,x)
        call subtract(r,Ax)
        ! ----------------------------------------------------------

        call operator(Ax,x,k,m,MFP,tempk)
        call multiply(Ax,vol)
        ! if (x%all_Neumann) call subtract_physical_mean(r)
        call subtract(r,Ax)
        call compute(norm_res0,r)
#ifdef _EXPORT_PCG_VF_CONVERGENCE_
          call compute(norm,r)
          res_norm = dot_product(r,r,m,x,tempx)
          write(un,*) N_iter,sqrt(res_norm)/norm_res0%L2,norm%L1,norm%L2,norm%Linf,&
                                            norm_res0%L1,norm_res0%L2,norm_res0%Linf,0
#endif

        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx); res_norm = rhok; i_earlyExit = 0
        do i=1,n
          call apply_stitches(p,m)
          call operator(Ax,p,k,m,MFP,tempk)
          call multiply(Ax,vol)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha) ! x = x + alpha p
          call apply_BCs(x,m) ! Needed for PPE
          N_iter = N_iter + 1
          call add_product(r,Ax,-alpha) ! x = x - alpha Ap
          res_norm = dot_product(r,r,m,x,tempx)

#ifdef _EXPORT_PCG_VF_CONVERGENCE_
          call compute(norm,r)
          write(un,*) N_iter,sqrt(res_norm)/norm_res0%L2,norm%L1,norm%L2,norm%Linf,&
                                            norm_res0%L1,norm_res0%L2,norm_res0%Linf,i
#endif
          if ((sqrt(res_norm)/norm_res0%L2.lt.tol).or.(sqrt(res_norm).lt.tol_abs)) then; i_earlyExit=1; exit; endif

          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call multiply(p,rhokp1/rhok) ! p = z + beta p
          call add(p,z)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo

#ifdef _EXPORT_PCG_VF_CONVERGENCE_
        flush(un)
#endif
        
        if (compute_norms) then
          call operator_explicit(Ax,x,k,m,MFP,tempk)
          call multiply(Ax,vol)
          call multiply(r,b,vol)
          ! if (x%all_Neumann) call subtract_physical_mean(r)
          call subtract(r,Ax)
          call zeroWall_conditional(r,m,x)
          call zeroGhostPoints(r)
          call compute(norm,r); call print(norm,'PCG_VF Residuals for '//name)
          write(un,*) N_iter,sqrt(res_norm)/norm_res0%L2,norm%L1,norm%L2,norm%Linf,&
                                            norm_res0%L1,norm_res0%L2,norm_res0%Linf,i-1+i_earlyExit
          flush(un)
          write(*,*) 'PCG_VF iterations (executed/max) = ',i-1+i_earlyExit,n
          write(*,*) 'PCG_VF exit condition = ',sqrt(res_norm)/norm_res0%L2
          write(*,*) ''
        endif
      end subroutine

      end module