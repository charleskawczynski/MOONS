      module Jacobi_solver_mod
      use current_precision_mod
      use mesh_mod
      use mesh_domain_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_export_mod
      use ops_embedExtract_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod

      implicit none

      real(cp) :: tol_abs = 10.0_cp**(-12.0_cp)

      private
      public :: solve_Jacobi
      interface solve_Jacobi;   module procedure solve_Jacobi_SF;         end interface
      interface solve_Jacobi;   module procedure solve_Jacobi_VF;         end interface

      contains

      subroutine solve_Jacobi_SF(operator,x,x_interior,f,vol,k,Dinv,Diag,m,MD_interior,MFP,n,N_iter,&
        norm,compute_norm,un,n_skip_check_res,tol,name,Ax,res,tempk)
        implicit none
        procedure(op_SF_explicit) :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: f,x_interior,Dinv,Diag,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh_domain),intent(in) :: MD_interior
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: tol
        character(len=*),intent(in) :: name
        type(matrix_free_params),intent(in) :: MFP
        integer,intent(in) :: n,un,n_skip_check_res
        integer,intent(inout) :: N_iter
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norm
        type(SF),intent(inout) :: Ax,res
        type(norms) :: norm0
        integer :: i,i_earlyExit
        logical :: skip_loop,suppress_warning
        suppress_warning = x_interior%is_Face
        suppress_warning = MD_interior%D%s.eq.0
        call apply_BCs(x) ! Boundaries

        call operator(Ax,x,k,m,MFP,tempk)
        call multiply(res,x,Diag)
        call subtract(Ax,res) ! LU = Ax - Dx
        call subtract(res,f,Ax)
        call compute(norm0,res,vol)

        i_earlyExit=0
        if (.not.sqrt(norm0%L2).lt.tol_abs) then ! Only do iterations if necessary!
          skip_loop = .false.
          do i=1,n
            call operator(Ax,x,k,m,MFP,tempk)
            call multiply(res,x,Diag)
            call subtract(Ax,res) ! LU = Ax - Dx
            call subtract(res,f,Ax)
            call multiply(x,Dinv,res)
            call apply_BCs(x)
            N_iter = N_iter + 1
            if (mod(i,n_skip_check_res).eq.0) then
              call compute(norm,res,vol)
#ifdef _EXPORT_JAC_SF_CONVERGENCE_
              write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,&
                                 norm0%L1,norm0%L2,norm0%Linf,i-1
              flush(un)
#endif
              if ((norm%L2/norm0%L2.lt.tol).or.(norm%L2.lt.tol_abs)) then; i_earlyExit=1; exit; endif
            endif
          enddo
        else; i=1; skip_loop = .true.
        endif

        if (x%all_neumann) call subtract(x,mean(x))
        if (compute_norm) then
          if (.not.skip_loop) then
            call zeroGhostPoints(res)
            call compute(norm,res,vol)
            call print(norm,'Jacobi_SF Residuals for '//name)
            write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,&
                               norm0%L1,norm0%L2,norm0%Linf,i-1
            write(*,*) 'Jacobi_SF iterations (executed/max) = ',i-1+i_earlyExit,n
            write(*,*) 'Jacobi_SF exit condition = ',norm%L2/norm0%L2
          else
            write(*,*) 'Jacobi_SF skip_loop = ',skip_loop
          endif
          write(*,*) ''
        endif
      end subroutine

      subroutine solve_Jacobi_VF(operator,x,x_interior,f,vol,k,Dinv,Diag,m,MD_interior,MFP,n,N_iter,&
        norm,compute_norm,un,n_skip_check_res,tol,name,Ax,res,tempk)
        implicit none
        procedure(op_VF_explicit) :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: f,x_interior,Dinv,Diag,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(mesh_domain),intent(in) :: MD_interior
        real(cp),intent(in) :: tol
        character(len=*),intent(in) :: name
        type(matrix_free_params),intent(in) :: MFP
        integer,intent(in) :: n,un,n_skip_check_res
        integer,intent(inout) :: N_iter
        type(norms),intent(inout) :: norm
        logical,intent(in) :: compute_norm
        type(VF),intent(inout) :: Ax,res
        type(norms) :: norm0
        integer :: i,i_earlyExit
        logical :: skip_loop
        if (x%is_Face) call embedFace(x,x_interior,MD_interior)
        call apply_BCs(x) ! Boundaries

        call operator(Ax,x,k,m,MFP,tempk)
        call multiply(res,x,Diag)
        call subtract(Ax,res) ! LU = Ax - Dx
        call subtract(res,f,Ax)
        call compute(norm0,res,vol)

        i_earlyExit=0
        if (.not.sqrt(norm0%L2).lt.tol_abs) then ! Only do iterations if necessary!
          skip_loop = .false.
          do i=1,n
            call operator(Ax,x,k,m,MFP,tempk)
            call multiply(res,x,Diag) ! = Dx
            call subtract(Ax,res)     ! = LUx
            call subtract(res,f,Ax)   ! = (b - LUx)
            call multiply(res,Dinv)   ! = Dinv (b - LUx)
            call multiply(x,0.1_cp)
            call multiply(res,(1.0_cp-0.1_cp))
            call add(x,res) ! x^n+1 = x^n w + (1-w) Dinv (b - LUx)
            call apply_BCs(x)
            if (x%is_Face) call embedFace(x,x_interior,MD_interior)

            N_iter = N_iter + 1
            if (mod(i,n_skip_check_res).eq.0) then
              call compute(norm,res,vol)
#ifdef _EXPORT_JAC_VF_CONVERGENCE_
              write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,&
                                 norm0%L1,norm0%L2,norm0%Linf,i-1
              flush(un)
#endif
              if ((norm%L2/norm0%L2.lt.tol).or.(norm%L2.lt.tol_abs)) then; i_earlyExit=1; exit; endif
            endif
          enddo
        else; i=1; skip_loop = .true.
        endif

        ! if (x%all_neumann) call subtract(x,mean(x))
        if (compute_norm) then
          if (.not.skip_loop) then
            call zeroGhostPoints(res)
            call compute(norm,res,vol)
            call print(norm,'Jacobi_VF Residuals for '//name)
            write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,&
                               norm0%L1,norm0%L2,norm0%Linf,i-1
            write(*,*) 'Jacobi_VF iterations (executed/max) = ',i-1+i_earlyExit,n
            write(*,*) 'Jacobi_VF exit condition = ',norm%L2/norm0%L2
          else
            write(*,*) 'Jacobi_VF skip_loop = ',skip_loop
          endif
          write(*,*) ''
        endif
      end subroutine

      end module