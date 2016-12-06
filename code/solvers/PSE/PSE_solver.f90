      module PSE_solver_mod
      ! Compiler flags: (_EXPORT_PSE_CONVERGENCE_)
      use current_precision_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      use boundary_conditions_mod
      use apply_BCs_mod
      use norms_mod
      use ops_norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: solve_PSE
      interface solve_PSE;       module procedure solve_PSE_SF;    end interface
      interface solve_PSE;       module procedure solve_PSE_VF;    end interface

#ifdef _EXPORT_PSE_CONVERGENCE_
      real(cp) :: tol_abs = 10.0_cp**(-12.0_cp)
#endif

      contains

      subroutine solve_PSE_SF(operator,x,b,vol,k,m,MFP,n,ds,norm,compute_norms,un,tempk,Ax,r,N_iter)
        implicit none
        procedure(op_SF_explicit) :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        integer,intent(inout) :: N_iter
        real(cp),intent(in) :: ds
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: Ax,r
#ifdef _EXPORT_PSE_CONVERGENCE_
        type(norms) :: norm_res0
#endif
        integer :: i
        call apply_BCs(x)
        do i=1,n
          call operator(Ax,x,k,m,MFP,tempk)
          call subtract(r,Ax,b)
          call multiply(r,ds)
          call add(x,r)
          call apply_BCs(x)
          N_iter = N_iter + 1
#ifdef _EXPORT_PSE_CONVERGENCE_
          if (n.eq.1) call compute(norm_res0,r,vol)
          call compute(norm,r,vol)
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,&
                                    norm_res0%L1,norm_res0%L2,norm_res0%Linf,i
#endif
        enddo
        if (x%all_Neumann) call subtract(x,mean(x))
#ifndef _EXPORT_PSE_CONVERGENCE_
        if (compute_norms) then
          call operator(Ax,x,k,m,MFP,tempk)
          call subtract(r,Ax,b)
          call zeroGhostPoints(r)
          call assign_wall_Dirichlet(r,0.0_cp,x)
          call compute(norm,r,vol); call print(norm,'PSE Residuals SF')
          write(un,*) norm%L1,norm%L2,norm%Linf
          write(*,*) 'PSE iterations (executed/max) = ',i-1,n
        endif
#endif
      end subroutine

      subroutine solve_PSE_VF(operator,x,b,vol,k,m,MFP,n,ds,norm,compute_norms,un,tempk,Ax,r,N_iter)
        implicit none
        procedure(op_VF_explicit) :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        real(cp),intent(in) :: ds
        integer,intent(inout) :: N_iter
        logical,intent(in) :: compute_norms
        type(VF),intent(inout) :: Ax,r
#ifdef _EXPORT_PSE_CONVERGENCE_
        type(norms) :: norm_res0
#endif
        integer :: i
        call apply_BCs(x)
        do i=1,n
          call operator(Ax,x,k,m,MFP,tempk)
          call subtract(r,Ax,b)
          call multiply(r,ds)
          call add(x,r)
          call apply_BCs(x)
          N_iter = N_iter + 1
#ifdef _EXPORT_PSE_CONVERGENCE_
          if (n.eq.1) call compute(norm_res0,r,vol)
          call compute(norm,r,vol)
          write(un,*) N_iter,norm%L1,norm%L2,norm%Linf,&
                                    norm_res0%L1,norm_res0%L2,norm_res0%Linf,i
#endif
        enddo

        call apply_BCs(x)
#ifndef _EXPORT_PSE_CONVERGENCE_
        if (compute_norms) then
          call operator(Ax,x,k,m,MFP,tempk)
          call subtract(r,Ax,b)
          call zeroGhostPoints(r)
          call assign_wall_Dirichlet(r,0.0_cp,x)
          call compute(norm,r,vol); call print(norm,'PSE Residuals VF')
          write(un,*) norm%L1,norm%L2,norm%Linf
          write(*,*) 'PSE iterations (executed/max) = ',i-1,n
        endif
#endif
      end subroutine

      end module