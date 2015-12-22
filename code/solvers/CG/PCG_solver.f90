      module PCG_solver_mod
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
      public :: solve_PCG
      interface solve_PCG;      module procedure solve_PCG_SF;   end interface
      interface solve_PCG;      module procedure solve_PCG_VF;   end interface

      real(cp) :: tol = 10.0_cp**(-30.0_cp)

      contains

      subroutine solve_PCG_SF(operator,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,z,Minv)
        implicit none
        external :: operator
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n,un
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        call apply_BCs(x,m)
        call operator(Ax,x,k,vol,m,MFP,tempk)
        call multiply(r,b,vol)
        if (x%all_Neumann) call subtract_physical_mean(r)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx)
        do i=1,n
          call operator(Ax,p,k,vol,m,MFP,tempk)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha) ! x = x + alpha p
          call apply_BCs(x,m)
          call add_product(r,Ax,-alpha) ! x = x - alpha Ap
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          res_norm = dot_product(r,r,m,x,tempx)
          if (res_norm.lt.tol) then; exit; endif

#ifdef _EXPORT_PCG_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r); write(un,*) norm%L1,norm%L2,norm%Linf
#endif
          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call multiply(p,rhokp1/rhok) ! p = z + beta p
          call add(p,z)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo
        
        call apply_BCs(x,m)
        if (compute_norms) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r); call print(norm,'PCG Residuals')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un,*) norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'PCG iterations (executed/max) = ',i-1,n
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,x,b,vol,k,m,MFP,n,norm,compute_norms,un,tempx,tempk,Ax,r,p,z,Minv)
        implicit none
        external :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        integer,dimension(3),intent(in) :: un
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        call apply_BCs(x,m)
        call operator(Ax,x,k,vol,m,MFP,tempk)
        call multiply(r,b,vol)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx)
        do i=1,n
          call operator(Ax,p,k,vol,m,MFP,tempk)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call add_product(x,p,alpha) ! x = x + alpha p
          call apply_BCs(x,m)
          call add_product(r,Ax,-alpha) ! x = x - alpha Ap
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          res_norm = dot_product(r,r,m,x,tempx)
          if (res_norm.lt.tol) then; exit; endif

#ifdef _EXPORT_PCG_CONVERGENCE_
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x); write(un(1),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%y); write(un(2),*) norm%L1,norm%L2,norm%Linf
          call compute(norm,r%z); write(un(3),*) norm%L1,norm%L2,norm%Linf
#endif
          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call multiply(p,rhokp1/rhok) ! p = z + beta p
          call add(p,z)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo
        
        call apply_BCs(x,m)
        if (compute_norms) then
          call operator(Ax,x,k,vol,m,MFP,tempk)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm,r%x); call print(norm,'PCG Residuals (x)')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un(1),*) norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%y); call print(norm,'PCG Residuals (y)')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un(2),*) norm%L1,norm%L2,norm%Linf
#endif
          call compute(norm,r%z); call print(norm,'PCG Residuals (z)')
#ifndef _EXPORT_PCG_CONVERGENCE_
          write(un(3),*) norm%L1,norm%L2,norm%Linf
#endif
          write(*,*) 'PCG iterations (executed/max) = ',i-1,n
        endif
      end subroutine

      end module