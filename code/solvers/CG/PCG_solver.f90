      module PCG_solver_mod
      use IO_tools_mod
      use IO_SF_mod
      use IO_VF_mod
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

      private
      public :: solve_PCG

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      interface solve_PCG;      module procedure solve_PCG_VF;   end interface

      contains

      subroutine solve_PCG_VF(operator,x,b,vol,c,k,m,n,norm,compute_norms,tempx,tempk,Ax,r,p,z,Minv)
        implicit none
        external :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol,Minv
        real(cp),intent(in) :: c
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        real(cp) :: alpha,rhok,rhokp1,res_norm ! betak = rhokp1/rhok
        call apply_BCs(x,m)
        call operator(Ax,x,vol,m,tempk,c,k)
        call multiply(r,b,vol)
        call subtract(r,Ax)
        call zeroGhostPoints(r)
        call zeroWall_conditional(r,m,x)
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx)
        do i=1,n
          call operator(Ax,p,vol,m,tempk,c,k)
          alpha = rhok/dot_product(p,Ax,m,x,tempx)
          call assign(tempx,p) ! x = x + alpha p
          call multiply(tempx,alpha)
          call add(x,tempx)
          call apply_BCs(x,m)
          call assign(tempx,Ax) ! r = r - alpha Ap
          call multiply(tempx,alpha)
          call subtract(r,tempx)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          res_norm = dot_product(r,r,m,x,tempx)
          if (sqrt(res_norm).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          ! ------------------------ My residual computation ------------------
          ! call operator(Ax,x,sigmaInv,dt,Rem,m)
          ! call subtract(tempx,b,Ax)
          ! call zeroGhostPoints(tempx)
          ! call zeroWall_conditional(tempx,m,x)
          ! call compute(norm,tempx,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,tempx,'out/','r_mine',0)
          ! write(un,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------
          call multiply(z,Minv,r)
          rhokp1 = dot_product(z,r,m,x,tempx)
          call assign(tempx,p) ! p = z + beta p
          call multiply(tempx,rhokp1/rhok)
          call add(p,z,tempx)
          call zeroGhostPoints(p)
          rhok = rhokp1
        enddo
        if (x%x%all_Neumann) call subtract(x%x,mean(x%x))
        if (x%y%all_Neumann) call subtract(x%y,mean(x%y))
        if (x%z%all_Neumann) call subtract(x%z,mean(x%z))
        call apply_BCs(x,m)
        if (compute_norms) then
          call operator(Ax,x,vol,m,tempk,c,k)
          call multiply(r,b,vol)
          call subtract(r,Ax)
          call zeroGhostPoints(r)
          call zeroWall_conditional(r,m,x)
          call compute(norm(1),tempx%x,m)
          call compute(norm(2),tempx%y,m)
          call compute(norm(3),tempx%z,m)
          call print(norm(1),'PCG Residuals (x)')
          call print(norm(2),'PCG Residuals (y)')
          call print(norm(3),'PCG Residuals (z)')
          write(*,*) 'PCG iterations (executed/max) = ',i-1,n
        endif
      end subroutine

      end module