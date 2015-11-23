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
      use TF_mod
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

      subroutine solve_PCG_VF(operator,x,b,vol,k,c,m,n,norm,displayTF,temp,Ax,rk,rkp1,p)
        implicit none
        external :: operator
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,k,vol
        real(cp),intent(in) :: c
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: temp,Ax,rk,rkp1,p,z
        integer :: k
        real(cp) :: alpha,beta,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)

        call operator(Ax,x,vol,m,tempk,k,c)
        call multiply(rk,b,vol)
        call subtract(rk,Ax)
        call zeroGhostPoints(rk)
        call zeroWall(rk,m)

        call multiply(z,Minv,rk)
        call assign(p,rk)
        rsold = dotProduct(rk,rk,m,x,temp)

        do k=1,n
          call operator(Ax,p,vol,m,tempk,k,c)
          alpha_temp = dotProduct(p,Ax,m,x,temp)
          alpha = dotProduct(rk,z,m,x,temp)
          alpha = alpha/alpha_temp
          
          call assign(temp,p)
          call multiply(temp,alpha)
          call add(x,temp)
          call apply_BCs(x,m)

          call assign(temp,Ax)
          call multiply(temp,alpha)
          call subtract(rkp1,rk,temp)
          call zeroGhostPoints(rkp1)
          call zeroWall(rkp1,m)
          rsnew = dotProduct(rkp1,rkp1,m,x,temp)
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif

          ! ------------------------ My residual computation ------------------
          ! call operator(Ax,x,sigmaInv,dt,Rem,m)
          ! call subtract(temp,b,Ax)
          ! call zeroGhostPoints(temp)
          ! call zeroWall(temp,m)
          ! call compute(norm,temp,m)
          ! if (k.eq.k_stop) call export_3D_1C(m,temp,'out/','r_mine',0)
          ! write(un,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------

          call multiply(z,Minv,rkp1)
          beta_temp = dotProduct(zkp1,rkp1,m,x,temp)
          beta = dotProduct(zk,rk,m,x,temp)
          beta = beta_temp/beta
          call assign(temp,p)
          call multiply(temp,beta)
          call add(p,rkp1,temp)
          call zeroGhostPoints(p)
          rsold = rsnew
          call assign(rkp1,rk)
        enddo
        endif
        if (x%x%all_Neumann) call subtract(x%x,mean(x%x))
        if (x%y%all_Neumann) call subtract(x%y,mean(x%y))
        if (x%z%all_Neumann) call subtract(x%z,mean(x%z))
        call apply_BCs(x,m)
        if (displayTF) then
          call operator(Ax,x,vol,m,tempk,k,c)
          call subtract(temp,b,Ax)
          call zeroGhostPoints(temp)
          call zeroWall(temp,m)
          call compute(norm(1),temp%x,m)
          call compute(norm(2),temp%y,m)
          call compute(norm(3),temp%z,m)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells_tot/)
          call print(norm(1),'CG Residuals (x)')
          call print(norm(2),'CG Residuals (y)')
          call print(norm(3),'CG Residuals (z)')
        endif
      end subroutine

      end module