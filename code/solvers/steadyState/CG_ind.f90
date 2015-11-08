      module CG_ind_mod
      use IO_SF_mod
      use mesh_mod
      use applyBCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use BCs_mod
      use SF_mod
      use VF_mod
      use ops_interp_mod
      implicit none

      private
      public :: CG_ind_transient,CG_ind_SS,compute_Ax

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      interface compute_Ax; module procedure compute_Ax_SS;         end interface
      interface compute_Ax; module procedure compute_Ax_transient;  end interface

      contains

      subroutine compute_Ax_SS(Ax,x,sigmaInv,m)
        ! Computes Ax = curl(sigmaInv curl(x))
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,sigmaInv
        type(mesh),intent(in) :: m
        type(VF) :: temp
        call init(temp,sigmaInv)
        call assign(temp,0.0_cp)
        call curlcurl(Ax,x,sigmaInv,temp,m)
        call delete(temp)
      end subroutine

      subroutine compute_Ax_transient(Ax,x,xnm1,sigmaInv,dt,Rem,m)
        ! Computes Ax = Rem^-1 curl(sigmaInv curl(x)) + dx/dt
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,xnm1,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(VF) :: temp,tempx
        call init(temp,sigmaInv); call init(tempx,x)
        call assign(temp,0.0_cp)
        call curlcurl(Ax,x,sigmaInv,temp,m)
        call divide(Ax,Rem)
        call subtract(tempx,x,xnm1)
        call divide(tempx,dt)
        call add(Ax,tempx)
        call delete(temp)
        call delete(tempx)
      end subroutine

      subroutine CG_ind_transient(x,xnm1,b,sigmaInv,Rem,dt,m,n,norm,displayTF,temp,Ax,r,p)
        implicit none
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,xnm1,sigmaInv
        real(cp),intent(in) :: dt,Rem
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: temp,Ax,r,p
        integer :: i
        real(cp),dimension(3) :: alpha,rsold,rsnew,alpha_temp
        call applyBCs(x,m)
        call compute_Ax(Ax,x,xnm1,sigmaInv,dt,Rem,m) ! Compute Ax
        call subtract(r,b,Ax)                        ! r = b - Ax
        call zeroGhostPoints(r)                      ! Ghost cells of input should not matter
        call zeroWall(r,m,x)                         ! Boundary residual for Dirichlet BCs is zero
        call assign(p,r)
        call dotProduct(rsold,r,r,m,x,temp)                        ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,xnm1,sigmaInv,dt,Rem,m)                         ! Compute Ax / alpha
          call dotProduct(alpha_temp,p,Ax,m,x,temp)                ! Compute Ax / alpha
          alpha = rsold/alpha_temp                                 ! Compute Ax / alpha
          call assign(temp,p)                                      ! Update x
          call multiply(temp,alpha)                                ! Update x
          call add(x,temp)                                         ! Update x
          call applyBCs(x,m)                                    ! Update x
          call assign(temp,Ax)                                     ! Update r
          call multiply(temp,alpha)                                ! Update r
          call subtract(r,temp)                                    ! Update r
          call zeroGhostPoints(r)                                  ! Update r
          call zeroWall(r,m,x)                                     ! Update r
          call dotProduct(rsnew,r,r,m,x,temp)
          if (sqrt(sum(rsnew)).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          ! ------------------------ My residual computation ------------------
          ! call compute_Ax(Ax,x,xnm1,sigmaInv,dt,Rem,m)
          ! call subtract(temp,b,Ax)
          ! call zeroGhostPoints(temp)
          ! call zeroWall(temp,m)
          ! call compute(norm,temp,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,temp,'out/','r_mine',0)
          ! write(*,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------
          call assign(temp,p)                                       ! Update p
          call multiply(temp,rsnew/rsold)                           ! Update p
          call add(p,r,temp)                                        ! Update p
          call zeroGhostPoints(p)                                   ! Update p
          rsold = rsnew
        enddo
        if (x%x%all_Neumann) call subtract(x%x,mean(x%x))
        if (x%y%all_Neumann) call subtract(x%y,mean(x%y))
        if (x%z%all_Neumann) call subtract(x%z,mean(x%z))
        call applyBCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,xnm1,sigmaInv,dt,Rem,m)
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

      subroutine CG_ind_SS(x,b,sigmaInv,m,n,norm,displayTF,temp,Ax,r,p)
        implicit none
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,sigmaInv
        type(mesh),intent(in) :: m
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: temp,Ax,r,p
        integer :: i
        real(cp),dimension(3) :: alpha,rsold,rsnew,alpha_temp
        call applyBCs(x,m)
        call compute_Ax(Ax,x,sigmaInv,m) ! Compute Ax
        call subtract(r,b,Ax)            ! r = b - Ax
        call zeroGhostPoints(r)          ! Ghost cells of input should not matter
        call zeroWall(r,m,x)             ! Boundary residual for Dirichlet BCs is zero
        call assign(p,r)
        call dotProduct(rsold,r,r,m,x,temp)                        ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,sigmaInv,m)                         ! Compute Ax / alpha
          call dotProduct(alpha_temp,p,Ax,m,x,temp)                ! Compute Ax / alpha
          alpha = rsold/alpha_temp                                 ! Compute Ax / alpha
          call assign(temp,p)                                      ! Update x
          call multiply(temp,alpha)                                ! Update x
          call add(x,temp)                                         ! Update x
          call applyBCs(x,m)                                    ! Update x
          call assign(temp,Ax)                                     ! Update r
          call multiply(temp,alpha)                                ! Update r
          call subtract(r,temp)                                    ! Update r
          call zeroGhostPoints(r)                                  ! Update r
          call zeroWall(r,m,x)                                     ! Update r
          call dotProduct(rsnew,r,r,m,x,temp)
          if (sqrt(sum(rsnew)).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          ! ------------------------ My residual computation ------------------
          ! call compute_Ax(Ax,x,sigmaInv,m)
          ! call subtract(temp,b,Ax)
          ! call zeroGhostPoints(temp)
          ! call zeroWall(temp,m)
          ! call compute(norm,temp,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,temp,'out/','r_mine',0)
          ! write(*,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------
          call assign(temp,p)                                       ! Update p
          call multiply(temp,rsnew/rsold)                           ! Update p
          call add(p,r,temp)                                        ! Update p
          call zeroGhostPoints(p)                                   ! Update p
          rsold = rsnew
        enddo
        if (x%x%all_Neumann) call subtract(x%x,mean(x%x))
        if (x%y%all_Neumann) call subtract(x%y,mean(x%y))
        if (x%z%all_Neumann) call subtract(x%z,mean(x%z))
        call applyBCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,sigmaInv,m)
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

      subroutine dotProduct(dot,A,B,m,x,temp)
        implicit none
        real(cp),dimension(3),intent(inout) :: dot
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: A,B,x
        type(VF),intent(inout) :: temp
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall(temp,m,x)
        dot(1) = sum(temp%x); dot(2) = sum(temp%y); dot(3) = sum(temp%z)
      end subroutine

      end module