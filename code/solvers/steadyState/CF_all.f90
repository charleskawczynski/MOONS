      module CG_mod
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
      public :: CG,compute_Ax

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      interface CG;   module procedure CG_SF; end interface
      ! interface CG;   module procedure CG_VF; end interface

      interface compute_Ax;   module procedure compute_Ax_SF; end interface
      interface compute_Ax;   module procedure compute_Ax_VF1; end interface
      interface compute_Ax;   module procedure compute_Ax_VF2; end interface

      interface dotProduct;   module procedure dotProduct_SF; end interface
      interface dotProduct;   module procedure dotProduct_VF; end interface

      contains

      subroutine compute_Ax_SF(Ax,x,m)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        ! type(SF) :: temp_x
        ! type(VF) :: temp
        ! call init_Face(temp,m)
        ! call init(temp_x,x); call assign(temp_x,x); call extrap(temp_x,m)
        ! call grad(temp,x,m)
        ! call div(Ax,temp,m)
        ! call delete(temp)
        call lap(Ax,x,m)
        ! call delete(temp_x)
      end subroutine

      subroutine compute_Ax_VF1(Ax,x,m)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x
        type(mesh),intent(in) :: m
        call lap(Ax,x,m)
      end subroutine

      subroutine compute_Ax_VF2(Ax,x,sigma,m,temp_sig)
        implicit none
        type(VF),intent(inout) :: Ax
        type(VF),intent(in) :: x,sigma
        type(mesh),intent(in) :: m
        type(VF),intent(inout) :: temp_sig
        call curl(temp_sig,x,m)
        call divide(temp_sig,sigma)
        call curl(Ax,temp_sig,m)
      end subroutine

      subroutine CG_SF(x,b,m,n,norm,displayTF,temp,Ax,r,p)
        implicit none
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(SF),intent(inout) :: temp,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call applyAllBCs(x,m)
        call compute_Ax(Ax,x,m) ! Compute Ax
        call subtract(r,b,Ax) ! r = b - Ax
        call assign(p,r)
        call zeroGhostPoints(p)
        call dotProduct(rsold,r,r,temp) ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/))
          call compute_Ax(Ax,p,m)               ! Compute Ax / alpha
          call dotProduct(alpha_temp,p,Ax,temp) ! Compute Ax / alpha
          alpha = rsold/alpha_temp              ! Compute Ax / alpha
          call assign(temp,p)                   ! Update x
          call multiply(temp,alpha)             ! Update x
          call add(x,temp)                      ! Update x
          call applyAllBCs(x,m)                 ! Apply BCs to x
          call assign(temp,Ax)                  ! Update r
          call multiply(temp,alpha)             ! Update r
          call subtract(r,temp)                 ! Update r
          call dotProduct(rsnew,r,r,temp)       ! Update r
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          write(*,*) 'Residual = ',sqrt(rsnew)
          call assign(temp,p)                   ! Update p & rsold
          call multiply(temp,rsnew/rsold)
          call add(p,r,temp)
          rsold = rsnew
        enddo
        ! if (getAllNeumann(x)) call subtract(x,mean(x))
        if (getAllNeumann(x%RF(1)%b)) call subtract(x,mean(x))
        call applyAllBCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,m)
          call subtract(r,b,Ax)
          call zeroGhostPoints(r)
          call compute(norm,r,m)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/)
          call print(norm,'CG Residuals')
        endif
      end subroutine

      subroutine CG_VF(x,b,sigma,m,n,norm,displayTF,temp,Ax,r,p,temp_sig)
        implicit none
        type(VF),intent(inout) :: x             ! Face
        type(VF),intent(in) :: b                ! Face
        type(VF),intent(in) :: sigma            ! Edge
        type(VF),intent(inout) :: temp_sig      ! Edge
        type(VF),intent(inout) :: temp,Ax,r,p   ! Face
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        integer :: i
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call applyAllBCs(x,m)
        call compute_Ax(Ax,x,sigma,m,temp_sig)   ! Compute Ax
        call subtract(r,b,Ax)                    ! r = b - Ax
        call assign(p,r)
        ! call zeroGhostPoints(p)
        call dotProduct(rsold,r,r,temp) ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/))
          call compute_Ax(Ax,p,sigma,m,temp_sig) ! Compute Ax / alpha
          call dotProduct(alpha_temp,p,Ax,temp)  ! Compute Ax / alpha
          alpha = rsold/alpha_temp               ! Compute Ax / alpha
          call assign(temp,p)                    ! Update x
          call multiply(temp,alpha)              ! Update x
          call add(x,temp)                       ! Update x
          call assign(temp,Ax)                   ! Update r
          call multiply(temp,alpha)              ! Update r
          call subtract(r,temp)                  ! Update r
          call dotProduct(rsnew,r,r,temp)        ! Update r
          call applyAllBCs(x,m)
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif
          write(*,*) 'Residual = ',sqrt(rsnew)
          call assign(temp,p)                    ! Update p & rsold
          call multiply(temp,rsnew/rsold)
          call add(p,r,temp)
          call applyAllBCs(p,m,x)
          rsold = rsnew
        enddo
        ! if (getAllNeumann(x)) call subtract(x,mean(x))
        if (getAllNeumann(x%x%RF(1)%b)) call subtract(x%x,mean(x%x))
        if (getAllNeumann(x%y%RF(1)%b)) call subtract(x%y,mean(x%y))
        if (getAllNeumann(x%z%RF(1)%b)) call subtract(x%z,mean(x%z))
        call applyAllBCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,sigma,m,temp_sig)
          call subtract(r,b,Ax)
          ! call zeroGhostPoints(r)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/)

          call compute(norm,r%x,m)
          call print(norm,'CG Residuals')
          call compute(norm,r%y,m)
          call print(norm,'CG Residuals')
          call compute(norm,r%z,m)
          call print(norm,'CG Residuals')
        endif
      end subroutine

      subroutine dotProduct_SF(dot,A,B,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(SF),intent(in) :: A,B
        type(SF),intent(inout) :: temp
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        dot = sum(temp)
      end subroutine

      subroutine dotProduct_VF(dot,A,B,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(VF),intent(in) :: A,B
        type(VF),intent(inout) :: temp
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        dot = sum(temp%x) + sum(temp%y) + sum(temp%z)
      end subroutine

      end module