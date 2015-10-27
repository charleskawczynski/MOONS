      module CG_mod
      use mesh_mod
      use applyBCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use BCs_mod
      use SF_mod
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

      contains

      subroutine compute_Ax(Ax,x,m)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        call lap(Ax,x,m)
      end subroutine

      subroutine CG(x,b,m,n,norm,displayTF,temp,Ax,r,p)
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
        call applyAllBCs(r,m,x) ! Ensures residual on boundary is zero
        call assign(p,r)
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
          call applyAllBCs(r,m,x)
          call add(p,r,temp)
          call applyAllBCs(p,m,x)
          rsold = rsnew
        enddo
        call delete(p%RF(1)%b)
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

      subroutine dotProduct_SF(dot,A,B,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(SF),intent(in) :: A,B
        type(SF),intent(inout) :: temp
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        dot = sum(temp)
      end subroutine

      end module