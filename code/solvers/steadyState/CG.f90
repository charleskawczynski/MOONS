      module CG_mod
      use IO_SF_mod
      use mesh_mod
      use apply_BCs_mod
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

      contains

      subroutine compute_Ax(Ax,x,m)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(VF) :: temp
        if (x%is_Node) then;    call init_Edge(temp,m)
        elseif (x%is_CC) then;  call init_Face(temp,m)
        else; stop 'Error: data type unknown in compute_Ax in CG.f90'
        endif
        call assign(temp,0.0_cp)
        call grad(temp,x,m)
        call div(Ax,temp,m)
        call delete(temp)
      end subroutine

      subroutine CG(x,b,m,n,norm,displayTF,temp,Ax,r,p)
        implicit none
        type(SF),intent(inout) :: x
        type(SF),intent(inout) :: b
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: displayTF
        type(SF),intent(inout) :: temp,Ax,r,p
        integer :: i
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call apply_BCs(x,m)
        call compute_Ax(Ax,x,m) ! Compute Ax

        ! call extrap(b,m)
        call subtract(r,b,Ax)   ! r = b - Ax

        call zeroGhostPoints(r) ! Ghost cells of input should not matter
        call zeroWall(r,m,x)    ! Boundary residual for Dirichlet BCs is zero

        call assign(p,r)
        call dotProduct(rsold,r,r,m,x,temp) ! rsold = r^T*r
        ! do i=1,minval((/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/))
        do i=1,minval((/n,m%N_cells_tot/))
          call compute_Ax(Ax,p,m)                   ! Compute Ax / alpha
          call dotProduct(alpha_temp,p,Ax,m,x,temp) ! Compute Ax / alpha
          alpha = rsold/alpha_temp                  ! Compute Ax / alpha

          call assign(temp,p)                       ! Update x
          call multiply(temp,alpha)                 ! Update x
          call add(x,temp)                          ! Update x
          call apply_BCs(x,m)                     ! Update x

          call assign(temp,Ax)                      ! Update r
          call multiply(temp,alpha)                 ! Update r
          call subtract(r,temp)                     ! Update r
          call zeroGhostPoints(r)                   ! Update r
          call zeroWall(r,m,x)                      ! Update r

          call dotProduct(rsnew,r,r,m,x,temp)
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif

          ! ------------------------ My residual computation ------------------
          ! call compute_Ax(Ax,x,m)
          ! call subtract(temp,b,Ax)
          ! call zeroGhostPoints(temp)
          ! call zeroWall(temp,m)
          ! call compute(norm,temp,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,temp,'out/','r_mine',0)
          ! write(*,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          ! -------------------------------------------------------------------

          call assign(temp,p)                       ! Update p
          call multiply(temp,rsnew/rsold)           ! Update p
          call add(p,r,temp)                        ! Update p
          call zeroGhostPoints(p)                   ! Update p

          rsold = rsnew
          if (x%all_Neumann) call subtract(x,mean(x))

        enddo
        if (x%all_Neumann) call subtract(x,mean(x))
        call apply_BCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,m)
          call subtract(temp,b,Ax)
          call zeroGhostPoints(temp)
          call zeroWall(temp,m)
          call compute(norm,temp,m)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells_tot/)
          call print(norm,'CG Residuals')
        endif
      end subroutine

      subroutine dotProduct(dot,A,B,m,x,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: A,B,x
        type(SF),intent(inout) :: temp
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall(temp,m,x)
        dot = sum(temp)
      end subroutine

      end module