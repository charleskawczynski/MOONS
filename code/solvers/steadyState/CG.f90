      module CG_mod
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

      subroutine compute_Ax1(Ax,x,m)
        implicit none
        type(SF),intent(inout) :: Ax
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        call lap(Ax,x,m)
        call zeroGhostPoints(Ax)
      end subroutine

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
        integer :: i,i_stop
        real(cp) :: alpha,rsold,rsnew,alpha_temp
        call applyAllBCs(x,m)
        call compute_Ax(Ax,x,m) ! Compute Ax
        call subtract(r,b,Ax)   ! r = b - Ax
        call zeroWall(r,m)
        call assign(p,r)
        i_stop = 3
        ! call export_3D_1C(m,b,'out/','b',0)
        ! call export_3D_1C(m,x,'out/','x_sol',0)
        call dotProduct(rsold,r,r,temp) ! rsold = r^T*r
        do i=1,minval((/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/))
          call compute_Ax(Ax,p,m)               ! Compute Ax / alpha
          ! if (i.eq.i_stop) call export_3D_1C(m,Ax,'out/','Ax',0)
          ! if (i.eq.i_stop) call export_3D_1C(m,p,'out/','p',0)

          call dotProduct(alpha_temp,p,Ax,temp) ! Compute Ax / alpha
          alpha = rsold/alpha_temp              ! Compute Ax / alpha
          call assign(temp,p)                   ! Update x
          call multiply(temp,alpha)             ! Update x
          call add(x,temp)                      ! Update x
          call applyAllBCs(x,m)                 ! Apply BCs to x
          call assign(temp,Ax)                  ! Update r
          call multiply(temp,alpha)             ! Update r
          call subtract(r,temp)                 ! Update r
          call zeroWall(r,m)                    ! For dirichlet BCs
          call dotProduct(rsnew,r,r,temp)       ! Update r
          if (sqrt(rsnew).lt.10.0_cp**(-10.0_cp)) then; exit; endif

          ! if (i.eq.i_stop) call export_3D_1C(m,x,'out/','x_istop',0)
          call compute_Ax(Ax,x,m)
          call subtract(temp,b,Ax)
          call zeroGhostPoints(temp); call zeroWall(temp,m) ! OK (for MY residual computation)
          call compute(norm,temp,m)
          ! if (i.eq.i_stop) call export_3D_1C(m,temp,'out/','R_mine',0)
          write(*,*) 'Residual (CG,mine) = ',sqrt(rsnew),norm%Linf
          
          ! if (i.eq.i_stop) call export_3D_1C(m,r,'out/','r',0)

          ! if (i.eq.i_stop) stop 'Done'

          call assign(temp,p)                   ! Update p & rsold
          call multiply(temp,rsnew/rsold)
          call add(p,r,temp)
          call zeroGhostPoints(p)
          rsold = rsnew
          if (getAllNeumann(x%RF(1)%b)) call subtract(x,mean(x))

        enddo
        ! if (getAllNeumann(x)) call subtract(x,mean(x))
        if (getAllNeumann(x%RF(1)%b)) call subtract(x,mean(x))
        call applyAllBCs(x,m)
        if (displayTF) then
          call compute_Ax(Ax,x,m)
          call subtract(temp,b,Ax)
          call zeroGhostPoints(temp); call zeroWall(temp,m) ! OK (for MY residual computation)
          call compute(norm,temp,m)
          write(*,*) 'Number of CG iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells(1)*m%N_cells(2)*m%N_cells(3)/)
          call print(norm,'CG Residuals')
        endif
      end subroutine

      subroutine dotProduct(dot,A,B,temp)
        implicit none
        real(cp),intent(inout) :: dot
        type(SF),intent(in) :: A,B
        type(SF),intent(inout) :: temp
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        dot = sum(temp)
      end subroutine

      end module