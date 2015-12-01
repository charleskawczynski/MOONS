      module GS_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      implicit none

      private
      public :: solve_GS

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

      subroutine solve_GS(operator,u,f,m,n,norm,displayTF,res,Au)
        implicit none
        external :: operator
        type(SF),intent(inout) :: u,Au,res
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(SF),save :: Au,res,Dinv,Lx,Ux,Ly,Uy,Lz,Uz
        logical :: first
        if (first) then
          call init(res,u)
          call init(Au,u)
          call get_diagonal(Dinv,operator,m)
          call divide(1.0_cp,Dinv)
          first = .false.
        endif
        call solve_GS(operator,u,Au,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,n,norm,displayTF,res,Au)
      end subroutine

      subroutine solve_GS(operator,u,Au,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,n,norm,displayTF,res,Au)
        implicit none
        external :: operator
        type(SF),intent(inout) :: u,Au,res
        type(SF),intent(in) :: f,Dinv,Lx,Ux,Ly,Uy,Lz,Uz
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        integer :: i
        call apply_BCs(u,m) ! Boundaries
        do i=1,n ! THE ORDER OF THESE ROUTINE CALLS IS IMPORTANT. DO NOT CHANGE.
          !$OMP PARALLEL
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/0,0,0/)) ! Even in odd plane
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/1,0,0/)) ! Even in even plane
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/0,1,0/)) ! Even in even plane
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/0,0,1/)) ! Even in even plane
          !$OMP END PARALLEL
          !$OMP PARALLEL
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/1,1,1/)) ! Odd in odd plane
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/0,1,1/)) ! Odd in even plane
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/1,0,1/)) ! Odd in even plane
          call innerLoop(u,f,m,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,(/1,1,0/)) ! Odd in even plane
          !$OMP END PARALLEL
          call apply_BCs(u,m)
#ifdef _EXPORT_SOR_CONVERGENCE_
            call operator(Au,u,m)
            call subtract(res,Au,f)
            call zeroGhostPoints(res)
            call compute(norm,res,m)
            write(NU,*) norm%L1,norm%L2,norm%Linf
#endif
        enddo
        if (u%all_Neumann) call subtract(u,mean(u))
        if (displayTF) then
          write(*,*) 'GS iterations = ',n
          call operator(Au,u,m)
          call subtract(res,Au,f)
          call zeroGhostPoints(res)
          call compute(norm,res,m)
          call print(norm,'GS Residuals')
        endif
      end subroutine

      subroutine innerLoop(u,f,Dinv,Lx,Ux,Ly,Uy,Lz,Uz,odd)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,Dinv,Lx,Ux,Ly,Uy,Lz,Uz
        integer,dimension(3),intent(in) :: odd
        integer :: i
        do i=1,u%s
          call redBlack(u%RF(i)%f,&
                        f%RF(i)%f,&
                        Dinv%RF(i)%f,&
                        u%RF(i)%s,&
                        Lx%RF(i)%f,Ux%RF(i)%f,&
                        Ly%RF(i)%f,Uy%RF(i)%f,&
                        Lz%RF(i)%f,Uz%RF(i)%f,&
                        odd)
          ! call apply_stitches(u,m,i)
        enddo
      end subroutine

      subroutine redBlack(u,f,Dinv,s,Lx,Ux,Ly,Uy,Lz,Uz,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f,Dinv
        real(cp),dimension(:,:,:),intent(in) :: Lx,Ux,Ly,Uy,Lz,Uz
        integer,dimension(3),intent(in) :: s,odd
        integer :: i,j,k
        !$OMP DO
        do k=2+odd(3),s(3)-1,2; do j=2+odd(2),s(2)-1,2; do i=2+odd(1),s(1)-1,2
        u(i,j,k) = ( u(i-1,j,k)*  Lx(i,j,k) + &
                     u(i+1,j,k)*  Ux(i,j,k) + &
                     u(i,j-1,k)*  Ly(i,j,k) + &
                     u(i,j+1,k)*  Uy(i,j,k) + &
                     u(i,j,k-1)*  Lz(i,j,k) + &
                     u(i,j,k+1)*  Uz(i,j,k) &
                   - f(i,j,k) )*Dinv(i,j,k)
        enddo; enddo; enddo
        !$OMP END DO
      end subroutine

      end module