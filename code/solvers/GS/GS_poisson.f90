      module GS_poisson_mod
      ! call GS_poisson(GS,u,f,m,n,compute_norm)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, mesh (m) using the Gauss-Seidel (GS) method
      ! 
      ! Note that the variant of Gauss-Seidel/GS called
      ! "red-black" Gauss-Seidel is used, where the fields are 
      ! traversed in a 3D checkerboarding manner.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     m            = contains mesh information (dhc,dhn)
      !     compute_norm    = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_GS_,_EXPORT_GS_CONVERGENCE_)

      use mesh_mod
      use apply_BCs_mod
      use apply_stitches_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_tools_mod
      implicit none

      private
      public :: GS_poisson,solve
      public :: init,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type GS_poisson
        type(mesh) :: p,d         ! Primary / Dual grids
        type(SF) :: lapu,res,Dinv ! laplacian, residual, Diagonal inverse
        integer,dimension(3) :: gt,s
        logical :: setCoeff
        integer :: un,N_iter
        type(SF) :: vol
        type(norms) :: norm
      end type
      
      interface init;        module procedure init_GS;        end interface
      interface delete;      module procedure delete_GS;      end interface
      interface solve;       module procedure solve_GS;       end interface

      interface init_Dinv;   module procedure init_Dinv_RF;   end interface
      interface init_Dinv;   module procedure init_Dinv_SF;   end interface

      contains

      subroutine init_GS(GS,u,m,dir,name)
        implicit none
        type(GS_poisson),intent(inout) :: GS
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        integer :: i,t
        
        call init(GS%p,m)
        call init(GS%d,m)
        GS%un = newAndOpen(dir,'norm_GS_'//name)
        call init(GS%norm)
        call init(GS%vol,u)
        call volume(GS%vol,m)

        if (u%is_CC) then
          do t=1,u%s; do i=1,3
            call init(GS%p%g(t),m%g(t)%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
            GS%gt(i) = 1
          enddo; enddo
        elseif(u%is_Node) then
          do t=1,u%s; do i=1,3
            call init(GS%p%g(t),m%g(t)%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
              GS%gt(i) = 0
          enddo; enddo
        elseif (u%is_Face) then
        elseif (u%is_Edge) then
        else; stop 'Error: mesh type was not determined in GS.f90'
        endif

        call init(GS%lapu,u)
        call init(GS%res,u)
        call init(GS%Dinv,u)
        call init_Dinv(GS%Dinv,GS%p,GS%d,GS%gt)
        GS%N_iter = 1
      end subroutine

      subroutine delete_GS(GS)
        implicit none
        type(GS_poisson),intent(inout) :: GS
        call delete(GS%p)
        call delete(GS%d)
        call delete(GS%lapu)
        call delete(GS%res)
        call delete(GS%Dinv)
        call delete(GS%vol)
        close(GS%un)
        GS%un = 0
        GS%N_iter = 1
      end subroutine

      subroutine solve_GS(GS,u,f,m,n,compute_norm)
        implicit none
        type(GS_poisson),intent(inout) :: GS
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norm
        integer :: i
        call apply_BCs(u,m)
        do i=1,n ! THE ORDER OF THESE ROUTINE CALLS IS IMPORTANT. DO NOT CHANGE.
          !$OMP PARALLEL
          call innerLoop(u,f,m,GS%Dinv,GS,(/0,0,0/)) ! Even in odd plane
          call innerLoop(u,f,m,GS%Dinv,GS,(/1,0,0/)) ! Even in even plane
          call innerLoop(u,f,m,GS%Dinv,GS,(/0,1,0/)) ! Even in even plane
          call innerLoop(u,f,m,GS%Dinv,GS,(/0,0,1/)) ! Even in even plane
          !$OMP END PARALLEL
          !$OMP PARALLEL
          call innerLoop(u,f,m,GS%Dinv,GS,(/1,1,1/)) ! Odd in odd plane
          call innerLoop(u,f,m,GS%Dinv,GS,(/0,1,1/)) ! Odd in even plane
          call innerLoop(u,f,m,GS%Dinv,GS,(/1,0,1/)) ! Odd in even plane
          call innerLoop(u,f,m,GS%Dinv,GS,(/1,1,0/)) ! Odd in even plane
          !$OMP END PARALLEL
          call apply_BCs(u,m)
          GS%N_iter = GS%N_iter + 1

#ifdef _EXPORT_GS_CONVERGENCE_
            call lap(GS%lapu,u,m)
            call subtract(GS%res,GS%lapu,f)
            call zeroGhostPoints(GS%res)
            call compute(GS%norm,GS%res,GS%vol)
            write(GS%un,*) GS%N_iter,GS%norm%L1,GS%norm%L2,GS%norm%Linf
#endif
        enddo
        if (u%all_Neumann) call subtract(u,mean(u))
        if (compute_norm) then
          call lap(GS%lapu,u,m)
          call subtract(GS%res,GS%lapu,f)
          call zeroGhostPoints(GS%res)
          call compute(GS%norm,GS%res,GS%vol)
          call print(GS%norm,'GS Residuals')
          write(*,*) 'GS iterations = ',i-1
        endif
      end subroutine

      subroutine innerLoop(u,f,m,Dinv,GS,odd)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,Dinv
        type(mesh),intent(in) :: m
        type(GS_poisson),intent(in) :: GS
        integer,dimension(3),intent(in) :: odd
        integer :: i
        do i=1,m%s
          call redBlack(u%RF(i)%f,f%RF(i)%f,Dinv%RF(i)%f,u%RF(i)%s,&
          GS%p%g(i)%c(1)%dhn,GS%p%g(i)%c(2)%dhn,GS%p%g(i)%c(3)%dhn,&
          GS%d%g(i)%c(1)%dhn,GS%d%g(i)%c(2)%dhn,GS%d%g(i)%c(3)%dhn,&
          GS%gt,odd)
          call apply_stitches(u,m)
        enddo
      end subroutine

      subroutine redBlack(u,f,Dinv,s,dxp,dyp,dzp,dxd,dyd,dzd,gt,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f,Dinv
        integer,dimension(3),intent(in) :: s,odd
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=2+odd(3),s(3)-1,2; do j=2+odd(2),s(2)-1,2; do i=2+odd(1),s(1)-1,2
        u(i,j,k) = ( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1))) + &
                     u(i+1,j,k)/(dxp( i ) * dxd(i-1+gt(1))) + &
                     u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2))) + &
                     u(i,j+1,k)/(dyp( j ) * dyd(j-1+gt(2))) + &
                     u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3))) + &
                     u(i,j,k+1)/(dzp( k ) * dzd(k-1+gt(3))) &
                   - f(i,j,k) )*Dinv(i,j,k)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine init_Dinv_SF(Dinv,p,d,gt)
        implicit none
        type(SF),intent(inout) :: Dinv
        type(mesh),intent(in) :: p,d
        integer,dimension(3),intent(in) :: gt
        integer :: i
        do i=1,p%s
          call init_Dinv(Dinv%RF(i)%f,Dinv%RF(i)%s,&
            p%g(i)%c(1)%dhn,p%g(i)%c(2)%dhn,p%g(i)%c(3)%dhn,&
            d%g(i)%c(1)%dhn,d%g(i)%c(2)%dhn,d%g(i)%c(3)%dhn,gt)
        enddo
      end subroutine

      subroutine init_Dinv_RF(Dinv,s,dxp,dyp,dzp,dxd,dyd,dzd,gt)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: Dinv
        integer,dimension(3),intent(in) :: s
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
        Dinv(i,j,k) = 1.0_cp/(1.0_cp/dxd(i-1+gt(1))*(1.0_cp/dxp(i) + 1.0_cp/dxp(i-1)) + & 
                              1.0_cp/dyd(j-1+gt(2))*(1.0_cp/dyp(j) + 1.0_cp/dyp(j-1)) + & 
                              1.0_cp/dzd(k-1+gt(3))*(1.0_cp/dzp(k) + 1.0_cp/dzp(k-1)))
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      end module