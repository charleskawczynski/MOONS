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
      use current_precision_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_stitches_mod
      use BCs_mod
      use string_mod
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

      type GS_poisson
        type(mesh) :: p,d         ! Primary / Dual grids
        type(SF) :: lapu,res,f,D_inv ! laplacian, residual, Diagonal inverse
        integer,dimension(3) :: gt,s
        logical :: setCoeff
        integer :: un,N_iter
        type(SF) :: vol
        type(norms) :: norm
        type(string) :: name
      end type
      
      interface init;        module procedure init_GS;        end interface
      interface delete;      module procedure delete_GS;      end interface
      interface solve;       module procedure solve_GS;       end interface

      interface init_D_inv;   module procedure init_D_inv_RF;   end interface
      interface init_D_inv;   module procedure init_D_inv_SF;   end interface

      contains

      subroutine init_GS(GS,u,m,tol,n_skip_check_res,dir,name)
        implicit none
        type(GS_poisson),intent(inout) :: GS
        type(SF),intent(in) :: u
        real(cp),intent(in) :: tol
        integer,intent(in) :: n_skip_check_res
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        integer :: i,t
        
        call init(GS%p,m)
        call init(GS%d,m)
        call init(GS%name,name)
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

        GS%n_skip_check_res = 100
        GS%tol = 10.0_cp**(-10.0_cp)
        call init(GS%lapu,u)
        call init(GS%f,u)
        call init(GS%res,u)
        call init(GS%D_inv,u)
        call init_D_inv(GS%D_inv,GS%p,GS%d,GS%gt)
        GS%N_iter = 1
      end subroutine

      subroutine delete_GS(GS)
        implicit none
        type(GS_poisson),intent(inout) :: GS
        call delete(GS%p)
        call delete(GS%d)
        call delete(GS%f)
        call delete(GS%lapu)
        call delete(GS%res)
        call delete(GS%D_inv)
        call delete(GS%vol)
        call delete(GS%name)
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
        call solve_GS(u,f,GS%D_inv,m,GS%p,GS%d,GS%vol,GS%gt,n,GS%tol,GS%norm,compute_norm,&
        GS%N_iter,GS%un,GS%lapu,GS%res,GS%name,GS%n_skip_check_res)
      end subroutine

      subroutine init_D_inv_SF(D_inv,p,d,gt)
        implicit none
        type(SF),intent(inout) :: D_inv
        type(mesh),intent(in) :: p,d
        integer,dimension(3),intent(in) :: gt
        integer :: i
        do i=1,p%s
          call init_D_inv(D_inv%RF(i)%f,D_inv%RF(i)%s,&
            p%g(i)%c(1)%dhn,p%g(i)%c(2)%dhn,p%g(i)%c(3)%dhn,&
            d%g(i)%c(1)%dhn,d%g(i)%c(2)%dhn,d%g(i)%c(3)%dhn,gt)
        enddo
      end subroutine

      subroutine init_D_inv_RF(D_inv,s,dxp,dyp,dzp,dxd,dyd,dzd,gt)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: D_inv
        integer,dimension(3),intent(in) :: s
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k
        !$OMP PARALLEL DO
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
        D_inv(i,j,k) = 1.0_cp/(1.0_cp/dxd(i-1+gt(1))*(1.0_cp/dxp(i) + 1.0_cp/dxp(i-1)) + & 
                               1.0_cp/dyd(j-1+gt(2))*(1.0_cp/dyp(j) + 1.0_cp/dyp(j-1)) + & 
                               1.0_cp/dzd(k-1+gt(3))*(1.0_cp/dzp(k) + 1.0_cp/dzp(k-1)))
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      end module