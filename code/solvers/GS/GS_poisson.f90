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
      use boundary_conditions_mod
      use string_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_tools_mod
      use preconditioners_mod
      use diagonals_mod
      use GS_solver_mod
      use iter_solver_params_mod
      implicit none

      private
      public :: GS_poisson_SF,GS_poisson_VF
      public :: init,solve,delete

      type GS_poisson_SF
        type(mesh) :: p,d         ! Primary / Dual grids
        integer :: un,N_iter
        type(norms) :: norm
        type(string) :: name
        logical :: setCoeff = .false.
        type(iter_solver_params) :: ISP

        type(SF) :: vol,lapu,res,f,D_inv ! cell volume, laplacian, residual, Diagonal inverse
        integer,dimension(3) :: gt,s
      end type

      type GS_poisson_VF
        type(mesh) :: p,d         ! Primary / Dual grids
        integer :: un,N_iter
        type(norms) :: norm
        type(string) :: name
        logical :: setCoeff = .false.
        type(iter_solver_params) :: ISP

        type(VF) :: vol,lapu,res,f,D_inv ! cell volume, laplacian, residual, Diagonal inverse
        integer,dimension(3) :: gtx,gty,gtz,sx,sy,sz
      end type

      interface init;         module procedure init_GS_SF;           end interface
      interface init;         module procedure init_GS_VF;           end interface
      interface delete;       module procedure delete_GS_SF;         end interface
      interface delete;       module procedure delete_GS_VF;         end interface
      interface solve;        module procedure solve_GS_poisson_SF;  end interface
      ! interface solve;        module procedure solve_GS_poisson_VF;  end interface

      contains

      subroutine init_GS_SF(GS,u,m,ISP,dir,name)
        implicit none
        type(GS_poisson_SF),intent(inout) :: GS
        type(SF),intent(in) :: u
        type(iter_solver_params),intent(in) :: ISP
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        integer :: i,t
        call init(GS%ISP,ISP)
        call init(GS%p,m)
        call init(GS%d,m)
        call init(GS%name,name)
        GS%un = new_and_open(dir,'norm_GS_'//name)
        call init(GS%norm)
        call init(GS%vol,u)
        call volume(GS%vol,m)
        GS%un = new_and_open(dir,'norm_GS_SF_'//str(GS%name))
        call tecHeader(str(GS%name),GS%un,.false.)

        if (u%is_CC) then
          do t=1,m%s; do i=1,3
            call init(GS%p%B(t)%g,m%B(t)%g%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
            GS%gt(i) = 1
          enddo; enddo
        elseif(u%is_Node) then
          do t=1,m%s; do i=1,3
            call init(GS%p%B(t)%g,m%B(t)%g%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
              GS%gt(i) = 0
          enddo; enddo
        elseif (u%is_Face) then
        elseif (u%is_Edge) then
        else; stop 'Error: mesh type was not determined in GS.f90'
        endif

        call init(GS%lapu,u)
        call init(GS%f,u)
        call init(GS%res,u)
        call init(GS%D_inv,u)

        call diag_Lap(GS%D_inv,m)
        call invert(GS%D_inv)

        GS%N_iter = 1
      end subroutine

      subroutine init_GS_VF(GS,u,m,ISP,dir,name)
        implicit none
        type(GS_poisson_VF),intent(inout) :: GS
        type(VF),intent(in) :: u
        type(iter_solver_params),intent(in) :: ISP
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        integer :: i,t

        call init(GS%ISP,ISP)
        call init(GS%p,m)
        call init(GS%d,m)
        call init(GS%name,name)
        GS%un = new_and_open(dir,'norm_GS_'//name)
        call init(GS%norm)
        call init(GS%vol,u)
        call volume(GS%vol,m)
        GS%un = new_and_open(dir,'norm_GS_SF_'//str(GS%name))
        call tecHeader(str(GS%name),GS%un,.true.)

        if (u%is_CC) then
          do t=1,m%s; do i=1,3
            call init(GS%p%B(t)%g,m%B(t)%g%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
              GS%gtx(i) = 1; GS%gty(i) = 1; GS%gtz(i) = 1
          enddo; enddo
        elseif(u%is_Node) then
          do t=1,m%s; do i=1,3
            call init(GS%p%B(t)%g,m%B(t)%g%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
              GS%gtx(i) = 0; GS%gty(i) = 0; GS%gtz(i) = 0
          enddo; enddo
        elseif (u%is_Face) then
          do t=1,m%s; do i=1,3
            call init(GS%p%B(t)%g,m%B(t)%g%c(i)%hc,i) ! mesh made from cc --> p%dhn is dhc
              GS%gtx(i) = 0; GS%gty(i) = 0; GS%gtz(i) = 0
          enddo; enddo
        elseif (u%is_Edge) then
        else; stop 'Error: mesh type was not determined in GS.f90'
        endif

        call init(GS%lapu,u)
        call init(GS%f,u)
        call init(GS%res,u)
        call init(GS%D_inv,u)

        call diag_Lap(GS%D_inv,m)
        call invert(GS%D_inv)

        GS%N_iter = 1
      end subroutine

      subroutine solve_GS_poisson_SF(GS,u,f,m,compute_norm)
        implicit none
        type(GS_poisson_SF),intent(inout) :: GS
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        logical,intent(in) :: compute_norm
        call solve_GS(u,f,GS%D_inv,m,GS%p,GS%d,GS%vol,GS%gt,GS%ISP%iter_max,&
        GS%ISP%tol_rel,GS%norm,compute_norm,GS%N_iter,GS%un,GS%lapu,GS%res,GS%f,&
        str(GS%name),GS%ISP%n_skip_check_res)
      end subroutine

      ! subroutine solve_GS_poisson_VF(GS,u,f,m,n,compute_norm)
      !   implicit none
      !   type(GS_poisson_VF),intent(inout) :: GS
      !   type(VF),intent(inout) :: u
      !   type(VF),intent(in) :: f
      !   type(mesh),intent(in) :: m
      !   integer,intent(in) :: n
      !   logical,intent(in) :: compute_norm
      !   call solve_GS(u,f,GS%D_inv,m,GS%p,GS%d,GS%vol,GS%gt,n,GS%tol,GS%norm,compute_norm,&
      !   GS%N_iter,GS%un,GS%lapu,GS%res,GS%f,str(GS%name,GS%n_skip_check_res)
      ! end subroutine

      subroutine delete_GS_SF(GS)
        implicit none
        type(GS_poisson_SF),intent(inout) :: GS
        call delete(GS%ISP)
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

      subroutine delete_GS_VF(GS)
        implicit none
        type(GS_poisson_VF),intent(inout) :: GS
        call delete(GS%ISP)
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

      subroutine tecHeader(name,un,VF)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: un
        logical,intent(in) :: VF
        if (VF) then; write(un,*) 'TITLE = "PCG_VF residuals for '//name//'"'
        else;         write(un,*) 'TITLE = "PCG_SF residuals for '//name//'"'
        endif
        write(un,*) 'VARIABLES = N_iter,L1,L2,Linf,L1_0,L2_0,Linf_0,i-1+i_earlyExit'
        write(un,*) 'ZONE DATAPACKING = POINT'
        flush(un)
      end subroutine

      end module