      module GS_solver_mod
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
      !     compute_norm = print residuals to screen (T,F)
      ! 
      ! Flags: (_PARALLELIZE_GS_,_EXPORT_GS_SF_CONVERGENCE_)
      use current_precision_mod
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

      real(cp) :: tol_abs = 10.0_cp**(-12.0_cp)
      character(len=19) :: norm_fmt = '(I10,6E40.28E3,I10)'

      private
      public :: solve_GS
      ! public :: solve_GS_SF
      interface solve_GS;     module procedure solve_GS_SF;      end interface
      interface solve_GS;     module procedure solve_GS_VF;      end interface

      interface redBlack;     module procedure redBlack_SF;      end interface
      interface redBlack;     module procedure redBlack_VF;      end interface

      interface innerloop;    module procedure innerloop_SF;     end interface
      interface innerloop;    module procedure innerloop_VF;     end interface

      contains

      subroutine solve_GS_SF(u,f_in,D_inv,m,p,d,vol,gt,n,tol,norm,compute_norm,&
        N_iter,un,lapu,res,f,name,n_skip_check_res)
        implicit none
        type(SF),intent(inout) :: u,f,lapu,res
        type(SF),intent(in) :: D_inv,vol,f_in
        type(mesh),intent(in) :: m,p,d
        integer,intent(in) :: n,un
        real(cp),intent(in) :: tol
        logical,intent(in) :: compute_norm
        type(norms),intent(inout) :: norm
        integer,intent(inout) :: N_iter,n_skip_check_res
        integer,dimension(3),intent(in) :: gt
        character(len=*),intent(in) :: name
        type(norms) :: norm0
        logical :: skip_loop
        integer :: i,i_earlyExit
        call assign(f,f_in)
        if (u%all_Neumann) call subtract_physical_mean(f)

        if (compute_norm) then
          call lap_centered(lapu,u,m)
          call subtract(res,lapu,f)
          call zeroGhostPoints(res)
          call compute(norm0,res,vol)
        endif

        call apply_BCs(u,m)
        i_earlyExit=0
        if (.not.sqrt(norm0%L2).lt.tol_abs) then ! Only do PCG if necessary!
          skip_loop = .false.
          do i=1,n
            !$OMP PARALLEL
            call innerLoop(u,f,m,p,d,D_inv,gt,(/0,0,0/)) ! Even in odd plane
            call innerLoop(u,f,m,p,d,D_inv,gt,(/1,0,0/)) ! Even in even plane
            call innerLoop(u,f,m,p,d,D_inv,gt,(/0,1,0/)) ! Even in even plane
            call innerLoop(u,f,m,p,d,D_inv,gt,(/0,0,1/)) ! Even in even plane
            !$OMP END PARALLEL
            !$OMP PARALLEL
            call innerLoop(u,f,m,p,d,D_inv,gt,(/1,1,1/)) ! Odd in odd plane
            call innerLoop(u,f,m,p,d,D_inv,gt,(/0,1,1/)) ! Odd in even plane
            call innerLoop(u,f,m,p,d,D_inv,gt,(/1,0,1/)) ! Odd in even plane
            call innerLoop(u,f,m,p,d,D_inv,gt,(/1,1,0/)) ! Odd in even plane
            !$OMP END PARALLEL
            call apply_BCs(u,m)
            N_iter = N_iter + 1

            if (mod(i,n_skip_check_res).eq.0) then
              call lap_centered(lapu,u,m)
              call subtract(res,lapu,f)
              call zeroGhostPoints(res)
              call compute(norm,res,vol)
#ifdef _EXPORT_GS_SF_CONVERGENCE_
              write(un,norm_fmt) N_iter,norm%L1,norm%L2 ,norm%Linf ,&
                                       norm0%L1,norm0%L2,norm0%Linf,i-1
#endif
              if ((norm%L2/norm0%L2.lt.tol).or.(norm%L2.lt.tol_abs)) then; i_earlyExit=1; exit; endif
            endif
          enddo
        else; i=1; skip_loop = .true.
        endif

#ifdef _EXPORT_GS_SF_CONVERGENCE_
        flush(un)
#endif

        ! if (u%all_Neumann) call subtract(u,mean(u))
        if (compute_norm) then
          if (.not.skip_loop) then
            call lap_centered(lapu,u,m)
            call subtract(res,lapu,f)
            call zeroGhostPoints(res)
            call compute(norm,res,vol); call print(norm,'GS_SF Residuals for '//name)
            write(un,norm_fmt) N_iter,norm%L1,norm%L2 ,norm%Linf ,&
                                     norm0%L1,norm0%L2,norm0%Linf,i-1+i_earlyExit
            write(*,*) 'GS_SF iterations (executed/max) = ',i-1+i_earlyExit,n
            write(*,*) 'GS_SF exit condition = ',norm%L2/norm0%L2
          else
            write(*,*) 'GS_SF skip_loop = ',skip_loop
          endif
          write(*,*) ''

        endif
      end subroutine

      subroutine solve_GS_VF(u,f_in,D_inv,m,p,d,vol,gtx,gty,gtz,n,tol,norm,compute_norm,&
        N_iter,un,lapu,res,f,name,n_skip_check_res)
        implicit none
        type(VF),intent(inout) :: u,f,lapu,res
        type(VF),intent(in) :: vol,D_inv,f_in
        type(mesh),intent(in) :: m,p,d
        integer,intent(in) :: n,un
        real(cp),intent(in) :: tol
        logical,intent(in) :: compute_norm
        type(norms),intent(inout) :: norm
        integer,intent(inout) :: N_iter,n_skip_check_res
        integer,dimension(3),intent(in) :: gtx,gty,gtz
        character(len=*),intent(in) :: name
        type(norms) :: norm0
        logical :: skip_loop
        integer :: i,i_earlyExit
        call assign(f,f_in)
        ! if (u%all_Neumann) call subtract_physical_mean(f)

        if (compute_norm) then
          call lap(lapu,u,m)
          call subtract(res,lapu,f)
          call zeroGhostPoints(res)
          call compute(norm0,res,vol)
        endif

        call apply_BCs(u,m)
        i_earlyExit=0
        if (.not.sqrt(norm0%L2).lt.tol_abs) then ! Only do PCG if necessary!
          skip_loop = .false.
          do i=1,n
            !$OMP PARALLEL
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/0,0,0/)) ! Even in odd plane
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/1,0,0/)) ! Even in even plane
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/0,1,0/)) ! Even in even plane
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/0,0,1/)) ! Even in even plane
            !$OMP END PARALLEL
            !$OMP PARALLEL
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/1,1,1/)) ! Odd in odd plane
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/0,1,1/)) ! Odd in even plane
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/1,0,1/)) ! Odd in even plane
            call innerLoop(u,f,m,p,d,D_inv,gtx,gty,gtz,(/1,1,0/)) ! Odd in even plane
            !$OMP END PARALLEL
            call apply_BCs(u,m)
            N_iter = N_iter + 1

            if (mod(i,n_skip_check_res).eq.0) then
              call lap(lapu,u,m)
              call subtract(res,lapu,f)
              call zeroGhostPoints(res)
              call compute(norm,res,vol)
#ifdef _EXPORT_GS_SF_CONVERGENCE_
              write(un,norm_fmt) N_iter,norm%L1,norm%L2,norm%Linf,&
                                 norm0%L1,norm0%L2,norm0%Linf,i-1
#endif
              if ((norm%L2/norm0%L2.lt.tol).or.(norm%L2.lt.tol_abs)) then; i_earlyExit=1; exit; endif
            endif
          enddo
        else; i=1; skip_loop = .true.
        endif

#ifdef _EXPORT_GS_SF_CONVERGENCE_
        flush(un)
#endif

        ! if (u%all_Neumann) call subtract(u,mean(u))
        if (compute_norm) then
          if (.not.skip_loop) then
            call lap(lapu,u,m)
            call subtract(res,lapu,f)
            call zeroGhostPoints(res)
            call compute(norm,res,vol); call print(norm,'GS_SF Residuals for '//name)
              write(un,norm_fmt) N_iter,norm%L1,norm%L2,norm%Linf,&
                                 norm0%L1,norm0%L2,norm0%Linf,i-1
            write(*,*) 'GS_VF iterations (executed/max) = ',i-1+i_earlyExit,n
            write(*,*) 'GS_VF exit condition = ',norm%L2/norm0%L2
          else
            write(*,*) 'GS_VF skip_loop = ',skip_loop
          endif
          write(*,*) ''

        endif
      end subroutine

      subroutine innerLoop_SF(u,f,m,p,d,D_inv,gt,odd)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,D_inv
        type(mesh),intent(in) :: m,p,d
        integer,dimension(3),intent(in) :: odd
        integer,dimension(3),intent(in) :: gt
        integer :: i
        do i=1,m%s
          call redBlack(u%GF(i)%f,f%GF(i)%f,D_inv%GF(i)%f,u%GF(i)%s,&
          p%g(i)%c(1)%dhn,p%g(i)%c(2)%dhn,p%g(i)%c(3)%dhn,&
          d%g(i)%c(1)%dhn,d%g(i)%c(2)%dhn,d%g(i)%c(3)%dhn,&
          gt,odd)
          ! call apply_stitches(u,m)
          call apply_BCs(u,m)
        enddo
      end subroutine

      subroutine innerLoop_VF(u,f,m,p,d,D_inv,gtx,gty,gtz,odd)
        implicit none
        type(VF),intent(inout) :: u
        type(VF),intent(in) :: f,D_inv
        type(mesh),intent(in) :: m,p,d
        integer,dimension(3),intent(in) :: odd
        integer,dimension(3),intent(in) :: gtx,gty,gtz
        integer :: i
        do i=1,m%s
          call redBlack(u%x%GF(i)%f,u%y%GF(i)%f,u%z%GF(i)%f,&
                        f%x%GF(i)%f,f%y%GF(i)%f,f%z%GF(i)%f,&
                        D_inv%x%GF(i)%f,D_inv%y%GF(i)%f,D_inv%z%GF(i)%f,&
                        u%x%GF(i)%s,u%y%GF(i)%s,u%z%GF(i)%s,&
          p%g(i)%c(1)%dhn,p%g(i)%c(2)%dhn,p%g(i)%c(3)%dhn,&
          d%g(i)%c(1)%dhn,d%g(i)%c(2)%dhn,d%g(i)%c(3)%dhn,&
          gtx,gty,gtz,odd)
          ! call apply_stitches(u,m)
          call apply_BCs(u,m)
        enddo
      end subroutine

      subroutine redBlack_SF(u,f,D_inv,s,dxp,dyp,dzp,dxd,dyd,dzd,gt,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f,D_inv
        integer,dimension(3),intent(in) :: s,odd
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k
#ifdef _PARALLELIZE_GS_
        !$OMP PARALLEL DO

#endif
        do k=2+odd(3),s(3)-1,2; do j=2+odd(2),s(2)-1,2; do i=2+odd(1),s(1)-1,2
        u(i,j,k) = ( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1))) + &
                     u(i+1,j,k)/(dxp( i ) * dxd(i-1+gt(1))) + &
                     u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2))) + &
                     u(i,j+1,k)/(dyp( j ) * dyd(j-1+gt(2))) + &
                     u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3))) + &
                     u(i,j,k+1)/(dzp( k ) * dzd(k-1+gt(3))) &
                   - f(i,j,k) )*D_inv(i,j,k)
        enddo; enddo; enddo
#ifdef _PARALLELIZE_GS_
        !$OMP END PARALLEL DO

#endif
      end subroutine

      subroutine redBlack_VF(u,v,w,fx,fy,fz,Dx_inv,Dy_inv,Dz_inv,sx,sy,sz,&
        dxp,dyp,dzp,dxd,dyd,dzd,gtx,gty,gtz,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u,v,w
        real(cp),dimension(:,:,:),intent(in) :: fx,fy,fz,Dx_inv,Dy_inv,Dz_inv
        integer,dimension(3),intent(in) :: sx,sy,sz,odd
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        integer,dimension(3),intent(in) :: gtx,gty,gtz
        integer :: i,j,k
#ifdef _PARALLELIZE_GS_
        !$OMP PARALLEL DO

#endif
        do k=2+odd(3),sx(3)-1,2; do j=2+odd(2),sx(2)-1,2; do i=2+odd(1),sx(1)-1,2
        u(i,j,k) = ( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gtx(1))) + &
                     u(i+1,j,k)/(dxp( i ) * dxd(i-1+gtx(1))) + &
                     u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gtx(2))) + &
                     u(i,j+1,k)/(dyp( j ) * dyd(j-1+gtx(2))) + &
                     u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gtx(3))) + &
                     u(i,j,k+1)/(dzp( k ) * dzd(k-1+gtx(3))) &
                   - fx(i,j,k) )*Dx_inv(i,j,k)
        enddo; enddo; enddo
        do k=2+odd(3),sy(3)-1,2; do j=2+odd(2),sy(2)-1,2; do i=2+odd(1),sy(1)-1,2
        v(i,j,k) = ( v(i-1,j,k)/(dxp(i-1) * dxd(i-1+gty(1))) + &
                     v(i+1,j,k)/(dxp( i ) * dxd(i-1+gty(1))) + &
                     v(i,j-1,k)/(dyp(j-1) * dyd(j-1+gty(2))) + &
                     v(i,j+1,k)/(dyp( j ) * dyd(j-1+gty(2))) + &
                     v(i,j,k-1)/(dzp(k-1) * dzd(k-1+gty(3))) + &
                     v(i,j,k+1)/(dzp( k ) * dzd(k-1+gty(3))) &
                   - fy(i,j,k) )*Dy_inv(i,j,k)
        enddo; enddo; enddo
        do k=2+odd(3),sz(3)-1,2; do j=2+odd(2),sz(2)-1,2; do i=2+odd(1),sz(1)-1,2
        w(i,j,k) = ( w(i-1,j,k)/(dxp(i-1) * dxd(i-1+gtz(1))) + &
                     w(i+1,j,k)/(dxp( i ) * dxd(i-1+gtz(1))) + &
                     w(i,j-1,k)/(dyp(j-1) * dyd(j-1+gtz(2))) + &
                     w(i,j+1,k)/(dyp( j ) * dyd(j-1+gtz(2))) + &
                     w(i,j,k-1)/(dzp(k-1) * dzd(k-1+gtz(3))) + &
                     w(i,j,k+1)/(dzp( k ) * dzd(k-1+gtz(3))) &
                   - fz(i,j,k) )*Dz_inv(i,j,k)
        enddo; enddo; enddo
#ifdef _PARALLELIZE_GS_
        !$OMP END PARALLEL DO

#endif

      end subroutine

      end module