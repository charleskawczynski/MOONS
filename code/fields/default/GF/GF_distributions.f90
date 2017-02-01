      module GF_distributions_mod
        use GF_base_mod
        use GF_assign_mod
        use GF_assign_plane_mod
        use GF_multiply_mod
        use grid_mod
        use array_mod
        use data_location_mod
        use current_precision_mod
        use face_edge_corner_indexing_mod
        use constants_mod
        implicit none
        private

        public :: volume
        public :: sine_waves
        public :: sinh_waves
        public :: cosine_waves
        public :: cosh_waves
        public :: random_noise
        public :: fringe_ALEX
        public :: fringe_SERGEY
        public :: smooth_lid
        public :: smooth_lid_Shatrov
        public :: fully_developed_duct_velocity
        public :: isolated_2D_eddy
        public :: single_2D_eddy
        public :: cylinder_2D_velocity
        public :: parabolic_1D

        interface volume;             module procedure volume_DL_GF;          end interface
        interface volume;             module procedure volume_GF;             end interface
        interface sine_waves;         module procedure sine_waves_GF;         end interface
        interface sinh_waves;         module procedure sinh_waves_GF;         end interface
        interface cosine_waves;       module procedure cosine_waves_GF;       end interface
        interface cosh_waves;         module procedure cosh_waves_GF;         end interface
        interface fringe_ALEX;        module procedure fringe_ALEX_GF;        end interface
        interface fringe_SERGEY;      module procedure fringe_SERGEY_GF;      end interface
        interface smooth_lid;         module procedure smooth_lid_GF;         end interface
        interface smooth_lid_Shatrov; module procedure smooth_lid_Shatrov_GF; end interface
        interface random_noise;       module procedure random_noise_GF;       end interface
        interface random_noise;       module procedure random_noise_GF_dir;   end interface
        interface parabolic_1D;       module procedure parabolic_1D_GF;       end interface

        contains

        subroutine volume_DL_GF(u,g,DL)
          ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
          implicit none
          type(grid_field),intent(inout) :: u
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: dh
          integer :: i,j,k
          integer,dimension(3) :: e
          call assign(u,0.0_cp)
          call get_coordinates_dual_dh(dh,g,DL)
          e = N_eye(DL)

#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = dh(1)%f(i-e(1))*&
                           dh(2)%f(j-e(2))*&
                           dh(3)%f(k-e(3))
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          do i=1,3; call delete(dh(i)); enddo
        end subroutine

        subroutine volume_GF(u,g)
          implicit none
          type(grid_field),intent(inout) :: u
          type(grid),intent(in) :: g
          call volume(u,g,DL_CC())
        end subroutine

        subroutine sine_waves_GF(f,g,wavenum,phi,DL)
          implicit none
          type(grid_field),intent(inout) :: f
          type(grid),intent(in) :: g
          real(cp),dimension(3),intent(in) :: wavenum,phi
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: h
          integer :: i,j,k
          call get_coordinates_h(h,g,DL)
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = sin(wavenum(1)*PI*(h(1)%f(i) - phi(1)))*&
                         sin(wavenum(2)*PI*(h(2)%f(j) - phi(2)))*&
                         sin(wavenum(3)*PI*(h(3)%f(k) - phi(3)))
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine sinh_waves_GF(f,g,wavenum,phi,DL)
          implicit none
          type(grid_field),intent(inout) :: f
          type(grid),intent(in) :: g
          real(cp),dimension(3),intent(in) :: wavenum,phi
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: h
          integer :: i,j,k
          call get_coordinates_h(h,g,DL)
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = sinh(wavenum(1)*PI*(h(1)%f(i) - phi(1)))*&
                         sinh(wavenum(2)*PI*(h(2)%f(j) - phi(2)))*&
                         sinh(wavenum(3)*PI*(h(3)%f(k) - phi(3)))
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine cosine_waves_GF(f,g,wavenum,phi,DL)
          implicit none
          type(grid_field),intent(inout) :: f
          type(grid),intent(in) :: g
          real(cp),dimension(3),intent(in) :: wavenum,phi
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: h
          integer :: i,j,k

          call get_coordinates_h(h,g,DL)
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
          f%f(i,j,k) = cos(wavenum(1)*PI*(h(1)%f(i) - phi(1)))*&
                       cos(wavenum(2)*PI*(h(2)%f(j) - phi(2)))*&
                       cos(wavenum(3)*PI*(h(3)%f(k) - phi(3)))
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine cosh_waves_GF(f,g,wavenum,phi,DL)
          implicit none
          type(grid_field),intent(inout) :: f
          type(grid),intent(in) :: g
          real(cp),dimension(3),intent(in) :: wavenum,phi
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: h
          integer :: i,j,k

          call get_coordinates_h(h,g,DL)
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
          f%f(i,j,k) = cosh(wavenum(1)*PI*(h(1)%f(i) - phi(1)))*&
                       cosh(wavenum(2)*PI*(h(2)%f(j) - phi(2)))*&
                       cosh(wavenum(3)*PI*(h(3)%f(k) - phi(3)))
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine fringe_ALEX_GF(B,g,DL,dir)
          implicit none
          type(grid_field),intent(inout) :: B
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          integer,intent(in) :: dir
          type(array),dimension(3) :: h
          real(cp) :: Bstretch,Bshift,theta,val
          integer :: i
          Bstretch = 0.45_cp   ! stretching parameter
          Bshift = 12.5_cp     ! shift parameter
          call get_coordinates_h(h,g,DL)
          do i=1,B%s(dir)
            theta = dble(h(dir)%f(i)-Bshift*Bstretch)
            val = 0.5_cp*(1.0_cp-tanh(theta))
            call assign_plane(B,val,i,dir)
          enddo
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine fringe_SERGEY_GF(B,g,DL,dir)
          implicit none
          type(grid_field),intent(inout) :: B
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          integer,intent(in) :: dir
          type(array),dimension(3) :: h
          type(grid_field) :: temp
          real(cp) :: Bstretch,Bshift,theta,val
          integer :: i,i2
          Bstretch = 0.2_cp   ! Fringe slope
          Bshift = 10.0_cp    ! Fringe location
          call init(temp,B)
          call get_coordinates_h(h,g,DL)
          do i=1,B%s(dir)
            theta = dble((h(dir)%f(i)-Bshift)/Bstretch)
            val = 0.5_cp*(1.0_cp+tanh(theta))
            call assign_plane(temp,val,i,dir)
          enddo
          call assign(B,temp)
          i2 = 0
          do i=1+(B%s(dir)-1)/2,B%s(dir)
            call assign_plane(B,temp,i,1+(B%s(dir)+1)/2-i2,dir)
          enddo
          call delete(temp)
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine smooth_lid_GF(U,g,DL,plane,n)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          integer,intent(in) :: plane
          real(cp),intent(in) :: n
          type(array),dimension(3) :: h
          real(cp),dimension(3) :: L
          integer,dimension(2) :: a
          integer,dimension(3) :: e
          real(cp) :: xhat,yhat
          integer :: i,j,i_p,j_p,k_p,p
          call get_coordinates_h(h,g,DL)
          a = adj_dir_given_dir(plane)
          e = eye_given_dir(plane)
          L = (/(g%c(i)%maxRange/2.0_cp,i=1,3)/)
          do i=1,U%s(a(1))
          do j=1,U%s(a(2))
          do p=1,U%s(plane)
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            xhat = ( h(a(1))%f(i) / L(a(1)) )**n
            yhat = ( h(a(2))%f(j) / L(a(2)) )**n
            U%f(i_p,j_p,k_p) = (1.0_cp - xhat)*(1.0_cp - yhat)
          enddo
          enddo
          enddo
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine smooth_lid_Shatrov_GF(U,g,DL,plane,k_lid)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          integer,intent(in) :: plane
          real(cp),intent(in) :: k_lid
          type(array),dimension(3) :: h
          real(cp),dimension(3) :: L
          integer,dimension(2) :: a
          integer,dimension(3) :: e
          real(cp) :: xhat,yhat
          integer :: i,j,i_p,j_p,k_p,p
          call get_coordinates_h(h,g,DL)
          a = adj_dir_given_dir(plane)
          e = eye_given_dir(plane)
          L = (/(g%c(i)%maxRange/2.0_cp,i=1,3)/)
          do i=1,U%s(a(1))
          do j=1,U%s(a(2))
          do p=1,U%s(plane)
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            xhat = exp( -k_lid* (1.0_cp - (1.0_cp - 2.0_cp*h(a(1))%f(i))**2.0_cp) )
            yhat = exp( -k_lid* (1.0_cp - (1.0_cp - 2.0_cp*h(a(2))%f(j))**2.0_cp) )
            U%f(i_p,j_p,k_p) = (1.0_cp - xhat)**2.0_cp*(1.0_cp - yhat)**2.0_cp
          enddo
          enddo
          enddo
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine fully_developed_duct_velocity(U,g,DL,dir,posNeg)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: h
          integer,intent(in) :: dir,posNeg
          integer :: i,j,i_p,j_p,k_p
          real(cp) :: alpha,height,width,F,A,A1,A2,A3
          integer :: p,n,m,nMax,mMax
          integer,dimension(3) :: s,e
          integer,dimension(2) :: d
          ! For max number of iterations in
          ! infinite series solution:
          nMax = 100; mMax = 100
          F = 1.0_cp
          s = get_shape(g,DL)
          d = adj_dir_given_dir(dir)
          e = eye_given_dir(dir)
          call get_coordinates_h(h,g,DL)
          width  = (g%c(d(1))%hmax - g%c(d(1))%hmin)/2.0_cp
          height = (g%c(d(2))%hmax - g%c(d(2))%hmin)/2.0_cp
          alpha = width/height
          do p=1,s(dir)
          do m=1,mMax; do n=1,nMax
          do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            A1 = 16.0_cp*F*alpha**2.0_cp*height**2.0_cp/&
            ((real(m,cp)*PI)**2.0_cp+(alpha*real(n,cp)*PI)**2.0_cp)
            A2 = 1.0_cp/(real(m,cp)*PI)*1.0_cp/(real(n,cp)*PI)
            A3 = (1.0_cp-cos(real(m,cp)*PI))*(1.0_cp-cos(real(n,cp)*PI))
            A = A1*A2*A3
            U%f(i_p,j_p,k_p) = U%f(i_p,j_p,k_p) + &
            A*sin(real(m,cp)*PI*(h(d(1))%f(i)-g%c(d(1))%hmin)/(2.0_cp*width))*&
              sin(real(n,cp)*PI*(h(d(2))%f(j)-g%c(d(2))%hmin)/(2.0_cp*height))
          enddo; enddo
          enddo; enddo
          enddo
          if (posNeg.eq.-1) call multiply(U,-1.0_cp)
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine isolated_2D_eddy(U,V,DL_U,DL_V,g,dir,vsign)
          ! From
          !      Weiss, N. O. The Expulsion of Magnetic Flux
          !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
          !      Sci. 293, 310–328 (1966).
          !
          ! Computes
          !           U = curl(psi)
          ! Where
          !           psi = (-1/(2 pi)) cos(2 pi x) cos(2 pi y)
          !           u = dpsi/dy
          !           v =-dpsi/dx
          ! Computes
          !       u = dpsi/dx_1 = k cos(2 pi x_1) sin(2 pi y_2)
          !       v = dpsi/dx_2 = k sin(2 pi x_1) cos(2 pi y_2)
          implicit none
          type(grid_field),intent(inout) :: U,V
          type(data_location),intent(in) :: DL_U,DL_V
          type(grid),intent(in) :: g
          integer,intent(in) :: dir,vsign
          type(array),dimension(3) :: h
          integer :: i,j,p,i_p,j_p,k_p
          integer,dimension(3) :: s,e
          integer,dimension(2) :: d
          real(cp) :: two
          two = 2.0_cp
          d = adj_dir_given_dir(dir)
          e = eye_given_dir(dir)
          call assign(U,0.0_cp)
          call assign(V,0.0_cp)
          call get_coordinates_h(h,g,DL_U)
          s = get_shape(g,DL_U)
          do p=1,s(dir)
          do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            U%f(i_p,j_p,k_p) = cos(two*PI*h(d(1))%f(i))*sin(two*PI*h(d(2))%f(j))
          enddo; enddo
          enddo
          call get_coordinates_h(h,g,DL_V)
          s = get_shape(g,DL_V)
          do p=1,s(dir)
          do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            V%f(i_p,j_p,k_p) =-sin(two*PI*h(d(1))%f(i))*cos(two*PI*h(d(2))%f(j))
          enddo; enddo
          enddo
          if (vsign.eq.-1) then
            call multiply(U,-1.0_cp)
            call multiply(V,-1.0_cp)
          endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine single_2D_eddy(U,V,g,DL_U,DL_V,dir,vsign)
          ! From
          !      Weiss, N. O. The Expulsion of Magnetic Flux
          !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
          !      Sci. 293, 310–328 (1966).
          !
          ! Computes
          !           U = curl(psi)
          ! Where
          !           psi = (-1/pi) (1-4 y^2)^4 cos(pi x)
          !           u = dpsi/dy = (-1/pi) cos(pi x) 4 (1-4 y^2)^3(-8y)
          !                       = (32y/pi) cos(pi x) (1-4 y^2)^3
          !           v =-dpsi/dx = (1-4 y^2)^4 sin(pi x)
          !           w = 0
          ! Computes
          !       u = dpsi/dy =   (32y/pi) cos(pi x) (1-4 y^2)^3
          !       v =-dpsi/dx = - (1-4 y^2)^4 sin(pi x)
          implicit none
          type(grid_field),intent(inout) :: U,V
          type(data_location),intent(in) :: DL_U,DL_V
          type(grid),intent(in) :: g
          integer,intent(in) :: dir,vsign
          type(array),dimension(3) :: h
          integer :: i,j,p,i_p,j_p,k_p
          integer,dimension(3) :: s,e
          integer,dimension(2) :: d
          real(cp) :: one,two,three,four
          d = adj_dir_given_dir(dir)
          e = eye_given_dir(dir)
          call assign(U,0.0_cp)
          call assign(V,0.0_cp)
          one = 1.0_cp;   two = 2.0_cp
          three = 3.0_cp; four = 4.0_cp
          s = get_shape(g,DL_U)
          call get_coordinates_h(h,g,DL_U)
          do p=1,s(dir)
          do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            u%f(i_p,j_p,k_p)=(32.0_cp*h(d(2))%f(j)/PI)*((one-four*h(d(2))%f(j)**two)**three)*cos(PI*h(d(1))%f(i))
          enddo;enddo
          enddo
          s = get_shape(g,DL_V)
          call get_coordinates_h(h,g,DL_V)
          do p=1,s(dir)
          do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            v%f(i_p,j_p,k_p) = - ((one-four*h(d(2))%f(j)**two)**four)*sin(PI*h(d(1))%f(i))
          enddo;enddo
          enddo
          if (vsign.eq.-1) then
            call multiply(U,-1.0_cp)
            call multiply(V,-1.0_cp)
          endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine cylinder_2D_velocity(U,V,g,DL_U,DL_V,dir,vsign)
          ! From
          !      Weiss, N. O. The Expulsion of Magnetic Flux
          !      by Eddies. Proc. R. Soc. A Math. Phys. Eng.
          !      Sci. 293, 310–328 (1966).
          !
          ! Computes
          !           U = curl(psi)
          ! Where
          !           psi = (-1/pi) (1-4 y^2)^4 cos(pi x)
          !           u = dpsi/dy = (-1/pi) cos(pi x) 4 (1-4 y^2)^3(-8y)
          !                       = (32y/pi) cos(pi x) (1-4 y^2)^3
          !           v =-dpsi/dx = (1-4 y^2)^4 sin(pi x)
          !           w = 0
          ! Computes
          !       u = dpsi/dy =   (32y/pi) cos(pi x) (1-4 y^2)^3
          !       v =-dpsi/dx = - (1-4 y^2)^4 sin(pi x)
          implicit none
          type(grid_field),intent(inout) :: U,V
          type(data_location),intent(in) :: DL_U,DL_V
          type(grid),intent(in) :: g
          integer,intent(in) :: dir,vsign
          type(array),dimension(3) :: h
          integer :: i,j,p,i_p,j_p,k_p
          integer,dimension(3) :: s,e
          integer,dimension(2) :: d
          real(cp),dimension(3) :: hc
          real(cp) :: r,theta,r0,two,omega0
          d = adj_dir_given_dir(dir)
          e = eye_given_dir(dir)
          two = 2.0_cp
          omega0 = 1.0_cp
          r0 = 1.0_cp
          call assign(U,0.0_cp)
          call assign(V,0.0_cp)
          hc = (/((g%c(i)%hmax+g%c(i)%hmin)/2.0_cp,i=1,3)/)
          s = get_shape(g,DL_U)
          call get_coordinates_h(h,g,DL_U)
          do p=1,s(dir); do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            r = sqrt((h(d(1))%f(i)-hc(d(1)))**two + (h(d(2))%f(j)-hc(d(2)))**two)
            theta = atan2(h(d(2))%f(j),h(d(1))%f(i))
            if (r.lt.r0) u%f(i_p,j_p,k_p) =-omega0*r*sin(theta)
          enddo;enddo; enddo
          s = get_shape(g,DL_V)
          call get_coordinates_h(h,g,DL_V)
          do p=1,s(dir); do j=2,s(d(2))-1; do i=2,s(d(1))-1
            i_p = e(1)*p + i*(1-e(1))
            j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
            k_p = e(3)*p + j*(1-e(3))
            r = sqrt((h(d(1))%f(i)-hc(d(1)))**two + (h(d(2))%f(j)-hc(d(2)))**two)
            theta = atan2(h(d(2))%f(j),h(d(1))%f(i))
            if (r.lt.r0) v%f(i_p,j_p,k_p) = omega0*r*cos(theta)
          enddo;enddo; enddo
          if (vsign.eq.-1) then
            call multiply(U,-1.0_cp)
            call multiply(V,-1.0_cp)
          endif
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine parabolic_1D_GF(U,g,DL,dir)
          implicit none
          type(grid_field),intent(inout) :: U
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          integer,intent(in) :: dir
          type(array),dimension(3) :: h
          integer :: i
          real(cp) :: temp
          call assign(U,0.0_cp)
          call get_coordinates_h(h,g,DL)
          do i=1,h(dir)%N
            temp = 0.5_cp*(1.0_cp - h(dir)%f(i)**2.0_cp)
            call assign_plane(U,temp,i,dir)
          enddo
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine random_noise_GF(f)
          implicit none
          type(grid_field),intent(inout) :: f
          integer :: i,j,k
          real(cp) :: r
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO PRIVATE(r)

#endif
          do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
          call random_number(r)
          f%f(i,j,k) = r
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine random_noise_GF_dir(f,dir)
          implicit none
          type(grid_field),intent(inout) :: f
          integer,intent(in) :: dir
          real(cp) :: r
          integer :: i,j,k
          select case(dir)
          case (1)
            do i=1,f%s(1)
            call random_number(r)
            f%f(i,:,:) = r
            enddo
          case (2)
            do j=1,f%s(2)
            call random_number(r)
            f%f(:,j,:) = r
            enddo
          case (3)
            do k=1,f%s(3)
            call random_number(r)
            f%f(:,:,k) = r
            enddo
          case default
          write(*,*) 'Error: dir must = 1:3 in random_noise_GF_dir in GF_distributions.f90'
          end select
        end subroutine

      end module