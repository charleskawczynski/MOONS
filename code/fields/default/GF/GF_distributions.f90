      module GF_distributions_mod
        use GF_base_mod
        use GF_assign_mod
        use grid_mod
        use data_location_mod
        use current_precision_mod
        implicit none
        private

        public :: volume
        public :: sine_waves
        public :: cosine_waves
        public :: random_noise

        real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp

        interface volume;        module procedure volume_DL_GF;          end interface
        interface volume;        module procedure volume_GF;             end interface
        interface sine_waves;    module procedure sine_waves_GF;         end interface
        interface cosine_waves;  module procedure cosine_waves_GF;       end interface
        interface random_noise;  module procedure random_noise_GF;       end interface
        interface random_noise;  module procedure random_noise_GF_dir;   end interface

        contains

        subroutine volume_DL_GF(u,g,DL)
          ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
          implicit none
          type(grid_field),intent(inout) :: u
          type(grid),intent(in) :: g
          type(data_location),intent(in) :: DL
          integer :: i,j,k
          call assign(u,0.0_cp)
          if (is_CC(DL)) then
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhn(i)*&
                           g%c(2)%dhn(j)*&
                           g%c(3)%dhn(k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          elseif (is_Node(DL)) then
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhc(i-1)*&
                           g%c(2)%dhc(j-1)*&
                           g%c(3)%dhc(k-1)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          elseif (is_Face(DL)) then
          select case (DL%face)
          case (1);
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhc(i-1)*&
                           g%c(2)%dhn(j)*&
                           g%c(3)%dhn(k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          case (2);
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhn(i)*&
                           g%c(2)%dhc(j-1)*&
                           g%c(3)%dhn(k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          case (3);
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhn(i)*&
                           g%c(2)%dhn(j)*&
                           g%c(3)%dhc(k-1)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          case default; stop 'Error: bad face location in volume_DL_GF in GF_distributions.f90'
          end select
          elseif (is_Edge(DL)) then
          select case (DL%edge)
          case (1);
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhn(i)*&
                           g%c(2)%dhc(j-1)*&
                           g%c(3)%dhc(k-1)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          case (2);
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhc(i-1)*&
                           g%c(2)%dhn(j)*&
                           g%c(3)%dhc(k-1)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          case (3);
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhc(i-1)*&
                           g%c(2)%dhc(j-1)*&
                           g%c(3)%dhn(k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
          case default; stop 'Error: bad edge location in volume_DL_GF in GF_distributions.f90'
          end select
          else; stop 'Error: bad location in volume_DL_GF in GF_distributions.f90'
          endif
        end subroutine

        subroutine volume_GF(u,g)
          ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
          implicit none
          type(grid_field),intent(inout) :: u
          type(grid),intent(in) :: g
          integer :: i,j,k
          call assign(u,0.0_cp)
#ifdef _PARALLELIZE_GF_
          !$OMP PARALLEL DO SHARED(g)

#endif
          do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
              u%f(i,j,k) = g%c(1)%dhn(i)*&
                           g%c(2)%dhn(j)*&
                           g%c(3)%dhn(k)
          enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
          !$OMP END PARALLEL DO

#endif
        end subroutine

        subroutine sine_waves_GF(f,g,wavenum,phi,DL)
          implicit none
          type(grid_field),intent(inout) :: f
          type(grid),intent(in) :: g
          real(cp),dimension(3),intent(in) :: wavenum,phi
          type(data_location),intent(in) :: DL
          integer :: i,j,k
          if (is_CC(DL)) then
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          elseif (is_Node(DL)) then
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          elseif (is_Face(DL)) then
          select case (DL%face)
          case (1)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hc(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hc(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (2)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hc(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hc(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (3)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hc(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hc(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case default; stop 'Error: face must = 1,2,3 in sine_waves_GF in GF_distributions.f90'
          end select
          elseif (is_Edge(DL)) then
          select case (DL%edge)
          case (1)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hc(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (2)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hc(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (3)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
              f%f(i,j,k) = sin(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                           sin(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                           sin(wavenum(3)*PI*(g%c(3)%hc(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case default; stop 'Error: face must = 1,2,3 in sine_waves_GF in GF_distributions.f90'
          end select
          else; stop 'Error: bad input to sine_waves_GF in GF_distributions.f90'
          endif
        end subroutine

        subroutine cosine_waves_GF(f,g,wavenum,phi,DL)
          implicit none
          type(grid_field),intent(inout) :: f
          type(grid),intent(in) :: g
          real(cp),dimension(3),intent(in) :: wavenum,phi
          type(data_location),intent(in) :: DL
          integer :: i,j,k
          if (is_CC(DL)) then
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          elseif (is_Node(DL)) then
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          elseif (is_Face(DL)) then
          select case (DL%face)
          case (1)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hc(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hc(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (2)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hc(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hc(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (3)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hc(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hc(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case default; stop 'Error: face must = 1,2,3 in cosineWaves_GF in GF_distributions.f90'
          end select
          elseif (is_Edge(DL)) then
          select case (DL%edge)
          case (1)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hc(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (2)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hc(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hn(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case (3)
#ifdef _PARALLELIZE_GF_
            !$OMP PARALLEL DO SHARED(g)

#endif
            do k=1,f%s(3); do j=1,f%s(2); do i=1,f%s(1)
            f%f(i,j,k) = cos(wavenum(1)*PI*(g%c(1)%hn(i) - phi(1)))*&
                         cos(wavenum(2)*PI*(g%c(2)%hn(j) - phi(2)))*&
                         cos(wavenum(3)*PI*(g%c(3)%hc(k) - phi(3)))
            enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_
            !$OMP END PARALLEL DO

#endif
          case default; stop 'Error: face must = 1,2,3 in cosineWaves_GF in GF_distributions.f90'
          end select
          else; stop 'Error: bad input to cosineWaves_GF in GF_distributions.f90'
          endif
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