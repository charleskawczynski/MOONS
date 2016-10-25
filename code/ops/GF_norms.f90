       module GF_norms_mod
       use current_precision_mod
       use GF_base_mod
       use GF_aux_mod
       use grid_mod
       implicit none

       private
       public :: Ln
       public :: Linf

       ! Interfaces:
       ! Ln(e,u,n) = L(n) = ΣΣΣ u(i,j,k)ⁿ
       ! Linf(e,u) = L(∞) = max(abs(u))

       interface Ln;    module procedure Ln_GF_1; end interface
       interface Ln;    module procedure Ln_GF_3; end interface
       interface Ln;    module procedure Ln_GF_9; end interface
       interface Linf;  module procedure Linf_GF; end interface

       contains

       function Ln_GF_1(u,n) result(e)
         implicit none
         type(grid_field),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: e,temp
         integer :: i,j,k
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1,U%s(3); do j=1,U%s(2); do i=1,U%s(1)
           temp = temp + U%f(i,j,k)**n
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         e = temp
       end function

       function Ln_GF_3(u_x,u_y,u_z,n) result(e)
         implicit none
         type(grid_field),intent(in) :: u_x,u_y,u_z
         real(cp),intent(in) :: n
         real(cp) :: e,temp
         integer :: i,j,k
         temp = 0.0_cp
#ifdef _DEBUG_GF_NORMS_
         call insist_shape_match(u_x,u_y,'Ln_GF_3 (1)')
         call insist_shape_match(u_x,u_z,'Ln_GF_3 (2)')
#endif

#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1,u_x%s(3); do j=1,u_x%s(2); do i=1,u_x%s(1)
           temp = temp + u_x%f(i,j,k)**n+&
                         u_y%f(i,j,k)**n+&
                         u_z%f(i,j,k)**n
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         e = temp
       end function

       function Ln_GF_9(u_xx,u_xy,u_xz,u_yx,u_yy,u_yz,u_zx,u_zy,u_zz,n) result(e)
         implicit none
         type(grid_field),intent(in) :: u_xx,u_xy,u_xz,u_yx,u_yy,u_yz,u_zx,u_zy,u_zz
         real(cp),intent(in) :: n
         real(cp) :: e,temp
         integer :: i,j,k
         temp = 0.0_cp
#ifdef _DEBUG_GF_NORMS_
         call insist_shape_match(u_xx,u_xy,'Ln_GF_3 (1)')
         call insist_shape_match(u_xx,u_xy,'Ln_GF_3 (2)')
         call insist_shape_match(u_xx,u_xz,'Ln_GF_3 (3)')
         call insist_shape_match(u_xx,u_yx,'Ln_GF_3 (4)')
         call insist_shape_match(u_xx,u_yy,'Ln_GF_3 (5)')
         call insist_shape_match(u_xx,u_yz,'Ln_GF_3 (6)')
         call insist_shape_match(u_xx,u_zx,'Ln_GF_3 (7)')
         call insist_shape_match(u_xx,u_zy,'Ln_GF_3 (8)')
         call insist_shape_match(u_xx,u_zz,'Ln_GF_3 (9)')
#endif

#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1,u_xx%s(3); do j=1,u_xx%s(2); do i=1,u_xx%s(1)
           temp = temp + &
           u_xx%f(i,j,k)**n+&
           u_xy%f(i,j,k)**n+&
           u_xy%f(i,j,k)**n+&
           u_xz%f(i,j,k)**n+&
           u_yx%f(i,j,k)**n+&
           u_yy%f(i,j,k)**n+&
           u_yz%f(i,j,k)**n+&
           u_zx%f(i,j,k)**n+&
           u_zy%f(i,j,k)**n+&
           u_zz%f(i,j,k)**n
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         e = temp
       end function

       function Linf_GF(u) result(e)
         implicit none
         type(grid_field),intent(in) :: u
         real(cp) :: e
         e = amax(u)
       end function

       end module