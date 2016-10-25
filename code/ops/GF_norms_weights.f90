       module GF_norms_weights_mod
       use current_precision_mod
       use GF_base_mod
       use grid_mod
       implicit none

       private
       public :: Ln

       ! Interfaces:
       ! Ln(e,u,n,mesh) = L(n) = ∫∫∫ u(i,j,k)ⁿ dx dy dz   ! Only for CC data
       ! Ln(e,u,n,vol)  = L(n) = ∫∫∫ u(i,j,k)ⁿ dx dy dz

       interface Ln;    module procedure Ln_weights_1_GF;  end interface
       interface Ln;    module procedure Ln_weights_3_GF;  end interface
       interface Ln;    module procedure Ln_weights_9_GF;  end interface

       contains

       function Ln_weights_1_GF(u,n,vol) result(e)
         implicit none
         type(grid_field),intent(in) :: u,vol
         real(cp),intent(in) :: n
         real(cp) :: e,temp
         integer :: i,j,k
#ifdef _DEBUG_GF_NORMS_
         call insist_shape_match(u,vol,'Ln_weights_GF')
#endif
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1,u%s(3); do j=1,u%s(2); do i=1,u%s(1)
           temp = temp + (U%f(i,j,k)**n)*vol%f(i,j,k)
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         e = temp
       end function

       function Ln_weights_3_GF(u_x,u_y,u_z,n,vol) result(e)
         implicit none
         type(grid_field),intent(in) :: u_x,u_y,u_z,vol
         real(cp),intent(in) :: n
         real(cp) :: e,temp
         integer :: i,j,k
#ifdef _DEBUG_GF_NORMS_
         call insist_shape_match(u_x,u_y,'Ln_weights_3_GF (1)')
         call insist_shape_match(u_x,u_z,'Ln_weights_3_GF (2)')
#endif
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1,u_x%s(3); do j=1,u_x%s(2); do i=1,u_x%s(1)
           temp = temp + (u_x%f(i,j,k)**n+&
                          u_y%f(i,j,k)**n+&
                          u_z%f(i,j,k)**n)*vol%f(i,j,k)
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         e = temp
       end function

       function Ln_weights_9_GF(u_xx,u_xy,u_xz,u_yx,u_yy,u_yz,u_zx,u_zy,u_zz,n,vol) result(e)
         implicit none
         type(grid_field),intent(in) :: u_xx,u_xy,u_xz,u_yx,u_yy,u_yz,u_zx,u_zy,u_zz,vol
         real(cp),intent(in) :: n
         real(cp) :: e,temp
         integer :: i,j,k
#ifdef _DEBUG_GF_NORMS_
         call insist_shape_match(u_xx,u_xy,'Ln_weights_9_GF (1)')
         call insist_shape_match(u_xx,u_xz,'Ln_weights_9_GF (2)')
         call insist_shape_match(u_xx,u_yx,'Ln_weights_9_GF (3)')
         call insist_shape_match(u_xx,u_yy,'Ln_weights_9_GF (4)')
         call insist_shape_match(u_xx,u_yz,'Ln_weights_9_GF (5)')
         call insist_shape_match(u_xx,u_zx,'Ln_weights_9_GF (6)')
         call insist_shape_match(u_xx,u_zy,'Ln_weights_9_GF (7)')
         call insist_shape_match(u_xx,u_zz,'Ln_weights_9_GF (8)')
#endif
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1,u_xx%s(3); do j=1,u_xx%s(2); do i=1,u_xx%s(1)
           temp = temp + (u_xx%f(i,j,k)**n+&
                          u_xy%f(i,j,k)**n+&
                          u_xz%f(i,j,k)**n+&
                          u_yx%f(i,j,k)**n+&
                          u_yy%f(i,j,k)**n+&
                          u_yz%f(i,j,k)**n+&
                          u_zx%f(i,j,k)**n+&
                          u_zy%f(i,j,k)**n+&
                          u_zz%f(i,j,k)**n)*vol%f(i,j,k)
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         e = temp
       end function

       end module