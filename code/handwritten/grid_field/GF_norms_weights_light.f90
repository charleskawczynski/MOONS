       module GF_norms_weights_light_mod
       use current_precision_mod
       use grid_field_extend_mod
       use grid_extend_mod
       use data_location_extend_mod
       use array_mod
       implicit none

       private
       public :: compute_Ln_norm

       ! Interfaces:
       ! Ln(e,u,n,mesh) = L(n) = ∫∫∫ u(i,j,k)ⁿ dx dy dz   ! Only for CC data
       ! Ln(e,u,n,vol)  = L(n) = ∫∫∫ u(i,j,k)ⁿ dx dy dz

       interface compute_Ln_norm;    module procedure Ln_weights_1_GF;  end interface
       interface compute_Ln_norm;    module procedure Ln_weights_3_GF;  end interface
       interface compute_Ln_norm;    module procedure Ln_weights_9_GF;  end interface

       contains

       subroutine Ln_weights_1_GF(val,u,n,g,DL)
         implicit none
         real(cp),intent(inout) :: val
         type(grid_field),intent(in) :: u
         type(data_location),intent(in) :: DL
         type(grid),intent(in) :: g
         real(cp),intent(in) :: n
         type(array),dimension(3) :: dh
         integer,dimension(3) :: e
         real(cp) :: temp
         integer :: i,j,k
         call get_coordinates_dual_dh(dh,g,DL)
         e = N_eye(DL)
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1+e(3),u%s(3)-e(3); do j=1+e(2),u%s(2)-e(2); do i=1+e(1),u%s(1)-e(1)
           temp = temp + (U%f(i,j,k)**n)*(dh(1)%f(i-e(1))*&
                                          dh(2)%f(j-e(2))*&
                                          dh(3)%f(k-e(3)))
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         do i=1,3; call delete(dh(i)); enddo
         val = temp
       end subroutine

       subroutine Ln_weights_3_GF(val,u_x,u_y,u_z,n,g,DL)
         implicit none
         real(cp),intent(inout) :: val
         type(grid_field),intent(in) :: u_x,u_y,u_z
         type(data_location),intent(in) :: DL
         type(grid),intent(in) :: g
         real(cp),intent(in) :: n
         type(array),dimension(3) :: dh
         integer,dimension(3) :: e
         real(cp) :: temp
         integer :: i,j,k
#ifdef _DEBUG_GF_NORMS_
         call insist_shape_match(u_x,u_y,'Ln_weights_3_GF (1)')
         call insist_shape_match(u_x,u_z,'Ln_weights_3_GF (2)')
#endif
         call get_coordinates_dual_dh(dh,g,DL)
         e = N_eye(DL)
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1+e(3),u_x%s(3)-e(3); do j=1+e(2),u_x%s(2)-e(2); do i=1+e(1),u_x%s(1)-e(1)
           temp = temp + (u_x%f(i,j,k)**n+&
                          u_y%f(i,j,k)**n+&
                          u_z%f(i,j,k)**n)*(dh(1)%f(i-e(1))*&
                                            dh(2)%f(j-e(2))*&
                                            dh(3)%f(k-e(3)))
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         do i=1,3; call delete(dh(i)); enddo
         val = temp
       end subroutine

       subroutine Ln_weights_9_GF(val,u_xx,u_xy,u_xz,u_yx,u_yy,u_yz,u_zx,u_zy,u_zz,n,g,DL)
         implicit none
         real(cp),intent(inout) :: val
         type(grid_field),intent(in) :: u_xx,u_xy,u_xz,u_yx,u_yy,u_yz,u_zx,u_zy,u_zz
         type(data_location),intent(in) :: DL
         type(grid),intent(in) :: g
         real(cp),intent(in) :: n
         type(array),dimension(3) :: dh
         integer,dimension(3) :: e
         real(cp) :: temp
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
         call get_coordinates_dual_dh(dh,g,DL)
         e = N_eye(DL)
         temp = 0.0_cp
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP PARALLEL DO REDUCTION(+:temp)

#endif
         do k=1+e(3),u_xx%s(3)-e(3); do j=1+e(2),u_xx%s(2)-e(2); do i=1+e(1),u_xx%s(1)-e(1)
           temp = temp + (u_xx%f(i,j,k)**n+&
                          u_xy%f(i,j,k)**n+&
                          u_xz%f(i,j,k)**n+&
                          u_yx%f(i,j,k)**n+&
                          u_yy%f(i,j,k)**n+&
                          u_yz%f(i,j,k)**n+&
                          u_zx%f(i,j,k)**n+&
                          u_zy%f(i,j,k)**n+&
                          u_zz%f(i,j,k)**n)*(dh(1)%f(i-e(1))*&
                                             dh(2)%f(j-e(2))*&
                                             dh(3)%f(k-e(3)))
         enddo; enddo; enddo
#ifdef _PARALLELIZE_GF_NORMS_
         !$OMP END PARALLEL DO

#endif
         do i=1,3; call delete(dh(i)); enddo
         val = temp
       end subroutine

       end module