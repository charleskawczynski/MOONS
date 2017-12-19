       module ops_Biosavart_BCs_mod
       use current_precision_mod
       use ops_del_mod
       use mesh_mod
       use mesh_domain_mod
       use ops_embedExtract_mod
       use VF_mod
       use SF_mod
       use index_mapping_mod
       use constants_mod

       implicit none

       private

       public :: update_BCs_Biosavart
       interface update_BCs_Biosavart;   module procedure update_BCs_Biosavart_VF; end interface

       contains

       subroutine update_BCs_Biosavart_VF(B,J,m,J_N,temp_N)
         ! Computes
         !
         !          mu_0           x - x'
         !   B(x) = ---- ∫ J(x) x --------- dV
         !           4pi          |x - x'|^3
         implicit none
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: J
         type(VF),intent(inout) :: J_N,temp_N
         type(mesh),intent(in) :: m
         real(cp),dimension(3) :: xprime,Jprime,x_surf,integrand
         integer,dimension(3) :: s
         integer :: i_3D,j_3D,k_3D,t_3D
         integer :: i,j,k,t
         call assign(temp_N,0.0_cp)
         call edge2Node(J_N,J,m)
         call multiply(temp_N,m%B(t)%vol(vol_ID(DL_Node())))
         call node2Face(temp_F,temp_N,m)
         ! call assign_boundaries(B,temp_F,m) ! Complicated in itself
         do t=1,m%s
             !$OMP PARALLEL DO SHARED(m) PRIVATE(Jprime,x_surf,xprime,integrand)
             s = J_N%x%BF(t)%GF%s
             i = 2
             do k=2,s(3)-1; do j=2,s(2)-1
             do k_3D=2,s(3)-1; do j_3D=2,s(2)-1; do i_3D=2,s(1)-1
               Jprime(1) = J_N%x%BF(t)%GF%f(i_3D,j_3D,k_3D)
               Jprime(2) = J_N%y%BF(t)%GF%f(i_3D,j_3D,k_3D)
               Jprime(3) = J_N%z%BF(t)%GF%f(i_3D,j_3D,k_3D)
               x_surf = get_x_from_index(m,DL_Node(),i,j,k,t)
               xprime = get_x_from_index(m,DL_Node(),i_3D,j_3D,k_3D,t)
               integrand = collocated_cross_product(Jprime,xprime,x_surf)
               call add(temp_N%x,i,j,k,t,integrand(1))
               call add(temp_N%y,i,j,k,t,integrand(2))
               call add(temp_N%z,i,j,k,t,integrand(3))
             enddo; enddo; enddo
             enddo; enddo
             !$OMP END PARALLEL DO

             !$OMP PARALLEL DO SHARED(m) PRIVATE(Jprime,xprime,x_surf)
             s = temp_E%x%BF(t)%GF%s; i = 2
             do k=2,s(3)-1; do j=2,s(2)-1
             do k_3D=2,s(3)-1; do j_3D=2,s(2)-1; do i_3D=2,s(1)-1
               Jprime(1) = J%x%BF(t)%GF%f(i_3D,j_3D,k_3D)
               Jprime(2) = J%y%BF(t)%GF%f(i_3D,j_3D,k_3D)
               Jprime(3) = J%z%BF(t)%GF%f(i_3D,j_3D,k_3D)
               x_surf = get_x_from_index(m,DL_Node(),i,j,k,t)
               xprime = get_x_from_index(m,DL_Node(),i_3D,j_3D,k_3D,t)
               integrand = collocated_cross_product(Jprime,xprime,x_surf)
               call add(temp_N%x,i,j,k,t,integrand(1))
               call add(temp_N%y,i,j,k,t,integrand(2))
               call add(temp_N%z,i,j,k,t,integrand(3))
             enddo; enddo; enddo
             enddo; enddo
             !$OMP END PARALLEL DO
         enddo
       end function

       subroutine add_single_index(A,i_3D,j_3D,k_3D,t_3D,val)
         implicit none
         type(SF),intent(inout) :: A
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: val
         temp_E%BF(t)%GF%f(i,j,k) = temp_E%BF(t)%GF%f(i,j,k) + val
       end subroutine

       function get_x_from_index(m,DL,i_3D,j_3D,k_3D,t_3D) result(x)
         implicit none
         type(mesh),intent(in) :: m
         type(data_location),intent(in) :: DL
         integer,intent(in) :: i_3D,j_3D,k_3D,t_3D
         real(cp),dimension(3) :: x
         if (N_along(DL,1)) then; x(1) = m%B(t_3D)%g%c(1)%hn(i_3D)
         else;                    x(1) = m%B(t_3D)%g%c(1)%hc(i_3D)
         endif
         if (N_along(DL,3)) then; x(2) = m%B(t_3D)%g%c(2)%hn(j_3D)
         else;                    x(2) = m%B(t_3D)%g%c(2)%hc(j_3D)
         endif
         if (N_along(DL,3)) then; x(3) = m%B(t_3D)%g%c(3)%hn(k_3D)
         else;                    x(3) = m%B(t_3D)%g%c(3)%hc(k_3D)
         endif
       end function

       function collocated_cross_product(Jprime,xprime,x_surf) result(integrand)
         implicit none
         real(cp),dimension(3),intent(in) :: Jprime,xprime,x_surf
         real(cp),dimension(3) :: integrand
         real(cp),dimension(3) :: diff,temp
         real(cp) :: denom
         if (equal(x_surf,xprime)) then
           integrand = 0.0_cp
         else
           diff = x_surf - xprime
           denom = sqrt(diff(1)**2.0_cp+diff(2)**2.0_cp+diff(3)**2.0_cp)**3.0_cp
           temp = diff/denom
           integrand(1) =+(Jprime(2)*temp(3) - Jprime(3)*temp(2))
           integrand(2) =-(Jprime(1)*temp(3) - Jprime(3)*temp(1))
           integrand(3) =+(Jprime(1)*temp(2) - Jprime(2)*temp(1))
         endif
       end function

       end module