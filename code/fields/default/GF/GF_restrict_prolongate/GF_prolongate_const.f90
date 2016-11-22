       module GF_prolongate_const_mod
       use current_precision_mod
       use grid_mod
       use face_edge_corner_indexing_mod
       use GF_base_mod
       use GF_assign_mod
       implicit none

       private
       public :: prolongate_C_const
       public :: prolongate_N_const
       interface prolongate_C_const;  module procedure prolongate_C_GF;      end interface
       interface prolongate_C_const;  module procedure prolongate_C_reset_GF;end interface
       interface prolongate_N_const;  module procedure prolongate_N_GF;      end interface
       interface prolongate_N_const;  module procedure prolongate_N_reset_GF;end interface

       contains

       subroutine prolongate_C_GF(p,u,g,dir,x,y,z)
         ! Prolongates cell centered data if N_cells along dir is even
         implicit none
         type(grid_field),intent(inout) :: p ! fine field (size = 2*s-1)
         type(grid_field),intent(in) :: u    ! original field (size = s)
         type(grid),intent(in) :: g          ! fine grid
         integer,intent(in) :: dir,x,y,z     ! x,y,z = eye(dir)
         integer :: i,j,k
         if (mod(g%c(dir)%sc,2).eq.0) then
           ! Starting from the physical boundaries odd locations have
           ! weighting factors according to their cell size:
           do k=1+z,u%s(3)-z
           do j=1+y,u%s(2)-y
           do i=1+x,u%s(1)-x
           p%f((1+x)*i-2*x,(1+y)*j-2*y,(1+z)*k-2*z) = u%f(i,j,k)
           enddo; enddo; enddo
           ! Starting from the physical boundaries, even locations have
           ! weighting factors according to their cell size:
           do k=1+z,u%s(3)-z
           do j=1+y,u%s(2)-y
           do i=1+x,u%s(1)-x
           p%f((1+x)*i-x,(1+y)*j-y,(1+z)*k-z) = u%f(i,j,k)
           enddo; enddo; enddo
         endif
       end subroutine

       subroutine prolongate_N_GF(p,u,g,dir,x,y,z)
         ! Prolongates node centered data if N_cells along dir is even
         implicit none
         type(grid_field),intent(inout) :: p ! fine field (size = 2*s-1)
         type(grid_field),intent(in) :: u    ! original field (size = s)
         type(grid),intent(in) :: g          ! fine grid
         integer,intent(in) :: dir,x,y,z     ! x,y,z = eye(dir)
         integer :: i,j,k
         if (mod(g%c(dir)%sc,2).eq.0) then
           ! Starting from the physical boundaries, odd locations have
           ! coincident values: Index for p must be even: 2n-2
           do k=1+z,u%s(3)-z
           do j=1+y,u%s(2)-y
           do i=1+x,u%s(1)-x
           p%f((1+x)*i-2*x,(1+y)*j-2*y,(1+z)*k-2*z) = u%f(i,j,k)
           enddo; enddo; enddo

           ! All even locations are interpolated: Index for p must be even: 2n-1 (starts at ghost)
           do k=1,u%s(3)-z
           do j=1,u%s(2)-y
           do i=1,u%s(1)-x
           p%f((1+x)*i-x,(1+y)*j-y,(1+z)*k-z) = u%f( i , j , k )*0.5_cp + &
                                                u%f(i+x,j+y,k+z)*0.5_cp
           enddo; enddo; enddo
         endif
       end subroutine

       subroutine prolongate_C_reset_GF(u,g,dir,x,y,z)
         implicit none
         type(grid_field),intent(inout) :: u
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,x,y,z
         type(grid_field) :: temp
         integer,dimension(3) :: e,s
         if (u%s(dir).gt.3) then
           e = eye_given_dir(dir)
           s = (u%s-e)*(e+1)
           call init(temp,s)
           call assign(temp,0.0_cp)
           call prolongate_C_const(temp,u,g,dir,x,y,z)
           call init(u,temp)
           call assign(u,temp)
           call delete(temp)
         endif
       end subroutine

       subroutine prolongate_N_reset_GF(u,g,dir,x,y,z)
         implicit none
         type(grid_field),intent(inout) :: u
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,x,y,z
         type(grid_field) :: temp
         integer,dimension(3) :: e,s
         if (u%s(dir).gt.4) then
           e = eye_given_dir(dir)
           s = (u%s-e)*(e+1)-e
           call init(temp,s)
           call assign(temp,0.0_cp)
           call prolongate_N_const(temp,u,g,dir,x,y,z)
           call init(u,temp)
           call assign(u,temp)
           call delete(temp)
         endif
       end subroutine

       end module