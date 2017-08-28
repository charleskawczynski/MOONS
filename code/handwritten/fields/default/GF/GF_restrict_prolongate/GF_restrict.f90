       module GF_restrict_mod
       use current_precision_mod
       use grid_mod
       use face_edge_corner_indexing_mod
       use GF_base_mod
       use GF_assign_mod
       use GF_assign_plane_mod
       use GF_extrap_mod
       implicit none

       private
       public :: restrict_C
       public :: restrict_N
       interface restrict_C;    module procedure restrict_C_GF;         end interface
       interface restrict_C;    module procedure restrict_C_reset_GF;   end interface
       interface restrict_N;    module procedure restrict_N_GF;         end interface
       interface restrict_N;    module procedure restrict_N_reset_GF;   end interface

       contains

       subroutine restrict_C_GF(r,u,g,dir,x,y,z)
         implicit none
         type(grid_field),intent(inout) :: r  ! coarse field
         type(grid_field),intent(in) :: u     ! original field
         type(grid),intent(in) :: g           ! u-grid
         integer,intent(in) :: dir,x,y,z ! x,y,z = eye(dir)
         real(cp) :: alpha,beta
         integer :: i,j,k,t,i_R,j_R,k_R
         if (mod(g%c(dir)%sc,2).eq.0) then
           !    Linearly interpolate the average of two cells to the cell center:
           ! or Linearly interpolate the two adjecent cells and average:
           do k=1+z,u%s(3)-z,1+z
           do j=1+y,u%s(2)-y,1+y
           do i=1+x,u%s(1)-x,1+x
           t = i*x + j*y + k*z
           i_R = i*(1-x)+x*i/2+x
           j_R = i*(1-y)+y*j/2+y
           k_R = i*(1-z)+z*k/2+z
           alpha = g%c(dir)%dhn%f(t)/(g%c(dir)%dhn%f(t)+g%c(dir)%dhn%f(t+1))
           beta  = 1.0_cp-alpha
           r%f(i_R,j_R,k_R) = u%f( i , j , k )*alpha + &
                              u%f(i+x,j+y,k+z)*beta
           enddo; enddo; enddo
           ! Linearly extrapolate to ghost points?
           call extrap(r,dir) ! Extrapolate!
         endif
       end subroutine

       subroutine restrict_N_GF(r,u,g,dir,x,y,z)
         ! u {N},  mod(sc/2,2)=0
         implicit none
         type(grid_field),intent(inout) :: r  ! coarse field
         type(grid_field),intent(in) :: u     ! original field
         type(grid),intent(in) :: g           ! u-grid
         integer,intent(in) :: dir,x,y,z ! x,y,z = eye(dir)
         real(cp) :: alpha,beta
         integer :: i,j,k,t,i_R,j_R,k_R
         if (mod(g%c(dir)%sc,2).eq.0) then
           ! Starting from the physical boundary, every odd becomes the
           ! average of the value itself and its linearly interpolated neighbors:
           do k=1+z,u%s(3)-z,1+z
           do j=1+y,u%s(2)-y,1+y
           do i=1+x,u%s(1)-x,1+x
           t = i*x + j*y + k*z
           i_R = i*(1-x)+x*i/2+x
           j_R = i*(1-y)+y*j/2+y
           k_R = i*(1-z)+z*k/2+z
           alpha = g%c(dir)%dhn%f(t)/(g%c(dir)%dhn%f(t)+g%c(dir)%dhn%f(t-1))
           beta  = 1.0_cp-alpha
           r%f(i_R,j_R,k_R) = 0.5_cp*(u%f( i , j , k ) + &
                                      u%f(i-x,j-y,k-z)*alpha + &
                                      u%f(i+x,j+y,k+z)*beta)
           enddo; enddo; enddo
           ! Boundary values, along dir, remain the same:
           !
           call assign_plane(r,u,2,2,dir)
           call assign_plane(r,u,r%s(dir)-1,u%s(dir)-1,dir)
           ! Linearly extrapolate to ghost points?
           call extrap(r,dir) ! Extrapolate!
         endif
       end subroutine

       subroutine restrict_C_reset_GF(u,g,dir,x,y,z)
         implicit none
         type(grid_field),intent(inout) :: u
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,x,y,z
         type(grid_field) :: temp
         call init(temp,u%s/(eye_given_dir(dir)+1))
         call assign(temp,0.0_cp)
         call restrict_C(temp,u,g,dir,x,y,z)
         call init(u,temp)
         call assign(u,temp)
         call delete(temp)
       end subroutine

       subroutine restrict_N_reset_GF(u,g,dir,x,y,z)
         implicit none
         type(grid_field),intent(inout) :: u
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,x,y,z
         type(grid_field) :: temp
         call init(temp,u%s/(eye_given_dir(dir)+1))
         call assign(temp,0.0_cp)
         call restrict_N(temp,u,g,dir,x,y,z)
         call init(u,temp)
         call assign(u,temp)
         call delete(temp)
       end subroutine

       end module