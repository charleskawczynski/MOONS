       module GF_restrict_mod
       use current_precision_mod
       use grid_mod
       use face_edge_corner_indexing_mod
       use GF_base_mod
       use GF_assign_mod
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
         real(cp) :: alpha
         integer :: i,j,k,t
         if (mod(g%c(dir)%sc,2).eq.0) then
           !    Linearly interpolate the average of two cells to the cell center:
           ! or Linearly interpolate the two adjecent cells and average:
           do k=1+z,u%s(3)-z,1+z
           do j=1+y,u%s(2)-y,1+y
           do i=1+x,u%s(1)-x,1+x
           t = i*x + j*y + k*z
           alpha = g%c(dir)%dhn(t)/(g%c(dir)%dhn(t)+g%c(dir)%dhn(t+1))
           r%f(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = &
           u%f(i,j,k)*alpha + &
           u%f(i+x,j+y,k+z)*(1.0_cp-alpha)
           enddo; enddo; enddo
           ! Linearly extrapolate to ghost points?
           ! select case (dir)
           ! case (1); r(1,:,:) = 2.0_cp*r(2,:,:)-r(3,:,:)
           ! case (2); r(:,1,:) = 2.0_cp*r(:,2,:)-r(:,3,:)
           ! case (3); r(:,:,1) = 2.0_cp*r(:,:,2)-r(:,:,3)
           ! end select
           ! select case (dir)
           ! case (1); r(r%s(1),:,:) = 2.0_cp*r(r%s(1)-1,:,:)-r(r%s(1)-2,:,:)
           ! case (2); r(:,r%s(2),:) = 2.0_cp*r(:,r%s(2)-1,:)-r(:,r%s(2)-2,:)
           ! case (3); r(:,:,r%s(3)) = 2.0_cp*r(:,:,r%s(3)-1)-r(:,:,r%s(3)-2)
           ! end select
         endif
       end subroutine

       subroutine restrict_N_GF(r,u,g,dir,x,y,z)
         ! u {N},  mod(sc/2,2)=0
         implicit none
         type(grid_field),intent(inout) :: r  ! coarse field
         type(grid_field),intent(in) :: u     ! original field
         type(grid),intent(in) :: g           ! u-grid
         integer,intent(in) :: dir,x,y,z ! x,y,z = eye(dir)
         real(cp) :: alpha
         integer :: i,j,k,t
         if (mod(g%c(dir)%sc,2).eq.0) then
           ! Starting from the physical boundary, every odd becomes the
           ! average of the value itself and its linearly interpolated neighbors:
           do k=1+z,u%s(3)-z,1+z
           do j=1+y,u%s(2)-y,1+y
           do i=1+x,u%s(1)-x,1+x
           t = i*x + j*y + k*z
           alpha = g%c(dir)%dhn(t)/(g%c(dir)%dhn(t)+g%c(dir)%dhn(t-1))
           r%f(i*(1-x)+x*i/2+x,j*(1-y)+y*j/2+y,k*(1-z)+z*k/2+z) = 0.5_cp*(u%f(i,j,k) + &
           u%f(i-x,j-y,k-z)*alpha + &
           u%f(i+x,j+y,k+z)*(1.0_cp-alpha))
           enddo; enddo; enddo
           ! Boundary values, along dir, remain the same:
           select case (dir)
           case (1); r%f(2,:,:) = u%f(2,:,:); r%f(r%s(1)-1,:,:) = u%f(u%s(1)-1,:,:)
           case (2); r%f(:,2,:) = u%f(:,2,:); r%f(:,r%s(2)-1,:) = u%f(:,u%s(2)-1,:)
           case (3); r%f(:,:,2) = u%f(:,:,2); r%f(:,:,r%s(3)-1) = u%f(:,:,u%s(3)-1)
           end select
           ! Linearly extrapolate to ghost points?
           select case (dir)
           case (1); r%f(1,:,:) = 2.0_cp*r%f(2,:,:)-r%f(3,:,:)
           case (2); r%f(:,1,:) = 2.0_cp*r%f(:,2,:)-r%f(:,3,:)
           case (3); r%f(:,:,1) = 2.0_cp*r%f(:,:,2)-r%f(:,:,3)
           end select
           select case (dir)
           case (1); r%f(r%s(1),:,:) = 2.0_cp*r%f(r%s(1)-1,:,:)-r%f(r%s(1)-2,:,:)
           case (2); r%f(:,r%s(2),:) = 2.0_cp*r%f(:,r%s(2)-1,:)-r%f(:,r%s(2)-2,:)
           case (3); r%f(:,:,r%s(3)) = 2.0_cp*r%f(:,:,r%s(3)-1)-r%f(:,:,r%s(3)-2)
           end select
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