      module GF_export_mod
      use data_location_mod
      use GF_base_mod
      use grid_mod
      use current_precision_mod
      use face_edge_corner_indexing_mod
      use exp_Tecplot_Zone_mod
      implicit none

      private

      public :: exp_3D_3C,exp_3D_2C,exp_3D_1C ! 3D Fields
      public :: exp_2D_3C,exp_2D_2C,exp_2D_1C ! 2D Fields
      public :: exp_1D_3C,exp_1D_2C,exp_1D_1C ! 1D Fields

      public :: exp_3D_1C_S ! For mesh export
      public :: exp_2D_1C_S ! For mesh export
      public :: exp_1D_1C_S ! For mesh export

      contains

      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_3D_3C(g,DL,t,pad,un,u,v,w)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        type(grid_field),intent(in) :: u,v,w
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u%GF(i,j,k),v%GF(i,j,k),w%GF(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_2C(g,DL,t,pad,un,u,v)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        type(grid_field),intent(in) :: u,v
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u%GF(i,j,k),v%GF(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_1C(g,DL,t,pad,un,u)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        type(grid_field),intent(in) :: u
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u%GF(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_1C_S(g,DL,t,pad,un,u)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        real(cp),intent(in) :: u
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u
        enddo; enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ****************************** 2D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_2D_3C(g,DL,t,pad,un,u,v,w,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        type(grid_field),intent(in) :: u,v,w
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,i_p,j_p,k_p
        integer,dimension(3) :: ind,s,e
        integer,dimension(2) :: d,s_2D
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(j),&
                      u%GF(i_p,j_p,k_p),&
                      v%GF(i_p,j_p,k_p),&
                      w%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_2D_2C(g,DL,t,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        type(grid_field),dimension(2),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,i_p,j_p,k_p
        integer,dimension(3) :: ind,s,e
        integer,dimension(2) :: d,s_2D
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(j),&
                      u(d(1))%GF(i_p,j_p,k_p),&
                      u(d(2))%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C(g,DL,t,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        type(grid_field),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,i_p,j_p,k_p
        integer,dimension(3) :: ind,s,e
        integer,dimension(2) :: d,s_2D
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(j),&
                      u%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C(g,DL,t,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        real(cp),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,i_p,j_p,k_p
        integer,dimension(3) :: ind,s,e
        integer,dimension(2) :: d,s_2D
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(j),&
                      u
        enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_1D_3C(g,DL,t,pad,un,u,v,w,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        type(grid_field),intent(in) :: u,v,w
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: ind,s,e,i_line
        integer,dimension(2) :: d
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! does not matter
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) g%c(dir)%h(ind(dir))%f(i),&
                      u%GF(i_L,j_L,k_L),&
                      v%GF(i_L,j_L,k_L),&
                      w%GF(i_L,j_L,k_L)
        enddo
      end subroutine

      subroutine exp_1D_2C(g,DL,t,pad,un,u,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        type(grid_field),dimension(2),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: ind,s,e,i_line
        integer,dimension(2) :: d
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! does not matter
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) g%c(dir)%h(ind(dir))%f(i),&
                      u(d(1))%GF(i_L,j_L,k_L),&
                      u(d(2))%GF(i_L,j_L,k_L)
        enddo
      end subroutine

      subroutine exp_1D_2C(g,DL,t,pad,un,u,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        type(grid_field),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: ind,s,e,i_line
        integer,dimension(2) :: d
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! does not matter
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) g%c(dir)%h(ind(dir))%f(i),&
                      u(dir)%GF(i_L,j_L,k_L)
        enddo
      end subroutine

      subroutine exp_1D_2C(g,DL,t,pad,un,u,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        real(cp),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: ind,s,e,i_line
        integer,dimension(2) :: d
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! does not matter
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) g%c(dir)%h(ind(dir))%f(i),&
                      u
        enddo
      end subroutine

      end module