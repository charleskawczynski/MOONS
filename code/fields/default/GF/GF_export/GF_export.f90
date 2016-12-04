      module GF_export_mod
      use data_location_mod
      use GF_base_mod
      use grid_mod
      use array_mod
      use current_precision_mod
      use face_edge_corner_indexing_mod
      use exp_Tecplot_Zone_mod
      implicit none

      private

      public :: exp_3D_3C_GF,exp_3D_2C_GF,exp_3D_1C_GF,exp_3D_0C_GF ! 3D Fields
      public :: exp_2D_3C_GF,exp_2D_2C_GF,exp_2D_1C_GF,exp_2D_0C_GF ! 2D Fields
      public :: exp_1D_3C_GF,exp_1D_2C_GF,exp_1D_1C_GF,exp_1D_0C_GF ! 1D Fields

      contains

      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_3D_3C_GF(g,DL,t,pad,un,u,v,w)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        type(grid_field),intent(in) :: u,v,w
        type(array),dimension(3) :: h
        integer,dimension(3) :: s
        integer :: i,j,k
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        call get_coordinates(h,g,DL)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) h(1)%f(i),&
                      h(2)%f(j),&
                      h(3)%f(k),&
                      u%f(i,j,k),v%f(i,j,k),w%f(i,j,k)
        enddo; enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_3D_2C_GF(g,DL,t,pad,un,u,v)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        type(grid_field),intent(in) :: u,v
        type(array),dimension(3) :: h
        integer,dimension(3) :: s
        integer :: i,j,k
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        call get_coordinates(h,g,DL)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) h(1)%f(i),&
                      h(2)%f(j),&
                      h(3)%f(k),&
                      u%f(i,j,k),v%f(i,j,k)
        enddo; enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_3D_1C_GF(g,DL,t,pad,un,u)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        type(grid_field),intent(in) :: u
        type(array),dimension(3) :: h
        integer,dimension(3) :: s
        integer :: i,j,k
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        call get_coordinates(h,g,DL)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) h(1)%f(i),&
                      h(2)%f(j),&
                      h(3)%f(k),&
                      u%f(i,j,k)
        enddo; enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_3D_0C_GF(g,DL,t,pad,un,u)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: un,pad,t
        real(cp),intent(in) :: u
        type(array),dimension(3) :: h
        integer,dimension(3) :: s
        integer :: i,j,k
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,t)
        call get_coordinates(h,g,DL)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) h(1)%f(i),&
                      h(2)%f(j),&
                      h(3)%f(k),&
                      u
        enddo; enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      ! ***********************************************************************
      ! ****************************** 2D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_2D_3C_GF(g,DL,t,pad,un,u,v,w,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        type(grid_field),intent(in) :: u,v,w
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,j,i_p,j_p,k_p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates(h,g,DL)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) h(d(1))%f(i),&
                      h(d(2))%f(j),&
                      u%f(i_p,j_p,k_p),&
                      v%f(i_p,j_p,k_p),&
                      w%f(i_p,j_p,k_p)
        enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_2D_2C_GF(g,DL,t,pad,un,u,v,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        type(grid_field),intent(in) :: u,v
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,j,i_p,j_p,k_p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates(h,g,DL)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) h(d(1))%f(i),&
                      h(d(2))%f(j),&
                      u%f(i_p,j_p,k_p),&
                      v%f(i_p,j_p,k_p)
        enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_2D_1C_GF(g,DL,t,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        type(grid_field),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,j,i_p,j_p,k_p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates(h,g,DL)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) h(d(1))%f(i),&
                      h(d(2))%f(j),&
                      u%f(i_p,j_p,k_p)
        enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_2D_0C_GF(g,DL,t,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane,t
        real(cp),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,j,i_p,j_p,k_p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates(h,g,DL)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*plane + i*(1-e(1))
          j_p = e(2)*plane + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*plane + j*(1-e(3))
          write(un,*) h(d(1))%f(i),&
                      h(d(2))%f(j),&
                      u
        enddo; enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_1D_3C_GF(g,DL,t,pad,un,u,v,w,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        type(grid_field),intent(in) :: u,v,w
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: s,e,i_line
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates(h,g,DL)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) h(dir)%f(i),&
                      u%f(i_L,j_L,k_L),&
                      v%f(i_L,j_L,k_L),&
                      w%f(i_L,j_L,k_L)
        enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_1D_2C_GF(g,DL,t,pad,un,u,v,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        type(grid_field),intent(in) :: u,v
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: s,e,i_line
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates(h,g,DL)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) h(dir)%f(i),&
                      u%f(i_L,j_L,k_L),&
                      v%f(i_L,j_L,k_L)
        enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_1D_1C_GF(g,DL,t,pad,un,u,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        type(grid_field),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: s,e,i_line
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates(h,g,DL)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) h(dir)%f(i),&
                      u%f(i_L,j_L,k_L)
        enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      subroutine exp_1D_0C_GF(g,DL,t,pad,un,u,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir,t
        integer,dimension(2),intent(in) :: line
        real(cp),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(array),dimension(3) :: h
        integer :: i,i_L,j_L,k_L
        integer,dimension(3) :: s,e,i_line
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(d(1))
        i_line(d(2)) = line(d(2))
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates(h,g,DL)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line(1)
          j_L = i*e(2)+(1-e(2))*i_line(2)
          k_L = i*e(3)+(1-e(3))*i_line(3)
          write(un,*) h(dir)%f(i),&
                      u
        enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      end module