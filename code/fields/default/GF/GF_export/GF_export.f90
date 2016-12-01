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

      subroutine exp_3D_3C(g,DL,TMP,pad,un,u,v,w)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(time_marching_params),intent(in) :: TMP
        integer,intent(in) :: un,pad
        type(grid_field),intent(in) :: u,v,w
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,TMP%n_step)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u%GF(i,j,k),v%GF(i,j,k),w%GF(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_2C(g,DL,TMP,pad,un,u,v)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(time_marching_params),intent(in) :: TMP
        integer,intent(in) :: un,pad
        type(grid_field),intent(in) :: u,v
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,TMP%n_step)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u%GF(i,j,k),v%GF(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_1C(g,DL,TMP,pad,un,u)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(time_marching_params),intent(in) :: TMP
        integer,intent(in) :: un,pad
        type(grid_field),intent(in) :: u
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,TMP%n_step)
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          write(un,*) g%c(1)%h(ind(1))%f(i),&
                      g%c(2)%h(ind(2))%f(j),&
                      g%c(3)%h(ind(3))%f(k),&
                      u%GF(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_1C_S(g,DL,TMP,pad,un,u)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(time_marching_params),intent(in) :: TMP
        integer,intent(in) :: un,pad
        real(cp),intent(in) :: u
        integer :: i,j,k
        integer,dimension(3) :: ind,s
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        call exp_Zone_3I(un,s-2*pad,TMP%n_step)
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

      subroutine exp_2D_3C(g,DL,pad,un,u,v,w,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane
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
        call exp_Zone_2I(un,s_2D-2*pad,TMP%n_step)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = i*(1-e(1))+e(1)*plane
          j_p = j*(1-e(2))+e(2)*plane
          k_p = k*(1-e(3))+e(3)*plane
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(i),&
                      u%GF(i_p,j_p,k_p),&
                      v%GF(i_p,j_p,k_p),&
                      w%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_2D_2C(g,DL,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane
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
        call exp_Zone_2I(un,s_2D-2*pad,TMP%n_step)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = i*(1-e(1))+e(1)*plane
          j_p = j*(1-e(2))+e(2)*plane
          k_p = k*(1-e(3))+e(3)*plane
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(i),&
                      u(d(1))%GF(i_p,j_p,k_p),&
                      u(d(2))%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C(g,DL,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane
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
        call exp_Zone_2I(un,s_2D-2*pad,TMP%n_step)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = i*(1-e(1))+e(1)*plane
          j_p = j*(1-e(2))+e(2)*plane
          k_p = k*(1-e(3))+e(3)*plane
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(i),&
                      u%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C_S(g,DL,pad,un,u,dir,plane)
        implicit none
        integer,intent(in) :: un,pad,dir,plane
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
        call exp_Zone_2I(un,s_2D-2*pad,TMP%n_step)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = i*(1-e(1))+e(1)*plane
          j_p = j*(1-e(2))+e(2)*plane
          k_p = k*(1-e(3))+e(3)*plane
          write(un,*) g%c(d(1))%h(ind(d(1)))%f(i),&
                      g%c(d(2))%h(ind(d(2)))%f(i),&
                      u
        enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************

      subroutine exp_1D_3C(g,DL,pad,un,u,v,w,dir,line)
        implicit none
        integer,intent(in) :: un,pad,dir
        integer,dimension(2),intent(in) :: line
        type(grid_field),intent(in) :: u,v,w
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,t,i_p,j_p,k_p
        integer,dimension(3) :: ind,s,e,i_line
        integer,dimension(2) :: d
        ind = get_coordinate_ind(DL)
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = e(d(1))*line(d(1))
        i_line(d(2)) = e(d(2))*line(d(2))
        i_line(dir)  = e(dir) *line(dir)
        call exp_Zone_1I(un,s(dir)-2*pad,TMP%n_step)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          t = i*e(1)+j*e(2)+k*e(3)
          i_p = i*e(1)+(1-e(1))*dir*i_line(1)
          j_p = j*e(2)+(1-e(2))*dir*i_line(2)
          k_p = k*e(3)+(1-e(3))*dir*i_line(3)
          write(un,*) g%c(dir)%h(ind(dir))%f(t),&
                      u%GF(i_p,j_p,k_p),&
                      v%GF(i_p,j_p,k_p),&
                      w%GF(i_p,j_p,k_p)
        enddo; enddo
      end subroutine

      subroutine exp_1D_3C(a,pad,un,x,u,v,w)
        implicit none
        integer,intent(in) :: un,pad,a
        real(cp),dimension(a),intent(in) :: u,v,w
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i=1+pad,a-pad
          write(un,*) x(i),u(i),v(i),w(i)
        enddo
      end subroutine

      subroutine exp_1D_2C(a,pad,un,x,u,v)
        implicit none
        integer,intent(in) :: un,pad,a
        real(cp),dimension(a),intent(in) :: u,v
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i=1+pad,a-pad
          write(un,*) x(i),u(i),v(i)
        enddo
      end subroutine

      subroutine exp_1D_1C(a,pad,un,x,u)
        implicit none
        integer,intent(in) :: un,pad,a
        real(cp),dimension(a),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i=1+pad,a-pad
          write(un,*) x(i),u(i)
        enddo
      end subroutine

      subroutine exp_1D_1C_S(a,pad,un,x,u)
        implicit none
        integer,intent(in) :: un,pad,a
        real(cp),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i=1+pad,a-pad
          write(un,*) x(i),u
        enddo
      end subroutine

      function get_coordinates(g,DL,dir) result(h)
        implicit none
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: dir
        type(array) :: h
        if (is_Node(DL)) then; call init(h,g%c(dir)%hn)
        if (is_CC(DL)) then;   call init(h,g%c(dir)%hn)
        if (is_Face(DL)) then
              if (dir.eq.get_Face(DL)) then; call init(h,g%c(dir)%hn)
          elseif (dir.ne.get_Face(DL)) then; call init(h,g%c(dir)%hc)
          else; stop 'Error: bad face DL in exp_3D_3C in GF_export.f90'
          endif
        if (is_Edge(DL)) then
              if (dir.eq.get_Face(DL)) then; call init(h,g%c(dir)%hc)
          elseif (dir.ne.get_Face(DL)) then; call init(h,g%c(dir)%hn)
          else; stop 'Error: bad edge DL in exp_3D_3C in GF_export.f90'
          endif
        else; stop 'Error: bad DL in exp_3D_3C in GF_export.f90'
        endif
      end function

      end module