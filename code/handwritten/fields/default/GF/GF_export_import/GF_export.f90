      module GF_export_mod
      use data_location_mod
      use string_mod
      use grid_field_mod
      use grid_mod
      use grid_extend_mod
      use array_mod
      use array_extend_mod
      use datatype_conversion_mod
      use current_precision_mod
      use face_edge_corner_indexing_mod
      use exp_Tecplot_Zone_mod
      implicit none

      logical :: export_mid_plane = .false.
      logical :: export_mid_line  = .false.
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
        call get_coordinates_h(h,g,DL)
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
        call get_coordinates_h(h,g,DL)
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
        call get_coordinates_h(h,g,DL)
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
        call get_coordinates_h(h,g,DL)
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
        integer :: i,j,i_p,j_p,k_p,p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates_h(h,g,DL)
        p = get_p_plane_used(g,dir,plane)
        call write_tec_comment_plane(un,h,dir,p)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*p + i*(1-e(1))
          j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*p + j*(1-e(3))
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
        integer :: i,j,i_p,j_p,k_p,p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates_h(h,g,DL)
        p = get_p_plane_used(g,dir,plane)
        call write_tec_comment_plane(un,h,dir,p)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*p + i*(1-e(1))
          j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*p + j*(1-e(3))
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
        integer :: i,j,i_p,j_p,k_p,p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates_h(h,g,DL)
        p = get_p_plane_used(g,dir,plane)
        call write_tec_comment_plane(un,h,dir,p)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*p + i*(1-e(1))
          j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*p + j*(1-e(3))
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
        integer :: i,j,i_p,j_p,k_p,p
        integer,dimension(3) :: s,e
        integer,dimension(2) :: d,s_2D
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        s_2D = adj_shape_given_dir(s,dir)
        call exp_Zone_2I(un,s_2D-2*pad,t)
        call get_coordinates_h(h,g,DL)
        p = get_p_plane_used(g,dir,plane)
        call write_tec_comment_plane(un,h,dir,p)
        do j=1+pad,s(d(2))-pad; do i=1+pad,s(d(1))-pad
          i_p = e(1)*p + i*(1-e(1))
          j_p = e(2)*p + (i*(1-e(3)) + j*(1-e(1)))*(1-e(2))
          k_p = e(3)*p + j*(1-e(3))
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
        integer,dimension(3) :: s,e,i_line,i_line_used
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(1)
        i_line(d(2)) = line(2)
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates_h(h,g,DL)
        i_line_used = get_i_line_used(g,dir,i_line)
        call write_tec_comment_line(un,h,dir,i_line_used,d)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line_used(1)
          j_L = i*e(2)+(1-e(2))*i_line_used(2)
          k_L = i*e(3)+(1-e(3))*i_line_used(3)
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
        integer,dimension(3) :: s,e,i_line,i_line_used
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(1)
        i_line(d(2)) = line(2)
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates_h(h,g,DL)
        i_line_used = get_i_line_used(g,dir,i_line)
        call write_tec_comment_line(un,h,dir,i_line_used,d)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line_used(1)
          j_L = i*e(2)+(1-e(2))*i_line_used(2)
          k_L = i*e(3)+(1-e(3))*i_line_used(3)
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
        integer,dimension(3) :: s,e,i_line,i_line_used
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(1)
        i_line(d(2)) = line(2)
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates_h(h,g,DL)
        i_line_used = get_i_line_used(g,dir,i_line)
        call write_tec_comment_line(un,h,dir,i_line_used,d)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line_used(1)
          j_L = i*e(2)+(1-e(2))*i_line_used(2)
          k_L = i*e(3)+(1-e(3))*i_line_used(3)
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
        integer,dimension(3) :: s,e,i_line,i_line_used
        integer,dimension(2) :: d
        s = get_shape(g,DL)
        d = adj_dir_given_dir(dir)
        e = eye_given_dir(dir)
        i_line(d(1)) = line(1)
        i_line(d(2)) = line(2)
        i_line(dir)  = 0 ! multiplied by zero anyway
        call exp_Zone_1I(un,s(dir)-2*pad,t)
        call get_coordinates_h(h,g,DL)
        i_line_used = get_i_line_used(g,dir,i_line)
        call write_tec_comment_line(un,h,dir,i_line_used,d)
        do i=1+pad,s(dir)-pad
          i_L = i*e(1)+(1-e(1))*i_line_used(1)
          j_L = i*e(2)+(1-e(2))*i_line_used(2)
          k_L = i*e(3)+(1-e(3))*i_line_used(3)
          write(un,*) h(dir)%f(i),&
                      u
        enddo
        do i=1,3; call delete(h(i)); enddo
      end subroutine

      function get_i_line_used(g,dir,i_line) result(i_line_used)
        implicit none
        integer,intent(in) :: dir
        type(grid),intent(in) :: g
        integer,dimension(3),intent(in) :: i_line
        integer,dimension(3) :: i_line_used
        integer,dimension(2) :: d
        d = adj_dir_given_dir(dir)
        if (export_mid_line) then; i_line_used(d(1)) = g%c(d(1))%i_midplane
                                   i_line_used(d(2)) = g%c(d(2))%i_midplane
        else;                      i_line_used = i_line
        endif
      end function

      function get_p_plane_used(g,dir,plane) result(p_plane_used)
        implicit none
        type(grid),intent(in) :: g
        integer,intent(in) :: dir
        integer,intent(in) :: plane
        integer :: p_plane_used
        if (export_mid_plane) then; p_plane_used = g%c(dir)%i_midplane
        else;                       p_plane_used = plane
        endif
      end function

      subroutine write_tec_comment_line(un,h,dir,i_line_used,d)
        implicit none
        integer,intent(in) :: un,dir
        type(array),dimension(3),intent(in) :: h
        integer,dimension(3),intent(in) :: i_line_used
        integer,dimension(2),intent(in) :: d
        integer :: i
        type(string),dimension(5) :: comment
        type(string),dimension(2) :: loc
        character :: c_dir1,c_dir2
        c_dir1 = xyz_given_dir(d(1))
        c_dir2 = xyz_given_dir(d(2))
        call init(loc(1),cp2str(h(d(1))%f(i_line_used(d(1)))))
        call init(loc(2),cp2str(h(d(2))%f(i_line_used(d(2)))))
        call init(comment(1),'# Line direction = '//int2str(dir))
        call init(comment(2),'# Line index('//c_dir1//')  = '//int2str(i_line_used(d(1))))
        call init(comment(3),'# Line index('//c_dir2//')  = '//int2str(i_line_used(d(2))))
        call init(comment(4),'# Line location('//c_dir1//')  = '//str(loc(1)))
        call init(comment(5),'# Line location('//c_dir2//')  = '//str(loc(2)))
        do i=1,5; call write_formatted(comment(i),un); enddo
        do i=1,5; call delete(comment(i)); enddo
        do i=1,2; call delete(loc(i)); enddo
      end subroutine

      subroutine write_tec_comment_plane(un,h,dir,p_plane_used)
        implicit none
        integer,intent(in) :: un,dir
        type(array),dimension(3),intent(in) :: h
        integer,intent(in) :: p_plane_used
        integer :: i
        type(string),dimension(3) :: comment
        type(string) :: loc
        character :: c_dir
        c_dir = xyz_given_dir(dir)
        call init(loc,cp2str(h(dir)%f(p_plane_used)))
        call init(comment(1),'# Plane direction = '//int2str(dir))
        call init(comment(2),'# Plane index('//c_dir//')  = '//int2str(p_plane_used))
        call init(comment(3),'# Plane location('//c_dir//')  = '//str(loc))
        do i=1,3; call write_formatted(comment(i),un); enddo
        do i=1,3; call delete(comment(i)); enddo
        call delete(loc)
      end subroutine

      end module