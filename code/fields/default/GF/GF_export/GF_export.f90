      module GF_export_mod
      use data_location_mod
      use current_precision_mod
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
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_3D_3C(g,DL,pad,un,u,v,w)
        implicit none
        integer,intent(in) :: un,pad
        type(grid_field),intent(in) :: u,v,w
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,t
        type(array),dimension(3) :: h
        do t=1,3; call init(h(t),get_coordinates(g,DL,t)); enddo
        do k=1+pad,z%N-pad; do j=1+pad,y%N-pad; do i=1+pad,x%N-pad
          write(un,*) h(1)%f(i),h(2)%f(j),h(2)%f(k),u%GF(i,j,k),v%GF(i,j,k),w%GF(i,j,k)
        enddo; enddo; enddo
        do t=1,3; call delete(h(t)); enddo
      end subroutine

      subroutine exp_3D_2C(g,DL,pad,un,u,v)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        type(grid_field),intent(in) :: u,v
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,t
        type(array),dimension(3) :: h
        do t=1,3; call init(h(t),get_coordinates(g,DL,t)); enddo
        do k=1+pad,z%N-pad; do j=1+pad,y%N-pad; do i=1+pad,x%N-pad
          write(un,*) h(1)%f(i),h(2)%f(j),h(2)%f(k),u%GF(i,j,k),v%GF(i,j,k)
        enddo; enddo; enddo
        do t=1,3; call delete(h(t)); enddo
      end subroutine

      subroutine exp_3D_1C(g,DL,pad,un,u)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        type(grid_field),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,t
        type(array),dimension(3) :: h
        do t=1,3; call init(h(t),get_coordinates(g,DL,t)); enddo
        do k=1+pad,z%N-pad; do j=1+pad,y%N-pad; do i=1+pad,x%N-pad
          write(un,*) h(1)%f(i),h(2)%f(j),h(2)%f(k),u%GF(i,j,k)
        enddo; enddo; enddo
        do t=1,3; call delete(h(t)); enddo
      end subroutine

      subroutine exp_3D_1C(g,DL,pad,un,u)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        real(cp),intent(in) :: u
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,t
        type(array),dimension(3) :: h
        do t=1,3; call init(h(t),get_coordinates(g,DL,t)); enddo
        do k=1+pad,z%N-pad; do j=1+pad,y%N-pad; do i=1+pad,x%N-pad
          write(un,*) h(1)%f(i),h(2)%f(j),h(2)%f(k),u%GF(i,j,k)
        enddo; enddo; enddo
        do t=1,3; call delete(h(t)); enddo
      end subroutine

      subroutine exp_3D_1C_S(a,b,c,pad,un,x,y,z,u)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        real(cp),dimension(c),intent(in) :: z
        integer :: i,j,k
        do k=1+pad,c-pad; do j=1+pad,b-pad; do i=1+pad,a-pad
          write(un,*) x(i),y(j),z(k),u
        enddo; enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 2D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_2D_3C(a,b,pad,un,x,y,u,v,w)
        implicit none
        integer,intent(in) :: un,pad,a,b
        real(cp),dimension(a,b),intent(in) :: u,v,w
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j=1+pad,b-pad; do i=1+pad,a-pad
          write(un,*) x(i),y(j),u(i,j),v(i,j),w(i,j)
        enddo; enddo
      end subroutine

      subroutine exp_2D_2C(a,b,pad,un,x,y,u,v)
        implicit none
        integer,intent(in) :: un,pad,a,b
        real(cp),dimension(a,b),intent(in) :: u,v
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j=1+pad,b-pad; do i=1+pad,a-pad
          write(un,*) x(i),y(j),u(i,j),v(i,j)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C(a,b,pad,un,x,y,u)
        implicit none
        integer,intent(in) :: un,pad,a,b
        real(cp),dimension(a,b),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j=1+pad,b-pad; do i=1+pad,a-pad
          write(un,*) x(i),y(j),u(i,j)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C_S(a,b,pad,un,x,y,u)
        implicit none
        integer,intent(in) :: un,pad,a,b
        real(cp),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j=1+pad,b-pad; do i=1+pad,a-pad
          write(un,*) x(i),y(j),u
        enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

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