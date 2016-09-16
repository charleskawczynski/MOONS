      module import_raw_formatted_mod
      use current_precision_mod
      implicit none

      private

      public :: imp_3D_3C_fmt,imp_3D_2C_fmt,imp_3D_1C_fmt ! 3D Fields
      public :: imp_2D_3C_fmt,imp_2D_2C_fmt,imp_2D_1C_fmt ! 2D Fields
      public :: imp_1D_3C_fmt,imp_1D_2C_fmt,imp_1D_1C_fmt ! 1D Fields

      public :: imp_3D_1C_S_fmt ! For mesh import
      public :: imp_2D_1C_S_fmt ! For mesh import
      public :: imp_1D_1C_S_fmt ! For mesh import

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_3D_3C_fmt(a,b,c,pad,un,fmt,x,y,z,u,v,w)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a,b,c),intent(in) :: u,v,w
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        real(cp),dimension(c),intent(in) :: z
        integer :: i,j,k
        do k = 1+pad,c-pad; do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine imp_3D_2C_fmt(a,b,c,pad,un,fmt,x,y,z,u,v)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a,b,c),intent(in) :: u,v
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        real(cp),dimension(c),intent(in) :: z
        integer :: i,j,k
        do k = 1+pad,c-pad; do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),z(k),u(i,j,k),v(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine imp_3D_1C_fmt(a,b,c,pad,un,fmt,x,y,z,u)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a,b,c),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        real(cp),dimension(c),intent(in) :: z
        integer :: i,j,k
        do k = 1+pad,c-pad; do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),z(k),u(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine imp_3D_1C_S_fmt(a,b,c,pad,un,fmt,x,y,z,u)
        implicit none
        integer,intent(in) :: un,pad,a,b,c
        character(len=*),intent(in) :: fmt
        real(cp),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        real(cp),dimension(c),intent(in) :: z
        integer :: i,j,k
        do k = 1+pad,c-pad; do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),z(k),u
        enddo; enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 2D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_2D_3C_fmt(a,b,pad,un,fmt,x,y,u,v,w)
        implicit none
        integer,intent(in) :: un,pad,a,b
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a,b),intent(in) :: u,v,w
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),u(i,j),v(i,j),w(i,j)
        enddo; enddo
      end subroutine

      subroutine imp_2D_2C_fmt(a,b,pad,un,fmt,x,y,u,v)
        implicit none
        integer,intent(in) :: un,pad,a,b
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a,b),intent(in) :: u,v
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),u(i,j),v(i,j)
        enddo; enddo
      end subroutine

      subroutine imp_2D_1C_fmt(a,b,pad,un,fmt,x,y,u)
        implicit none
        integer,intent(in) :: un,pad,a,b
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a,b),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),u(i,j)
        enddo; enddo
      end subroutine

      subroutine imp_2D_1C_S_fmt(a,b,pad,un,fmt,x,y,u)
        implicit none
        integer,intent(in) :: un,pad,a,b
        character(len=*),intent(in) :: fmt
        real(cp),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        real(cp),dimension(b),intent(in) :: y
        integer :: i,j
        do j = 1+pad,b-pad; do i = 1+pad,a-pad
          read(un,fmt) x(i),y(j),u
        enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_1D_3C_fmt(a,pad,un,fmt,x,u,v,w)
        implicit none
        integer,intent(in) :: un,pad,a
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a),intent(in) :: u,v,w
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i = 1+pad,a-pad
          read(un,fmt) x(i),u(i),v(i),w(i)
        enddo
      end subroutine

      subroutine imp_1D_2C_fmt(a,pad,un,fmt,x,u,v)
        implicit none
        integer,intent(in) :: un,pad,a
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a),intent(in) :: u,v
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i = 1+pad,a-pad
          read(un,fmt) x(i),u(i),v(i)
        enddo
      end subroutine

      subroutine imp_1D_1C_fmt(a,pad,un,fmt,x,u)
        implicit none
        integer,intent(in) :: un,pad,a
        character(len=*),intent(in) :: fmt
        real(cp),dimension(a),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i = 1+pad,a-pad
          read(un,fmt) x(i),u(i)
        enddo
      end subroutine

      subroutine imp_1D_1C_S_fmt(a,pad,un,fmt,x,u)
        implicit none
        integer,intent(in) :: un,pad,a
        character(len=*),intent(in) :: fmt
        real(cp),intent(in) :: u
        real(cp),dimension(a),intent(in) :: x
        integer :: i
        do i = 1+pad,a-pad
          read(un,fmt) x(i),u
        enddo
      end subroutine

      end module