      module import_raw_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: imp_3D_3C,imp_3D_2C,imp_3D_1C ! 3D Fields
      public :: imp_2D_3C,imp_2D_2C,imp_2D_1C ! 2D Fields
      public :: imp_1D_3C,imp_1D_2C,imp_1D_1C ! 1D Fields

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_3D_3C(s,pad,un,arrfmt,x,y,z,u,v,w)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u,v,w
        real(cp),dimension(:),intent(inout) :: x,y,z
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3),intent(in) :: s
        integer :: i,j,k
        do k = 1+pad,s(3)-pad; do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          read(un,'(6'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine imp_3D_2C(s,pad,un,arrfmt,x,y,z,u,v)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u,v
        real(cp),dimension(:),intent(inout) :: x,y,z
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3),intent(in) :: s
        integer :: i,j,k
        do k = 1+pad,s(3)-pad; do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          read(un,'(5'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine imp_3D_1C(s,pad,un,arrfmt,x,y,z,u)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:),intent(inout) :: x,y,z
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3),intent(in) :: s
        integer :: i,j,k
        do k = 1+pad,s(3)-pad; do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          read(un,'(4'//arrfmt//')') x(i),y(j),z(k),u(i,j,k)
        enddo; enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 2D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_2D_3C(s,pad,un,arrfmt,x,y,u,v,w)
        implicit none
        real(cp),dimension(:,:),intent(inout) :: u,v,w
        real(cp),dimension(:),intent(inout) :: x,y
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(2),intent(in) :: s
        integer :: i,j
        do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          read(un,'(5'//arrfmt//')') x(i),y(j),u(i,j),v(i,j),w(i,j)
        enddo; enddo
      end subroutine

      subroutine imp_2D_2C(s,pad,un,arrfmt,x,y,u,v)
        implicit none
        real(cp),dimension(:,:),intent(inout) :: u,v
        real(cp),dimension(:),intent(inout) :: x,y
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(2),intent(in) :: s
        integer :: i,j
        do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          read(un,'(4'//arrfmt//')') x(i),y(j),u(i,j),v(i,j)
        enddo; enddo
      end subroutine

      subroutine imp_2D_1C(s,pad,un,arrfmt,x,y,u)
        implicit none
        real(cp),dimension(:,:),intent(inout) :: u
        real(cp),dimension(:),intent(inout) :: x,y
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(2),intent(in) :: s
        integer :: i,j
        do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          read(un,'(3'//arrfmt//')') x(i),y(j),u(i,j)
        enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_1D_3C(s,pad,un,arrfmt,x,u,v,w)
        implicit none
        real(cp),dimension(:),intent(inout) :: u,v,w
        real(cp),dimension(:),intent(inout) :: x
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(1),intent(in) :: s
        integer :: i
        do i = 1+pad,s(1)-pad
          read(un,'(4'//arrfmt//')') x(i),u(i),v(i),w(i)
        enddo
      end subroutine

      subroutine imp_1D_2C(s,pad,un,arrfmt,x,u,v)
        implicit none
        real(cp),dimension(:),intent(inout) :: u,v
        real(cp),dimension(:),intent(inout) :: x
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(1),intent(in) :: s
        integer :: i
        do i = 1+pad,s(1)-pad
          read(un,'(3'//arrfmt//')') x(i),u(i),v(i)
        enddo
      end subroutine

      subroutine imp_1D_1C(s,pad,un,arrfmt,x,u)
        implicit none
        real(cp),dimension(:),intent(inout) :: u
        real(cp),dimension(:),intent(inout) :: x
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(1),intent(in) :: s
        integer :: i
        do i = 1+pad,s(1)-pad
          read(un,'(2'//arrfmt//')') x(i),u(i)
        enddo
      end subroutine

      end module