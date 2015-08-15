      module exportRaw_mod
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

      ! character(len=8),parameter ::  arrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting

      public :: exp_3D_3C,exp_3D_2C,exp_3D_1C ! 3D Fields
      public :: exp_2D_3C,exp_2D_2C,exp_2D_1C ! 2D Fields
      public :: exp_1D_3C,exp_1D_2C,exp_1D_1C ! 1D Fields

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_3D_3C(s,pad,un,arrfmt,x,y,z,u,v,w)
        implicit none
        real(cp),dimension(:,:,:),intent(in) :: u,v,w
        real(cp),dimension(:),intent(in) :: x,y,z
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3),intent(in) :: s
        integer :: i,j,k
        do k = 1+pad,s(3)-pad; do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          write(un,'(6'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_2C(s,pad,un,arrfmt,x,y,z,u,v)
        implicit none
        real(cp),dimension(:,:,:),intent(in) :: u,v
        real(cp),dimension(:),intent(in) :: x,y,z
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3),intent(in) :: s
        integer :: i,j,k
        do k = 1+pad,s(3)-pad; do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          write(un,'(5'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine exp_3D_1C(s,pad,un,arrfmt,x,y,z,u)
        implicit none
        real(cp),dimension(:,:,:),intent(in) :: u
        real(cp),dimension(:),intent(in) :: x,y,z
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3),intent(in) :: s
        integer :: i,j,k
        do k = 1+pad,s(3)-pad; do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          write(un,'(4'//arrfmt//')') x(i),y(j),z(k),u(i,j,k)
        enddo; enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 2D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_2D_3C(s,pad,un,arrfmt,x,y,u,v,w)
        implicit none
        real(cp),dimension(:,:),intent(in) :: u,v,w
        real(cp),dimension(:),intent(in) :: x,y
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(2),intent(in) :: s
        integer :: i,j
        do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          write(un,'(5'//arrfmt//')') x(i),y(j),u(i,j),v(i,j),w(i,j)
        enddo; enddo
      end subroutine

      subroutine exp_2D_2C(s,pad,un,arrfmt,x,y,u,v)
        implicit none
        real(cp),dimension(:,:),intent(in) :: u,v
        real(cp),dimension(:),intent(in) :: x,y
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(2),intent(in) :: s
        integer :: i,j
        do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          write(un,'(4'//arrfmt//')') x(i),y(j),u(i,j),v(i,j)
        enddo; enddo
      end subroutine

      subroutine exp_2D_1C(s,pad,un,arrfmt,x,y,u)
        implicit none
        real(cp),dimension(:,:),intent(in) :: u
        real(cp),dimension(:),intent(in) :: x,y
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(2),intent(in) :: s
        integer :: i,j
        do j = 1+pad,s(2)-pad; do i = 1+pad,s(1)-pad
          write(un,'(3'//arrfmt//')') x(i),y(j),u(i,j)
        enddo; enddo
      end subroutine

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 1D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_1D_3C(s,pad,un,arrfmt,x,u,v,w)
        implicit none
        real(cp),dimension(:),intent(in) :: u,v,w
        real(cp),dimension(:),intent(in) :: x
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(1),intent(in) :: s
        integer :: i
        do i = 1+pad,s(1)-pad
          write(un,'(4'//arrfmt//')') x(i),u(i),v(i),w(i)
        enddo
      end subroutine

      subroutine exp_1D_2C(s,pad,un,arrfmt,x,u,v)
        implicit none
        real(cp),dimension(:),intent(in) :: u,v
        real(cp),dimension(:),intent(in) :: x
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(1),intent(in) :: s
        integer :: i
        do i = 1+pad,s(1)-pad
          write(un,'(3'//arrfmt//')') x(i),u(i),v(i)
        enddo
      end subroutine

      subroutine exp_1D_1C(s,pad,un,arrfmt,x,u)
        implicit none
        real(cp),dimension(:),intent(in) :: u
        real(cp),dimension(:),intent(in) :: x
        integer,intent(in) :: un,pad
        character(len=*),intent(in) :: arrfmt
        integer,dimension(1),intent(in) :: s
        integer :: i
        do i = 1+pad,s(1)-pad
          write(un,'(2'//arrfmt//')') x(i),u(i)
        enddo
      end subroutine

      end module