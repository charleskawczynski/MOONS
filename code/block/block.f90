       module block_mod
       use grid_mod
       use domain_mod
       use IO_tools_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import ! Essentials

       type block
         type(grid) :: B                ! Bulk
         type(grid),dimension(6) :: f   ! Faces
         type(grid),dimension(12) :: e  ! Edges
         type(grid),dimension(8) :: c   ! Corners
         type(domain) :: D_f ! Faces
         type(domain) :: D_e ! Edges
         type(domain) :: D_c ! Corners
       end type

       interface init;               module procedure init_block;               end interface
       interface init;               module procedure init_block_copy;          end interface
       interface delete;             module procedure delete_block;             end interface
       interface display;            module procedure display_block;            end interface
       interface print;              module procedure print_block;              end interface
       interface export;             module procedure export_block;             end interface
       interface import;             module procedure import_block;             end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_block(b,g)
         implicit none
         type(block),intent(inout) :: b
         type(grid),intent(in) :: g
         integer :: i
         call init(b%B,g)
         ! do i=1,6;  call init(b%f(i),g); call get_face(b%f(i),i);   enddo
         ! do i=1,12; call init(b%e(i),g); call get_edge(b%e(i),i);   enddo
         ! do i=1,8;  call init(b%c(i),g); call get_corner(b%c(i),i); enddo

         ! do i=1,6;  call init(b%D_f(i),b%f(i),b%B); enddo
         ! do i=1,12; call init(b%D_e(i),b%e(i),b%B); enddo
         ! do i=1,8;  call init(b%D_c(i),b%c(i),b%B); enddo
       end subroutine

       subroutine init_block_copy(b_out,b_in)
         implicit none
         type(block),intent(inout) :: b_out
         type(block),intent(in) :: b_in
         integer :: i
         call init(b_out%B,b_in%B)
         do i=1,6;  call init(b_out%f(i),b_in%f(i)); enddo
         do i=1,12; call init(b_out%e(i),b_in%e(i)); enddo
         do i=1,8;  call init(b_out%c(i),b_in%c(i)); enddo
       end subroutine

       subroutine delete_block(b)
         implicit none
         type(block),intent(inout) :: b
         integer :: i
         call delete(b%B)
         do i=1,6;  call delete(b%f(i)); enddo
         do i=1,12; call delete(b%e(i)); enddo
         do i=1,8;  call delete(b%c(i)); enddo
       end subroutine

       subroutine display_block(b,un)
         implicit none
         type(block),intent(in) :: b
         integer,intent(in) :: un
         integer :: i
         call display(b%B,un)
         do i=1,6;  call display(b%f(i),un); enddo
         do i=1,12; call display(b%e(i),un); enddo
         do i=1,8;  call display(b%c(i),un); enddo
       end subroutine

       subroutine print_block(b)
         implicit none
         type(block),intent(in) :: b
         integer :: i
         call print(b%B)
         do i=1,6;  call print(b%f(i)); enddo
         do i=1,12; call print(b%e(i)); enddo
         do i=1,8;  call print(b%c(i)); enddo
       end subroutine

       subroutine export_block(b,un)
         implicit none
         type(block),intent(in) :: b
         integer,intent(in) :: un
         integer :: i
         call export(b%B,un)
         do i=1,6;  call export(b%f(i),un); enddo
         do i=1,12; call export(b%e(i),un); enddo
         do i=1,8;  call export(b%c(i),un); enddo
       end subroutine

       subroutine import_block(b,un)
         implicit none
         type(block),intent(inout) :: b
         integer,intent(in) :: un
         integer :: i
         call import(b%B,un)
         do i=1,6;  call import(b%f(i),un); enddo
         do i=1,12; call import(b%e(i),un); enddo
         do i=1,8;  call import(b%c(i),un); enddo
       end subroutine

       end module