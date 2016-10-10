       module mesh_block_mod
       use mesh_mod
       use block_mod
       use IO_tools_mod
       implicit none

       private
       public :: mesh_block
       public :: init,delete,display,print,export,import ! Essentials

       type mesh_block
         type(block) :: B
         type(mesh) :: m
       end type

       interface init;               module procedure init_mesh_block;               end interface
       interface init;               module procedure init_mesh_block_copy;          end interface
       interface delete;             module procedure delete_mesh_block;             end interface
       interface display;            module procedure display_mesh_block;            end interface
       interface print;              module procedure print_mesh_block;              end interface
       interface export;             module procedure export_mesh_block;             end interface
       interface import;             module procedure import_mesh_block;             end interface
       interface export;             module procedure export_mesh_block_wrapper;     end interface
       interface import;             module procedure import_mesh_block_wrapper;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_mesh_block(MB,B)
         implicit none
         type(mesh_block),intent(inout) :: MB
         type(block),intent(in) :: B
         integer :: i
         call delete(MB%m)
         call delete(MB%B)
         call init(MB%m,B%g)
         ! do i=1,6;  call add(MB%m,B%fg(i)); enddo
         ! do i=1,6;  call add(MB%m,B%fb(i)); enddo
         ! do i=1,6;  call add(MB%m,B%fi(i)); enddo

         ! do i=1,12; call add(MB%m,B%eg(i)); enddo
         ! do i=1,12; call add(MB%m,B%eb(i)); enddo
         ! do i=1,12; call add(MB%m,B%ei(i)); enddo

         ! do i=1,8;  call add(MB%m,B%cg(i)); enddo
         ! do i=1,8;  call add(MB%m,B%cb(i)); enddo
         ! do i=1,8;  call add(MB%m,B%ci(i)); enddo
       end subroutine

       subroutine init_mesh_block_copy(MB_out,MB_in)
         implicit none
         type(mesh_block),intent(inout) :: MB_out
         type(mesh_block),intent(in) :: MB_in
         call init(MB_out%B,MB_in%B)
         call init(MB_out%m,MB_in%m)
       end subroutine

       subroutine delete_mesh_block(MB)
         implicit none
         type(mesh_block),intent(inout) :: MB
         call delete(MB%B)
         call delete(MB%m)
       end subroutine

       subroutine display_mesh_block(MB,un)
         implicit none
         type(mesh_block),intent(in) :: MB
         integer,intent(in) :: un
         call display(MB%B,un)
         call display(MB%m,un)
       end subroutine

       subroutine print_mesh_block(MB)
         implicit none
         type(mesh_block),intent(in) :: MB
         call display(MB%B,6)
         call display(MB%m,6)
       end subroutine

       subroutine export_mesh_block(MB,un)
         implicit none
         type(mesh_block),intent(in) :: MB
         integer,intent(in) :: un
         call export(MB%B,un)
         call export(MB%m,un)
       end subroutine

       subroutine import_mesh_block(MB,un)
         implicit none
         type(mesh_block),intent(inout) :: MB
         integer,intent(in) :: un
         call import(MB%B,un)
         call import(MB%m,un)
       end subroutine

       subroutine export_mesh_block_wrapper(MB,dir,name)
         implicit none
         type(mesh_block),intent(in) :: MB
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(MB%B,un)
         call export(MB%m,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_mesh_block_wrapper(MB,dir,name)
         implicit none
         type(mesh_block),intent(inout) :: MB
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(MB%B,un)
         call import(MB%m,un)
         call close_and_message(un,dir,name)
       end subroutine

       end module