       module block_mod
       use grid_mod
       use IO_tools_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import ! Essentials

       type block
         type(grid) :: g                                 ! Bulk
         type(grid),dimension(:),allocatable :: fg,fb,fi ! Faces (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: eg,eb,ei ! Edges (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: cg,cb,ci ! Corners (boundary,ghost,interior)
       end type

       interface init;               module procedure init_block;               end interface
       interface init;               module procedure init_block_copy;          end interface
       interface delete;             module procedure delete_block;             end interface
       interface display;            module procedure display_block;            end interface
       interface print;              module procedure print_block;              end interface
       interface export;             module procedure export_block;             end interface
       interface import;             module procedure import_block;             end interface
       interface export;             module procedure export_block_wrapper;     end interface
       interface import;             module procedure import_block_wrapper;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_block(B,g)
         implicit none
         type(block),intent(inout) :: B
         type(grid),intent(in) :: g
         integer :: i
         call delete(B)
         call init(B%g,g)
         i = 6;allocate(B%fg(i))
         i = 6;allocate(B%fb(i))
         i = 6;allocate(B%fi(i))

         i =12;allocate(B%eg(i))
         i =12;allocate(B%eb(i))
         i =12;allocate(B%ei(i))

         i = 8;allocate(B%cg(i))
         i = 8;allocate(B%cb(i))
         i = 8;allocate(B%ci(i))

         do i=1,6;  call get_face_g(B%fg(i),g,i);   enddo
         do i=1,6;  call get_face_b(B%fb(i),g,i);   enddo
         do i=1,6;  call get_face_i(B%fi(i),g,i);   enddo

         do i=1,12; call get_edge_g(B%eg(i),g,i);   enddo
         do i=1,12; call get_edge_b(B%eb(i),g,i);   enddo
         do i=1,12; call get_edge_i(B%ei(i),g,i);   enddo

         do i=1,8;  call get_corner_g(B%cg(i),g,i); enddo
         do i=1,8;  call get_corner_b(B%cb(i),g,i); enddo
         do i=1,8;  call get_corner_i(B%ci(i),g,i); enddo
       end subroutine

       subroutine init_block_copy(B_out,B_in)
         implicit none
         type(block),intent(inout) :: B_out
         type(block),intent(in) :: B_in
         integer :: i
         call init(B_out%g,B_in%g)
         do i=1,6;  call init(B_out%fg(i),B_in%fg(i)); enddo
         do i=1,6;  call init(B_out%fb(i),B_in%fb(i)); enddo
         do i=1,6;  call init(B_out%fi(i),B_in%fi(i)); enddo

         do i=1,12; call init(B_out%eg(i),B_in%eg(i)); enddo
         do i=1,12; call init(B_out%eb(i),B_in%eb(i)); enddo
         do i=1,12; call init(B_out%ei(i),B_in%ei(i)); enddo

         do i=1,8;  call init(B_out%cg(i),B_in%cg(i)); enddo
         do i=1,8;  call init(B_out%cb(i),B_in%cb(i)); enddo
         do i=1,8;  call init(B_out%ci(i),B_in%ci(i)); enddo
       end subroutine

       subroutine delete_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         call delete(B%g)
         
         if (allocated(B%fg)) then; do i=1,6;  call delete(B%fg(i)); enddo; deallocate(B%fg); endif
         if (allocated(B%fb)) then; do i=1,12; call delete(B%fb(i)); enddo; deallocate(B%fb); endif
         if (allocated(B%fi)) then; do i=1,8;  call delete(B%fi(i)); enddo; deallocate(B%fi); endif

         if (allocated(B%eg)) then; do i=1,6;  call delete(B%eg(i)); enddo; deallocate(B%eg); endif
         if (allocated(B%eb)) then; do i=1,12; call delete(B%eb(i)); enddo; deallocate(B%eb); endif
         if (allocated(B%ei)) then; do i=1,8;  call delete(B%ei(i)); enddo; deallocate(B%ei); endif

         if (allocated(B%cg)) then; do i=1,6;  call delete(B%cg(i)); enddo; deallocate(B%cg); endif
         if (allocated(B%cb)) then; do i=1,12; call delete(B%cb(i)); enddo; deallocate(B%cb); endif
         if (allocated(B%ci)) then; do i=1,8;  call delete(B%ci(i)); enddo; deallocate(B%ci); endif
       end subroutine

       subroutine display_block(B,un)
         implicit none
         type(block),intent(in) :: B
         integer,intent(in) :: un
         integer :: i
         call display(B%g,un)
         do i=1,6;  call display(B%fg(i),un); enddo
         do i=1,6;  call display(B%fb(i),un); enddo
         do i=1,6;  call display(B%fi(i),un); enddo

         do i=1,12; call display(B%eg(i),un); enddo
         do i=1,12; call display(B%eb(i),un); enddo
         do i=1,12; call display(B%ei(i),un); enddo

         do i=1,8;  call display(B%cg(i),un); enddo
         do i=1,8;  call display(B%cb(i),un); enddo
         do i=1,8;  call display(B%ci(i),un); enddo
       end subroutine

       subroutine print_block(B)
         implicit none
         type(block),intent(in) :: B
         integer :: i
         call print(B%g)
         do i=1,6;  call print(B%fg(i)); enddo
         do i=1,6;  call print(B%fb(i)); enddo
         do i=1,6;  call print(B%fi(i)); enddo

         do i=1,12; call print(B%eb(i)); enddo
         do i=1,12; call print(B%eg(i)); enddo
         do i=1,12; call print(B%ei(i)); enddo

         do i=1,8;  call print(B%cg(i)); enddo
         do i=1,8;  call print(B%cb(i)); enddo
         do i=1,8;  call print(B%ci(i)); enddo
       end subroutine

       subroutine export_block(B,un)
         implicit none
         type(block),intent(in) :: B
         integer,intent(in) :: un
         integer :: i
         call export(B%g,un)
         do i=1,6;  call export(B%fg(i),un); enddo
         do i=1,6;  call export(B%fb(i),un); enddo
         do i=1,6;  call export(B%fi(i),un); enddo

         do i=1,12; call export(B%eg(i),un); enddo
         do i=1,12; call export(B%eb(i),un); enddo
         do i=1,12; call export(B%ei(i),un); enddo

         do i=1,8;  call export(B%cg(i),un); enddo
         do i=1,8;  call export(B%cb(i),un); enddo
         do i=1,8;  call export(B%ci(i),un); enddo
       end subroutine

       subroutine import_block(B,un)
         implicit none
         type(block),intent(inout) :: B
         integer,intent(in) :: un
         integer :: i
         call import(B%g,un)
         do i=1,6;  call import(B%fg(i),un); enddo
         do i=1,6;  call import(B%fb(i),un); enddo
         do i=1,6;  call import(B%fi(i),un); enddo

         do i=1,12; call import(B%eg(i),un); enddo
         do i=1,12; call import(B%eb(i),un); enddo
         do i=1,12; call import(B%ei(i),un); enddo

         do i=1,8;  call import(B%cg(i),un); enddo
         do i=1,8;  call import(B%cb(i),un); enddo
         do i=1,8;  call import(B%ci(i),un); enddo
       end subroutine

       subroutine export_block_wrapper(B,dir,name)
         implicit none
         type(block),intent(in) :: B
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(B,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_block_wrapper(B,dir,name)
         implicit none
         type(block),intent(inout) :: B
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(B,un)
         call close_and_message(un,dir,name)
       end subroutine

       end module