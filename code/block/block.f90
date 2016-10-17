       module block_mod
       use current_precision_mod
       use grid_mod
       use IO_tools_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_FEC

       type block
         type(grid) :: g                             ! Bulk
         type(grid),dimension(:),allocatable :: f,fb ! Faces (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: e,eb ! Edges (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: c,cb ! Corners (boundary,ghost,interior)
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

       interface init_FEC;           module procedure init_FEC_block;           end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_block(B,g)
         implicit none
         type(block),intent(inout) :: B
         type(grid),intent(in) :: g
         call delete(B)
         call init(B%g,g)
       end subroutine

       subroutine init_FEC_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         call delete_FEC_block(B)
         i=6;  allocate(B%f(i));  do i=1,6; call get_face_GI(B%f(i),B%g,i);   enddo
         i=12; allocate(B%e(i));  do i=1,12;call get_edge_GI(B%e(i),B%g,i);   enddo
         i=8;  allocate(B%c(i));  do i=1,8; call get_corner_GI(B%c(i),B%g,i); enddo

         i=6;  allocate(B%fb(i)); do i=1,6; call get_face_b(  B%fb(i),B%g,i); enddo
         i=12; allocate(B%eb(i)); do i=1,12;call get_edge_b(  B%eb(i),B%g,i); enddo
         i=8;  allocate(B%cb(i)); do i=1,8; call get_corner_b(B%cb(i),B%g,i); enddo
       end subroutine

       subroutine init_block_copy(B_out,B_in)
         implicit none
         type(block),intent(inout) :: B_out
         type(block),intent(in) :: B_in
         integer :: i
         call delete(B_out)
         call init(B_out%g,B_in%g)
         ! call inisist_allocated(B_in,'init_block_copy')
         i=6; allocate(B_out%f(i));  do i=1,6;  call init(B_out%f(i),B_in%f(i));   enddo
         i=6; allocate(B_out%fb(i)); do i=1,6;  call init(B_out%fb(i),B_in%fb(i)); enddo

         i=12; allocate(B_out%e(i)); do i=1,12; call init(B_out%e(i),B_in%e(i)); enddo
         i=12; allocate(B_out%eb(i));do i=1,12; call init(B_out%eb(i),B_in%eb(i)); enddo

         i=8; allocate(B_out%c(i));  do i=1,8;  call init(B_out%c(i),B_in%c(i)); enddo
         i=8; allocate(B_out%cb(i)); do i=1,8;  call init(B_out%cb(i),B_in%cb(i)); enddo
       end subroutine

       subroutine delete_block(B)
         implicit none
         type(block),intent(inout) :: B
         call delete(B%g)
         call delete_FEC_block(B)
       end subroutine

       subroutine delete_FEC_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         if (allocated(B%f)) then; do i=1,6;  call delete(B%f(i)); enddo; deallocate(B%f); endif
         if (allocated(B%fb)) then; do i=1,6;  call delete(B%fb(i)); enddo; deallocate(B%fb); endif

         if (allocated(B%e)) then; do i=1,12; call delete(B%e(i)); enddo; deallocate(B%e); endif
         if (allocated(B%eb)) then; do i=1,12; call delete(B%eb(i)); enddo; deallocate(B%eb); endif

         if (allocated(B%c)) then; do i=1,8;  call delete(B%c(i)); enddo; deallocate(B%c); endif
         if (allocated(B%cb)) then; do i=1,8;  call delete(B%cb(i)); enddo; deallocate(B%cb); endif
       end subroutine

       subroutine display_block(B,un)
         implicit none
         type(block),intent(in) :: B
         integer,intent(in) :: un
         integer :: i
         call display(B%g,un)
         do i=1,6;  call display(B%f(i),un); enddo
         do i=1,6;  call display(B%fb(i),un); enddo

         do i=1,12; call display(B%e(i),un); enddo
         do i=1,12; call display(B%eb(i),un); enddo

         do i=1,8;  call display(B%c(i),un); enddo
         do i=1,8;  call display(B%cb(i),un); enddo
       end subroutine

       subroutine print_block(B)
         implicit none
         type(block),intent(in) :: B
         integer :: i
         call print(B%g)
         do i=1,6;  call print(B%f(i)); enddo
         do i=1,6;  call print(B%fb(i)); enddo

         do i=1,12; call print(B%eb(i)); enddo
         do i=1,12; call print(B%e(i)); enddo

         do i=1,8;  call print(B%c(i)); enddo
         do i=1,8;  call print(B%cb(i)); enddo
       end subroutine

       subroutine export_block(B,un)
         implicit none
         type(block),intent(in) :: B
         integer,intent(in) :: un
         integer :: i
         call export(B%g,un)
         do i=1,6;  call export(B%f(i),un); enddo
         do i=1,6;  call export(B%fb(i),un); enddo

         do i=1,12; call export(B%e(i),un); enddo
         do i=1,12; call export(B%eb(i),un); enddo

         do i=1,8;  call export(B%c(i),un); enddo
         do i=1,8;  call export(B%cb(i),un); enddo
       end subroutine

       subroutine import_block(B,un)
         implicit none
         type(block),intent(inout) :: B
         integer,intent(in) :: un
         integer :: i
         call import(B%g,un)
         do i=1,6;  call import(B%f(i),un); enddo
         do i=1,6;  call import(B%fb(i),un); enddo

         do i=1,12; call import(B%e(i),un); enddo
         do i=1,12; call import(B%eb(i),un); enddo

         do i=1,8;  call import(B%c(i),un); enddo
         do i=1,8;  call import(B%cb(i),un); enddo
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