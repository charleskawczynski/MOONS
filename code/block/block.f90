       module block_mod
       use current_precision_mod
       use grid_mod
       use IO_tools_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_FEC

       type adjacent_faces
         integer,dimension(2) :: f
       end type

       type block
         type(grid) :: g                                 ! Bulk
         type(grid),dimension(:),allocatable :: fg,fb,fi ! Faces (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: eg,eb,ei ! Edges (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: cg,cb,ci ! Corners (boundary,ghost,interior)
         real(cp),dimension(6) :: f_dh,f_nhat
         real(cp),dimension(12) :: e_dh,e_nhat
         real(cp),dimension(8) :: c_dh,c_nhat
         type(adjacent_faces),dimension(6) :: a
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
         integer :: i
         call delete(B)
         call init(B%g,g)
         ! call init_FEC(B)
       end subroutine

       subroutine init_FEC_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         call delete_FEC_block(B)
         i = 6;allocate(B%fg(i)); do i=1,6;  call get_face_g(  B%fg(i),B%g,i); enddo
         i = 6;allocate(B%fb(i)); do i=1,6;  call get_face_b(  B%fb(i),B%g,i); enddo
         i = 6;allocate(B%fi(i)); do i=1,6;  call get_face_i(  B%fi(i),B%g,i); enddo
         i =12;allocate(B%eg(i)); do i=1,12; call get_edge_g(  B%eg(i),B%g,i); enddo
         i =12;allocate(B%eb(i)); do i=1,12; call get_edge_b(  B%eb(i),B%g,i); enddo
         i =12;allocate(B%ei(i)); do i=1,12; call get_edge_i(  B%ei(i),B%g,i); enddo
         i = 8;allocate(B%cg(i)); do i=1,8;  call get_corner_g(B%cg(i),B%g,i); enddo
         i = 8;allocate(B%cb(i)); do i=1,8;  call get_corner_b(B%cb(i),B%g,i); enddo
         i = 8;allocate(B%ci(i)); do i=1,8;  call get_corner_i(B%ci(i),B%g,i); enddo

         B%f_dh(1) = B%g%c(1)%dhn(1); B%f_nhat(1) = -1.0_cp; B%a(1)%f = (/2,3/)
         B%f_dh(2) = B%g%c(1)%dhn_e;  B%f_nhat(2) =  1.0_cp; B%a(2)%f = (/2,3/)
         B%f_dh(3) = B%g%c(2)%dhn(1); B%f_nhat(3) = -1.0_cp; B%a(3)%f = (/1,3/)
         B%f_dh(4) = B%g%c(2)%dhn_e;  B%f_nhat(4) =  1.0_cp; B%a(4)%f = (/1,3/)
         B%f_dh(5) = B%g%c(3)%dhn(1); B%f_nhat(5) = -1.0_cp; B%a(5)%f = (/1,2/)
         B%f_dh(6) = B%g%c(3)%dhn_e;  B%f_nhat(6) =  1.0_cp; B%a(6)%f = (/1,2/)
       end subroutine

       subroutine init_block_copy(B_out,B_in)
         implicit none
         type(block),intent(inout) :: B_out
         type(block),intent(in) :: B_in
         integer :: i
         call delete(B_out)
         call init(B_out%g,B_in%g)
         i = 6;allocate(B_out%fg(i))
         i = 6;allocate(B_out%fb(i))
         i = 6;allocate(B_out%fi(i))

         i =12;allocate(B_out%eg(i))
         i =12;allocate(B_out%eb(i))
         i =12;allocate(B_out%ei(i))

         i = 8;allocate(B_out%cg(i))
         i = 8;allocate(B_out%cb(i))
         i = 8;allocate(B_out%ci(i))

         do i=1,6;  call init(B_out%fg(i),B_in%fg(i)); enddo
         do i=1,6;  call init(B_out%fb(i),B_in%fb(i)); enddo
         do i=1,6;  call init(B_out%fi(i),B_in%fi(i)); enddo

         do i=1,12; call init(B_out%eg(i),B_in%eg(i)); enddo
         do i=1,12; call init(B_out%eb(i),B_in%eb(i)); enddo
         do i=1,12; call init(B_out%ei(i),B_in%ei(i)); enddo

         do i=1,8;  call init(B_out%cg(i),B_in%cg(i)); enddo
         do i=1,8;  call init(B_out%cb(i),B_in%cb(i)); enddo
         do i=1,8;  call init(B_out%ci(i),B_in%ci(i)); enddo

         B_out%f_dh = B_in%f_dh
         B_out%f_nhat = B_in%f_nhat
         do i=1,6; B_out%a(i)%f = B_in%a(i)%f; enddo
       end subroutine

       subroutine delete_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         call delete(B%g)
         call delete_FEC_block(B)
       end subroutine

       subroutine delete_FEC_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         if (allocated(B%fg)) then; do i=1,6;  call delete(B%fg(i)); enddo; deallocate(B%fg); endif
         if (allocated(B%fb)) then; do i=1,6;  call delete(B%fb(i)); enddo; deallocate(B%fb); endif
         if (allocated(B%fi)) then; do i=1,6;  call delete(B%fi(i)); enddo; deallocate(B%fi); endif

         if (allocated(B%eg)) then; do i=1,12; call delete(B%eg(i)); enddo; deallocate(B%eg); endif
         if (allocated(B%eb)) then; do i=1,12; call delete(B%eb(i)); enddo; deallocate(B%eb); endif
         if (allocated(B%ei)) then; do i=1,12; call delete(B%ei(i)); enddo; deallocate(B%ei); endif

         if (allocated(B%cg)) then; do i=1,8;  call delete(B%cg(i)); enddo; deallocate(B%cg); endif
         if (allocated(B%cb)) then; do i=1,8;  call delete(B%cb(i)); enddo; deallocate(B%cb); endif
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

         do i=1,6; write(un,*) B%f_dh(i);   enddo
         do i=1,6; write(un,*) B%f_nhat(i); enddo
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

         do i=1,6; read(un,*) B%f_dh(i);   enddo
         do i=1,6; read(un,*) B%f_nhat(i); enddo
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