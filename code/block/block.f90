       module block_mod
       use current_precision_mod
       use grid_mod
       use GF_mod
       use sparse_mod
       use stencil_mod
       use stencil_field_mod
       use data_location_mod
       use IO_tools_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_FEC

       type block
         type(grid) :: g                                  ! Bulk
         type(grid),dimension(:),allocatable :: f,fb      ! Faces (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: e,eb      ! Edges (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: c,cb      ! Corners (boundary,ghost,interior)
         type(grid_field),dimension(:),allocatable :: vol ! index must match volume_ID in data_location
         type(stencil_field),dimension(3) :: curl_curl
         type(stencil_field),dimension(3) :: lap_F_VF
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
         call init_vol_block(B)
         ! call init_curl_curl_stencil_block(B)
         call init_Laplacian_stencil_block(B)
       end subroutine

       subroutine init_curl_curl_stencil_block(B)
         implicit none
         type(block),intent(inout) :: B
         type(stencil) :: S
         type(data_location) :: DL
         integer :: dir
         dir = 1; call init_Face(DL,dir)
         call init(S%S(1),B%g%c(1)%stagN2CC)
         call init(S%S(2),B%g%c(2)%stagCC2N)
         call init(S%S(3),B%g%c(3)%stagCC2N)
         call combine_diag(S,DL)
         call init(B%curl_curl(dir),S,B%g,DL)

         dir = 2; call init_Face(DL,dir)
         call init(S%S(1),B%g%c(1)%stagCC2N)
         call init(S%S(2),B%g%c(2)%stagN2CC)
         call init(S%S(3),B%g%c(3)%stagCC2N)
         call combine_diag(S,DL)
         call init(B%curl_curl(dir),S,B%g,DL)

         dir = 3; call init_Face(DL,dir)
         call init(S%S(1),B%g%c(1)%stagCC2N)
         call init(S%S(2),B%g%c(2)%stagCC2N)
         call init(S%S(3),B%g%c(3)%stagN2CC)
         call combine_diag(S,DL)
         call init(B%curl_curl(dir),S,B%g,DL)
         call delete(DL)
         call delete(S)
       end subroutine

       subroutine init_Laplacian_stencil_block(B)
         implicit none
         type(block),intent(inout) :: B
         type(stencil) :: S
         type(data_location) :: DL
         integer :: dir
         dir = 1; call init_Face(DL,dir)
         call init(S%S(1),B%g%c(1)%colN(2))
         call init(S%S(2),B%g%c(2)%colCC(2))
         call init(S%S(3),B%g%c(3)%colCC(2))
         call init(B%lap_F_VF(dir),S,B%g,DL)
         dir = 2; call init_Face(DL,dir)
         call init(S%S(1),B%g%c(1)%colCC(2))
         call init(S%S(2),B%g%c(2)%colN(2))
         call init(S%S(3),B%g%c(3)%colCC(2))
         call init(B%lap_F_VF(dir),S,B%g,DL)
         dir = 3; call init_Face(DL,dir)
         call init(S%S(1),B%g%c(1)%colCC(2))
         call init(S%S(2),B%g%c(2)%colCC(2))
         call init(S%S(3),B%g%c(3)%colN(2))
         call init(B%lap_F_VF(dir),S,B%g,DL)
         call delete(DL)
         call delete(S)
       end subroutine

       subroutine init_vol_block(B)
         implicit none
         type(block),intent(inout) :: B
         type(data_location) :: DL
         allocate(B%vol(8))
         call init_CC(B%vol(1),B%g)
         call init_Node(B%vol(2),B%g)
         call init_Face(B%vol(3),B%g,1)
         call init_Face(B%vol(4),B%g,2)
         call init_Face(B%vol(5),B%g,3)
         call init_Edge(B%vol(6),B%g,1)
         call init_Edge(B%vol(7),B%g,2)
         call init_Edge(B%vol(8),B%g,3)

         call init_CC(DL);     call volume(B%vol(1),B%g,DL)
         call init_Node(DL);   call volume(B%vol(2),B%g,DL)
         call init_Face(DL,1); call volume(B%vol(3),B%g,DL)
         call init_Face(DL,2); call volume(B%vol(4),B%g,DL)
         call init_Face(DL,3); call volume(B%vol(5),B%g,DL)
         call init_Edge(DL,1); call volume(B%vol(6),B%g,DL)
         call init_Edge(DL,2); call volume(B%vol(7),B%g,DL)
         call init_Edge(DL,3); call volume(B%vol(8),B%g,DL)
         call delete(DL)
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

       subroutine init_block_copy(B,B_in)
         implicit none
         type(block),intent(inout) :: B
         type(block),intent(in) :: B_in
         integer :: i
         call delete(B)
         call init(B%g,B_in%g)
         ! call inisist_allocated(B_in,'init_block_copy')
         i=6; allocate(B%f(i));  do i=1,6;  call init(B%f(i),B_in%f(i));   enddo
         i=6; allocate(B%fb(i)); do i=1,6;  call init(B%fb(i),B_in%fb(i)); enddo

         i=12; allocate(B%e(i)); do i=1,12; call init(B%e(i),B_in%e(i)); enddo
         i=12; allocate(B%eb(i));do i=1,12; call init(B%eb(i),B_in%eb(i)); enddo

         i=8; allocate(B%c(i));  do i=1,8;  call init(B%c(i),B_in%c(i)); enddo
         i=8; allocate(B%cb(i)); do i=1,8;  call init(B%cb(i),B_in%cb(i)); enddo

         allocate(B%vol(8))
         do i=1,8;
          call init(B%vol(i),B_in%vol(i))
          call assign(B%vol(i),B_in%vol(i))
         enddo
       end subroutine

       subroutine delete_block(B)
         implicit none
         type(block),intent(inout) :: B
         integer :: i
         call delete(B%g)
         if (allocated(B%vol)) then
           do i=1,8; call delete(B%vol(i)); enddo
           deallocate(B%vol)
         endif
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
         call display(B%vol(1),un)
         call display(B%vol(2),un)
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
         call print(B%vol(1))
         call print(B%vol(2))
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
         call export(B%vol(1),un)
         call export(B%vol(2),un)
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
         call import(B%vol(1),un)
         call import(B%vol(2),un)
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