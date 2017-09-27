       module block_extend_mod
       use block_mod
       use current_precision_mod
       use grid_mod
       use grid_extend_mod
       use GF_mod
       use sparse_mod
       use sparse_extend_mod
       use face_edge_corner_indexing_mod
       use data_location_extend_mod
       use IO_tools_mod
       use GF_diagonals_mod
       implicit none

       private
       public :: init

       public :: init_FEC
       public :: init_vol
       public :: init_apply_BC_order

       public :: restrict
       public :: prolongate

       public :: mirror_about_hmin
       public :: mirror_about_hmax

       interface init;               module procedure init_block;               end interface

       interface init_FEC;           module procedure init_FEC_block;           end interface
       interface init_apply_BC_order;module procedure init_apply_BC_order_block;end interface

       interface restrict;           module procedure restrict_dir_b;           end interface
       interface prolongate;         module procedure prolongate_dir_b;         end interface

       interface mirror_about_hmin;  module procedure mirror_about_hmin_b;      end interface
       interface mirror_about_hmax;  module procedure mirror_about_hmax_b;      end interface

       interface init_vol;           module procedure init_vol_block;           end interface

       contains

       subroutine init_block(B,g)
         implicit none
         type(block),intent(inout) :: B
         type(grid),intent(in) :: g
         call delete(B)
         call init(B%g,g)
         call init_vol_block(B)
       end subroutine

       subroutine init_vol_block(B)
         implicit none
         type(block),intent(inout) :: B
         type(data_location) :: DL
         integer :: i
         if (allocated(B%vol)) then
         do i=1,8; call delete(B%vol(i)); enddo; deallocate(B%vol)
         endif
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

       subroutine init_apply_BC_order_block(B,apply_BC_order)
         implicit none
         type(block),intent(inout) :: B
         integer,dimension(6),intent(in) :: apply_BC_order
         B%apply_BC_order = apply_BC_order
       end subroutine

       subroutine mirror_about_hmin_b(B,dir)
         implicit none
         type(block),intent(inout) :: B
         integer,intent(in) :: dir
         call mirror_about_hmin(B%g,dir)
       end subroutine
       subroutine mirror_about_hmax_b(B,dir)
         implicit none
         type(block),intent(inout) :: B
         integer,intent(in) :: dir
         call mirror_about_hmax(B%g,dir)
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

       ! **********************************************************
       ! ************************ MG ******************************
       ! **********************************************************

       subroutine restrict_dir_b(B,dir)
         implicit none
         type(block),intent(inout) :: B
         integer,intent(in) :: dir
         call restrict(B%g,dir)
         call init_vol_block(B)
         call init_FEC_block(B)
       end subroutine

       subroutine prolongate_dir_b(B,dir)
         implicit none
         type(block),intent(inout) :: B
         integer,intent(in) :: dir
         call prolongate(B%g,dir)
         call init_vol_block(B)
         call init_FEC_block(B)
       end subroutine

       end module