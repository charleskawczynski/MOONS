       module block_mod
       use current_precision_mod
       use grid_mod
       use GF_mod
       use sparse_mod
       use face_edge_corner_indexing_mod
       use stencil_1D_mod
       use stencil_3D_mod
       use data_location_mod
       use IO_tools_mod
       use GF_diagonals_mod
       use block_Laplacian_stencil_mod
       use block_curl_curl_stencil_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import ! Essentials

       public :: init_FEC

       public :: init_curl_curl
       public :: init_Laplacian_VF
       public :: init_Laplacian_SF

       public :: mirror_about_hmin
       public :: mirror_about_hmax

       type block
         type(grid) :: g                                  ! Bulk
         type(grid),dimension(:),allocatable :: f,fb      ! Faces (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: e,eb      ! Edges (boundary,ghost,interior)
         type(grid),dimension(:),allocatable :: c,cb      ! Corners (boundary,ghost,interior)
         type(grid_field),dimension(:),allocatable :: vol ! index must match volume_ID in data_location

         type(stencil_3D),dimension(3) :: curl_curlX   ! X component data
         type(stencil_3D),dimension(3) :: curl_curlY   ! Y component data
         type(stencil_3D),dimension(3) :: curl_curlZ   ! Z component data
         type(stencil_3D),dimension(3) :: lap_VF
         type(stencil_3D) :: lap_SF
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

       interface mirror_about_hmin;  module procedure mirror_about_hmin_b;      end interface
       interface mirror_about_hmax;  module procedure mirror_about_hmax_b;      end interface

       interface init_curl_curl;     module procedure init_curl_curl_SB;        end interface
       interface init_curl_curl;     module procedure init_curl_curl_SB_VP;     end interface

       interface init_Laplacian_VF;  module procedure init_Laplacian_SB_VF;     end interface
       interface init_Laplacian_VF;  module procedure init_Laplacian_SB_VF_VP;  end interface

       interface init_Laplacian_SF;  module procedure init_Laplacian_SB_SF;     end interface
       interface init_Laplacian_SF;  module procedure init_Laplacian_SB_SF_VP;  end interface

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

       subroutine init_block_copy(B,B_in)
         implicit none
         type(block),intent(inout) :: B
         type(block),intent(in) :: B_in
         integer :: i
         call delete(B)
         call init(B%g,B_in%g)
         call init(B%lap_SF,B_in%lap_SF)
         do i=1,3; call init(B%lap_VF(i),B_in%lap_VF(i)); enddo
         do i=1,3; call init(B%curl_curlX(i),B_in%curl_curlX(i)); enddo
         do i=1,3; call init(B%curl_curlY(i),B_in%curl_curlY(i)); enddo
         do i=1,3; call init(B%curl_curlZ(i),B_in%curl_curlZ(i)); enddo

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
         call delete(B%lap_SF)
         do i=1,3; call delete(B%lap_VF(i)); enddo
         do i=1,3; call delete(B%curl_curlX(i)); enddo
         do i=1,3; call delete(B%curl_curlY(i)); enddo
         do i=1,3; call delete(B%curl_curlZ(i)); enddo
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

       ! **********************************************************
       ! ********************* LAPLACIAN **************************
       ! **********************************************************

       subroutine init_Laplacian_SB_SF(B)
         implicit none
         type(block),intent(inout) :: B
         call init_Laplacian(B%lap_SF,B%g)
       end subroutine

       subroutine init_Laplacian_SB_SF_VP(B,sig)
         implicit none
         type(block),intent(inout) :: B
         type(grid_field),dimension(3),intent(in) :: sig
         call init_Laplacian(B%lap_SF,B%g,sig)
       end subroutine

       subroutine init_Laplacian_SB_VF(B)
         implicit none
         type(block),intent(inout) :: B
         call init_Laplacian(B%lap_VF,B%g)
       end subroutine

       subroutine init_Laplacian_SB_VF_VP(B,sig)
         implicit none
         type(block),intent(inout) :: B
         type(grid_field),dimension(3),intent(in) :: sig
         call init_Laplacian(B%lap_VF,B%g,sig)
       end subroutine

       ! **********************************************************
       ! ********************* CURL-CURL **************************
       ! **********************************************************

       subroutine init_curl_curl_SB(B)
         implicit none
         type(block),intent(inout) :: B
         call init_curl_curl(B%curl_curlX,B%curl_curlY,B%curl_curlZ,B%g)
       end subroutine

       subroutine init_curl_curl_SB_VP(B,sig)
         implicit none
         type(block),intent(inout) :: B
         type(grid_field),dimension(3),intent(in) :: sig
         call init_curl_curl(B%curl_curlX,B%curl_curlY,B%curl_curlZ,B%g,sig)
       end subroutine

       end module