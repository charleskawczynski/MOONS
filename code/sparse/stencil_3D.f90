      module stencil_3D_mod
      use stencil_1D_mod
      use data_location_mod
      use grid_mod
      use GF_mod
      use face_edge_corner_indexing_mod
      use current_precision_mod
      implicit none
      private
      public :: stencil_3D
      public :: init,delete,display,print,export,import

      public :: clean
      public :: assign_consecutive
      public :: add_diagonals

      public :: add_to_diag
      public :: multiply_diag

      public :: insist_allocated

      type stencil_3D
        type(stencil_1D),dimension(3) :: S
        type(grid_field) :: D_3D
      end type

      interface init;               module procedure init_stencil_3D;                  end interface
      interface init;               module procedure init_Copy;                        end interface
      interface delete;             module procedure delete_stencil_3D;                end interface
      interface print;              module procedure print_stencil_3D;                 end interface
      interface display;            module procedure display_stencil_3D;               end interface
      interface import;             module procedure import_stencil_3D;                end interface
      interface export;             module procedure export_stencil_3D;                end interface

      interface clean;              module procedure clean_stencil_3D;                 end interface

      interface assign_consecutive; module procedure assign_consecutive_stencil_3D_dir;end interface
      interface assign_consecutive; module procedure assign_consecutive_stencil_3D;    end interface
      interface add_diagonals;      module procedure add_diagonals_stencil_3D;         end interface

      interface add_to_diag;        module procedure add_to_diag_stencil_1D;           end interface
      interface multiply_diag;      module procedure multiply_diag_stencil_1D;         end interface

      interface insist_allocated;   module procedure insist_allocated_stencil_3D;      end interface

      contains

      subroutine init_stencil_3D(S,g,DL)
        implicit none
        type(stencil_3D),intent(inout) :: S
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i
        do i=1,3; call init(S%S(i),g,DL,i); enddo
        call init(S%D_3D,g,DL)
      end subroutine

      subroutine init_Copy(S,S_in)
        implicit none
        type(stencil_3D),intent(inout) :: S
        type(stencil_3D),intent(in) :: S_in
        integer :: i
        call delete(S)
        do i=1,3
          call init(S%S(i),S_in%S(i))
        enddo
        call init(S%D_3D,S_in%D_3D)
      end subroutine

      subroutine delete_stencil_3D(S)
        implicit none
        type(stencil_3D),intent(inout) :: S
        integer :: i
        do i=1,3; call delete(S%S(i)); enddo
        call delete(S%D_3D)
      end subroutine

      subroutine print_stencil_3D(S)
        implicit none
        type(stencil_3D),intent(in) :: S
        integer :: i
        do i=1,3; call print(S%S(i)); enddo
        call print(S%D_3D)
      end subroutine

      subroutine display_stencil_3D(S,un)
        implicit none
        type(stencil_3D),intent(in) :: S
        integer,intent(in) :: un
        integer :: i
        do i=1,3; call display(S%S(i),un); enddo
        call display(S%D_3D,un)
      end subroutine

      subroutine export_stencil_3D(S,un)
        implicit none
        type(stencil_3D),intent(in) :: S
        integer,intent(in) :: un
        integer :: i
        do i=1,3; call export(S%S(i),un); enddo
        call export(S%D_3D,un)
      end subroutine

      subroutine import_stencil_3D(S,un)
        implicit none
        type(stencil_3D),intent(inout) :: S
        integer,intent(in) :: un
        integer :: i
        do i=1,3; call import(S%S(i),un); enddo
        call import(S%D_3D,un)
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine clean_stencil_3D(S)
        implicit none
        type(stencil_3D),intent(inout) :: S
        integer :: i
        do i=1,3; call clean(S%S(i)); enddo
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine add_diagonals_stencil_3D(S)
        implicit none
        type(stencil_3D),intent(inout) :: S
#ifdef _DEBUG_STENCIL_3D_
        call insist_allocated_stencil_3D(S,'add_diagonals_stencil_3D')
#endif
        call assign(S%D_3D,S%S(1)%SF%D)
        call    add(S%D_3D,S%S(2)%SF%D)
        call    add(S%D_3D,S%S(3)%SF%D)
      end subroutine

      subroutine assign_consecutive_stencil_3D(S)
        implicit none
        type(stencil_3D),intent(inout) :: S
        integer :: i
        do i=1,3; call assign_consecutive(S%S(i)); enddo
      end subroutine

      subroutine assign_consecutive_stencil_3D_dir(S,dir)
        implicit none
        type(stencil_3D),intent(inout) :: S
        integer,intent(in) :: dir
        call assign_consecutive(S%S(dir))
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine add_to_diag_stencil_1D(S,v)
        implicit none
        type(stencil_3D),intent(inout) :: S
        real(cp),intent(in) :: v
        call add(S%D_3D,v)
      end subroutine

      subroutine multiply_diag_stencil_1D(S,v)
        implicit none
        type(stencil_3D),intent(inout) :: S
        real(cp),intent(in) :: v
        call multiply(S%D_3D,v)
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine insist_allocated_stencil_3D(S,caller)
        implicit none
        type(stencil_3D),intent(in) :: S
        character(len=*),intent(in) :: caller
        call insist_allocated(S%S(1),caller//' S(1)')
        call insist_allocated(S%S(2),caller//' S(2)')
        call insist_allocated(S%S(3),caller//' S(3)')
        call insist_allocated(S%D_3D,caller//' D_3D')
      end subroutine

      end module