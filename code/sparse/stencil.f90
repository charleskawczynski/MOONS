      module stencil_mod
      use array_mod
      use sparse_mod
      use current_precision_mod
      implicit none
      private
      public :: stencil
      public :: init,delete,display,print,export,import

      public :: init_X,init_Y,init_Z
      public :: combine_diag
      public :: insist_allocated

      type stencil
        type(sparse),dimension(3) :: S  ! Sparse 1D stencil
        type(array) :: D                ! Combined diagonal
      end type

      interface init;               module procedure init_stencil;              end interface
      interface init;               module procedure init_Copy;                 end interface
      interface init_X;             module procedure init_stencil_X;            end interface
      interface init_Y;             module procedure init_stencil_Y;            end interface
      interface init_Z;             module procedure init_stencil_Z;            end interface
      interface delete;             module procedure delete_stencil;            end interface
      interface print;              module procedure print_stencil;             end interface
      interface display;            module procedure display_stencil;           end interface
      interface import;             module procedure import_stencil;            end interface
      interface export;             module procedure export_stencil;            end interface

      interface insist_allocated;   module procedure insist_allocated_stencil;  end interface
      interface combine_diag;       module procedure combine_diag_stencil;      end interface

      contains

      subroutine init_stencil(S,X,Y,Z)
        implicit none
        type(stencil),intent(inout) :: S
        type(sparse),intent(in) :: X,Y,Z
        call delete(S)
        call init(S%S(1),X)
        call init(S%S(2),Y)
        call init(S%S(3),Z)
        call combine_diag(S)
      end subroutine

      subroutine init_Copy(S,S_in)
        implicit none
        type(stencil),intent(inout) :: S
        type(stencil),intent(in) :: S_in
        call delete(S)
        call init(S%S(1),S_in%S(1))
        call init(S%S(2),S_in%S(2))
        call init(S%S(3),S_in%S(3))
        call init(S%D,S_in%D)
      end subroutine

      subroutine init_stencil_X(S,X)
        implicit none
        type(stencil),intent(inout) :: S
        type(sparse),intent(in) :: X
        call init(S%S(1),X)
      end subroutine

      subroutine init_stencil_Y(S,Y)
        implicit none
        type(stencil),intent(inout) :: S
        type(sparse),intent(in) :: Y
        call init(S%S(2),Y)
      end subroutine

      subroutine init_stencil_Z(S,Z)
        implicit none
        type(stencil),intent(inout) :: S
        type(sparse),intent(in) :: Z
        call init(S%S(3),Z)
      end subroutine

      subroutine delete_stencil(S)
        implicit none
        type(stencil),intent(inout) :: S
        call delete(S%S(1))
        call delete(S%S(2))
        call delete(S%S(3))
        call delete(S%D)
      end subroutine

      subroutine print_stencil(S)
        implicit none
        type(stencil),intent(in) :: S
        call print(S%S(1))
        call print(S%S(2))
        call print(S%S(3))
        call print(S%D)
      end subroutine

      subroutine display_stencil(S,un)
        implicit none
        type(stencil),intent(in) :: S
        integer,intent(in) :: un
        call display(S%S(1),un)
        call display(S%S(2),un)
        call display(S%S(3),un)
        call display(S%D,un)
      end subroutine

      subroutine export_stencil(S,un)
        implicit none
        type(stencil),intent(in) :: S
        integer,intent(in) :: un
        call export(S%S(1),un)
        call export(S%S(2),un)
        call export(S%S(3),un)
        call export(S%D,un)
      end subroutine

      subroutine import_stencil(S,un)
        implicit none
        type(stencil),intent(inout) :: S
        integer,intent(in) :: un
        call import(S%S(1),un)
        call import(S%S(2),un)
        call import(S%S(3),un)
        call import(S%D,un)
      end subroutine

      ! *********************************************************************

      subroutine insist_allocated_stencil(S,caller)
        implicit none
        type(stencil),intent(in) :: S
        character(len=*),intent(in) :: caller
        call insist_allocated(S%S(1),caller//' S(1)')
        call insist_allocated(S%S(2),caller//' S(2)')
        call insist_allocated(S%S(3),caller//' S(3)')
        call insist_allocated(S%D,caller//' D')
      end subroutine

      subroutine combine_diag_stencil(S)
        implicit none
        type(stencil),intent(inout) :: S
#ifdef _DEBUG_STENCIL_
        call insist_allocated(S,'combine_diag_stencil')
#endif
        call assign(S%D,S%S(1)%D)
        call    add(S%D,S%S(2)%D)
        call    add(S%D,S%S(3)%D)
      end subroutine

      end module