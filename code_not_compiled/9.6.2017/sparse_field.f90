      module sparse_field_mod
      use GF_mod
      use grid_mod
      use data_location_mod
      use current_precision_mod
      implicit none
      private
      public :: sparse_field
      public :: init,delete,display,print,export,import

      public :: assign
      public :: add
      public :: multiply
      public :: insist_allocated

      type sparse_field
        type(grid_field) :: L,D,U
      end type

      interface init;                module procedure init_SF_size;           end interface
      interface init;                module procedure init_Copy;              end interface
      interface delete;              module procedure delete_SF;              end interface
      interface print;               module procedure print_SF;               end interface
      interface display;             module procedure display_SF;             end interface
      interface import;              module procedure import_SF;              end interface
      interface export;              module procedure export_SF;              end interface

      interface assign;              module procedure assign_SF;              end interface
      interface add;                 module procedure add_SF;                 end interface
      interface multiply;            module procedure multiply_SF;            end interface

      interface insist_allocated;    module procedure insist_allocated_SF;    end interface

      contains

      subroutine init_SF_size(SF,g,DL)
        implicit none
        type(sparse_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        call delete(SF)
        call init(SF%L,g,DL); call assign(SF%L,0.0_cp)
        call init(SF%D,g,DL); call assign(SF%D,0.0_cp)
        call init(SF%U,g,DL); call assign(SF%U,0.0_cp)
      end subroutine

      subroutine init_Copy(SF,S_in)
        implicit none
        type(sparse_field),intent(inout) :: SF
        type(sparse_field),intent(in) :: S_in
        call delete(SF)
        call init(SF%L,S_in%L)
        call init(SF%D,S_in%D)
        call init(SF%U,S_in%U)
      end subroutine

      subroutine delete_SF(SF)
        implicit none
        type(sparse_field),intent(inout) :: SF
        call delete(SF%L)
        call delete(SF%D)
        call delete(SF%U)
      end subroutine

      subroutine print_SF(SF)
        implicit none
        type(sparse_field),intent(in) :: SF
        call display_SF(SF,6)
      end subroutine

      subroutine display_SF(SF,un)
        implicit none
        type(sparse_field),intent(in) :: SF
        integer,intent(in) :: un
        call display(SF%L,un)
        call display(SF%D,un)
        call display(SF%U,un)
      end subroutine

      subroutine export_SF(SF,un)
        implicit none
        type(sparse_field),intent(in) :: SF
        integer,intent(in) :: un
        call export(SF%L,un)
        call export(SF%D,un)
        call export(SF%U,un)
      end subroutine

      subroutine import_SF(SF,un)
        implicit none
        type(sparse_field),intent(inout) :: SF
        integer,intent(in) :: un
        call import(SF%L,un)
        call import(SF%D,un)
        call import(SF%U,un)
      end subroutine

      ! *********************************************************************

      subroutine insist_allocated_SF(SF,caller)
        implicit none
        type(sparse_field),intent(in) :: SF
        character(len=*),intent(in) :: caller
        call insist_allocated(SF%L,caller//' sparse_field(L)')
        call insist_allocated(SF%D,caller//' sparse_field(D)')
        call insist_allocated(SF%U,caller//' sparse_field(U)')
      end subroutine

      subroutine assign_SF(SF,val)
        implicit none
        type(sparse_field),intent(inout) :: SF
        real(cp),intent(in) :: val
        call assign(SF%L,val)
        call assign(SF%D,val)
        call assign(SF%U,val)
      end subroutine

      subroutine add_SF(SF,val)
        implicit none
        type(sparse_field),intent(inout) :: SF
        real(cp),intent(in) :: val
        call add(SF%L,val)
        call add(SF%D,val)
        call add(SF%U,val)
      end subroutine

      subroutine multiply_SF(SF,val)
        implicit none
        type(sparse_field),intent(inout) :: SF
        real(cp),intent(in) :: val
        call multiply(SF%L,val)
        call multiply(SF%D,val)
        call multiply(SF%U,val)
      end subroutine

      end module