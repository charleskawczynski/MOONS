      module dct_mod
      use current_precision_mod
      use ops_dct_mod
      use grid_mod
      implicit none

      private
      public :: DCT
      public :: init,delete,print,display,export,import

      public :: apply

      interface init;    module procedure init_dct;    end interface
      interface delete;  module procedure delete_dct;  end interface
      interface print;   module procedure print_dct;   end interface
      interface display; module procedure display_dct; end interface
      interface export;  module procedure export_dct;  end interface
      interface import;  module procedure import_dct;  end interface

      interface apply;   module procedure apply_dct;   end interface

      type DCT
        type(grid_field_CMP) :: e,tempX
        real(cp),dimension(3) :: dh = 0.0_cp
      endtype

      contains

      subroutine init_dct(DCT,x,)
        implicit none
        type(DCT),intent(inout) :: DCT
        type(grid_field),intent(in) :: x
        call init(DCT%e,x)
        call init(DCT%tempX,x)
      end subroutine

      subroutine delete_dct(DCT)
        implicit none
        type(DCT),intent(inout) :: DCT
        call delete(DCT%e)
        call delete(DCT%tempX)
        DCT%dh = 0.0_cp
      end subroutine

      subroutine display_dct(DCT,un)
        implicit none
        type(DCT),intent(inout) :: DCT
        integer,intent(in) :: un
        call display(DCT%e,un)
        call display(DCT%tempX,un)
        write(un,*) 'DCT%dh = ',DCT%dh
      end subroutine

      subroutine print_dct(DCT)
        implicit none
        type(DCT),intent(inout) :: DCT
        call display(DCT,6)
      end subroutine

      subroutine export_dct(DCT,un)
        implicit none
        type(DCT),intent(inout) :: DCT
        integer,intent(in) :: un
        call export(DCT%e,un)
        call export(DCT%tempX,un)
        write(un,*) 'DCT%dh = '; write(un,*) DCT%dh
      end subroutine

      subroutine import_dct(DCT,un)
        implicit none
        type(DCT),intent(inout) :: DCT
        integer,intent(in) :: un
        call import(DCT%e,un)
        call import(DCT%tempX,un)
        read(un,*); read(un,*) DCT%dh
      end subroutine

      subroutine apply_dct(DCT,x,dir,pad)
        implicit none
        type(DCT),intent(inout) :: DCT
        type(grid_field),intent(inout) :: x
        integer,intent(in) :: dir,pad
        call dct_operate(x,DCT%e,DCT%tempX,dir,pad)
      end subroutine

      end module