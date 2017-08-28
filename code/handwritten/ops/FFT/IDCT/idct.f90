      module idct_mod
      use current_precision_mod
      use ops_idct_mod
      use grid_mod
      implicit none

      private
      public :: IDCT
      public :: init,delete,print,display,export,import

      public :: apply

      interface init;    module procedure init_idct;    end interface
      interface delete;  module procedure delete_idct;  end interface
      interface print;   module procedure print_idct;   end interface
      interface display; module procedure display_idct; end interface
      interface export;  module procedure export_idct;  end interface
      interface import;  module procedure import_idct;  end interface

      interface apply;   module procedure apply_idct;   end interface

      type IDCT
        type(grid_field_CMP) :: e,tempX,tempX2
        real(cp),dimension(3) :: dh = 0.0_cp
      endtype

      contains

      subroutine init_idct(IDCT,x,)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        type(grid_field),intent(in) :: x
        call init(IDCT%e,x)
        call init(IDCT%tempX,x)
        call init(IDCT%tempX2,x)
      end subroutine

      subroutine delete_idct(IDCT)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        call delete(IDCT%e)
        call delete(IDCT%tempX)
        call delete(IDCT%tempX2)
        IDCT%dh = 0.0_cp
      end subroutine

      subroutine display_idct(IDCT,un)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        integer,intent(in) :: un
        call display(IDCT%e,un)
        call display(IDCT%tempX,un)
        call display(IDCT%tempX2,un)
        write(un,*) 'IDCT%dh = ',IDCT%dh
      end subroutine

      subroutine print_idct(IDCT)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        call display(IDCT,6)
      end subroutine

      subroutine export_idct(IDCT,un)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        integer,intent(in) :: un
        call export(IDCT%e,un)
        call export(IDCT%tempX,un)
        call export(IDCT%tempX2,un)
        write(un,*) 'IDCT%dh = '; write(un,*) IDCT%dh
      end subroutine

      subroutine import_idct(IDCT,un)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        integer,intent(in) :: un
        call import(IDCT%e,un)
        call import(IDCT%tempX,un)
        call import(IDCT%tempX2,un)
        read(un,*); read(un,*) IDCT%dh
      end subroutine

      subroutine apply_idct(IDCT,x,dir,pad)
        implicit none
        type(IDCT),intent(inout) :: IDCT
        type(grid_field),intent(inout) :: x
        integer,intent(in) :: dir,pad
        call idct_operate(x,IDCT%e,IDCT%tempX,IDCT%tempX2,dir,pad)
      end subroutine

      end module