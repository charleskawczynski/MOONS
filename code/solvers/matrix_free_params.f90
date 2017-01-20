      module matrix_free_params_mod
      use current_precision_mod
      implicit none

      private
      public :: matrix_free_params
      public :: init,delete,display,print,export,import ! Essentials

      interface init;     module procedure init_MFP;      end interface
      interface delete;   module procedure delete_MFP;    end interface
      interface display;  module procedure display_MFP;   end interface
      interface print;    module procedure print_MFP;     end interface
      interface export;   module procedure export_MFP;    end interface
      interface import;   module procedure import_MFP;    end interface

      type matrix_free_params
        logical :: suppress_warning = .true.
        real(cp) :: coeff = 0.0_cp
      end type

      contains

      subroutine init_MFP(m_out,m_in)
        implicit none
        type(matrix_free_params),intent(inout) :: m_out
        type(matrix_free_params),intent(in) :: m_in
        m_out%suppress_warning = m_in%suppress_warning
        m_out%coeff = m_in%coeff
      end subroutine

      subroutine delete_MFP(m)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        m%suppress_warning = .true.
        m%coeff = 0.0_cp
      end subroutine

      subroutine display_MFP(m,un)
        implicit none
        type(matrix_free_params),intent(in) :: m
        integer,intent(in) :: un
        write(un,*) 'suppress_warning=',m%suppress_warning
        write(un,*) 'coeff=',m%coeff
      end subroutine

      subroutine print_MFP(m)
        implicit none
        type(matrix_free_params),intent(in) :: m
        call display(m,6)
      end subroutine

      subroutine export_MFP(m,un)
        implicit none
        type(matrix_free_params),intent(in) :: m
        integer,intent(in) :: un
        write(un,*) 'suppress_warning = '; write(un,*) m%suppress_warning
        write(un,*) 'coeff = ';            write(un,*) m%coeff
      end subroutine

      subroutine import_MFP(m,un)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        integer,intent(in) :: un
        read(un,*) ; read(un,*) m%suppress_warning
        read(un,*) ; read(un,*) m%coeff
      end subroutine

      end module