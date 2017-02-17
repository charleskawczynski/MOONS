      module matrix_free_params_mod
      use current_precision_mod
      implicit none

      private
      public :: matrix_free_params
      public :: init,delete,display,print,export,import ! Essentials

      public :: prolongate

      interface init;       module procedure init_MFP;       end interface
      interface delete;     module procedure delete_MFP;     end interface
      interface display;    module procedure display_MFP;    end interface
      interface print;      module procedure print_MFP;      end interface
      interface export;     module procedure export_MFP;     end interface
      interface import;     module procedure import_MFP;     end interface

      interface prolongate; module procedure prolongate_MFP; end interface

      type matrix_free_params
        logical :: suppress_warning = .true.
        real(cp) :: alpha = 0.0_cp
        real(cp) :: beta = 0.0_cp
        real(cp) :: coeff_natural = 0.0_cp
        real(cp) :: coeff_explicit = 0.0_cp
        real(cp) :: coeff_implicit = 0.0_cp
        real(cp) :: coeff_implicit_time_split = 0.0_cp
      end type

      contains

      subroutine init_MFP(m,m_in)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        type(matrix_free_params),intent(in) :: m_in
        m%suppress_warning          = m_in%suppress_warning
        m%alpha                     = m_in%alpha
        m%beta                      = m_in%beta
        m%coeff_natural             = m_in%coeff_natural
        m%coeff_explicit            = m_in%coeff_explicit
        m%coeff_implicit            = m_in%coeff_implicit
        m%coeff_implicit_time_split = m_in%coeff_implicit_time_split
      end subroutine

      subroutine delete_MFP(m)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        m%suppress_warning          = .true.
        m%alpha                     = 0.0_cp
        m%beta                      = 0.0_cp
        m%coeff_natural             = 0.0_cp
        m%coeff_explicit            = 0.0_cp
        m%coeff_implicit            = 0.0_cp
        m%coeff_implicit_time_split = 0.0_cp
      end subroutine

      subroutine display_MFP(m,un)
        implicit none
        type(matrix_free_params),intent(in) :: m
        integer,intent(in) :: un
        write(un,*) 'suppress_warning          = ',m%suppress_warning
        write(un,*) 'alpha                     = ',m%alpha
        write(un,*) 'beta                      = ',m%beta
        write(un,*) 'coeff_natural             = ',m%coeff_natural
        write(un,*) 'coeff_explicit            = ',m%coeff_explicit
        write(un,*) 'coeff_implicit            = ',m%coeff_implicit
        write(un,*) 'coeff_implicit_time_split = ',m%coeff_implicit_time_split
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
        write(un,*) 'suppress_warning          = '; write(un,*) m%suppress_warning
        write(un,*) 'alpha                     = '; write(un,*) m%alpha
        write(un,*) 'beta                      = '; write(un,*) m%beta
        write(un,*) 'coeff_natural             = '; write(un,*) m%coeff_natural
        write(un,*) 'coeff_explicit            = '; write(un,*) m%coeff_explicit
        write(un,*) 'coeff_implicit            = '; write(un,*) m%coeff_implicit
        write(un,*) 'coeff_implicit_time_split = '; write(un,*) m%coeff_implicit_time_split
      end subroutine

      subroutine import_MFP(m,un)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        integer,intent(in) :: un
        read(un,*) ; read(un,*) m%suppress_warning
        read(un,*) ; read(un,*) m%alpha
        read(un,*) ; read(un,*) m%beta
        read(un,*) ; read(un,*) m%coeff_natural
        read(un,*) ; read(un,*) m%coeff_explicit
        read(un,*) ; read(un,*) m%coeff_implicit
        read(un,*) ; read(un,*) m%coeff_implicit_time_split
      end subroutine

      subroutine prolongate_MFP(m,coeff_multiplication_factor)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        real(cp),intent(in) :: coeff_multiplication_factor
        m%coeff_natural = coeff_multiplication_factor*m%coeff_natural
        m%coeff_explicit = coeff_multiplication_factor*m%coeff_explicit
      end subroutine

      end module