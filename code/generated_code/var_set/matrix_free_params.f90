       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module matrix_free_params_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: matrix_free_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_matrix_free_params;    end interface
       interface delete;       module procedure delete_matrix_free_params;       end interface
       interface display;      module procedure display_matrix_free_params;      end interface
       interface display_short;module procedure display_short_matrix_free_params;end interface
       interface display;      module procedure display_wrap_matrix_free_params; end interface
       interface print;        module procedure print_matrix_free_params;        end interface
       interface print_short;  module procedure print_short_matrix_free_params;  end interface
       interface export;       module procedure export_matrix_free_params;       end interface
       interface import;       module procedure import_matrix_free_params;       end interface
       interface export;       module procedure export_wrap_matrix_free_params;  end interface
       interface import;       module procedure import_wrap_matrix_free_params;  end interface

       type matrix_free_params
         logical :: suppress_warning = .false.
         real(cp) :: alpha = 0.0_cp
         real(cp) :: beta = 0.0_cp
         real(cp) :: coeff_natural = 0.0_cp
         real(cp) :: coeff_explicit = 0.0_cp
         real(cp) :: coeff_implicit = 0.0_cp
         real(cp) :: coeff_implicit_time_split = 0.0_cp
       end type

       contains

       subroutine init_copy_matrix_free_params(this,that)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         type(matrix_free_params),intent(in) :: that
         call delete(this)
         this%suppress_warning = that%suppress_warning
         this%alpha = that%alpha
         this%beta = that%beta
         this%coeff_natural = that%coeff_natural
         this%coeff_explicit = that%coeff_explicit
         this%coeff_implicit = that%coeff_implicit
         this%coeff_implicit_time_split = that%coeff_implicit_time_split
       end subroutine

       subroutine delete_matrix_free_params(this)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         this%suppress_warning = .false.
         this%alpha = 0.0_cp
         this%beta = 0.0_cp
         this%coeff_natural = 0.0_cp
         this%coeff_explicit = 0.0_cp
         this%coeff_implicit = 0.0_cp
         this%coeff_implicit_time_split = 0.0_cp
       end subroutine

       subroutine display_matrix_free_params(this,un)
         implicit none
         type(matrix_free_params),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_matrix_free_params(this,un)
         implicit none
         type(matrix_free_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning          = ',this%suppress_warning
         write(un,*) 'alpha                     = ',this%alpha
         write(un,*) 'beta                      = ',this%beta
         write(un,*) 'coeff_natural             = ',this%coeff_natural
         write(un,*) 'coeff_explicit            = ',this%coeff_explicit
         write(un,*) 'coeff_implicit            = ',this%coeff_implicit
         write(un,*) 'coeff_implicit_time_split = ',this%coeff_implicit_time_split
       end subroutine

       subroutine print_matrix_free_params(this)
         implicit none
         type(matrix_free_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_matrix_free_params(this)
         implicit none
         type(matrix_free_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_matrix_free_params(this,un)
         implicit none
         type(matrix_free_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning           = ';write(un,*) this%suppress_warning
         write(un,*) 'alpha                      = ';write(un,*) this%alpha
         write(un,*) 'beta                       = ';write(un,*) this%beta
         write(un,*) 'coeff_natural              = ';write(un,*) this%coeff_natural
         write(un,*) 'coeff_explicit             = ';write(un,*) this%coeff_explicit
         write(un,*) 'coeff_implicit             = ';write(un,*) this%coeff_implicit
         write(un,*) 'coeff_implicit_time_split  = ';write(un,*) this%coeff_implicit_time_split
       end subroutine

       subroutine import_matrix_free_params(this,un)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%suppress_warning
         read(un,*); read(un,*) this%alpha
         read(un,*); read(un,*) this%beta
         read(un,*); read(un,*) this%coeff_natural
         read(un,*); read(un,*) this%coeff_explicit
         read(un,*); read(un,*) this%coeff_implicit
         read(un,*); read(un,*) this%coeff_implicit_time_split
       end subroutine

       subroutine display_wrap_matrix_free_params(this,dir,name)
         implicit none
         type(matrix_free_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_matrix_free_params(this,dir,name)
         implicit none
         type(matrix_free_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_matrix_free_params(this,dir,name)
         implicit none
         type(matrix_free_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module