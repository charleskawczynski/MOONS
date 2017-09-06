       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module solver_settings_mod
       use IO_tools_mod
       implicit none

       private
       public :: solver_settings
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_so;    end interface
       interface delete;       module procedure delete_so;       end interface
       interface display;      module procedure display_so;      end interface
       interface display_short;module procedure display_short_so;end interface
       interface display;      module procedure display_wrap_so; end interface
       interface print;        module procedure print_so;        end interface
       interface print_short;  module procedure print_short_so;  end interface
       interface export;       module procedure export_so;       end interface
       interface import;       module procedure import_so;       end interface
       interface export;       module procedure export_wrap_so;  end interface
       interface import;       module procedure import_wrap_so;  end interface

       type solver_settings
         integer :: solve_method = 0
         logical :: initialize = .false.
         logical :: solve = .false.
         logical :: restart = .false.
         logical :: prescribed_BCs = .false.
       end type

       contains

       subroutine init_copy_so(this,that)
         implicit none
         type(solver_settings),intent(inout) :: this
         type(solver_settings),intent(in) :: that
         call delete(this)
         this%solve_method = that%solve_method
         this%initialize = that%initialize
         this%solve = that%solve
         this%restart = that%restart
         this%prescribed_BCs = that%prescribed_BCs
       end subroutine

       subroutine delete_so(this)
         implicit none
         type(solver_settings),intent(inout) :: this
         this%solve_method = 0
         this%initialize = .false.
         this%solve = .false.
         this%restart = .false.
         this%prescribed_BCs = .false.
       end subroutine

       subroutine display_so(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_so(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'solve_method   = ',this%solve_method
         write(un,*) 'initialize     = ',this%initialize
         write(un,*) 'solve          = ',this%solve
         write(un,*) 'restart        = ',this%restart
         write(un,*) 'prescribed_BCs = ',this%prescribed_BCs
       end subroutine

       subroutine print_so(this)
         implicit none
         type(solver_settings),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_so(this)
         implicit none
         type(solver_settings),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_so(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'solve_method    = ';write(un,*) this%solve_method
         write(un,*) 'initialize      = ';write(un,*) this%initialize
         write(un,*) 'solve           = ';write(un,*) this%solve
         write(un,*) 'restart         = ';write(un,*) this%restart
         write(un,*) 'prescribed_BCs  = ';write(un,*) this%prescribed_BCs
       end subroutine

       subroutine import_so(this,un)
         implicit none
         type(solver_settings),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%solve_method
         read(un,*); read(un,*) this%initialize
         read(un,*); read(un,*) this%solve
         read(un,*); read(un,*) this%restart
         read(un,*); read(un,*) this%prescribed_BCs
       end subroutine

       subroutine display_wrap_so(this,dir,name)
         implicit none
         type(solver_settings),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_so(this,dir,name)
         implicit none
         type(solver_settings),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_so(this,dir,name)
         implicit none
         type(solver_settings),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module