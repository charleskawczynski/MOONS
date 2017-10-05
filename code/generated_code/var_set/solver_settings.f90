       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module solver_settings_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: solver_settings
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_solver_settings;          end interface
       interface delete;           module procedure delete_solver_settings;             end interface
       interface display;          module procedure display_solver_settings;            end interface
       interface display_short;    module procedure display_short_solver_settings;      end interface
       interface display;          module procedure display_wrap_solver_settings;       end interface
       interface print;            module procedure print_solver_settings;              end interface
       interface print_short;      module procedure print_short_solver_settings;        end interface
       interface export;           module procedure export_solver_settings;             end interface
       interface export_primitives;module procedure export_primitives_solver_settings;  end interface
       interface import;           module procedure import_solver_settings;             end interface
       interface export_structured;module procedure export_structured_D_solver_settings;end interface
       interface import_structured;module procedure import_structured_D_solver_settings;end interface
       interface import_primitives;module procedure import_primitives_solver_settings;  end interface
       interface export;           module procedure export_wrap_solver_settings;        end interface
       interface import;           module procedure import_wrap_solver_settings;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_solver_settings;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_solver_settings;        end interface
       interface suppress_warnings;module procedure suppress_warnings_solver_settings;  end interface

       type solver_settings
         integer :: solve_method = 0
         logical :: initialize = .false.
         logical :: solve = .false.
         logical :: restart = .false.
         logical :: prescribed_BCs = .false.
       end type

       contains

       subroutine init_copy_solver_settings(this,that)
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

       subroutine delete_solver_settings(this)
         implicit none
         type(solver_settings),intent(inout) :: this
         this%solve_method = 0
         this%initialize = .false.
         this%solve = .false.
         this%restart = .false.
         this%prescribed_BCs = .false.
       end subroutine

       subroutine display_solver_settings(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'solve_method   = ',this%solve_method
         write(un,*) 'initialize     = ',this%initialize
         write(un,*) 'solve          = ',this%solve
         write(un,*) 'restart        = ',this%restart
         write(un,*) 'prescribed_BCs = ',this%prescribed_BCs
       end subroutine

       subroutine display_short_solver_settings(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'solve_method   = ',this%solve_method
         write(un,*) 'initialize     = ',this%initialize
         write(un,*) 'solve          = ',this%solve
         write(un,*) 'restart        = ',this%restart
         write(un,*) 'prescribed_BCs = ',this%prescribed_BCs
       end subroutine

       subroutine display_wrap_solver_settings(this,dir,name)
         implicit none
         type(solver_settings),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_solver_settings(this)
         implicit none
         type(solver_settings),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_solver_settings(this)
         implicit none
         type(solver_settings),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_solver_settings(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_solver_settings(this,un)
         implicit none
         type(solver_settings),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_solver_settings(this,un)
         implicit none
         type(solver_settings),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'solve_method    = ';write(un,*) this%solve_method
         write(un,*) 'initialize      = ';write(un,*) this%initialize
         write(un,*) 'solve           = ';write(un,*) this%solve
         write(un,*) 'restart         = ';write(un,*) this%restart
         write(un,*) 'prescribed_BCs  = ';write(un,*) this%prescribed_BCs
       end subroutine

       subroutine import_primitives_solver_settings(this,un)
         implicit none
         type(solver_settings),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%solve_method
         read(un,*); read(un,*) this%initialize
         read(un,*); read(un,*) this%solve
         read(un,*); read(un,*) this%restart
         read(un,*); read(un,*) this%prescribed_BCs
       end subroutine

       subroutine export_wrap_solver_settings(this,dir,name)
         implicit none
         type(solver_settings),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_solver_settings(this,dir,name)
         implicit none
         type(solver_settings),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_solver_settings(this,dir)
         implicit none
         type(solver_settings),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_solver_settings(this,dir)
         implicit none
         type(solver_settings),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_solver_settings(this,dir)
         implicit none
         type(solver_settings),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_solver_settings(this,dir)
         implicit none
         type(solver_settings),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_solver_settings(this)
         implicit none
         type(solver_settings),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module