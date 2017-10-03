       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module exit_criteria_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: exit_criteria
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_exit_criteria;           end interface
       interface delete;           module procedure delete_exit_criteria;              end interface
       interface display;          module procedure display_exit_criteria;             end interface
       interface display_short;    module procedure display_short_exit_criteria;       end interface
       interface display;          module procedure display_wrap_exit_criteria;        end interface
       interface print;            module procedure print_exit_criteria;               end interface
       interface print_short;      module procedure print_short_exit_criteria;         end interface
       interface export;           module procedure export_exit_criteria;              end interface
       interface export_primitives;module procedure export_primitives_exit_criteria;   end interface
       interface import;           module procedure import_exit_criteria;              end interface
       interface export_structured;module procedure export_structured_D_exit_criteria; end interface
       interface import_structured;module procedure import_structured_D_exit_criteria; end interface
       interface import_primitives;module procedure import_primitives_exit_criteria;   end interface
       interface export;           module procedure export_wrap_exit_criteria;         end interface
       interface import;           module procedure import_wrap_exit_criteria;         end interface
       interface set_IO_dir;       module procedure set_IO_dir_exit_criteria;          end interface
       interface make_IO_dir;      module procedure make_IO_dir_exit_criteria;         end interface
       interface suppress_warnings;module procedure suppress_warnings_exit_criteria;   end interface
       interface export;           module procedure export_DN_exit_criteria;           end interface
       interface import;           module procedure import_DN_exit_criteria;           end interface
       interface export_structured;module procedure export_structured_DN_exit_criteria;end interface
       interface import_structured;module procedure import_structured_DN_exit_criteria;end interface

       type exit_criteria
         type(string) :: dir
         type(string) :: name
         integer :: iter_max = 0
         real(cp) :: tol_abs = 0.0_cp
         real(cp) :: tol_rel = 0.0_cp
       end type

       contains

       subroutine init_copy_exit_criteria(this,that)
         implicit none
         type(exit_criteria),intent(inout) :: this
         type(exit_criteria),intent(in) :: that
         call delete(this)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         this%iter_max = that%iter_max
         this%tol_abs = that%tol_abs
         this%tol_rel = that%tol_rel
       end subroutine

       subroutine delete_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(inout) :: this
         call delete(this%dir)
         call delete(this%name)
         this%iter_max = 0
         this%tol_abs = 0.0_cp
         this%tol_rel = 0.0_cp
       end subroutine

       subroutine display_exit_criteria(this,un)
         implicit none
         type(exit_criteria),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_max = ',this%iter_max
         write(un,*) 'tol_abs  = ',this%tol_abs
         write(un,*) 'tol_rel  = ',this%tol_rel
       end subroutine

       subroutine display_short_exit_criteria(this,un)
         implicit none
         type(exit_criteria),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_max = ',this%iter_max
         write(un,*) 'tol_abs  = ',this%tol_abs
         write(un,*) 'tol_rel  = ',this%tol_rel
       end subroutine

       subroutine display_wrap_exit_criteria(this,dir,name)
         implicit none
         type(exit_criteria),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_exit_criteria(this,un)
         implicit none
         type(exit_criteria),intent(in) :: this
         integer,intent(in) :: un
         call export(this%dir,un)
         call export(this%name,un)
         write(un,*) 'iter_max  = ';write(un,*) this%iter_max
         write(un,*) 'tol_abs   = ';write(un,*) this%tol_abs
         write(un,*) 'tol_rel   = ';write(un,*) this%tol_rel
       end subroutine

       subroutine import_exit_criteria(this,un)
         implicit none
         type(exit_criteria),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%dir,un)
         call import(this%name,un)
         read(un,*); read(un,*) this%iter_max
         read(un,*); read(un,*) this%tol_abs
         read(un,*); read(un,*) this%tol_rel
       end subroutine

       subroutine export_primitives_exit_criteria(this,un)
         implicit none
         type(exit_criteria),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'iter_max  = ';write(un,*) this%iter_max
         write(un,*) 'tol_abs   = ';write(un,*) this%tol_abs
         write(un,*) 'tol_rel   = ';write(un,*) this%tol_rel
       end subroutine

       subroutine import_primitives_exit_criteria(this,un)
         implicit none
         type(exit_criteria),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%iter_max
         read(un,*); read(un,*) this%tol_abs
         read(un,*); read(un,*) this%tol_rel
       end subroutine

       subroutine export_wrap_exit_criteria(this,dir,name)
         implicit none
         type(exit_criteria),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_exit_criteria(this,dir,name)
         implicit none
         type(exit_criteria),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       subroutine export_structured_DN_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_DN_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_exit_criteria(this,dir)
         implicit none
         type(exit_criteria),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
         call init(this%dir,dir)
         call init(this%name,'primitives')
       end subroutine

       subroutine make_IO_dir_exit_criteria(this,dir)
         implicit none
         type(exit_criteria),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
       end subroutine

       subroutine export_structured_D_exit_criteria(this,dir)
         implicit none
         type(exit_criteria),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting exit_criteria structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_exit_criteria(this,dir)
         implicit none
         type(exit_criteria),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing exit_criteria structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_exit_criteria(this)
         implicit none
         type(exit_criteria),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module