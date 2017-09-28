       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module kill_switch_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: kill_switch
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_kill_switch;           end interface
       interface delete;           module procedure delete_kill_switch;              end interface
       interface display;          module procedure display_kill_switch;             end interface
       interface display_short;    module procedure display_short_kill_switch;       end interface
       interface display;          module procedure display_wrap_kill_switch;        end interface
       interface print;            module procedure print_kill_switch;               end interface
       interface print_short;      module procedure print_short_kill_switch;         end interface
       interface export;           module procedure export_kill_switch;              end interface
       interface export_primitives;module procedure export_primitives_kill_switch;   end interface
       interface import;           module procedure import_kill_switch;              end interface
       interface export_structured;module procedure export_structured_D_kill_switch; end interface
       interface import_structured;module procedure import_structured_D_kill_switch; end interface
       interface import_primitives;module procedure import_primitives_kill_switch;   end interface
       interface export;           module procedure export_wrap_kill_switch;         end interface
       interface import;           module procedure import_wrap_kill_switch;         end interface
       interface set_IO_dir;       module procedure set_IO_dir_kill_switch;          end interface
       interface suppress_warnings;module procedure suppress_warnings_kill_switch;   end interface
       interface export;           module procedure export_DN_kill_switch;           end interface
       interface import;           module procedure import_DN_kill_switch;           end interface
       interface export_structured;module procedure export_structured_DN_kill_switch;end interface
       interface import_structured;module procedure import_structured_DN_kill_switch;end interface

       type kill_switch
         integer :: un = 0
         logical :: terminate_loop = .false.
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_kill_switch(this,that)
         implicit none
         type(kill_switch),intent(inout) :: this
         type(kill_switch),intent(in) :: that
         call delete(this)
         this%un = that%un
         this%terminate_loop = that%terminate_loop
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_kill_switch(this)
         implicit none
         type(kill_switch),intent(inout) :: this
         this%un = 0
         this%terminate_loop = .false.
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un             = ',this%un
         write(un,*) 'terminate_loop = ',this%terminate_loop
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un             = ',this%un
         write(un,*) 'terminate_loop = ',this%terminate_loop
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_kill_switch(this,dir,name)
         implicit none
         type(kill_switch),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_kill_switch(this)
         implicit none
         type(kill_switch),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_kill_switch(this)
         implicit none
         type(kill_switch),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'terminate_loop  = ';write(un,*) this%terminate_loop
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%terminate_loop
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_primitives_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'terminate_loop  = ';write(un,*) this%terminate_loop
       end subroutine

       subroutine import_primitives_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%terminate_loop
       end subroutine

       subroutine export_wrap_kill_switch(this,dir,name)
         implicit none
         type(kill_switch),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_kill_switch(this,dir,name)
         implicit none
         type(kill_switch),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_kill_switch(this)
         implicit none
         type(kill_switch),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_kill_switch(this)
         implicit none
         type(kill_switch),intent(inout) :: this
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

       subroutine export_structured_DN_kill_switch(this)
         implicit none
         type(kill_switch),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_DN_kill_switch(this)
         implicit none
         type(kill_switch),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_kill_switch(this,dir)
         implicit none
         type(kill_switch),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
       end subroutine

       subroutine export_structured_D_kill_switch(this,dir)
         implicit none
         type(kill_switch),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_kill_switch(this,dir)
         implicit none
         type(kill_switch),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_kill_switch(this)
         implicit none
         type(kill_switch),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module