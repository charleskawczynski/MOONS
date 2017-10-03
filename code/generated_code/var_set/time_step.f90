       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_step_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: time_step
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_time_step;           end interface
       interface delete;           module procedure delete_time_step;              end interface
       interface display;          module procedure display_time_step;             end interface
       interface display_short;    module procedure display_short_time_step;       end interface
       interface display;          module procedure display_wrap_time_step;        end interface
       interface print;            module procedure print_time_step;               end interface
       interface print_short;      module procedure print_short_time_step;         end interface
       interface export;           module procedure export_time_step;              end interface
       interface export_primitives;module procedure export_primitives_time_step;   end interface
       interface import;           module procedure import_time_step;              end interface
       interface export_structured;module procedure export_structured_D_time_step; end interface
       interface import_structured;module procedure import_structured_D_time_step; end interface
       interface import_primitives;module procedure import_primitives_time_step;   end interface
       interface export;           module procedure export_wrap_time_step;         end interface
       interface import;           module procedure import_wrap_time_step;         end interface
       interface set_IO_dir;       module procedure set_IO_dir_time_step;          end interface
       interface make_IO_dir;      module procedure make_IO_dir_time_step;         end interface
       interface suppress_warnings;module procedure suppress_warnings_time_step;   end interface
       interface export;           module procedure export_DN_time_step;           end interface
       interface import;           module procedure import_DN_time_step;           end interface
       interface export_structured;module procedure export_structured_DN_time_step;end interface
       interface import_structured;module procedure import_structured_DN_time_step;end interface

       type time_step
         real(cp) :: dt = 0.0_cp
         real(cp) :: t_final = 0.0_cp
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_time_step(this,that)
         implicit none
         type(time_step),intent(inout) :: this
         type(time_step),intent(in) :: that
         call delete(this)
         this%dt = that%dt
         this%t_final = that%t_final
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_time_step(this)
         implicit none
         type(time_step),intent(inout) :: this
         this%dt = 0.0_cp
         this%t_final = 0.0_cp
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_time_step(this,un)
         implicit none
         type(time_step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'dt      = ',this%dt
         write(un,*) 't_final = ',this%t_final
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_time_step(this,un)
         implicit none
         type(time_step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'dt      = ',this%dt
         write(un,*) 't_final = ',this%t_final
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_time_step(this,dir,name)
         implicit none
         type(time_step),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_time_step(this)
         implicit none
         type(time_step),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_time_step(this)
         implicit none
         type(time_step),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_time_step(this,un)
         implicit none
         type(time_step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'dt       = ';write(un,*) this%dt
         write(un,*) 't_final  = ';write(un,*) this%t_final
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_time_step(this,un)
         implicit none
         type(time_step),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%dt
         read(un,*); read(un,*) this%t_final
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_primitives_time_step(this,un)
         implicit none
         type(time_step),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'dt       = ';write(un,*) this%dt
         write(un,*) 't_final  = ';write(un,*) this%t_final
       end subroutine

       subroutine import_primitives_time_step(this,un)
         implicit none
         type(time_step),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%dt
         read(un,*); read(un,*) this%t_final
       end subroutine

       subroutine export_wrap_time_step(this,dir,name)
         implicit none
         type(time_step),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_time_step(this,dir,name)
         implicit none
         type(time_step),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_time_step(this)
         implicit none
         type(time_step),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_time_step(this)
         implicit none
         type(time_step),intent(inout) :: this
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

       subroutine export_structured_DN_time_step(this)
         implicit none
         type(time_step),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_DN_time_step(this)
         implicit none
         type(time_step),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_time_step(this,dir)
         implicit none
         type(time_step),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
         call init(this%dir,dir)
         call init(this%name,'primitives')
       end subroutine

       subroutine make_IO_dir_time_step(this,dir)
         implicit none
         type(time_step),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
       end subroutine

       subroutine export_structured_D_time_step(this,dir)
         implicit none
         type(time_step),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting time_step structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_time_step(this,dir)
         implicit none
         type(time_step),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing time_step structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_time_step(this)
         implicit none
         type(time_step),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module