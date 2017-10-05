       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module geometry_props_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: geometry_props
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_geometry_props;          end interface
       interface delete;           module procedure delete_geometry_props;             end interface
       interface display;          module procedure display_geometry_props;            end interface
       interface display_short;    module procedure display_short_geometry_props;      end interface
       interface display;          module procedure display_wrap_geometry_props;       end interface
       interface print;            module procedure print_geometry_props;              end interface
       interface print_short;      module procedure print_short_geometry_props;        end interface
       interface export;           module procedure export_geometry_props;             end interface
       interface export_primitives;module procedure export_primitives_geometry_props;  end interface
       interface import;           module procedure import_geometry_props;             end interface
       interface export_structured;module procedure export_structured_D_geometry_props;end interface
       interface import_structured;module procedure import_structured_D_geometry_props;end interface
       interface import_primitives;module procedure import_primitives_geometry_props;  end interface
       interface export;           module procedure export_wrap_geometry_props;        end interface
       interface import;           module procedure import_wrap_geometry_props;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_geometry_props;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_geometry_props;        end interface
       interface suppress_warnings;module procedure suppress_warnings_geometry_props;  end interface

       type geometry_props
         integer :: geometry = 0
         real(cp) :: tw = 0.0_cp
         integer,dimension(3) :: periodic_dir = 0
         integer,dimension(6) :: apply_BC_order = 0
       end type

       contains

       subroutine init_copy_geometry_props(this,that)
         implicit none
         type(geometry_props),intent(inout) :: this
         type(geometry_props),intent(in) :: that
         call delete(this)
         this%geometry = that%geometry
         this%tw = that%tw
         this%periodic_dir = that%periodic_dir
         this%apply_BC_order = that%apply_BC_order
       end subroutine

       subroutine delete_geometry_props(this)
         implicit none
         type(geometry_props),intent(inout) :: this
         this%geometry = 0
         this%tw = 0.0_cp
         this%periodic_dir = 0
         this%apply_BC_order = 0
       end subroutine

       subroutine display_geometry_props(this,un)
         implicit none
         type(geometry_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'geometry       = ',this%geometry
         write(un,*) 'tw             = ',this%tw
         write(un,*) 'periodic_dir   = ',this%periodic_dir
         write(un,*) 'apply_BC_order = ',this%apply_BC_order
       end subroutine

       subroutine display_short_geometry_props(this,un)
         implicit none
         type(geometry_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'geometry       = ',this%geometry
         write(un,*) 'tw             = ',this%tw
         write(un,*) 'periodic_dir   = ',this%periodic_dir
         write(un,*) 'apply_BC_order = ',this%apply_BC_order
       end subroutine

       subroutine display_wrap_geometry_props(this,dir,name)
         implicit none
         type(geometry_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_geometry_props(this)
         implicit none
         type(geometry_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_geometry_props(this)
         implicit none
         type(geometry_props),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_geometry_props(this,un)
         implicit none
         type(geometry_props),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_geometry_props(this,un)
         implicit none
         type(geometry_props),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_geometry_props(this,un)
         implicit none
         type(geometry_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'geometry        = ';write(un,*) this%geometry
         write(un,*) 'tw              = ';write(un,*) this%tw
         write(un,*) 'periodic_dir    = ';write(un,*) this%periodic_dir
         write(un,*) 'apply_BC_order  = ';write(un,*) this%apply_BC_order
       end subroutine

       subroutine import_primitives_geometry_props(this,un)
         implicit none
         type(geometry_props),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%geometry
         read(un,*); read(un,*) this%tw
         read(un,*); read(un,*) this%periodic_dir
         read(un,*); read(un,*) this%apply_BC_order
       end subroutine

       subroutine export_wrap_geometry_props(this,dir,name)
         implicit none
         type(geometry_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_geometry_props(this,dir,name)
         implicit none
         type(geometry_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_geometry_props(this,dir)
         implicit none
         type(geometry_props),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_geometry_props(this,dir)
         implicit none
         type(geometry_props),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_geometry_props(this,dir)
         implicit none
         type(geometry_props),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_geometry_props(this,dir)
         implicit none
         type(geometry_props),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_geometry_props(this)
         implicit none
         type(geometry_props),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module