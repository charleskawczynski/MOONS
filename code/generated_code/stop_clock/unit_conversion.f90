       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module unit_conversion_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: unit_conversion
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_unit_conversion;          end interface
       interface delete;           module procedure delete_unit_conversion;             end interface
       interface display;          module procedure display_unit_conversion;            end interface
       interface display_short;    module procedure display_short_unit_conversion;      end interface
       interface display;          module procedure display_wrap_unit_conversion;       end interface
       interface print;            module procedure print_unit_conversion;              end interface
       interface print_short;      module procedure print_short_unit_conversion;        end interface
       interface export;           module procedure export_unit_conversion;             end interface
       interface export_primitives;module procedure export_primitives_unit_conversion;  end interface
       interface import;           module procedure import_unit_conversion;             end interface
       interface export_structured;module procedure export_structured_D_unit_conversion;end interface
       interface import_structured;module procedure import_structured_D_unit_conversion;end interface
       interface import_primitives;module procedure import_primitives_unit_conversion;  end interface
       interface export;           module procedure export_wrap_unit_conversion;        end interface
       interface import;           module procedure import_wrap_unit_conversion;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_unit_conversion;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_unit_conversion;        end interface
       interface suppress_warnings;module procedure suppress_warnings_unit_conversion;  end interface

       type unit_conversion
         real(cp) :: days_per_year = 0.0_cp
         real(cp) :: seconds_per_second = 0.0_cp
         real(cp) :: seconds_per_minute = 0.0_cp
         real(cp) :: seconds_per_hour = 0.0_cp
         real(cp) :: seconds_per_day = 0.0_cp
         real(cp) :: seconds_per_year = 0.0_cp
         real(cp) :: minute_per_seconds = 0.0_cp
         real(cp) :: hour_per_seconds = 0.0_cp
         real(cp) :: day_per_seconds = 0.0_cp
         real(cp) :: year_per_seconds = 0.0_cp
       end type

       contains

       subroutine init_copy_unit_conversion(this,that)
         implicit none
         type(unit_conversion),intent(inout) :: this
         type(unit_conversion),intent(in) :: that
         call delete(this)
         this%days_per_year = that%days_per_year
         this%seconds_per_second = that%seconds_per_second
         this%seconds_per_minute = that%seconds_per_minute
         this%seconds_per_hour = that%seconds_per_hour
         this%seconds_per_day = that%seconds_per_day
         this%seconds_per_year = that%seconds_per_year
         this%minute_per_seconds = that%minute_per_seconds
         this%hour_per_seconds = that%hour_per_seconds
         this%day_per_seconds = that%day_per_seconds
         this%year_per_seconds = that%year_per_seconds
       end subroutine

       subroutine delete_unit_conversion(this)
         implicit none
         type(unit_conversion),intent(inout) :: this
         this%days_per_year = 0.0_cp
         this%seconds_per_second = 0.0_cp
         this%seconds_per_minute = 0.0_cp
         this%seconds_per_hour = 0.0_cp
         this%seconds_per_day = 0.0_cp
         this%seconds_per_year = 0.0_cp
         this%minute_per_seconds = 0.0_cp
         this%hour_per_seconds = 0.0_cp
         this%day_per_seconds = 0.0_cp
         this%year_per_seconds = 0.0_cp
       end subroutine

       subroutine display_unit_conversion(this,un)
         implicit none
         type(unit_conversion),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'days_per_year      = ',this%days_per_year
         write(un,*) 'seconds_per_second = ',this%seconds_per_second
         write(un,*) 'seconds_per_minute = ',this%seconds_per_minute
         write(un,*) 'seconds_per_hour   = ',this%seconds_per_hour
         write(un,*) 'seconds_per_day    = ',this%seconds_per_day
         write(un,*) 'seconds_per_year   = ',this%seconds_per_year
         write(un,*) 'minute_per_seconds = ',this%minute_per_seconds
         write(un,*) 'hour_per_seconds   = ',this%hour_per_seconds
         write(un,*) 'day_per_seconds    = ',this%day_per_seconds
         write(un,*) 'year_per_seconds   = ',this%year_per_seconds
       end subroutine

       subroutine display_short_unit_conversion(this,un)
         implicit none
         type(unit_conversion),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'days_per_year      = ',this%days_per_year
         write(un,*) 'seconds_per_second = ',this%seconds_per_second
         write(un,*) 'seconds_per_minute = ',this%seconds_per_minute
         write(un,*) 'seconds_per_hour   = ',this%seconds_per_hour
         write(un,*) 'seconds_per_day    = ',this%seconds_per_day
         write(un,*) 'seconds_per_year   = ',this%seconds_per_year
         write(un,*) 'minute_per_seconds = ',this%minute_per_seconds
         write(un,*) 'hour_per_seconds   = ',this%hour_per_seconds
         write(un,*) 'day_per_seconds    = ',this%day_per_seconds
         write(un,*) 'year_per_seconds   = ',this%year_per_seconds
       end subroutine

       subroutine display_wrap_unit_conversion(this,dir,name)
         implicit none
         type(unit_conversion),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_unit_conversion(this)
         implicit none
         type(unit_conversion),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_unit_conversion(this)
         implicit none
         type(unit_conversion),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_unit_conversion(this,un)
         implicit none
         type(unit_conversion),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'days_per_year       = ';write(un,*) this%days_per_year
         write(un,*) 'seconds_per_second  = ';write(un,*) this%seconds_per_second
         write(un,*) 'seconds_per_minute  = ';write(un,*) this%seconds_per_minute
         write(un,*) 'seconds_per_hour    = ';write(un,*) this%seconds_per_hour
         write(un,*) 'seconds_per_day     = ';write(un,*) this%seconds_per_day
         write(un,*) 'seconds_per_year    = ';write(un,*) this%seconds_per_year
         write(un,*) 'minute_per_seconds  = ';write(un,*) this%minute_per_seconds
         write(un,*) 'hour_per_seconds    = ';write(un,*) this%hour_per_seconds
         write(un,*) 'day_per_seconds     = ';write(un,*) this%day_per_seconds
         write(un,*) 'year_per_seconds    = ';write(un,*) this%year_per_seconds
       end subroutine

       subroutine import_unit_conversion(this,un)
         implicit none
         type(unit_conversion),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%days_per_year
         read(un,*); read(un,*) this%seconds_per_second
         read(un,*); read(un,*) this%seconds_per_minute
         read(un,*); read(un,*) this%seconds_per_hour
         read(un,*); read(un,*) this%seconds_per_day
         read(un,*); read(un,*) this%seconds_per_year
         read(un,*); read(un,*) this%minute_per_seconds
         read(un,*); read(un,*) this%hour_per_seconds
         read(un,*); read(un,*) this%day_per_seconds
         read(un,*); read(un,*) this%year_per_seconds
       end subroutine

       subroutine export_primitives_unit_conversion(this,un)
         implicit none
         type(unit_conversion),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'days_per_year       = ';write(un,*) this%days_per_year
         write(un,*) 'seconds_per_second  = ';write(un,*) this%seconds_per_second
         write(un,*) 'seconds_per_minute  = ';write(un,*) this%seconds_per_minute
         write(un,*) 'seconds_per_hour    = ';write(un,*) this%seconds_per_hour
         write(un,*) 'seconds_per_day     = ';write(un,*) this%seconds_per_day
         write(un,*) 'seconds_per_year    = ';write(un,*) this%seconds_per_year
         write(un,*) 'minute_per_seconds  = ';write(un,*) this%minute_per_seconds
         write(un,*) 'hour_per_seconds    = ';write(un,*) this%hour_per_seconds
         write(un,*) 'day_per_seconds     = ';write(un,*) this%day_per_seconds
         write(un,*) 'year_per_seconds    = ';write(un,*) this%year_per_seconds
       end subroutine

       subroutine import_primitives_unit_conversion(this,un)
         implicit none
         type(unit_conversion),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%days_per_year
         read(un,*); read(un,*) this%seconds_per_second
         read(un,*); read(un,*) this%seconds_per_minute
         read(un,*); read(un,*) this%seconds_per_hour
         read(un,*); read(un,*) this%seconds_per_day
         read(un,*); read(un,*) this%seconds_per_year
         read(un,*); read(un,*) this%minute_per_seconds
         read(un,*); read(un,*) this%hour_per_seconds
         read(un,*); read(un,*) this%day_per_seconds
         read(un,*); read(un,*) this%year_per_seconds
       end subroutine

       subroutine export_wrap_unit_conversion(this,dir,name)
         implicit none
         type(unit_conversion),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_unit_conversion(this,dir,name)
         implicit none
         type(unit_conversion),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_unit_conversion(this,dir)
         implicit none
         type(unit_conversion),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) write(*,*) dir
       end subroutine

       subroutine make_IO_dir_unit_conversion(this,dir)
         implicit none
         type(unit_conversion),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_unit_conversion(this,dir)
         implicit none
         type(unit_conversion),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting unit_conversion structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_unit_conversion(this,dir)
         implicit none
         type(unit_conversion),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing unit_conversion structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_unit_conversion(this)
         implicit none
         type(unit_conversion),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module