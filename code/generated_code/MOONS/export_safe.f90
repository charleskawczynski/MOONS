       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_safe_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: export_safe
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_safe;          end interface
       interface delete;           module procedure delete_export_safe;             end interface
       interface display;          module procedure display_export_safe;            end interface
       interface display_short;    module procedure display_short_export_safe;      end interface
       interface display;          module procedure display_wrap_export_safe;       end interface
       interface print;            module procedure print_export_safe;              end interface
       interface print_short;      module procedure print_short_export_safe;        end interface
       interface export;           module procedure export_export_safe;             end interface
       interface export_primitives;module procedure export_primitives_export_safe;  end interface
       interface import;           module procedure import_export_safe;             end interface
       interface export_structured;module procedure export_structured_D_export_safe;end interface
       interface import_structured;module procedure import_structured_D_export_safe;end interface
       interface import_primitives;module procedure import_primitives_export_safe;  end interface
       interface export;           module procedure export_wrap_export_safe;        end interface
       interface import;           module procedure import_wrap_export_safe;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_export_safe;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_export_safe;        end interface
       interface suppress_warnings;module procedure suppress_warnings_export_safe;  end interface

       type export_safe
         logical :: export_now = .false.
         real(cp) :: export_period_sec = 0.0_cp
         real(cp) :: mod_period = 0.0_cp
         real(cp) :: mod_period_last = 0.0_cp
       end type

       contains

       subroutine init_copy_export_safe(this,that)
         implicit none
         type(export_safe),intent(inout) :: this
         type(export_safe),intent(in) :: that
         call delete(this)
         this%export_now = that%export_now
         this%export_period_sec = that%export_period_sec
         this%mod_period = that%mod_period
         this%mod_period_last = that%mod_period_last
       end subroutine

       subroutine delete_export_safe(this)
         implicit none
         type(export_safe),intent(inout) :: this
         this%export_now = .false.
         this%export_period_sec = 0.0_cp
         this%mod_period = 0.0_cp
         this%mod_period_last = 0.0_cp
       end subroutine

       subroutine display_export_safe(this,un)
         implicit none
         type(export_safe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_now        = ',this%export_now
         write(un,*) 'export_period_sec = ',this%export_period_sec
         write(un,*) 'mod_period        = ',this%mod_period
         write(un,*) 'mod_period_last   = ',this%mod_period_last
       end subroutine

       subroutine display_short_export_safe(this,un)
         implicit none
         type(export_safe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_now        = ',this%export_now
         write(un,*) 'export_period_sec = ',this%export_period_sec
         write(un,*) 'mod_period        = ',this%mod_period
         write(un,*) 'mod_period_last   = ',this%mod_period_last
       end subroutine

       subroutine display_wrap_export_safe(this,dir,name)
         implicit none
         type(export_safe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_safe(this)
         implicit none
         type(export_safe),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_safe(this)
         implicit none
         type(export_safe),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_export_safe(this,un)
         implicit none
         type(export_safe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_now         = ';write(un,*) this%export_now
         write(un,*) 'export_period_sec  = ';write(un,*) this%export_period_sec
         write(un,*) 'mod_period         = ';write(un,*) this%mod_period
         write(un,*) 'mod_period_last    = ';write(un,*) this%mod_period_last
       end subroutine

       subroutine import_export_safe(this,un)
         implicit none
         type(export_safe),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_now
         read(un,*); read(un,*) this%export_period_sec
         read(un,*); read(un,*) this%mod_period
         read(un,*); read(un,*) this%mod_period_last
       end subroutine

       subroutine export_primitives_export_safe(this,un)
         implicit none
         type(export_safe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_now         = ';write(un,*) this%export_now
         write(un,*) 'export_period_sec  = ';write(un,*) this%export_period_sec
         write(un,*) 'mod_period         = ';write(un,*) this%mod_period
         write(un,*) 'mod_period_last    = ';write(un,*) this%mod_period_last
       end subroutine

       subroutine import_primitives_export_safe(this,un)
         implicit none
         type(export_safe),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%export_now
         read(un,*); read(un,*) this%export_period_sec
         read(un,*); read(un,*) this%mod_period
         read(un,*); read(un,*) this%mod_period_last
       end subroutine

       subroutine export_wrap_export_safe(this,dir,name)
         implicit none
         type(export_safe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_safe(this,dir,name)
         implicit none
         type(export_safe),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_export_safe(this,dir)
         implicit none
         type(export_safe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_export_safe(this,dir)
         implicit none
         type(export_safe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir(dir)
       end subroutine

       subroutine export_structured_D_export_safe(this,dir)
         implicit none
         type(export_safe),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting export_safe structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_export_safe(this,dir)
         implicit none
         type(export_safe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing export_safe structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_export_safe(this)
         implicit none
         type(export_safe),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module