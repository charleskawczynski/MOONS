       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_frequency_params_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: export_frequency_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_frequency_params;        end interface
       interface delete;           module procedure delete_export_frequency_params;           end interface
       interface display;          module procedure display_export_frequency_params;          end interface
       interface display_short;    module procedure display_short_export_frequency_params;    end interface
       interface display;          module procedure display_wrap_export_frequency_params;     end interface
       interface print;            module procedure print_export_frequency_params;            end interface
       interface print_short;      module procedure print_short_export_frequency_params;      end interface
       interface export;           module procedure export_export_frequency_params;           end interface
       interface export_primitives;module procedure export_primitives_export_frequency_params;end interface
       interface export_restart;   module procedure export_restart_export_frequency_params;   end interface
       interface import;           module procedure import_export_frequency_params;           end interface
       interface import_restart;   module procedure import_restart_export_frequency_params;   end interface
       interface import_primitives;module procedure import_primitives_export_frequency_params;end interface
       interface export;           module procedure export_wrap_export_frequency_params;      end interface
       interface import;           module procedure import_wrap_export_frequency_params;      end interface
       interface make_restart_dir; module procedure make_restart_dir_export_frequency_params; end interface
       interface suppress_warnings;module procedure suppress_warnings_export_frequency_params;end interface

       type export_frequency_params
         logical :: export_ever = .false.
         logical :: export_first_step = .false.
         logical :: export_now = .false.
         integer :: frequency_coeff = 0
         integer :: frequency_base = 0
         integer :: frequency_exp = 0
       end type

       contains

       subroutine init_copy_export_frequency_params(this,that)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         type(export_frequency_params),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
         this%export_first_step = that%export_first_step
         this%export_now = that%export_now
         this%frequency_coeff = that%frequency_coeff
         this%frequency_base = that%frequency_base
         this%frequency_exp = that%frequency_exp
       end subroutine

       subroutine delete_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         this%export_ever = .false.
         this%export_first_step = .false.
         this%export_now = .false.
         this%frequency_coeff = 0
         this%frequency_base = 0
         this%frequency_exp = 0
       end subroutine

       subroutine display_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever       = ',this%export_ever
         write(un,*) 'export_first_step = ',this%export_first_step
         write(un,*) 'export_now        = ',this%export_now
         write(un,*) 'frequency_coeff   = ',this%frequency_coeff
         write(un,*) 'frequency_base    = ',this%frequency_base
         write(un,*) 'frequency_exp     = ',this%frequency_exp
       end subroutine

       subroutine display_short_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever       = ',this%export_ever
         write(un,*) 'export_first_step = ',this%export_first_step
         write(un,*) 'export_now        = ',this%export_now
         write(un,*) 'frequency_coeff   = ',this%frequency_coeff
         write(un,*) 'frequency_base    = ',this%frequency_base
         write(un,*) 'frequency_exp     = ',this%frequency_exp
       end subroutine

       subroutine display_wrap_export_frequency_params(this,dir,name)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever        = ';write(un,*) this%export_ever
         write(un,*) 'export_first_step  = ';write(un,*) this%export_first_step
         write(un,*) 'export_now         = ';write(un,*) this%export_now
         write(un,*) 'frequency_coeff    = ';write(un,*) this%frequency_coeff
         write(un,*) 'frequency_base     = ';write(un,*) this%frequency_base
         write(un,*) 'frequency_exp      = ';write(un,*) this%frequency_exp
       end subroutine

       subroutine export_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever        = ';write(un,*) this%export_ever
         write(un,*) 'export_first_step  = ';write(un,*) this%export_first_step
         write(un,*) 'export_now         = ';write(un,*) this%export_now
         write(un,*) 'frequency_coeff    = ';write(un,*) this%frequency_coeff
         write(un,*) 'frequency_base     = ';write(un,*) this%frequency_base
         write(un,*) 'frequency_exp      = ';write(un,*) this%frequency_exp
       end subroutine

       subroutine import_primitives_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%export_first_step
         read(un,*); read(un,*) this%export_now
         read(un,*); read(un,*) this%frequency_coeff
         read(un,*); read(un,*) this%frequency_base
         read(un,*); read(un,*) this%frequency_exp
       end subroutine

       subroutine import_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%export_first_step
         read(un,*); read(un,*) this%export_now
         read(un,*); read(un,*) this%frequency_coeff
         read(un,*); read(un,*) this%frequency_base
         read(un,*); read(un,*) this%frequency_exp
       end subroutine

       subroutine export_restart_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_restart_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_export_frequency_params(this,dir,name)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_frequency_params(this,dir,name)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine suppress_warnings_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module