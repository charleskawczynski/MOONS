       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module probe_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: probe
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_probe;          end interface
       interface delete;           module procedure delete_probe;             end interface
       interface display;          module procedure display_probe;            end interface
       interface display_short;    module procedure display_short_probe;      end interface
       interface display;          module procedure display_wrap_probe;       end interface
       interface print;            module procedure print_probe;              end interface
       interface print_short;      module procedure print_short_probe;        end interface
       interface export;           module procedure export_probe;             end interface
       interface export_primitives;module procedure export_primitives_probe;  end interface
       interface import;           module procedure import_probe;             end interface
       interface export_structured;module procedure export_structured_D_probe;end interface
       interface import_structured;module procedure import_structured_D_probe;end interface
       interface import_primitives;module procedure import_primitives_probe;  end interface
       interface export;           module procedure export_wrap_probe;        end interface
       interface import;           module procedure import_wrap_probe;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_probe;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_probe;        end interface
       interface suppress_warnings;module procedure suppress_warnings_probe;  end interface

       type probe
         type(string) :: tec_dir
         type(string) :: tec_name
         real(cp) :: d = 0.0_cp
         real(cp) :: d_data_dt = 0.0_cp
         real(cp) :: d_amax = 0.0_cp
         real(cp) :: t = 0.0_cp
         integer :: un = 0
         integer :: cols = 0
         integer(li) :: n_step = 0
         logical :: restart = .false.
         logical :: simple = .false.
       end type

       contains

       subroutine init_copy_probe(this,that)
         implicit none
         type(probe),intent(inout) :: this
         type(probe),intent(in) :: that
         call delete(this)
         call init(this%tec_dir,that%tec_dir)
         call init(this%tec_name,that%tec_name)
         this%d = that%d
         this%d_data_dt = that%d_data_dt
         this%d_amax = that%d_amax
         this%t = that%t
         this%un = that%un
         this%cols = that%cols
         this%n_step = that%n_step
         this%restart = that%restart
         this%simple = that%simple
       end subroutine

       subroutine delete_probe(this)
         implicit none
         type(probe),intent(inout) :: this
         call delete(this%tec_dir)
         call delete(this%tec_name)
         this%d = 0.0_cp
         this%d_data_dt = 0.0_cp
         this%d_amax = 0.0_cp
         this%t = 0.0_cp
         this%un = 0
         this%cols = 0
         this%n_step = 0
         this%restart = .false.
         this%simple = .false.
       end subroutine

       subroutine display_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call display(this%tec_dir,un)
         call display(this%tec_name,un)
         write(un,*) 'd         = ',this%d
         write(un,*) 'd_data_dt = ',this%d_data_dt
         write(un,*) 'd_amax    = ',this%d_amax
         write(un,*) 't         = ',this%t
         write(un,*) 'un        = ',this%un
         write(un,*) 'cols      = ',this%cols
         write(un,*) 'n_step    = ',this%n_step
         write(un,*) 'restart   = ',this%restart
         write(un,*) 'simple    = ',this%simple
       end subroutine

       subroutine display_short_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call display(this%tec_dir,un)
         call display(this%tec_name,un)
         write(un,*) 'd         = ',this%d
         write(un,*) 'd_data_dt = ',this%d_data_dt
         write(un,*) 'd_amax    = ',this%d_amax
         write(un,*) 't         = ',this%t
         write(un,*) 'un        = ',this%un
         write(un,*) 'cols      = ',this%cols
         write(un,*) 'n_step    = ',this%n_step
         write(un,*) 'restart   = ',this%restart
         write(un,*) 'simple    = ',this%simple
       end subroutine

       subroutine display_wrap_probe(this,dir,name)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_probe(this)
         implicit none
         type(probe),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_probe(this)
         implicit none
         type(probe),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call export(this%tec_dir,un)
         call export(this%tec_name,un)
         write(un,*) 'd          = ';write(un,*) this%d
         write(un,*) 'd_data_dt  = ';write(un,*) this%d_data_dt
         write(un,*) 'd_amax     = ';write(un,*) this%d_amax
         write(un,*) 't          = ';write(un,*) this%t
         write(un,*) 'un         = ';write(un,*) this%un
         write(un,*) 'cols       = ';write(un,*) this%cols
         write(un,*) 'n_step     = ';write(un,*) this%n_step
         write(un,*) 'restart    = ';write(un,*) this%restart
         write(un,*) 'simple     = ';write(un,*) this%simple
       end subroutine

       subroutine import_probe(this,un)
         implicit none
         type(probe),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%tec_dir,un)
         call import(this%tec_name,un)
         read(un,*); read(un,*) this%d
         read(un,*); read(un,*) this%d_data_dt
         read(un,*); read(un,*) this%d_amax
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%cols
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%restart
         read(un,*); read(un,*) this%simple
       end subroutine

       subroutine export_primitives_probe(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'd          = ';write(un,*) this%d
         write(un,*) 'd_data_dt  = ';write(un,*) this%d_data_dt
         write(un,*) 'd_amax     = ';write(un,*) this%d_amax
         write(un,*) 't          = ';write(un,*) this%t
         write(un,*) 'un         = ';write(un,*) this%un
         write(un,*) 'cols       = ';write(un,*) this%cols
         write(un,*) 'n_step     = ';write(un,*) this%n_step
         write(un,*) 'restart    = ';write(un,*) this%restart
         write(un,*) 'simple     = ';write(un,*) this%simple
       end subroutine

       subroutine import_primitives_probe(this,un)
         implicit none
         type(probe),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%d
         read(un,*); read(un,*) this%d_data_dt
         read(un,*); read(un,*) this%d_amax
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%cols
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%restart
         read(un,*); read(un,*) this%simple
       end subroutine

       subroutine export_wrap_probe(this,dir,name)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_probe(this,dir,name)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_probe(this,dir)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) write(*,*) dir
       end subroutine

       subroutine make_IO_dir_probe(this,dir)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir(dir)
       end subroutine

       subroutine export_structured_D_probe(this,dir)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting probe structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_probe(this,dir)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing probe structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_probe(this)
         implicit none
         type(probe),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module