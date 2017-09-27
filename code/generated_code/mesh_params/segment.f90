       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module segment_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: segment
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_segment;        end interface
       interface delete;           module procedure delete_segment;           end interface
       interface display;          module procedure display_segment;          end interface
       interface display_short;    module procedure display_short_segment;    end interface
       interface display;          module procedure display_wrap_segment;     end interface
       interface print;            module procedure print_segment;            end interface
       interface print_short;      module procedure print_short_segment;      end interface
       interface export;           module procedure export_segment;           end interface
       interface export_primitives;module procedure export_primitives_segment;end interface
       interface export_restart;   module procedure export_restart_segment;   end interface
       interface import;           module procedure import_segment;           end interface
       interface import_restart;   module procedure import_restart_segment;   end interface
       interface import_primitives;module procedure import_primitives_segment;end interface
       interface export;           module procedure export_wrap_segment;      end interface
       interface import;           module procedure import_wrap_segment;      end interface
       interface make_restart_dir; module procedure make_restart_dir_segment; end interface
       interface suppress_warnings;module procedure suppress_warnings_segment;end interface

       type segment
         integer :: N_cells = 0
         type(string) :: distribution
         real(cp) :: hmax = 0.0_cp
         real(cp) :: hmin = 0.0_cp
         real(cp) :: L = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: yc = 0.0_cp
         integer :: dir = 0
       end type

       contains

       subroutine init_copy_segment(this,that)
         implicit none
         type(segment),intent(inout) :: this
         type(segment),intent(in) :: that
         call delete(this)
         this%N_cells = that%N_cells
         call init(this%distribution,that%distribution)
         this%hmax = that%hmax
         this%hmin = that%hmin
         this%L = that%L
         this%tau = that%tau
         this%yc = that%yc
         this%dir = that%dir
       end subroutine

       subroutine delete_segment(this)
         implicit none
         type(segment),intent(inout) :: this
         this%N_cells = 0
         call delete(this%distribution)
         this%hmax = 0.0_cp
         this%hmin = 0.0_cp
         this%L = 0.0_cp
         this%tau = 0.0_cp
         this%yc = 0.0_cp
         this%dir = 0
       end subroutine

       subroutine display_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells      = ',this%N_cells
         call display(this%distribution,un)
         write(un,*) 'hmax         = ',this%hmax
         write(un,*) 'hmin         = ',this%hmin
         write(un,*) 'L            = ',this%L
         write(un,*) 'tau          = ',this%tau
         write(un,*) 'yc           = ',this%yc
         write(un,*) 'dir          = ',this%dir
       end subroutine

       subroutine display_short_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells      = ',this%N_cells
         call display(this%distribution,un)
         write(un,*) 'hmax         = ',this%hmax
         write(un,*) 'hmin         = ',this%hmin
         write(un,*) 'L            = ',this%L
         write(un,*) 'tau          = ',this%tau
         write(un,*) 'yc           = ',this%yc
         write(un,*) 'dir          = ',this%dir
       end subroutine

       subroutine display_wrap_segment(this,dir,name)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_segment(this)
         implicit none
         type(segment),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_segment(this)
         implicit none
         type(segment),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells       = ';write(un,*) this%N_cells
         write(un,*) 'hmax          = ';write(un,*) this%hmax
         write(un,*) 'hmin          = ';write(un,*) this%hmin
         write(un,*) 'L             = ';write(un,*) this%L
         write(un,*) 'tau           = ';write(un,*) this%tau
         write(un,*) 'yc            = ';write(un,*) this%yc
         write(un,*) 'dir           = ';write(un,*) this%dir
       end subroutine

       subroutine export_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells       = ';write(un,*) this%N_cells
         call export(this%distribution,un)
         write(un,*) 'hmax          = ';write(un,*) this%hmax
         write(un,*) 'hmin          = ';write(un,*) this%hmin
         write(un,*) 'L             = ';write(un,*) this%L
         write(un,*) 'tau           = ';write(un,*) this%tau
         write(un,*) 'yc            = ';write(un,*) this%yc
         write(un,*) 'dir           = ';write(un,*) this%dir
       end subroutine

       subroutine import_primitives_segment(this,un)
         implicit none
         type(segment),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%N_cells
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%L
         read(un,*); read(un,*) this%tau
         read(un,*); read(un,*) this%yc
         read(un,*); read(un,*) this%dir
       end subroutine

       subroutine import_segment(this,un)
         implicit none
         type(segment),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%N_cells
         call import(this%distribution,un)
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%L
         read(un,*); read(un,*) this%tau
         read(un,*); read(un,*) this%yc
         read(un,*); read(un,*) this%dir
       end subroutine

       subroutine export_restart_segment(this,dir)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_restart_segment(this,dir)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_segment(this,dir,name)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_segment(this,dir,name)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_segment(this,dir)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine suppress_warnings_segment(this)
         implicit none
         type(segment),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module