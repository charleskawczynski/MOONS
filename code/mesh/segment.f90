       module segment_mod
       use current_precision_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: segment
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_segment;          end interface
       interface delete; module procedure delete_segment;        end interface
       interface display;module procedure display_segment;       end interface
       interface print;  module procedure print_segment;         end interface
       interface export; module procedure export_segment;        end interface
       interface import; module procedure import_segment;        end interface
       interface export; module procedure export_wrapper_segment;end interface
       interface import; module procedure import_wrapper_segment;end interface

       type segment
         integer :: n_cells = 0
         type(string) :: distribution
         real(cp) :: hmax = 0.0_cp
         real(cp) :: hmin = 0.0_cp
         real(cp) :: l = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: yc = 0.0_cp
         integer :: dir = 0
       end type

       contains

       subroutine init_segment(this,that)
         implicit none
         type(segment),intent(inout) :: this
         type(segment),intent(in) :: that
         call delete(this)
         this%n_cells = that%n_cells
         call init(this%distribution,that%distribution)
         this%hmax = that%hmax
         this%hmin = that%hmin
         this%l = that%l
         this%tau = that%tau
         this%yc = that%yc
         this%dir = that%dir
       end subroutine

       subroutine delete_segment(this)
         implicit none
         type(segment),intent(inout) :: this
         this%n_cells = 0
         call delete(this%distribution)
         this%hmax = 0.0_cp
         this%hmin = 0.0_cp
         this%l = 0.0_cp
         this%tau = 0.0_cp
         this%yc = 0.0_cp
         this%dir = 0
       end subroutine

       subroutine display_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- segment'
         write(un,*) 'n_cells      = ',this%n_cells
         call display(this%distribution,un)
         write(un,*) 'hmax         = ',this%hmax
         write(un,*) 'hmin         = ',this%hmin
         write(un,*) 'l            = ',this%l
         write(un,*) 'tau          = ',this%tau
         write(un,*) 'yc           = ',this%yc
         write(un,*) 'dir          = ',this%dir
       end subroutine

       subroutine print_segment(this)
         implicit none
         type(segment),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_cells       = ';write(un,*) this%n_cells
         call export(this%distribution,un)
         write(un,*) 'hmax          = ';write(un,*) this%hmax
         write(un,*) 'hmin          = ';write(un,*) this%hmin
         write(un,*) 'l             = ';write(un,*) this%l
         write(un,*) 'tau           = ';write(un,*) this%tau
         write(un,*) 'yc            = ';write(un,*) this%yc
         write(un,*) 'dir           = ';write(un,*) this%dir
       end subroutine

       subroutine import_segment(this,un)
         implicit none
         type(segment),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%n_cells
         call import(this%distribution,un)
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%l
         read(un,*); read(un,*) this%tau
         read(un,*); read(un,*) this%yc
         read(un,*); read(un,*) this%dir
       end subroutine

       subroutine export_wrapper_segment(this,dir,name)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_segment(this,dir,name)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module