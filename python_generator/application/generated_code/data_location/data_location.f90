       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module data_location_mod
       use IO_tools_mod
       implicit none

       private
       public :: data_location
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_data_location;           end interface
       interface delete; module procedure delete_data_location;         end interface
       interface display;module procedure display_data_location;        end interface
       interface display;module procedure display_wrapper_data_location;end interface
       interface print;  module procedure print_data_location;          end interface
       interface export; module procedure export_data_location;         end interface
       interface import; module procedure import_data_location;         end interface
       interface export; module procedure export_wrapper_data_location; end interface
       interface import; module procedure import_wrapper_data_location; end interface

       type data_location
         logical :: c = .false.
         logical :: n = .false.
         logical :: e = .false.
         logical :: f = .false.
         logical :: face = .false.
         logical :: edge = .false.
         logical,dimension(3) :: cc_along = .false.
         logical,dimension(3) :: n_along = .false.
         integer,dimension(3) :: cc_eye = 0
         integer,dimension(3) :: n_eye = 0
         logical :: defined = .false.
       end type

       contains

       subroutine init_data_location(this,that)
         implicit none
         type(data_location),intent(inout) :: this
         type(data_location),intent(in) :: that
         call delete(this)
         this%c = that%c
         this%n = that%n
         this%e = that%e
         this%f = that%f
         this%face = that%face
         this%edge = that%edge
         this%cc_along = that%cc_along
         this%n_along = that%n_along
         this%cc_eye = that%cc_eye
         this%n_eye = that%n_eye
         this%defined = that%defined
       end subroutine

       subroutine delete_data_location(this)
         implicit none
         type(data_location),intent(inout) :: this
         this%c = .false.
         this%n = .false.
         this%e = .false.
         this%f = .false.
         this%face = .false.
         this%edge = .false.
         this%cc_along = .false.
         this%n_along = .false.
         this%cc_eye = 0
         this%n_eye = 0
         this%defined = .false.
       end subroutine

       subroutine display_data_location(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- data_location'
         write(un,*) 'c        = ',this%c
         write(un,*) 'n        = ',this%n
         write(un,*) 'e        = ',this%e
         write(un,*) 'f        = ',this%f
         write(un,*) 'face     = ',this%face
         write(un,*) 'edge     = ',this%edge
         write(un,*) 'cc_along = ',this%cc_along
         write(un,*) 'n_along  = ',this%n_along
         write(un,*) 'cc_eye   = ',this%cc_eye
         write(un,*) 'n_eye    = ',this%n_eye
         write(un,*) 'defined  = ',this%defined
       end subroutine

       subroutine print_data_location(this)
         implicit none
         type(data_location),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_data_location(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'c         = ';write(un,*) this%c
         write(un,*) 'n         = ';write(un,*) this%n
         write(un,*) 'e         = ';write(un,*) this%e
         write(un,*) 'f         = ';write(un,*) this%f
         write(un,*) 'face      = ';write(un,*) this%face
         write(un,*) 'edge      = ';write(un,*) this%edge
         write(un,*) 'cc_along  = ';write(un,*) this%cc_along
         write(un,*) 'n_along   = ';write(un,*) this%n_along
         write(un,*) 'cc_eye    = ';write(un,*) this%cc_eye
         write(un,*) 'n_eye     = ';write(un,*) this%n_eye
         write(un,*) 'defined   = ';write(un,*) this%defined
       end subroutine

       subroutine import_data_location(this,un)
         implicit none
         type(data_location),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%c
         read(un,*); read(un,*) this%n
         read(un,*); read(un,*) this%e
         read(un,*); read(un,*) this%f
         read(un,*); read(un,*) this%face
         read(un,*); read(un,*) this%edge
         read(un,*); read(un,*) this%cc_along
         read(un,*); read(un,*) this%n_along
         read(un,*); read(un,*) this%cc_eye
         read(un,*); read(un,*) this%n_eye
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrapper_data_location(this,dir,name)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_data_location(this,dir,name)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_data_location(this,dir,name)
         implicit none
         type(data_location),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module