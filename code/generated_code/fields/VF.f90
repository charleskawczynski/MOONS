       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module VF_mod
       use IO_tools_mod
       use SF_mod
       implicit none

       private
       public :: VF
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_VF;    end interface
       interface delete;       module procedure delete_VF;       end interface
       interface display;      module procedure display_VF;      end interface
       interface display_short;module procedure display_short_VF;end interface
       interface display;      module procedure display_wrap_VF; end interface
       interface print;        module procedure print_VF;        end interface
       interface print_short;  module procedure print_short_VF;  end interface
       interface export;       module procedure export_VF;       end interface
       interface import;       module procedure import_VF;       end interface
       interface export;       module procedure export_wrap_VF;  end interface
       interface import;       module procedure import_wrap_VF;  end interface

       type VF
         type(SF) :: x
         type(SF) :: y
         type(SF) :: z
       end type

       contains

       subroutine init_copy_VF(this,that)
         implicit none
         type(VF),intent(inout) :: this
         type(VF),intent(in) :: that
         call delete(this)
         call init(this%x,that%x)
         call init(this%y,that%y)
         call init(this%z,that%z)
       end subroutine

       subroutine delete_VF(this)
         implicit none
         type(VF),intent(inout) :: this
         call delete(this%x)
         call delete(this%y)
         call delete(this%z)
       end subroutine

       subroutine display_VF(this,un)
         implicit none
         type(VF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
       end subroutine

       subroutine display_short_VF(this,un)
         implicit none
         type(VF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
       end subroutine

       subroutine print_VF(this)
         implicit none
         type(VF),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_VF(this)
         implicit none
         type(VF),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_VF(this,un)
         implicit none
         type(VF),intent(in) :: this
         integer,intent(in) :: un
         call export(this%x,un)
         call export(this%y,un)
         call export(this%z,un)
       end subroutine

       subroutine import_VF(this,un)
         implicit none
         type(VF),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%x,un)
         call import(this%y,un)
         call import(this%z,un)
       end subroutine

       subroutine display_wrap_VF(this,dir,name)
         implicit none
         type(VF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_VF(this,dir,name)
         implicit none
         type(VF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         call export(this%x,dir,name//'_x')
         call export(this%y,dir,name//'_y')
         call export(this%z,dir,name//'_z')
       end subroutine

       subroutine import_wrap_VF(this,dir,name)
         implicit none
         type(VF),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module