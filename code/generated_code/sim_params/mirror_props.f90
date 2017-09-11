       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mirror_props_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: mirror_props
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_mirror_props;    end interface
       interface delete;       module procedure delete_mirror_props;       end interface
       interface display;      module procedure display_mirror_props;      end interface
       interface display_short;module procedure display_short_mirror_props;end interface
       interface display;      module procedure display_wrap_mirror_props; end interface
       interface print;        module procedure print_mirror_props;        end interface
       interface print_short;  module procedure print_short_mirror_props;  end interface
       interface export;       module procedure export_mirror_props;       end interface
       interface import;       module procedure import_mirror_props;       end interface
       interface export;       module procedure export_wrap_mirror_props;  end interface
       interface import;       module procedure import_wrap_mirror_props;  end interface

       type mirror_props
         logical :: mirror = .false.
         integer :: mirror_face = 0
         real(cp),dimension(3) :: mirror_sign = 0.0_cp
         real(cp),dimension(3) :: mirror_sign_a = 0.0_cp
       end type

       contains

       subroutine init_copy_mirror_props(this,that)
         implicit none
         type(mirror_props),intent(inout) :: this
         type(mirror_props),intent(in) :: that
         call delete(this)
         this%mirror = that%mirror
         this%mirror_face = that%mirror_face
         this%mirror_sign = that%mirror_sign
         this%mirror_sign_a = that%mirror_sign_a
       end subroutine

       subroutine delete_mirror_props(this)
         implicit none
         type(mirror_props),intent(inout) :: this
         this%mirror = .false.
         this%mirror_face = 0
         this%mirror_sign = 0.0_cp
         this%mirror_sign_a = 0.0_cp
       end subroutine

       subroutine display_mirror_props(this,un)
         implicit none
         type(mirror_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'mirror        = ',this%mirror
         write(un,*) 'mirror_face   = ',this%mirror_face
         write(un,*) 'mirror_sign   = ',this%mirror_sign
         write(un,*) 'mirror_sign_a = ',this%mirror_sign_a
       end subroutine

       subroutine display_short_mirror_props(this,un)
         implicit none
         type(mirror_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'mirror        = ',this%mirror
         write(un,*) 'mirror_face   = ',this%mirror_face
         write(un,*) 'mirror_sign   = ',this%mirror_sign
         write(un,*) 'mirror_sign_a = ',this%mirror_sign_a
       end subroutine

       subroutine print_mirror_props(this)
         implicit none
         type(mirror_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mirror_props(this)
         implicit none
         type(mirror_props),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_mirror_props(this,un)
         implicit none
         type(mirror_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'mirror         = ';write(un,*) this%mirror
         write(un,*) 'mirror_face    = ';write(un,*) this%mirror_face
         write(un,*) 'mirror_sign    = ';write(un,*) this%mirror_sign
         write(un,*) 'mirror_sign_a  = ';write(un,*) this%mirror_sign_a
       end subroutine

       subroutine import_mirror_props(this,un)
         implicit none
         type(mirror_props),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%mirror
         read(un,*); read(un,*) this%mirror_face
         read(un,*); read(un,*) this%mirror_sign
         read(un,*); read(un,*) this%mirror_sign_a
       end subroutine

       subroutine display_wrap_mirror_props(this,dir,name)
         implicit none
         type(mirror_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_mirror_props(this,dir,name)
         implicit none
         type(mirror_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mirror_props(this,dir,name)
         implicit none
         type(mirror_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module