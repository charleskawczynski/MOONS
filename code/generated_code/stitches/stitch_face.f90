       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module stitch_face_mod
       use IO_tools_mod
       implicit none

       private
       public :: stitch_face
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_st;    end interface
       interface delete;       module procedure delete_st;       end interface
       interface display;      module procedure display_st;      end interface
       interface display_short;module procedure display_short_st;end interface
       interface display;      module procedure display_wrap_st; end interface
       interface print;        module procedure print_st;        end interface
       interface print_short;  module procedure print_short_st;  end interface
       interface export;       module procedure export_st;       end interface
       interface import;       module procedure import_st;       end interface
       interface export;       module procedure export_wrap_st;  end interface
       interface import;       module procedure import_wrap_st;  end interface

       type stitch_face
         logical,dimension(3) :: hmin = .false.
         logical,dimension(3) :: hmax = .false.
         integer,dimension(3) :: hmin_id = 0
         integer,dimension(3) :: hmax_id = 0
       end type

       contains

       subroutine init_copy_st(this,that)
         implicit none
         type(stitch_face),intent(inout) :: this
         type(stitch_face),intent(in) :: that
         call delete(this)
         this%hmin = that%hmin
         this%hmax = that%hmax
         this%hmin_id = that%hmin_id
         this%hmax_id = that%hmax_id
       end subroutine

       subroutine delete_st(this)
         implicit none
         type(stitch_face),intent(inout) :: this
         this%hmin = .false.
         this%hmax = .false.
         this%hmin_id = 0
         this%hmax_id = 0
       end subroutine

       subroutine display_st(this,un)
         implicit none
         type(stitch_face),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- stitch_face'
         write(un,*) 'hmin    = ',this%hmin
         write(un,*) 'hmax    = ',this%hmax
         write(un,*) 'hmin_id = ',this%hmin_id
         write(un,*) 'hmax_id = ',this%hmax_id
       end subroutine

       subroutine display_short_st(this,un)
         implicit none
         type(stitch_face),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'hmin    = ',this%hmin
         write(un,*) 'hmax    = ',this%hmax
         write(un,*) 'hmin_id = ',this%hmin_id
         write(un,*) 'hmax_id = ',this%hmax_id
       end subroutine

       subroutine print_st(this)
         implicit none
         type(stitch_face),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_st(this)
         implicit none
         type(stitch_face),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_st(this,un)
         implicit none
         type(stitch_face),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'hmin     = ';write(un,*) this%hmin
         write(un,*) 'hmax     = ';write(un,*) this%hmax
         write(un,*) 'hmin_id  = ';write(un,*) this%hmin_id
         write(un,*) 'hmax_id  = ';write(un,*) this%hmax_id
       end subroutine

       subroutine import_st(this,un)
         implicit none
         type(stitch_face),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin_id
         read(un,*); read(un,*) this%hmax_id
       end subroutine

       subroutine display_wrap_st(this,dir,name)
         implicit none
         type(stitch_face),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_st(this,dir,name)
         implicit none
         type(stitch_face),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_st(this,dir,name)
         implicit none
         type(stitch_face),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module