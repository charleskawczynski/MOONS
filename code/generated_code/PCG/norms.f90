       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module norms_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: norms
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_norms;    end interface
       interface delete;       module procedure delete_norms;       end interface
       interface display;      module procedure display_norms;      end interface
       interface display_short;module procedure display_short_norms;end interface
       interface display;      module procedure display_wrap_norms; end interface
       interface print;        module procedure print_norms;        end interface
       interface print_short;  module procedure print_short_norms;  end interface
       interface export;       module procedure export_norms;       end interface
       interface import;       module procedure import_norms;       end interface
       interface export;       module procedure export_wrap_norms;  end interface
       interface import;       module procedure import_wrap_norms;  end interface

       type norms
         real(cp) :: L1 = 0.0_cp
         real(cp) :: L2 = 0.0_cp
         real(cp) :: Linf = 0.0_cp
       end type

       contains

       subroutine init_copy_norms(this,that)
         implicit none
         type(norms),intent(inout) :: this
         type(norms),intent(in) :: that
         call delete(this)
         this%L1 = that%L1
         this%L2 = that%L2
         this%Linf = that%Linf
       end subroutine

       subroutine delete_norms(this)
         implicit none
         type(norms),intent(inout) :: this
         this%L1 = 0.0_cp
         this%L2 = 0.0_cp
         this%Linf = 0.0_cp
       end subroutine

       subroutine display_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'L1   = ',this%L1
         write(un,*) 'L2   = ',this%L2
         write(un,*) 'Linf = ',this%Linf
       end subroutine

       subroutine print_norms(this)
         implicit none
         type(norms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_norms(this)
         implicit none
         type(norms),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'L1    = ';write(un,*) this%L1
         write(un,*) 'L2    = ';write(un,*) this%L2
         write(un,*) 'Linf  = ';write(un,*) this%Linf
       end subroutine

       subroutine import_norms(this,un)
         implicit none
         type(norms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%L1
         read(un,*); read(un,*) this%L2
         read(un,*); read(un,*) this%Linf
       end subroutine

       subroutine display_wrap_norms(this,dir,name)
         implicit none
         type(norms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_norms(this,dir,name)
         implicit none
         type(norms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_norms(this,dir,name)
         implicit none
         type(norms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module