       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module norms_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: norms
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_norms;          end interface
       interface delete;           module procedure delete_norms;             end interface
       interface display;          module procedure display_norms;            end interface
       interface display_short;    module procedure display_short_norms;      end interface
       interface display;          module procedure display_wrap_norms;       end interface
       interface print;            module procedure print_norms;              end interface
       interface print_short;      module procedure print_short_norms;        end interface
       interface export;           module procedure export_norms;             end interface
       interface export_primitives;module procedure export_primitives_norms;  end interface
       interface import;           module procedure import_norms;             end interface
       interface export_structured;module procedure export_structured_D_norms;end interface
       interface import_structured;module procedure import_structured_D_norms;end interface
       interface import_primitives;module procedure import_primitives_norms;  end interface
       interface export;           module procedure export_wrap_norms;        end interface
       interface import;           module procedure import_wrap_norms;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_norms;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_norms;        end interface
       interface suppress_warnings;module procedure suppress_warnings_norms;  end interface

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
         write(un,*) 'L1   = ',this%L1
         write(un,*) 'L2   = ',this%L2
         write(un,*) 'Linf = ',this%Linf
       end subroutine

       subroutine display_short_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'L1   = ',this%L1
         write(un,*) 'L2   = ',this%L2
         write(un,*) 'Linf = ',this%Linf
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
         call export_primitives(this,un)
       end subroutine

       subroutine import_norms(this,un)
         implicit none
         type(norms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'L1    = ';write(un,*) this%L1
         write(un,*) 'L2    = ';write(un,*) this%L2
         write(un,*) 'Linf  = ';write(un,*) this%Linf
       end subroutine

       subroutine import_primitives_norms(this,un)
         implicit none
         type(norms),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%L1
         read(un,*); read(un,*) this%L2
         read(un,*); read(un,*) this%Linf
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
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_norms(this,dir)
         implicit none
         type(norms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_norms(this,dir)
         implicit none
         type(norms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_norms(this,dir)
         implicit none
         type(norms),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_norms(this,dir)
         implicit none
         type(norms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_norms(this)
         implicit none
         type(norms),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module