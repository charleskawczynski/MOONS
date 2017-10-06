       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module single_procedure_plane_op_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use plane_op_mod
       use string_mod
       implicit none

       private
       public :: single_procedure_plane_op
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_single_procedure_plane_op;          end interface
       interface delete;           module procedure delete_single_procedure_plane_op;             end interface
       interface display;          module procedure display_single_procedure_plane_op;            end interface
       interface display_short;    module procedure display_short_single_procedure_plane_op;      end interface
       interface display;          module procedure display_wrap_single_procedure_plane_op;       end interface
       interface print;            module procedure print_single_procedure_plane_op;              end interface
       interface print_short;      module procedure print_short_single_procedure_plane_op;        end interface
       interface export;           module procedure export_single_procedure_plane_op;             end interface
       interface export_primitives;module procedure export_primitives_single_procedure_plane_op;  end interface
       interface import;           module procedure import_single_procedure_plane_op;             end interface
       interface export_structured;module procedure export_structured_D_single_procedure_plane_op;end interface
       interface import_structured;module procedure import_structured_D_single_procedure_plane_op;end interface
       interface import_primitives;module procedure import_primitives_single_procedure_plane_op;  end interface
       interface export;           module procedure export_wrap_single_procedure_plane_op;        end interface
       interface import;           module procedure import_wrap_single_procedure_plane_op;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_single_procedure_plane_op;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_single_procedure_plane_op;        end interface
       interface suppress_warnings;module procedure suppress_warnings_single_procedure_plane_op;  end interface

       type single_procedure_plane_op
         procedure(plane_op),pointer,nopass :: P
         logical :: defined = .false.
         integer :: ID = 0
       end type

       contains

       subroutine init_copy_single_procedure_plane_op(this,that)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         type(single_procedure_plane_op),intent(in) :: that
         call delete(this)
         this%P => that%P
         this%defined = that%defined
         this%ID = that%ID
       end subroutine

       subroutine delete_single_procedure_plane_op(this)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         nullify(this%P)
         this%defined = .false.
         this%ID = 0
       end subroutine

       subroutine display_single_procedure_plane_op(this,un)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined = ',this%defined
         write(un,*) 'ID      = ',this%ID
       end subroutine

       subroutine display_short_single_procedure_plane_op(this,un)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined = ',this%defined
         write(un,*) 'ID      = ',this%ID
       end subroutine

       subroutine display_wrap_single_procedure_plane_op(this,dir,name)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_single_procedure_plane_op(this)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_single_procedure_plane_op(this)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_single_procedure_plane_op(this,un)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_single_procedure_plane_op(this,un)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_single_procedure_plane_op(this,un)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'defined  = ';write(un,*) this%defined
         write(un,*) 'ID       = ';write(un,*) this%ID
       end subroutine

       subroutine import_primitives_single_procedure_plane_op(this,un)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%ID
       end subroutine

       subroutine export_wrap_single_procedure_plane_op(this,dir,name)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_single_procedure_plane_op(this,dir,name)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_single_procedure_plane_op(this,dir)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_single_procedure_plane_op(this,dir)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_single_procedure_plane_op(this,dir)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_single_procedure_plane_op(this,dir)
         implicit none
         type(single_procedure_plane_op),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_single_procedure_plane_op(this)
         implicit none
         type(single_procedure_plane_op),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module