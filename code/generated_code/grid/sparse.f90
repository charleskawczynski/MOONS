       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sparse_mod
       use IO_tools_mod
       use array_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: sparse
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_sparse;          end interface
       interface delete;           module procedure delete_sparse;             end interface
       interface display;          module procedure display_sparse;            end interface
       interface display_short;    module procedure display_short_sparse;      end interface
       interface display;          module procedure display_wrap_sparse;       end interface
       interface print;            module procedure print_sparse;              end interface
       interface print_short;      module procedure print_short_sparse;        end interface
       interface export;           module procedure export_sparse;             end interface
       interface export_primitives;module procedure export_primitives_sparse;  end interface
       interface import;           module procedure import_sparse;             end interface
       interface export_structured;module procedure export_structured_D_sparse;end interface
       interface import_structured;module procedure import_structured_D_sparse;end interface
       interface import_primitives;module procedure import_primitives_sparse;  end interface
       interface export;           module procedure export_wrap_sparse;        end interface
       interface import;           module procedure import_wrap_sparse;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_sparse;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_sparse;        end interface
       interface suppress_warnings;module procedure suppress_warnings_sparse;  end interface

       type sparse
         type(array) :: L
         type(array) :: D
         type(array) :: U
         logical :: staggered = .false.
       end type

       contains

       subroutine init_copy_sparse(this,that)
         implicit none
         type(sparse),intent(inout) :: this
         type(sparse),intent(in) :: that
         call delete(this)
         call init(this%L,that%L)
         call init(this%D,that%D)
         call init(this%U,that%U)
         this%staggered = that%staggered
       end subroutine

       subroutine delete_sparse(this)
         implicit none
         type(sparse),intent(inout) :: this
         call delete(this%L)
         call delete(this%D)
         call delete(this%U)
         this%staggered = .false.
       end subroutine

       subroutine display_sparse(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call display(this%L,un)
         call display(this%D,un)
         call display(this%U,un)
         write(un,*) 'staggered = ',this%staggered
       end subroutine

       subroutine display_short_sparse(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call display(this%L,un)
         call display(this%D,un)
         call display(this%U,un)
         write(un,*) 'staggered = ',this%staggered
       end subroutine

       subroutine display_wrap_sparse(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_sparse(this)
         implicit none
         type(sparse),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_sparse(this)
         implicit none
         type(sparse),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_sparse(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%L,un)
         call export(this%D,un)
         call export(this%U,un)
       end subroutine

       subroutine import_sparse(this,un)
         implicit none
         type(sparse),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%L,un)
         call import(this%D,un)
         call import(this%U,un)
       end subroutine

       subroutine export_primitives_sparse(this,un)
         implicit none
         type(sparse),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'staggered  = ';write(un,*) this%staggered
       end subroutine

       subroutine import_primitives_sparse(this,un)
         implicit none
         type(sparse),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%staggered
       end subroutine

       subroutine export_wrap_sparse(this,dir,name)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_sparse(this,dir,name)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_sparse(this,dir)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%L,dir//'L'//fortran_PS)
         call set_IO_dir(this%D,dir//'D'//fortran_PS)
         call set_IO_dir(this%U,dir//'U'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_sparse(this,dir)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%L,dir//'L'//fortran_PS)
         call make_IO_dir(this%D,dir//'D'//fortran_PS)
         call make_IO_dir(this%U,dir//'U'//fortran_PS)
       end subroutine

       subroutine export_structured_D_sparse(this,dir)
         implicit none
         type(sparse),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%L,dir//'L'//fortran_PS)
         call export_structured(this%D,dir//'D'//fortran_PS)
         call export_structured(this%U,dir//'U'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_sparse(this,dir)
         implicit none
         type(sparse),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%L,dir//'L'//fortran_PS)
         call import_structured(this%D,dir//'D'//fortran_PS)
         call import_structured(this%U,dir//'U'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_sparse(this)
         implicit none
         type(sparse),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module