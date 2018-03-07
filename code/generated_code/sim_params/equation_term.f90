       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module equation_term_mod
       use current_precision_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: equation_term
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_equation_term;              end interface
       interface delete;                 module procedure delete_equation_term;                 end interface
       interface display;                module procedure display_equation_term;                end interface
       interface display_short;          module procedure display_short_equation_term;          end interface
       interface display;                module procedure display_wrap_equation_term;           end interface
       interface print;                  module procedure print_equation_term;                  end interface
       interface print_short;            module procedure print_short_equation_term;            end interface
       interface export;                 module procedure export_equation_term;                 end interface
       interface export_primitives;      module procedure export_primitives_equation_term;      end interface
       interface import;                 module procedure import_equation_term;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_equation_term;end interface
       interface export_structured;      module procedure export_structured_D_equation_term;    end interface
       interface import_structured;      module procedure import_structured_D_equation_term;    end interface
       interface import_primitives;      module procedure import_primitives_equation_term;      end interface
       interface export;                 module procedure export_wrap_equation_term;            end interface
       interface import;                 module procedure import_wrap_equation_term;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_equation_term;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_equation_term;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_equation_term;      end interface

       type equation_term
         logical :: add = .false.
         real(cp) :: scale = 0.0_cp
       end type

       contains

       subroutine init_copy_equation_term(this,that)
         implicit none
         type(equation_term),intent(inout) :: this
         type(equation_term),intent(in) :: that
         call delete(this)
         this%add = that%add
         this%scale = that%scale
       end subroutine

       subroutine delete_equation_term(this)
         implicit none
         type(equation_term),intent(inout) :: this
         this%add = .false.
         this%scale = 0.0_cp
       end subroutine

       subroutine display_equation_term(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'add   = ',this%add
         write(un,*) 'scale = ',this%scale
       end subroutine

       subroutine display_short_equation_term(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'add   = ',this%add
         write(un,*) 'scale = ',this%scale
       end subroutine

       subroutine display_wrap_equation_term(this,dir,name)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_equation_term(this)
         implicit none
         type(equation_term),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_equation_term(this)
         implicit none
         type(equation_term),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_equation_term(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_equation_term(this,un)
         implicit none
         type(equation_term),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_equation_term(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'add    = ';write(un,*) this%add
         write(un,*) 'scale  = ';write(un,*) this%scale
       end subroutine

       subroutine import_primitives_equation_term(this,un)
         implicit none
         type(equation_term),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%add
         read(un,*); read(un,*) this%scale
       end subroutine

       subroutine export_wrap_equation_term(this,dir,name)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_equation_term(this,dir,name)
         implicit none
         type(equation_term),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_equation_term(this,dir)
         implicit none
         type(equation_term),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_equation_term(this,dir)
         implicit none
         type(equation_term),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_folder_structure_equation_term(this,dir)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
       end subroutine

       subroutine export_structured_D_equation_term(this,dir)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_equation_term(this,dir)
         implicit none
         type(equation_term),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_equation_term(this)
         implicit none
         type(equation_term),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module