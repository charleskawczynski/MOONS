       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module segment_mod
       use current_precision_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: segment
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_segment;              end interface
       interface delete;                 module procedure delete_segment;                 end interface
       interface display;                module procedure display_segment;                end interface
       interface display_short;          module procedure display_short_segment;          end interface
       interface display;                module procedure display_wrap_segment;           end interface
       interface print;                  module procedure print_segment;                  end interface
       interface print_short;            module procedure print_short_segment;            end interface
       interface export;                 module procedure export_segment;                 end interface
       interface export_primitives;      module procedure export_primitives_segment;      end interface
       interface import;                 module procedure import_segment;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_segment;end interface
       interface export_structured;      module procedure export_structured_D_segment;    end interface
       interface import_structured;      module procedure import_structured_D_segment;    end interface
       interface import_primitives;      module procedure import_primitives_segment;      end interface
       interface export;                 module procedure export_wrap_segment;            end interface
       interface import;                 module procedure import_wrap_segment;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_segment;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_segment;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_segment;      end interface

       type segment
         integer :: N_cells = 0
         type(string) :: distribution
         real(cp) :: hmax = 0.0_cp
         real(cp) :: hmin = 0.0_cp
         real(cp) :: buffer = 0.0_cp
         real(cp) :: L = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: yc = 0.0_cp
         integer :: dir = 0
       end type

       contains

       subroutine init_copy_segment(this,that)
         implicit none
         type(segment),intent(inout) :: this
         type(segment),intent(in) :: that
         call delete(this)
         this%N_cells = that%N_cells
         call init(this%distribution,that%distribution)
         this%hmax = that%hmax
         this%hmin = that%hmin
         this%buffer = that%buffer
         this%L = that%L
         this%tau = that%tau
         this%yc = that%yc
         this%dir = that%dir
       end subroutine

       subroutine delete_segment(this)
         implicit none
         type(segment),intent(inout) :: this
         this%N_cells = 0
         call delete(this%distribution)
         this%hmax = 0.0_cp
         this%hmin = 0.0_cp
         this%buffer = 0.0_cp
         this%L = 0.0_cp
         this%tau = 0.0_cp
         this%yc = 0.0_cp
         this%dir = 0
       end subroutine

       subroutine display_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells      = ',this%N_cells
         call display(this%distribution,un)
         write(un,*) 'hmax         = ',this%hmax
         write(un,*) 'hmin         = ',this%hmin
         write(un,*) 'buffer       = ',this%buffer
         write(un,*) 'L            = ',this%L
         write(un,*) 'tau          = ',this%tau
         write(un,*) 'yc           = ',this%yc
         write(un,*) 'dir          = ',this%dir
       end subroutine

       subroutine display_short_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells      = ',this%N_cells
         call display(this%distribution,un)
         write(un,*) 'hmax         = ',this%hmax
         write(un,*) 'hmin         = ',this%hmin
         write(un,*) 'buffer       = ',this%buffer
         write(un,*) 'L            = ',this%L
         write(un,*) 'tau          = ',this%tau
         write(un,*) 'yc           = ',this%yc
         write(un,*) 'dir          = ',this%dir
       end subroutine

       subroutine display_wrap_segment(this,dir,name)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_segment(this)
         implicit none
         type(segment),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_segment(this)
         implicit none
         type(segment),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%distribution,un)
       end subroutine

       subroutine import_segment(this,un)
         implicit none
         type(segment),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%distribution,un)
       end subroutine

       subroutine export_primitives_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_cells       = ';write(un,*) this%N_cells
         write(un,*) 'hmax          = ';write(un,*) this%hmax
         write(un,*) 'hmin          = ';write(un,*) this%hmin
         write(un,*) 'buffer        = ';write(un,*) this%buffer
         write(un,*) 'L             = ';write(un,*) this%L
         write(un,*) 'tau           = ';write(un,*) this%tau
         write(un,*) 'yc            = ';write(un,*) this%yc
         write(un,*) 'dir           = ';write(un,*) this%dir
       end subroutine

       subroutine import_primitives_segment(this,un)
         implicit none
         type(segment),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%N_cells
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%buffer
         read(un,*); read(un,*) this%L
         read(un,*); read(un,*) this%tau
         read(un,*); read(un,*) this%yc
         read(un,*); read(un,*) this%dir
       end subroutine

       subroutine export_wrap_segment(this,dir,name)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_segment(this,dir,name)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_segment(this,dir)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
         call set_IO_dir(this%distribution,dir//'distribution'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_segment(this,dir)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%distribution,dir//'distribution'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_segment(this,dir)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         call export_structured(this%distribution,&
         dir//'distribution'//fortran_PS)
       end subroutine

       subroutine export_structured_D_segment(this,dir)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%distribution,&
         dir//'distribution'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_segment(this,dir)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%distribution,&
         dir//'distribution'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_segment(this)
         implicit none
         type(segment),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module