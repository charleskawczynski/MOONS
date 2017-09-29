       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module refine_mesh_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use step_mod
       use string_mod
       implicit none

       private
       public :: refine_mesh
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_refine_mesh;           end interface
       interface delete;           module procedure delete_refine_mesh;              end interface
       interface display;          module procedure display_refine_mesh;             end interface
       interface display_short;    module procedure display_short_refine_mesh;       end interface
       interface display;          module procedure display_wrap_refine_mesh;        end interface
       interface print;            module procedure print_refine_mesh;               end interface
       interface print_short;      module procedure print_short_refine_mesh;         end interface
       interface export;           module procedure export_refine_mesh;              end interface
       interface export_primitives;module procedure export_primitives_refine_mesh;   end interface
       interface import;           module procedure import_refine_mesh;              end interface
       interface export_structured;module procedure export_structured_D_refine_mesh; end interface
       interface import_structured;module procedure import_structured_D_refine_mesh; end interface
       interface import_primitives;module procedure import_primitives_refine_mesh;   end interface
       interface export;           module procedure export_wrap_refine_mesh;         end interface
       interface import;           module procedure import_wrap_refine_mesh;         end interface
       interface set_IO_dir;       module procedure set_IO_dir_refine_mesh;          end interface
       interface make_IO_dir;      module procedure make_IO_dir_refine_mesh;         end interface
       interface suppress_warnings;module procedure suppress_warnings_refine_mesh;   end interface
       interface export;           module procedure export_DN_refine_mesh;           end interface
       interface import;           module procedure import_DN_refine_mesh;           end interface
       interface export_structured;module procedure export_structured_DN_refine_mesh;end interface
       interface import_structured;module procedure import_structured_DN_refine_mesh;end interface

       type refine_mesh
         type(step) :: all
         type(step) :: x
         type(step) :: y
         type(step) :: z
         type(step) :: x_plane
         type(step) :: y_plane
         type(step) :: z_plane
         logical :: any_next = .false.
         integer :: un = 0
         integer :: i_level = 0
         integer :: i_level_last = 0
         type(string) :: dir
         type(string) :: name
         type(string) :: level
         type(string) :: level_last
       end type

       contains

       subroutine init_copy_refine_mesh(this,that)
         implicit none
         type(refine_mesh),intent(inout) :: this
         type(refine_mesh),intent(in) :: that
         call delete(this)
         call init(this%all,that%all)
         call init(this%x,that%x)
         call init(this%y,that%y)
         call init(this%z,that%z)
         call init(this%x_plane,that%x_plane)
         call init(this%y_plane,that%y_plane)
         call init(this%z_plane,that%z_plane)
         this%any_next = that%any_next
         this%un = that%un
         this%i_level = that%i_level
         this%i_level_last = that%i_level_last
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         call init(this%level,that%level)
         call init(this%level_last,that%level_last)
       end subroutine

       subroutine delete_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(inout) :: this
         call delete(this%all)
         call delete(this%x)
         call delete(this%y)
         call delete(this%z)
         call delete(this%x_plane)
         call delete(this%y_plane)
         call delete(this%z_plane)
         this%any_next = .false.
         this%un = 0
         this%i_level = 0
         this%i_level_last = 0
         call delete(this%dir)
         call delete(this%name)
         call delete(this%level)
         call delete(this%level_last)
       end subroutine

       subroutine display_refine_mesh(this,un)
         implicit none
         type(refine_mesh),intent(in) :: this
         integer,intent(in) :: un
         call display(this%all,un)
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
         call display(this%x_plane,un)
         call display(this%y_plane,un)
         call display(this%z_plane,un)
         write(un,*) 'any_next     = ',this%any_next
         write(un,*) 'un           = ',this%un
         write(un,*) 'i_level      = ',this%i_level
         write(un,*) 'i_level_last = ',this%i_level_last
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%level,un)
         call display(this%level_last,un)
       end subroutine

       subroutine display_short_refine_mesh(this,un)
         implicit none
         type(refine_mesh),intent(in) :: this
         integer,intent(in) :: un
         call display(this%all,un)
         call display(this%x,un)
         call display(this%y,un)
         call display(this%z,un)
         call display(this%x_plane,un)
         call display(this%y_plane,un)
         call display(this%z_plane,un)
         write(un,*) 'any_next     = ',this%any_next
         write(un,*) 'un           = ',this%un
         write(un,*) 'i_level      = ',this%i_level
         write(un,*) 'i_level_last = ',this%i_level_last
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%level,un)
         call display(this%level_last,un)
       end subroutine

       subroutine display_wrap_refine_mesh(this,dir,name)
         implicit none
         type(refine_mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_refine_mesh(this,un)
         implicit none
         type(refine_mesh),intent(in) :: this
         integer,intent(in) :: un
         call export(this%all,un)
         call export(this%x,un)
         call export(this%y,un)
         call export(this%z,un)
         call export(this%x_plane,un)
         call export(this%y_plane,un)
         call export(this%z_plane,un)
         write(un,*) 'any_next      = ';write(un,*) this%any_next
         write(un,*) 'un            = ';write(un,*) this%un
         write(un,*) 'i_level       = ';write(un,*) this%i_level
         write(un,*) 'i_level_last  = ';write(un,*) this%i_level_last
         call export(this%dir,un)
         call export(this%name,un)
         call export(this%level,un)
         call export(this%level_last,un)
       end subroutine

       subroutine import_refine_mesh(this,un)
         implicit none
         type(refine_mesh),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%all,un)
         call import(this%x,un)
         call import(this%y,un)
         call import(this%z,un)
         call import(this%x_plane,un)
         call import(this%y_plane,un)
         call import(this%z_plane,un)
         read(un,*); read(un,*) this%any_next
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%i_level
         read(un,*); read(un,*) this%i_level_last
         call import(this%dir,un)
         call import(this%name,un)
         call import(this%level,un)
         call import(this%level_last,un)
       end subroutine

       subroutine export_primitives_refine_mesh(this,un)
         implicit none
         type(refine_mesh),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'any_next      = ';write(un,*) this%any_next
         write(un,*) 'un            = ';write(un,*) this%un
         write(un,*) 'i_level       = ';write(un,*) this%i_level
         write(un,*) 'i_level_last  = ';write(un,*) this%i_level_last
       end subroutine

       subroutine import_primitives_refine_mesh(this,un)
         implicit none
         type(refine_mesh),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%any_next
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%i_level
         read(un,*); read(un,*) this%i_level_last
       end subroutine

       subroutine export_wrap_refine_mesh(this,dir,name)
         implicit none
         type(refine_mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_refine_mesh(this,dir,name)
         implicit none
         type(refine_mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       subroutine export_structured_DN_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%all,str(this%dir)//'all'//fortran_PS)
         call export_structured(this%x,str(this%dir)//'x'//fortran_PS)
         call export_structured(this%y,str(this%dir)//'y'//fortran_PS)
         call export_structured(this%z,str(this%dir)//'z'//fortran_PS)
         call export_structured(this%x_plane,&
         str(this%dir)//'x_plane'//fortran_PS)
         call export_structured(this%y_plane,&
         str(this%dir)//'y_plane'//fortran_PS)
         call export_structured(this%z_plane,&
         str(this%dir)//'z_plane'//fortran_PS)
       end subroutine

       subroutine import_structured_DN_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%all,str(this%dir)//'all'//fortran_PS)
         call import_structured(this%x,str(this%dir)//'x'//fortran_PS)
         call import_structured(this%y,str(this%dir)//'y'//fortran_PS)
         call import_structured(this%z,str(this%dir)//'z'//fortran_PS)
         call import_structured(this%x_plane,&
         str(this%dir)//'x_plane'//fortran_PS)
         call import_structured(this%y_plane,&
         str(this%dir)//'y_plane'//fortran_PS)
         call import_structured(this%z_plane,&
         str(this%dir)//'z_plane'//fortran_PS)
       end subroutine

       subroutine set_IO_dir_refine_mesh(this,dir)
         implicit none
         type(refine_mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call set_IO_dir(this%all,dir//'all'//fortran_PS)
         call set_IO_dir(this%x,dir//'x'//fortran_PS)
         call set_IO_dir(this%y,dir//'y'//fortran_PS)
         call set_IO_dir(this%z,dir//'z'//fortran_PS)
         call set_IO_dir(this%x_plane,dir//'x_plane'//fortran_PS)
         call set_IO_dir(this%y_plane,dir//'y_plane'//fortran_PS)
         call set_IO_dir(this%z_plane,dir//'z_plane'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_refine_mesh(this,dir)
         implicit none
         type(refine_mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call make_IO_dir(this%all,dir//'all'//fortran_PS)
         call make_IO_dir(this%x,dir//'x'//fortran_PS)
         call make_IO_dir(this%y,dir//'y'//fortran_PS)
         call make_IO_dir(this%z,dir//'z'//fortran_PS)
         call make_IO_dir(this%x_plane,dir//'x_plane'//fortran_PS)
         call make_IO_dir(this%y_plane,dir//'y_plane'//fortran_PS)
         call make_IO_dir(this%z_plane,dir//'z_plane'//fortran_PS)
       end subroutine

       subroutine export_structured_D_refine_mesh(this,dir)
         implicit none
         type(refine_mesh),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%all,dir//'all'//fortran_PS)
         call export_structured(this%x,dir//'x'//fortran_PS)
         call export_structured(this%y,dir//'y'//fortran_PS)
         call export_structured(this%z,dir//'z'//fortran_PS)
         call export_structured(this%x_plane,dir//'x_plane'//fortran_PS)
         call export_structured(this%y_plane,dir//'y_plane'//fortran_PS)
         call export_structured(this%z_plane,dir//'z_plane'//fortran_PS)
       end subroutine

       subroutine import_structured_D_refine_mesh(this,dir)
         implicit none
         type(refine_mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%all,dir//'all'//fortran_PS)
         call import_structured(this%x,dir//'x'//fortran_PS)
         call import_structured(this%y,dir//'y'//fortran_PS)
         call import_structured(this%z,dir//'z'//fortran_PS)
         call import_structured(this%x_plane,dir//'x_plane'//fortran_PS)
         call import_structured(this%y_plane,dir//'y_plane'//fortran_PS)
         call import_structured(this%z_plane,dir//'z_plane'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_refine_mesh(this)
         implicit none
         type(refine_mesh),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module