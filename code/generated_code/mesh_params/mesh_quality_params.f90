       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_quality_params_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: mesh_quality_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_mesh_quality_params;      end interface
       interface delete;       module procedure delete_mesh_quality_params;         end interface
       interface display;      module procedure display_mesh_quality_params;        end interface
       interface display_short;module procedure display_short_mesh_quality_params;  end interface
       interface display;      module procedure display_wrapper_mesh_quality_params;end interface
       interface print;        module procedure print_mesh_quality_params;          end interface
       interface print_short;  module procedure print_short_mesh_quality_params;    end interface
       interface export;       module procedure export_mesh_quality_params;         end interface
       interface import;       module procedure import_mesh_quality_params;         end interface
       interface export;       module procedure export_wrapper_mesh_quality_params; end interface
       interface import;       module procedure import_wrapper_mesh_quality_params; end interface

       type mesh_quality_params
         real(cp) :: max_mesh_stretch_ratio = 0.0_cp
         integer :: n_max_points_add = 0
         integer :: n_iter = 0
         logical :: auto_find_n = .false.
       end type

       contains

       subroutine init_copy_mesh_quality_params(this,that)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         type(mesh_quality_params),intent(in) :: that
         call delete(this)
         this%max_mesh_stretch_ratio = that%max_mesh_stretch_ratio
         this%n_max_points_add = that%n_max_points_add
         this%n_iter = that%n_iter
         this%auto_find_n = that%auto_find_n
       end subroutine

       subroutine delete_mesh_quality_params(this)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         this%max_mesh_stretch_ratio = 0.0_cp
         this%n_max_points_add = 0
         this%n_iter = 0
         this%auto_find_n = .false.
       end subroutine

       subroutine display_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- mesh_quality_params'
         write(un,*) 'max_mesh_stretch_ratio = ',this%max_mesh_stretch_ratio
         write(un,*) 'n_max_points_add       = ',this%n_max_points_add
         write(un,*) 'n_iter                 = ',this%n_iter
         write(un,*) 'auto_find_n            = ',this%auto_find_n
       end subroutine

       subroutine display_short_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio = ',this%max_mesh_stretch_ratio
         write(un,*) 'n_max_points_add       = ',this%n_max_points_add
         write(un,*) 'n_iter                 = ',this%n_iter
         write(un,*) 'auto_find_n            = ',this%auto_find_n
       end subroutine

       subroutine print_mesh_quality_params(this)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mesh_quality_params(this)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio  = ';write(un,*) this%max_mesh_stretch_ratio
         write(un,*) 'n_max_points_add        = ';write(un,*) this%n_max_points_add
         write(un,*) 'n_iter                  = ';write(un,*) this%n_iter
         write(un,*) 'auto_find_n             = ';write(un,*) this%auto_find_n
       end subroutine

       subroutine import_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%max_mesh_stretch_ratio
         read(un,*); read(un,*) this%n_max_points_add
         read(un,*); read(un,*) this%n_iter
         read(un,*); read(un,*) this%auto_find_n
       end subroutine

       subroutine display_wrapper_mesh_quality_params(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_mesh_quality_params(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_mesh_quality_params(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module