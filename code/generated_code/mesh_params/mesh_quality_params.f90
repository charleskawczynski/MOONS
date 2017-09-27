       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_quality_params_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: mesh_quality_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_mesh_quality_params;        end interface
       interface delete;           module procedure delete_mesh_quality_params;           end interface
       interface display;          module procedure display_mesh_quality_params;          end interface
       interface display_short;    module procedure display_short_mesh_quality_params;    end interface
       interface display;          module procedure display_wrap_mesh_quality_params;     end interface
       interface print;            module procedure print_mesh_quality_params;            end interface
       interface print_short;      module procedure print_short_mesh_quality_params;      end interface
       interface export;           module procedure export_mesh_quality_params;           end interface
       interface export_primitives;module procedure export_primitives_mesh_quality_params;end interface
       interface export_restart;   module procedure export_restart_mesh_quality_params;   end interface
       interface import;           module procedure import_mesh_quality_params;           end interface
       interface import_restart;   module procedure import_restart_mesh_quality_params;   end interface
       interface import_primitives;module procedure import_primitives_mesh_quality_params;end interface
       interface export;           module procedure export_wrap_mesh_quality_params;      end interface
       interface import;           module procedure import_wrap_mesh_quality_params;      end interface
       interface make_restart_dir; module procedure make_restart_dir_mesh_quality_params; end interface
       interface suppress_warnings;module procedure suppress_warnings_mesh_quality_params;end interface

       type mesh_quality_params
         real(cp) :: max_mesh_stretch_ratio = 0.0_cp
         integer :: N_max_points_add = 0
         integer :: N_iter = 0
         logical :: auto_find_N = .false.
       end type

       contains

       subroutine init_copy_mesh_quality_params(this,that)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         type(mesh_quality_params),intent(in) :: that
         call delete(this)
         this%max_mesh_stretch_ratio = that%max_mesh_stretch_ratio
         this%N_max_points_add = that%N_max_points_add
         this%N_iter = that%N_iter
         this%auto_find_N = that%auto_find_N
       end subroutine

       subroutine delete_mesh_quality_params(this)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         this%max_mesh_stretch_ratio = 0.0_cp
         this%N_max_points_add = 0
         this%N_iter = 0
         this%auto_find_N = .false.
       end subroutine

       subroutine display_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio = ',this%max_mesh_stretch_ratio
         write(un,*) 'N_max_points_add       = ',this%N_max_points_add
         write(un,*) 'N_iter                 = ',this%N_iter
         write(un,*) 'auto_find_N            = ',this%auto_find_N
       end subroutine

       subroutine display_short_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio = ',this%max_mesh_stretch_ratio
         write(un,*) 'N_max_points_add       = ',this%N_max_points_add
         write(un,*) 'N_iter                 = ',this%N_iter
         write(un,*) 'auto_find_N            = ',this%auto_find_N
       end subroutine

       subroutine display_wrap_mesh_quality_params(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
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

       subroutine export_primitives_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio  = ';write(un,*) this%max_mesh_stretch_ratio
         write(un,*) 'N_max_points_add        = ';write(un,*) this%N_max_points_add
         write(un,*) 'N_iter                  = ';write(un,*) this%N_iter
         write(un,*) 'auto_find_N             = ';write(un,*) this%auto_find_N
       end subroutine

       subroutine export_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio  = ';write(un,*) this%max_mesh_stretch_ratio
         write(un,*) 'N_max_points_add        = ';write(un,*) this%N_max_points_add
         write(un,*) 'N_iter                  = ';write(un,*) this%N_iter
         write(un,*) 'auto_find_N             = ';write(un,*) this%auto_find_N
       end subroutine

       subroutine import_primitives_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%max_mesh_stretch_ratio
         read(un,*); read(un,*) this%N_max_points_add
         read(un,*); read(un,*) this%N_iter
         read(un,*); read(un,*) this%auto_find_N
       end subroutine

       subroutine import_mesh_quality_params(this,un)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%max_mesh_stretch_ratio
         read(un,*); read(un,*) this%N_max_points_add
         read(un,*); read(un,*) this%N_iter
         read(un,*); read(un,*) this%auto_find_N
       end subroutine

       subroutine export_wrap_mesh_quality_params(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mesh_quality_params(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_mesh_quality_params(this,dir)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_restart_mesh_quality_params(this,dir)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_restart_mesh_quality_params(this,dir)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_mesh_quality_params(this)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module