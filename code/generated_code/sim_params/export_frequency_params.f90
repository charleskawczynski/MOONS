       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_frequency_params_mod
       use current_precision_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: export_frequency_params
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_export_frequency_params;              end interface
       interface delete;                 module procedure delete_export_frequency_params;                 end interface
       interface display;                module procedure display_export_frequency_params;                end interface
       interface display_short;          module procedure display_short_export_frequency_params;          end interface
       interface display;                module procedure display_wrap_export_frequency_params;           end interface
       interface print;                  module procedure print_export_frequency_params;                  end interface
       interface print_short;            module procedure print_short_export_frequency_params;            end interface
       interface export;                 module procedure export_export_frequency_params;                 end interface
       interface export_primitives;      module procedure export_primitives_export_frequency_params;      end interface
       interface import;                 module procedure import_export_frequency_params;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_export_frequency_params;end interface
       interface export_structured;      module procedure export_structured_D_export_frequency_params;    end interface
       interface import_structured;      module procedure import_structured_D_export_frequency_params;    end interface
       interface import_primitives;      module procedure import_primitives_export_frequency_params;      end interface
       interface export;                 module procedure export_wrap_export_frequency_params;            end interface
       interface import;                 module procedure import_wrap_export_frequency_params;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_export_frequency_params;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_export_frequency_params;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_export_frequency_params;      end interface

       type export_frequency_params
         logical :: export_ever = .false.
         logical :: export_first_step = .false.
         logical :: export_now = .false.
         real(cp) :: t_window_start = 0.0_cp
         real(cp) :: t_window_stop = 0.0_cp
         integer :: N_points_in_window = 0
         integer :: left_point_export = 0
         integer :: right_point_export = 0
         real(cp) :: dt_star = 0.0_cp
         real(cp) :: dt_star_range = 0.0_cp
         real(cp) :: t_star_left = 0.0_cp
         real(cp) :: t_star_right = 0.0_cp
         real(cp) :: dt_window_factor = 0.0_cp
       end type

       contains

       subroutine init_copy_export_frequency_params(this,that)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         type(export_frequency_params),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
         this%export_first_step = that%export_first_step
         this%export_now = that%export_now
         this%t_window_start = that%t_window_start
         this%t_window_stop = that%t_window_stop
         this%N_points_in_window = that%N_points_in_window
         this%left_point_export = that%left_point_export
         this%right_point_export = that%right_point_export
         this%dt_star = that%dt_star
         this%dt_star_range = that%dt_star_range
         this%t_star_left = that%t_star_left
         this%t_star_right = that%t_star_right
         this%dt_window_factor = that%dt_window_factor
       end subroutine

       subroutine delete_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         this%export_ever = .false.
         this%export_first_step = .false.
         this%export_now = .false.
         this%t_window_start = 0.0_cp
         this%t_window_stop = 0.0_cp
         this%N_points_in_window = 0
         this%left_point_export = 0
         this%right_point_export = 0
         this%dt_star = 0.0_cp
         this%dt_star_range = 0.0_cp
         this%t_star_left = 0.0_cp
         this%t_star_right = 0.0_cp
         this%dt_window_factor = 0.0_cp
       end subroutine

       subroutine display_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever        = ',this%export_ever
         write(un,*) 'export_first_step  = ',this%export_first_step
         write(un,*) 'export_now         = ',this%export_now
         write(un,*) 't_window_start     = ',this%t_window_start
         write(un,*) 't_window_stop      = ',this%t_window_stop
         write(un,*) 'N_points_in_window = ',this%N_points_in_window
         write(un,*) 'left_point_export  = ',this%left_point_export
         write(un,*) 'right_point_export = ',this%right_point_export
         write(un,*) 'dt_star            = ',this%dt_star
         write(un,*) 'dt_star_range      = ',this%dt_star_range
         write(un,*) 't_star_left        = ',this%t_star_left
         write(un,*) 't_star_right       = ',this%t_star_right
         write(un,*) 'dt_window_factor   = ',this%dt_window_factor
       end subroutine

       subroutine display_short_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever        = ',this%export_ever
         write(un,*) 'export_first_step  = ',this%export_first_step
         write(un,*) 'export_now         = ',this%export_now
         write(un,*) 't_window_start     = ',this%t_window_start
         write(un,*) 't_window_stop      = ',this%t_window_stop
         write(un,*) 'N_points_in_window = ',this%N_points_in_window
         write(un,*) 'left_point_export  = ',this%left_point_export
         write(un,*) 'right_point_export = ',this%right_point_export
         write(un,*) 'dt_star            = ',this%dt_star
         write(un,*) 'dt_star_range      = ',this%dt_star_range
         write(un,*) 't_star_left        = ',this%t_star_left
         write(un,*) 't_star_right       = ',this%t_star_right
         write(un,*) 'dt_window_factor   = ',this%dt_window_factor
       end subroutine

       subroutine display_wrap_export_frequency_params(this,dir,name)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever         = ';write(un,*) this%export_ever
         write(un,*) 'export_first_step   = ';write(un,*) this%export_first_step
         write(un,*) 'export_now          = ';write(un,*) this%export_now
         write(un,*) 't_window_start      = ';write(un,*) this%t_window_start
         write(un,*) 't_window_stop       = ';write(un,*) this%t_window_stop
         write(un,*) 'N_points_in_window  = ';write(un,*) this%N_points_in_window
         write(un,*) 'left_point_export   = ';write(un,*) this%left_point_export
         write(un,*) 'right_point_export  = ';write(un,*) this%right_point_export
         write(un,*) 'dt_star             = ';write(un,*) this%dt_star
         write(un,*) 'dt_star_range       = ';write(un,*) this%dt_star_range
         write(un,*) 't_star_left         = ';write(un,*) this%t_star_left
         write(un,*) 't_star_right        = ';write(un,*) this%t_star_right
         write(un,*) 'dt_window_factor    = ';write(un,*) this%dt_window_factor
       end subroutine

       subroutine import_primitives_export_frequency_params(this,un)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%export_first_step
         read(un,*); read(un,*) this%export_now
         read(un,*); read(un,*) this%t_window_start
         read(un,*); read(un,*) this%t_window_stop
         read(un,*); read(un,*) this%N_points_in_window
         read(un,*); read(un,*) this%left_point_export
         read(un,*); read(un,*) this%right_point_export
         read(un,*); read(un,*) this%dt_star
         read(un,*); read(un,*) this%dt_star_range
         read(un,*); read(un,*) this%t_star_left
         read(un,*); read(un,*) this%t_star_right
         read(un,*); read(un,*) this%dt_window_factor
       end subroutine

       subroutine export_wrap_export_frequency_params(this,dir,name)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_frequency_params(this,dir,name)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_folder_structure_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
       end subroutine

       subroutine export_structured_D_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_export_frequency_params(this,dir)
         implicit none
         type(export_frequency_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_export_frequency_params(this)
         implicit none
         type(export_frequency_params),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module