       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_props_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use simple_int_tensor_mod
       use string_mod
       implicit none

       private
       public :: mesh_props
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_mesh_props;        end interface
       interface delete;           module procedure delete_mesh_props;           end interface
       interface display;          module procedure display_mesh_props;          end interface
       interface display_short;    module procedure display_short_mesh_props;    end interface
       interface display;          module procedure display_wrap_mesh_props;     end interface
       interface print;            module procedure print_mesh_props;            end interface
       interface print_short;      module procedure print_short_mesh_props;      end interface
       interface export;           module procedure export_mesh_props;           end interface
       interface export_primitives;module procedure export_primitives_mesh_props;end interface
       interface export_restart;   module procedure export_restart_mesh_props;   end interface
       interface import;           module procedure import_mesh_props;           end interface
       interface import_restart;   module procedure import_restart_mesh_props;   end interface
       interface import_primitives;module procedure import_primitives_mesh_props;end interface
       interface export;           module procedure export_wrap_mesh_props;      end interface
       interface import;           module procedure import_wrap_mesh_props;      end interface
       interface make_restart_dir; module procedure make_restart_dir_mesh_props; end interface
       interface suppress_warnings;module procedure suppress_warnings_mesh_props;end interface

       type mesh_props
         type(simple_int_tensor),dimension(3) :: int_tensor
         logical,dimension(3) :: plane = .false.
         integer,dimension(3) :: N_cells = 0
         logical :: plane_any = .false.
         integer :: N_cells_tot = 0
         real(cp) :: volume = 0.0_cp
         real(cp),dimension(3) :: hmax = 0.0_cp
         real(cp),dimension(3) :: hmin = 0.0_cp
         real(cp),dimension(3) :: dhmax = 0.0_cp
         real(cp),dimension(3) :: dhmin = 0.0_cp
         real(cp) :: dhmax_max = 0.0_cp
         real(cp) :: dhmin_min = 0.0_cp
       end type

       contains

       subroutine init_copy_mesh_props(this,that)
         implicit none
         type(mesh_props),intent(inout) :: this
         type(mesh_props),intent(in) :: that
         integer :: i_int_tensor
         integer :: s_int_tensor
         call delete(this)
         s_int_tensor = size(that%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call init(this%int_tensor(i_int_tensor),&
           that%int_tensor(i_int_tensor))
         enddo
         this%plane = that%plane
         this%N_cells = that%N_cells
         this%plane_any = that%plane_any
         this%N_cells_tot = that%N_cells_tot
         this%volume = that%volume
         this%hmax = that%hmax
         this%hmin = that%hmin
         this%dhmax = that%dhmax
         this%dhmin = that%dhmin
         this%dhmax_max = that%dhmax_max
         this%dhmin_min = that%dhmin_min
       end subroutine

       subroutine delete_mesh_props(this)
         implicit none
         type(mesh_props),intent(inout) :: this
         integer :: i_int_tensor
         integer :: s_int_tensor
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call delete(this%int_tensor(i_int_tensor))
         enddo
         this%plane = .false.
         this%N_cells = 0
         this%plane_any = .false.
         this%N_cells_tot = 0
         this%volume = 0.0_cp
         this%hmax = 0.0_cp
         this%hmin = 0.0_cp
         this%dhmax = 0.0_cp
         this%dhmin = 0.0_cp
         this%dhmax_max = 0.0_cp
         this%dhmin_min = 0.0_cp
       end subroutine

       subroutine display_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_int_tensor
         integer :: s_int_tensor
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call display(this%int_tensor(i_int_tensor),un)
         enddo
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'N_cells     = ',this%N_cells
         write(un,*) 'plane_any   = ',this%plane_any
         write(un,*) 'N_cells_tot = ',this%N_cells_tot
         write(un,*) 'volume      = ',this%volume
         write(un,*) 'hmax        = ',this%hmax
         write(un,*) 'hmin        = ',this%hmin
         write(un,*) 'dhmax       = ',this%dhmax
         write(un,*) 'dhmin       = ',this%dhmin
         write(un,*) 'dhmax_max   = ',this%dhmax_max
         write(un,*) 'dhmin_min   = ',this%dhmin_min
       end subroutine

       subroutine display_short_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_int_tensor
         integer :: s_int_tensor
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call display(this%int_tensor(i_int_tensor),un)
         enddo
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'N_cells     = ',this%N_cells
         write(un,*) 'plane_any   = ',this%plane_any
         write(un,*) 'N_cells_tot = ',this%N_cells_tot
         write(un,*) 'volume      = ',this%volume
         write(un,*) 'hmax        = ',this%hmax
         write(un,*) 'hmin        = ',this%hmin
         write(un,*) 'dhmax       = ',this%dhmax
         write(un,*) 'dhmin       = ',this%dhmin
         write(un,*) 'dhmax_max   = ',this%dhmax_max
         write(un,*) 'dhmin_min   = ',this%dhmin_min
       end subroutine

       subroutine display_wrap_mesh_props(this,dir,name)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_mesh_props(this)
         implicit none
         type(mesh_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mesh_props(this)
         implicit none
         type(mesh_props),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'plane        = ';write(un,*) this%plane
         write(un,*) 'N_cells      = ';write(un,*) this%N_cells
         write(un,*) 'plane_any    = ';write(un,*) this%plane_any
         write(un,*) 'N_cells_tot  = ';write(un,*) this%N_cells_tot
         write(un,*) 'volume       = ';write(un,*) this%volume
         write(un,*) 'hmax         = ';write(un,*) this%hmax
         write(un,*) 'hmin         = ';write(un,*) this%hmin
         write(un,*) 'dhmax        = ';write(un,*) this%dhmax
         write(un,*) 'dhmin        = ';write(un,*) this%dhmin
         write(un,*) 'dhmax_max    = ';write(un,*) this%dhmax_max
         write(un,*) 'dhmin_min    = ';write(un,*) this%dhmin_min
       end subroutine

       subroutine export_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_int_tensor
         integer :: s_int_tensor
         s_int_tensor = size(this%int_tensor)
         write(un,*) s_int_tensor
         do i_int_tensor=1,s_int_tensor
           call export(this%int_tensor(i_int_tensor),un)
         enddo
         write(un,*) 'plane        = ';write(un,*) this%plane
         write(un,*) 'N_cells      = ';write(un,*) this%N_cells
         write(un,*) 'plane_any    = ';write(un,*) this%plane_any
         write(un,*) 'N_cells_tot  = ';write(un,*) this%N_cells_tot
         write(un,*) 'volume       = ';write(un,*) this%volume
         write(un,*) 'hmax         = ';write(un,*) this%hmax
         write(un,*) 'hmin         = ';write(un,*) this%hmin
         write(un,*) 'dhmax        = ';write(un,*) this%dhmax
         write(un,*) 'dhmin        = ';write(un,*) this%dhmin
         write(un,*) 'dhmax_max    = ';write(un,*) this%dhmax_max
         write(un,*) 'dhmin_min    = ';write(un,*) this%dhmin_min
       end subroutine

       subroutine import_primitives_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%plane
         read(un,*); read(un,*) this%N_cells
         read(un,*); read(un,*) this%plane_any
         read(un,*); read(un,*) this%N_cells_tot
         read(un,*); read(un,*) this%volume
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%dhmax
         read(un,*); read(un,*) this%dhmin
         read(un,*); read(un,*) this%dhmax_max
         read(un,*); read(un,*) this%dhmin_min
       end subroutine

       subroutine import_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_int_tensor
         integer :: s_int_tensor
         call delete(this)
         read(un,*) s_int_tensor
         do i_int_tensor=1,s_int_tensor
           call import(this%int_tensor(i_int_tensor),un)
         enddo
         read(un,*); read(un,*) this%plane
         read(un,*); read(un,*) this%N_cells
         read(un,*); read(un,*) this%plane_any
         read(un,*); read(un,*) this%N_cells_tot
         read(un,*); read(un,*) this%volume
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%dhmax
         read(un,*); read(un,*) this%dhmin
         read(un,*); read(un,*) this%dhmax_max
         read(un,*); read(un,*) this%dhmin_min
       end subroutine

       subroutine export_wrap_mesh_props(this,dir,name)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mesh_props(this,dir,name)
         implicit none
         type(mesh_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_mesh_props(this,dir)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_int_tensor
         integer :: s_int_tensor
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call make_restart_dir(this%int_tensor(i_int_tensor),&
           dir//'int_tensor_'//int2str(i_int_tensor)//fortran_PS)
         enddo
       end subroutine

       subroutine export_restart_mesh_props(this,dir)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_int_tensor
         integer :: s_int_tensor
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call export_restart(this%int_tensor(i_int_tensor),&
           dir//'int_tensor_'//int2str(i_int_tensor)//fortran_PS)
         enddo
       end subroutine

       subroutine import_restart_mesh_props(this,dir)
         implicit none
         type(mesh_props),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_int_tensor
         integer :: s_int_tensor
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call import_restart(this%int_tensor(i_int_tensor),&
           dir//'int_tensor_'//int2str(i_int_tensor)//fortran_PS)
         enddo
       end subroutine

       subroutine suppress_warnings_mesh_props(this)
         implicit none
         type(mesh_props),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module