       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_props_mod
       use current_precision_mod
       use IO_tools_mod
       use simple_int_tensor_mod
       implicit none

       private
       public :: mesh_props
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh_props;           end interface
       interface delete; module procedure delete_mesh_props;         end interface
       interface display;module procedure display_mesh_props;        end interface
       interface display;module procedure display_wrapper_mesh_props;end interface
       interface print;  module procedure print_mesh_props;          end interface
       interface export; module procedure export_mesh_props;         end interface
       interface import; module procedure import_mesh_props;         end interface
       interface export; module procedure export_wrapper_mesh_props; end interface
       interface import; module procedure import_wrapper_mesh_props; end interface

       type mesh_props
         type(simple_int_tensor),dimension(3) :: int_tensor
         logical,dimension(:),allocatable :: plane
         integer,dimension(:),allocatable :: n_cells
         logical :: plane_any = .false.
         integer :: n_cells_tot = 0
         real(cp) :: volume = 0.0_cp
         real(cp),dimension(3) :: hmax = 0.0_cp
         real(cp),dimension(3) :: hmin = 0.0_cp
         real(cp),dimension(3) :: dhmax = 0.0_cp
         real(cp),dimension(3) :: dhmin = 0.0_cp
         real(cp) :: dhmax_max = 0.0_cp
         real(cp) :: dhmin_min = 0.0_cp
       end type

       contains

       subroutine init_mesh_props(this,that)
         implicit none
         type(mesh_props),intent(inout) :: this
         type(mesh_props),intent(in) :: that
         integer :: i_int_tensor
         integer :: s_int_tensor
         call delete(this)
         s_int_tensor = size(that%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call init(this%int_tensor(i_int_tensor),that%int_tensor(i_int_tensor))
         enddo
         this%plane = that%plane
         this%n_cells = that%n_cells
         this%plane_any = that%plane_any
         this%n_cells_tot = that%n_cells_tot
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
         deallocate(this%plane)
         this%n_cells = 0
         deallocate(this%n_cells)
         this%plane_any = .false.
         this%n_cells_tot = 0
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
         write(un,*) ' -------------------- mesh_props'
         integer :: i_int_tensor
         integer :: s_int_tensor
         s_int_tensor = size(this%int_tensor)
         do i_int_tensor=1,s_int_tensor
           call display(this%int_tensor(i_int_tensor),un)
         enddo
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'n_cells     = ',this%n_cells
         write(un,*) 'plane_any   = ',this%plane_any
         write(un,*) 'n_cells_tot = ',this%n_cells_tot
         write(un,*) 'volume      = ',this%volume
         write(un,*) 'hmax        = ',this%hmax
         write(un,*) 'hmin        = ',this%hmin
         write(un,*) 'dhmax       = ',this%dhmax
         write(un,*) 'dhmin       = ',this%dhmin
         write(un,*) 'dhmax_max   = ',this%dhmax_max
         write(un,*) 'dhmin_min   = ',this%dhmin_min
       end subroutine

       subroutine print_mesh_props(this)
         implicit none
         type(mesh_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_int_tensor
         integer :: s_int_tensor
         integer :: s_plane
         integer :: s_n_cells
         s_int_tensor = size(this%int_tensor)
         write(un,*) s_int_tensor
         do i_int_tensor=1,s_int_tensor
           call export(this%int_tensor(i_int_tensor),un)
         enddo
         if (allocated(this%plane)) then
           s_plane = size(this%plane)
           write(un,*) s_plane
           write(un,*) 'plane        = ';write(un,*) this%plane
         endif
         if (allocated(this%n_cells)) then
           s_n_cells = size(this%n_cells)
           write(un,*) s_n_cells
           write(un,*) 'n_cells      = ';write(un,*) this%n_cells
         endif
         write(un,*) 'plane_any    = ';write(un,*) this%plane_any
         write(un,*) 'n_cells_tot  = ';write(un,*) this%n_cells_tot
         write(un,*) 'volume       = ';write(un,*) this%volume
         write(un,*) 'hmax         = ';write(un,*) this%hmax
         write(un,*) 'hmin         = ';write(un,*) this%hmin
         write(un,*) 'dhmax        = ';write(un,*) this%dhmax
         write(un,*) 'dhmin        = ';write(un,*) this%dhmin
         write(un,*) 'dhmax_max    = ';write(un,*) this%dhmax_max
         write(un,*) 'dhmin_min    = ';write(un,*) this%dhmin_min
       end subroutine

       subroutine import_mesh_props(this,un)
         implicit none
         type(mesh_props),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_int_tensor
         integer :: s_int_tensor
         integer :: s_plane
         integer :: s_n_cells
         call delete(this)
         read(un,*) s_int_tensor
         do i_int_tensor=1,s_int_tensor
           call import(this%int_tensor(i_int_tensor),un)
         enddo
         read(un,*) s_plane
         allocate(this%plane(s_plane))
         read(un,*); read(un,*) this%plane
         read(un,*) s_n_cells
         allocate(this%n_cells(s_n_cells))
         read(un,*); read(un,*) this%n_cells
         read(un,*); read(un,*) this%plane_any
         read(un,*); read(un,*) this%n_cells_tot
         read(un,*); read(un,*) this%volume
         read(un,*); read(un,*) this%hmax
         read(un,*); read(un,*) this%hmin
         read(un,*); read(un,*) this%dhmax
         read(un,*); read(un,*) this%dhmin
         read(un,*); read(un,*) this%dhmax_max
         read(un,*); read(un,*) this%dhmin_min
       end subroutine

       subroutine display_wrapper_mesh_props(this,dir,name)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_mesh_props(this,dir,name)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_mesh_props(this,dir,name)
         implicit none
         type(mesh_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module