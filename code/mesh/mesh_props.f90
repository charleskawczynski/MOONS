       module mesh_props_mod
       use current_precision_mod
       use IO_tools_mod
       use simple_int_tensor_mod

       implicit none
       private
       public :: mesh_props
       public :: init,delete,display,print,export,import ! essentials

       type mesh_props
         type(simple_int_tensor),dimension(3) :: int_tensor
         logical,dimension(3) :: plane = .false.
         logical :: plane_any = .false.
         integer,dimension(3) :: N_cells = 0
         integer :: N_cells_tot = 0
         real(cp) :: volume = 0.0_cp
         real(cp),dimension(3) :: hmax = 0.0_cp
         real(cp),dimension(3) :: hmin = 0.0_cp
         real(cp),dimension(3) :: dhmax = 0.0_cp
         real(cp),dimension(3) :: dhmin = 0.0_cp
         real(cp) :: dhmax_max = 0.0_cp
         real(cp) :: dhmin_min = 0.0_cp
       end type

       interface init;     module procedure init_copy_MP;      end interface
       interface delete;   module procedure delete_MP;         end interface
       interface display;  module procedure display_MP;        end interface
       interface print;    module procedure print_MP;          end interface
       interface export;   module procedure export_MP;         end interface
       interface import;   module procedure import_MP;         end interface
       interface export;   module procedure export_MP_wrapper; end interface
       interface import;   module procedure import_MP_wrapper; end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

       subroutine init_copy_MP(MP,MP_in)
         implicit none
         type(mesh_props),intent(inout) :: MP
         type(mesh_props),intent(in) :: MP_in
         integer :: i
         do i=1,3; call init(MP%int_tensor(i),MP_in%int_tensor(i));enddo
         MP%plane       = MP_in%plane
         MP%plane_any   = MP_in%plane_any
         MP%N_cells     = MP_in%N_cells
         MP%N_cells_tot = MP_in%N_cells_tot
         MP%volume      = MP_in%volume
         MP%hmax        = MP_in%hmax
         MP%hmin        = MP_in%hmin
         MP%dhmax       = MP_in%dhmax
         MP%dhmin       = MP_in%dhmin
         MP%dhmax_max   = MP_in%dhmax_max
         MP%dhmin_min   = MP_in%dhmin_min
       end subroutine

       subroutine delete_MP(MP)
         implicit none
         type(mesh_props),intent(inout) :: MP
         integer :: i
         do i=1,3; call delete(MP%int_tensor(i)); enddo
         MP%plane = .false.
         MP%plane_any = .false.
         MP%N_cells = 0
         MP%N_cells_tot = 00
         MP%volume = 0.0_cp
         MP%hmax = 0.0_cp
         MP%hmin = 0.0_cp
         MP%dhmax = 0.0_cp
         MP%dhmin = 0.0_cp
         MP%dhmax_max = 0.0_cp
         MP%dhmin_min = 0.0_cp
       end subroutine

       subroutine display_MP(MP,un)
         implicit none
         type(mesh_props),intent(in) :: MP
         integer,intent(in) :: un
         integer :: i
         do i=1,3; call display(MP%int_tensor(i),un); enddo
         write(un,*) 'plane       = ',MP%plane
         write(un,*) 'plane_any   = ',MP%plane_any
         write(un,*) 'N_cells     = ',MP%N_cells
         write(un,*) 'N_cells_tot = ',MP%N_cells_tot
         write(un,*) 'volume      = ',MP%volume
         write(un,*) 'hmax        = ',MP%hmax
         write(un,*) 'hmin        = ',MP%hmin
         write(un,*) 'dhmax       = ',MP%dhmax
         write(un,*) 'dhmin       = ',MP%dhmin
         write(un,*) 'dhmax_max   = ',MP%dhmax_max
         write(un,*) 'dhmin_min   = ',MP%dhmin_min
       end subroutine

       subroutine print_MP(MP)
         implicit none
         type(mesh_props),intent(in) :: MP
         call display(MP,6)
       end subroutine

       subroutine export_MP(MP,un)
         implicit none
         type(mesh_props),intent(in) :: MP
         integer,intent(in) :: un
         integer :: i
         do i=1,3; call export(MP%int_tensor(i),un); enddo
         write(un,*) MP%plane
         write(un,*) MP%plane_any
         write(un,*) MP%N_cells
         write(un,*) MP%N_cells_tot
         write(un,*) MP%volume
         write(un,*) MP%hmax
         write(un,*) MP%hmin
         write(un,*) MP%dhmax
         write(un,*) MP%dhmin
         write(un,*) MP%dhmax_max
         write(un,*) MP%dhmin_min
       end subroutine

       subroutine import_MP(MP,un)
         implicit none
         type(mesh_props),intent(inout) :: MP
         integer,intent(in) :: un
         integer :: i
         do i=1,3; call import(MP%int_tensor(i),un); enddo
         read(un,*) MP%plane
         read(un,*) MP%plane_any
         read(un,*) MP%N_cells
         read(un,*) MP%N_cells_tot
         read(un,*) MP%volume
         read(un,*) MP%hmax
         read(un,*) MP%hmin
         read(un,*) MP%dhmax
         read(un,*) MP%dhmin
         read(un,*) MP%dhmax_max
         read(un,*) MP%dhmin_min
       end subroutine

       subroutine export_MP_wrapper(MP,dir,name)
         implicit none
         type(mesh_props),intent(in) :: MP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(MP,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_MP_wrapper(MP,dir,name)
         implicit none
         type(mesh_props),intent(inout) :: MP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(MP,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! ****************************************************************
       ! **************************** OTHER *****************************
       ! ****************************************************************

       end module