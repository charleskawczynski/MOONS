       module var_mod
       use solver_settings_mod
       use time_marching_params_mod
       use iter_solver_params_mod
       use matrix_free_params_mod
       use export_lines_mod
       use export_planes_mod
       use export_field_mod
       implicit none

       private
       public :: var
       public :: init,delete,export,import,display,print

       public :: init_IC_BC

       type var
         integer :: IC = 0
         integer :: BC = 0
         type(solver_settings) :: SS
         type(matrix_free_params) :: MFP
         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP
         type(export_lines) :: unsteady_lines
         type(export_planes) :: unsteady_planes
         type(export_field) :: unsteady_field
         ! type(dir_group) :: DG
         ! type(export_now) :: EN
       end type

       interface init;      module procedure init_copy_V; end interface
       interface init_IC_BC;module procedure init_IC_BC_V;end interface
       interface delete;    module procedure delete_V;    end interface
       interface export;    module procedure export_V;    end interface
       interface import;    module procedure import_V;    end interface
       interface display;   module procedure display_V;   end interface
       interface print;     module procedure print_V;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_IC_BC_V(V,IC,BC)
         implicit none
         type(var),intent(inout) :: V
         integer,intent(in) :: IC,BC
         V%IC = IC
         V%BC = BC
       end subroutine

       subroutine init_copy_V(V,V_in)
         implicit none
         type(var),intent(inout) :: V
         type(var),intent(in) :: V_in
         call init(V%SS,V_in%SS)
         call init(V%TMP,V_in%TMP)
         call init(V%ISP,V_in%ISP)
         call init(V%MFP,V_in%MFP)
         call init(V%unsteady_lines,V_in%unsteady_lines)
         call init(V%unsteady_planes,V_in%unsteady_planes)
         call init(V%unsteady_field,V_in%unsteady_field)
         V%IC = V_in%IC
         V%BC = V_in%BC
       end subroutine

       subroutine delete_V(V)
         implicit none
         type(var),intent(inout) :: V
         V%IC = 0
         V%BC = 0
         call delete(V%SS)
         call delete(V%TMP)
         call delete(V%ISP)
         call delete(V%MFP)
         call delete(V%unsteady_lines)
         call delete(V%unsteady_planes)
         call delete(V%unsteady_field)
       end subroutine

       subroutine export_V(V,un)
         implicit none
         type(var),intent(in) :: V
         integer,intent(in) :: un
         write(un,*) V%IC
         write(un,*) V%BC
         call export(V%SS,un)
         call export(V%TMP,un)
         call export(V%ISP,un)
         call export(V%MFP,un)
         call export(V%unsteady_lines,un)
         call export(V%unsteady_planes,un)
         call export(V%unsteady_field,un)
       end subroutine

       subroutine import_V(V,un)
         implicit none
         type(var),intent(inout) :: V
         integer,intent(in) :: un
         read(un,*) V%IC
         read(un,*) V%BC
         call import(V%SS,un)
         call import(V%TMP,un)
         call import(V%ISP,un)
         call import(V%MFP,un)
         call import(V%unsteady_lines,un)
         call import(V%unsteady_planes,un)
         call import(V%unsteady_field,un)
       end subroutine

       subroutine display_V(V,un)
         implicit none
         type(var),intent(in) :: V
         integer,intent(in) :: un
         write(un,*) 'IC = ',V%IC
         write(un,*) 'BC = ',V%BC
         call display(V%SS,un)
         call display(V%TMP,un)
         call display(V%ISP,un)
         call display(V%MFP,un)
         call display(V%unsteady_lines,un)
         call display(V%unsteady_planes,un)
         call display(V%unsteady_field,un)
       end subroutine

       subroutine print_V(V)
         implicit none
         type(var),intent(inout) :: V
         call display(V,6)
       end subroutine

       end module