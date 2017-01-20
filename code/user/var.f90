       module var_mod
       use solver_settings_mod
       use time_marching_params_mod
       use iter_solver_params_mod
       implicit none

       private
       public :: var
       public :: init,delete,export,import,display,print

       public :: init_IC_BC

       type var
         integer :: IC = 0
         integer :: BC = 0
         type(solver_settings) :: SS
         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP
       end type

       interface init;      module procedure init_V;      end interface
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

       subroutine init_V(V,SS,TMP,ISP,IC,BC)
         implicit none
         type(var),intent(inout) :: V
         type(solver_settings),intent(in) :: SS
         type(time_marching_params),intent(in) :: TMP
         type(iter_solver_params),intent(in) :: ISP
         integer,intent(in) :: IC,BC
         call init(V%SS,SS)
         call init(V%TMP,TMP)
         call init(V%ISP,ISP)
         V%IC = IC
         V%BC = BC
       end subroutine

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
       end subroutine

       subroutine print_V(V)
         implicit none
         type(var),intent(inout) :: V
         call display(V,6)
       end subroutine

       end module