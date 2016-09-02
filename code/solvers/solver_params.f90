       module solver_params_mod
       use current_precision_mod
       implicit none

       private
       public :: solver_params
       public :: init,delete,export,import,display,print

       type solver_params
         integer :: N_iter_max ! Maximum iterations for iterative solver
         real(cp) :: tol       ! relative tolerance for iterative solver
         real(cp) :: dt        ! time step (or pseudo) for time (or pseudo) marching solver
       end type

       interface init;      module procedure init_SP;       end interface
       interface init;      module procedure init_copy_SP;  end interface
       interface delete;    module procedure delete_SP;     end interface
       interface export;    module procedure export_SP;     end interface
       interface import;    module procedure import_SP;     end interface
       interface display;   module procedure display_SP;    end interface
       interface print;     module procedure print_SP;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_SP(SP,N_iter_max,dt,tol)
         implicit none
         type(solver_params),intent(inout) :: SP
         integer,intent(in) :: N_iter_max
         real(cp),intent(in) :: tol,dt
         SP%N_iter_max = N_iter_max
         SP%tol = tol
         SP%dt = dt
       end subroutine

       subroutine init_copy_SP(SP_out,SP_in)
         implicit none
         type(solver_params),intent(inout) :: SP_out
         type(solver_params),intent(in) :: SP_in
         SP_out%N_iter_max = SP_in%N_iter_max
         SP_out%tol = SP_in%tol
         SP_out%dt = SP_in%dt
       end subroutine

       subroutine delete_SP(SP)
         implicit none
         type(solver_params),intent(inout) :: SP
         SP%N_iter_max = 1
         SP%tol = 0.1_cp
         SP%dt = 10.0_cp**(-10.0_cp)
       end subroutine

       subroutine export_SP(SP,un)
         implicit none
         type(solver_params),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) SP%N_iter_max
         write(un,*) SP%tol
         write(un,*) SP%dt
       end subroutine

       subroutine import_SP(SP,un)
         implicit none
         type(solver_params),intent(inout) :: SP
         integer,intent(in) :: un
         read(un,*) SP%N_iter_max
         read(un,*) SP%tol
         read(un,*) SP%dt
       end subroutine

       subroutine display_SP(SP,un)
         implicit none
         type(solver_params),intent(inout) :: SP
         integer,intent(in) :: un
         write(un,*) 'N_iter_max = ',SP%N_iter_max
         write(un,*) 'tol = ',SP%tol
         write(un,*) 'dt = ',SP%dt
       end subroutine

       subroutine print_SP(SP)
         implicit none
         type(solver_params),intent(inout) :: SP
         call display(SP,6)
       end subroutine

       end module