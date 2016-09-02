       module iter_solver_params_mod
       use current_precision_mod
       implicit none

       private
       public :: iter_solver_params
       public :: init,delete,export,import,display,print

       type iter_solver_params
         integer :: iter_max ! Maximum iterations for iterative solver
         real(cp) :: tol_rel ! relative tolerance for iterative solver
         real(cp) :: tol_abs ! absolute tolerance for iterative solver
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

       subroutine init_SP(SP,iter_max,tol_abs,tol_rel)
         implicit none
         type(iter_solver_params),intent(inout) :: SP
         integer,intent(in) :: iter_max
         real(cp),intent(in) :: tol_rel,tol_abs
         SP%iter_max = iter_max
         SP%tol_rel = tol_rel
         SP%tol_abs = tol_abs
       end subroutine

       subroutine init_copy_SP(SP_out,SP_in)
         implicit none
         type(iter_solver_params),intent(inout) :: SP_out
         type(iter_solver_params),intent(in) :: SP_in
         SP_out%iter_max = SP_in%iter_max
         SP_out%tol_rel = SP_in%tol_rel
         SP_out%tol_abs = SP_in%tol_abs
       end subroutine

       subroutine delete_SP(SP)
         implicit none
         type(iter_solver_params),intent(inout) :: SP
         SP%iter_max = 1
         SP%tol_rel = 0.1_cp
         SP%tol_abs = 10.0_cp**(-10.0_cp)
       end subroutine

       subroutine export_SP(SP,un)
         implicit none
         type(iter_solver_params),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) SP%iter_max
         write(un,*) SP%tol_rel
         write(un,*) SP%tol_abs
         close(un)
       end subroutine

       subroutine import_SP(SP,un)
         implicit none
         type(iter_solver_params),intent(inout) :: SP
         integer,intent(in) :: un
         read(un,*) SP%iter_max
         read(un,*) SP%tol_rel
         read(un,*) SP%tol_abs
         close(un)
       end subroutine

       subroutine display_SP(SP,un)
         implicit none
         type(iter_solver_params),intent(inout) :: SP
         integer,intent(in) :: un
         write(un,*) 'iter_max = ',SP%iter_max
         write(un,*) 'tol_rel = ',SP%tol_rel
         write(un,*) 'tol_abs = ',SP%tol_abs
       end subroutine

       subroutine print_SP(SP)
         implicit none
         type(iter_solver_params),intent(inout) :: SP
         call display(SP,6)
       end subroutine

       end module