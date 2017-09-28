       module iter_solver_params_extend_mod
       use iter_solver_params_mod
       use current_precision_mod
       use datatype_conversion_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: init
       public :: display_exit_loop
       public :: print_exit_loop
       public :: import_exit_criteria

       public :: check_res
       public :: solve_exact
       public :: update_exit_loop
       public :: update_iter
       public :: init_iter_per_call

       public :: boost,reset

       interface init;                 module procedure init_ISP;                 end interface
       interface import_exit_criteria; module procedure import_exit_criteria_ISP; end interface
       interface display_exit_loop;    module procedure display_exit_loop_ISP;    end interface
       interface print_exit_loop;      module procedure print_exit_loop_ISP;      end interface

       interface check_res;            module procedure check_res_ISP;            end interface
       interface update_exit_loop;     module procedure update_exit_loop_ISP;     end interface
       interface update_exit_loop;     module procedure update_exit_loop_ISP2;    end interface
       interface update_iter;          module procedure update_iter_ISP;          end interface
       interface init_iter_per_call;   module procedure init_iter_per_call_ISP;   end interface

       interface boost;                module procedure boost_ISP;                end interface
       interface reset;                module procedure reset_ISP;                end interface

       interface solve_exact;          module procedure solve_exact_ISP;          end interface
       interface solve_exact;          module procedure solve_exact_N_ISP;        end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_ISP(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence,export_heavy)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: iter_max,n_skip_check_res
         real(cp),intent(in) :: tol_rel,tol_abs
         logical,intent(in) :: export_convergence,export_heavy
         ISP%iter_max = iter_max
         ISP%iter_total = 0
         ISP%iter_per_call = 0
         ISP%tol_rel = tol_rel
         ISP%tol_abs = tol_abs
         ISP%export_convergence = export_convergence
         ISP%export_heavy = export_heavy
         ISP%n_skip_check_res = n_skip_check_res
         ISP%exit_loop = .false.
       end subroutine

       subroutine import_exit_criteria_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         type(iter_solver_params) :: temp
         call init(temp,ISP)
         call import(temp,str(ISP%dir),str(ISP%name))
         ISP%tol_rel = temp%tol_rel
         ISP%tol_abs = temp%tol_abs
         ISP%iter_max = temp%iter_max
         call delete(temp)
       end subroutine

       subroutine display_exit_loop_ISP(ISP,un)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer,intent(in) :: un
         write(un,*) 'tol (rel,abs)      = ',ISP%tol_rel,ISP%tol_abs
         write(un,*) 'exit_loop,iter_max = ',ISP%exit_loop,ISP%iter_max
       end subroutine

       subroutine print_exit_loop_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         call display_exit_loop(ISP,6)
       end subroutine

       subroutine boost_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%iter_max = 4*ISP%iter_max
       end subroutine
       subroutine reset_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%iter_max = ISP%iter_max/4
       end subroutine

       subroutine update_exit_loop_ISP(ISP,res,res0)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         real(cp),intent(in) :: res,res0
         ISP%exit_loop(1) = res/res0.lt.ISP%tol_rel
         ISP%exit_loop(2) = res.lt.ISP%tol_abs
         ISP%exit_loop(3) = ISP%iter_per_call.ge.ISP%iter_max
       end subroutine

       subroutine update_exit_loop_ISP2(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%exit_loop(3) = ISP%iter_per_call.ge.ISP%iter_max
       end subroutine

       subroutine update_iter_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%iter_total = ISP%iter_total + 1
         ISP%iter_per_call = ISP%iter_per_call + 1
       end subroutine

       function check_res_ISP(ISP) result(L)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         logical :: L
         L = mod(ISP%iter_per_call,ISP%n_skip_check_res).eq.0
       end function

       subroutine init_iter_per_call_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%iter_per_call = 0
       end subroutine

       function solve_exact_ISP(dir) result(ISP)
         implicit none
         character(len=*),intent(in) :: dir
         type(iter_solver_params) :: ISP
         real(cp) :: tol_rel,tol_abs
         integer :: iter_max,n_skip_check_res
         logical :: export_heavy
         tol_rel = 0.0_cp
         tol_abs = 10.0_cp*machine_epsilon
         iter_max = 10000
         n_skip_check_res = 100
         export_heavy = .true.
         call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,.false.,export_heavy)
         call init(ISP%dir,dir)
         call init(ISP%name,'solve_exact')
       end function

       function solve_exact_N_ISP(dir,N) result(ISP)
         implicit none
         character(len=*),intent(in) :: dir
         integer,intent(in) :: N
         type(iter_solver_params) :: ISP
         real(cp) :: tol_rel,tol_abs
         integer :: iter_max,n_skip_check_res
         logical :: export_heavy
         tol_rel = 0.0_cp
         tol_abs = 10.0_cp*machine_epsilon
         iter_max = N
         n_skip_check_res = 100
         export_heavy = .true.
         call init(ISP,iter_max,tol_rel,tol_abs,&
         n_skip_check_res,.false.,export_heavy)
         call init(ISP%dir,dir)
         call init(ISP%name,'solve_in_'//int2str(N)//'_iter')
       end function

       end module