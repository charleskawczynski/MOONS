       module iter_solver_params_mod
       use current_precision_mod
       use datatype_conversion_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: iter_solver_params
       public :: init,delete,export,import,display,print
       public :: display_exit_loop
       public :: print_exit_loop

       public :: check_res
       public :: solve_exact
       public :: update_exit_loop
       public :: update_iter
       public :: init_iter_per_call

       public :: boost,reset

       type iter_solver_params
         integer :: un = 0                            ! file unit
         type(string) :: dir,name                     ! directory / filename
         integer :: iter_max = 1                      ! Maximum iterations for iterative solver
         real(cp) :: tol_rel = 10.0_cp**(-10.0_cp)    ! relative tolerance for iterative solver
         real(cp) :: tol_abs = 10.0_cp**(-10.0_cp)    ! absolute tolerance for iterative solver
         logical :: export_convergence = .false.      ! exit condition
         logical :: export_heavy = .false.            ! number of iterations to skip before checking residual
         logical,dimension(3) :: exit_loop = .false.  ! exit condition
         integer :: iter_total = 0                    ! Number of iterations (total)
         integer :: iter_per_call = 0                 ! Number of iterations per call
         integer :: n_skip_check_res = 1              ! number of iterations to skip before checking residual
       end type

       interface init;              module procedure init_ISP;              end interface
       interface init;              module procedure init_copy_ISP;         end interface
       interface delete;            module procedure delete_ISP;            end interface
       interface export;            module procedure export_ISP;            end interface
       interface export;            module procedure export_ISP_wrapper;    end interface
       interface import;            module procedure import_ISP;            end interface
       interface import;            module procedure import_ISP_wrapper;    end interface
       interface display;           module procedure display_ISP;           end interface
       interface print;             module procedure print_ISP;             end interface
       interface display_exit_loop; module procedure display_exit_loop_ISP; end interface
       interface print_exit_loop;   module procedure print_exit_loop_ISP;   end interface

       interface check_res;         module procedure check_res_ISP;         end interface
       interface update_exit_loop;  module procedure update_exit_loop_ISP;  end interface
       interface update_exit_loop;  module procedure update_exit_loop_ISP2; end interface
       interface update_iter;       module procedure update_iter_ISP;       end interface
       interface init_iter_per_call;module procedure init_iter_per_call_ISP;end interface

       interface boost;             module procedure boost_ISP;             end interface
       interface reset;             module procedure reset_ISP;             end interface

       interface solve_exact;       module procedure solve_exact_ISP;       end interface
       interface solve_exact;       module procedure solve_exact_N_ISP;     end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_ISP(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,export_convergence,export_heavy,dir,name)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: iter_max,n_skip_check_res
         real(cp),intent(in) :: tol_rel,tol_abs
         logical,intent(in) :: export_convergence,export_heavy
         character(len=*),intent(in) :: dir,name
         ISP%iter_max = iter_max
         ISP%iter_total = 0
         ISP%iter_per_call = 0
         ISP%tol_rel = tol_rel
         ISP%tol_abs = tol_abs
         ISP%export_convergence = export_convergence
         ISP%export_heavy = export_heavy
         ISP%n_skip_check_res = n_skip_check_res
         ISP%exit_loop = .false.
         call init(ISP%dir,dir)
         call init(ISP%name,name)
       end subroutine

       subroutine init_copy_ISP(ISP,ISP_in)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         type(iter_solver_params),intent(in) :: ISP_in
         ISP%iter_max             = ISP_in%iter_max
         ISP%iter_total           = ISP_in%iter_total
         ISP%iter_per_call        = ISP_in%iter_per_call
         ISP%tol_rel              = ISP_in%tol_rel
         ISP%tol_abs              = ISP_in%tol_abs
         ISP%n_skip_check_res     = ISP_in%n_skip_check_res
         ISP%un                   = ISP_in%un
         ISP%exit_loop            = ISP_in%exit_loop
         ISP%export_convergence   = ISP_in%export_convergence
         ISP%export_heavy         = ISP_in%export_heavy
         call init(ISP%dir,ISP_in%dir)
         call init(ISP%name,ISP_in%name)
       end subroutine

       subroutine delete_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%n_skip_check_res = 1
         ISP%iter_max = 1
         ISP%iter_total = 0
         ISP%iter_per_call = 0
         ISP%tol_rel = 0.1_cp
         ISP%tol_abs = 10.0_cp**(-10.0_cp)
         ISP%exit_loop = .false.
         ISP%export_convergence = .false.
         ISP%export_heavy = .false.
         call delete(ISP%dir)
         call delete(ISP%name)
       end subroutine

       subroutine export_ISP(ISP,un)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer,intent(in) :: un
         write(un,*) ' -------- iter_solver_params -------- '
         write(un,*) 'iter_max           = '; write(un,*) ISP%iter_max
         write(un,*) 'iter_total         = '; write(un,*) ISP%iter_total
         write(un,*) 'iter_per_call      = '; write(un,*) ISP%iter_per_call
         write(un,*) 'tol_rel            = '; write(un,*) ISP%tol_rel
         write(un,*) 'tol_abs            = '; write(un,*) ISP%tol_abs
         write(un,*) 'n_skip_check_res   = '; write(un,*) ISP%n_skip_check_res
         write(un,*) 'export_convergence = '; write(un,*) ISP%export_convergence
         write(un,*) 'export_heavy       = '; write(un,*) ISP%export_heavy
         write(un,*) 'exit_loop          = '; write(un,*) ISP%exit_loop
         call export(ISP%dir,un)
         call export(ISP%name,un)
         write(un,*) ' ------------------------------------ '
       end subroutine

       subroutine export_ISP_wrapper(ISP)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer :: un
         un = new_and_open(str(ISP%dir),str(ISP%name))
         call export(ISP,un)
         close(un)
       end subroutine

       subroutine import_ISP(ISP,un)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: un
         read(un,*);
         read(un,*); read(un,*) ISP%iter_max
         read(un,*); read(un,*) ISP%iter_total
         read(un,*); read(un,*) ISP%iter_per_call
         read(un,*); read(un,*) ISP%tol_rel
         read(un,*); read(un,*) ISP%tol_abs
         read(un,*); read(un,*) ISP%n_skip_check_res
         read(un,*); read(un,*) ISP%export_convergence
         read(un,*); read(un,*) ISP%export_heavy
         read(un,*); read(un,*) ISP%exit_loop
         call import(ISP%dir,un)
         call import(ISP%name,un)
         read(un,*);
       end subroutine

       subroutine import_ISP_wrapper(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer :: un
         un = open_to_read(str(ISP%dir),str(ISP%name))
         call import(ISP,un)
         close(un)
       end subroutine

       subroutine display_ISP(ISP,un)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer,intent(in) :: un
         write(un,*) 'iter_max             = ',ISP%iter_max
         write(un,*) 'iter_total           = ',ISP%iter_total
         write(un,*) 'iter_per_call        = ',ISP%iter_per_call
         write(un,*) 'tol_rel              = ',ISP%tol_rel
         write(un,*) 'tol_abs              = ',ISP%tol_abs
         write(un,*) 'n_skip_check_res     = ',ISP%n_skip_check_res
         write(un,*) 'exit_loop            = ',ISP%exit_loop
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

       subroutine print_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         call display(ISP,6)
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
         call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,.false.,export_heavy,dir,'solve_exact')
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
         n_skip_check_res,.false.,export_heavy,dir,'solve_in_'//int2str(N)//'_iter')
       end function

       end module