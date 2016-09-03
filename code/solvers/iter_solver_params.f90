       module iter_solver_params_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: iter_solver_params
       public :: init,delete,export,import,display,print

       type iter_solver_params
         integer :: iter_max          ! Maximum iterations for iterative solver
         real(cp) :: tol_rel          ! relative tolerance for iterative solver
         real(cp) :: tol_abs          ! absolute tolerance for iterative solver
         integer :: n_skip_check_res  ! number of iterations to skip before checking residual
         integer :: un                ! file unit
         type(string) :: dir,name     ! directory / filename
         ! n_skip_check_res is NOT used in Preconditioned Conjugate Gradient (PCG) Method
         !                                 since the residual is computed, as a part
         !                                 of the CG algorithm, at every step.
         ! n_skip_check_res is     used in Gauss-Seidel Method
         !                                 since the residual is NOT computed, as a
         !                                 part of the GS algorithm, at every step.
       end type

       interface init;      module procedure init_ISP;          end interface
       interface init;      module procedure init_copy_ISP;     end interface
       interface delete;    module procedure delete_ISP;        end interface
       interface export;    module procedure export_ISP;        end interface
       interface import;    module procedure import_ISP;        end interface
       interface display;   module procedure display_ISP;       end interface
       interface print;     module procedure print_ISP;         end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_ISP(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res,dir,name)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: iter_max,n_skip_check_res
         real(cp),intent(in) :: tol_rel,tol_abs
         character(len=*),intent(in) :: dir,name
         ISP%iter_max = iter_max
         ISP%tol_rel = tol_rel
         ISP%tol_abs = tol_abs
         ISP%n_skip_check_res = n_skip_check_res
         call init(ISP%dir,dir)
         call init(ISP%name,name)
         ! ISP%un = newAndOpen(str(ISP%dir),str(ISP%name))
       end subroutine

       subroutine init_copy_ISP(ISP_out,ISP_in)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP_out
         type(iter_solver_params),intent(in) :: ISP_in
         ISP_out%iter_max = ISP_in%iter_max
         ISP_out%tol_rel = ISP_in%tol_rel
         ISP_out%tol_abs = ISP_in%tol_abs
         ISP_out%n_skip_check_res = ISP_in%n_skip_check_res
         ISP_out%un = ISP_in%un
         call init(ISP_out%dir,ISP_in%dir)
         call init(ISP_out%name,ISP_in%name)
       end subroutine

       subroutine delete_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%n_skip_check_res = 1
         ISP%iter_max = 1
         ISP%tol_rel = 0.1_cp
         ISP%tol_abs = 10.0_cp**(-10.0_cp)
         ! prefer closeAndMessage, but not all copies are initialized, and fail to have dir / name
         ! call closeAndMessage(ISP%un,str(ISP%dir),str(ISP%name))
         close(ISP%un)
         ISP%un = 0
         call delete(ISP%dir)
         call delete(ISP%name)
       end subroutine

       subroutine export_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer :: un
         un = newAndOpen(str(ISP%dir),str(ISP%name))
         write(un,*) 'iter_max = ';         write(un,*) ISP%iter_max
         write(un,*) 'tol_rel = ';          write(un,*) ISP%tol_rel
         write(un,*) 'tol_abs = ';          write(un,*) ISP%tol_abs
         write(un,*) 'n_skip_check_res = '; write(un,*) ISP%n_skip_check_res
         close(un)
       end subroutine

       subroutine import_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%un = openToRead(str(ISP%dir),str(ISP%name))
         read(ISP%un,*); read(ISP%un,*) ISP%iter_max
         read(ISP%un,*); read(ISP%un,*) ISP%tol_rel
         read(ISP%un,*); read(ISP%un,*) ISP%tol_abs
         read(ISP%un,*); read(ISP%un,*) ISP%n_skip_check_res
         close(ISP%un)
       end subroutine

       subroutine display_ISP(ISP,un)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: un
         write(un,*) 'iter_max = ',ISP%iter_max
         write(un,*) 'tol_rel = ',ISP%tol_rel
         write(un,*) 'tol_abs = ',ISP%tol_abs
         write(un,*) 'n_skip_check_res = ',ISP%n_skip_check_res
       end subroutine

       subroutine print_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         call display(ISP,6)
       end subroutine

       end module