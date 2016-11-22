       module iter_solver_params_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: iter_solver_params
       public :: init,delete,export,import,display,print

       real(cp) :: i_smooth = 0.02_cp
       real(cp) :: i_buffer = 0.8_cp
       real(cp) :: i_scale = 0.5_cp

       public :: check_res
       public :: exit_loop
       public :: update_check_res
       public :: update_last_iter

       public :: boost,reset

       type iter_solver_params
         integer :: iter_max = 1                    ! Maximum iterations for iterative solver
         real(cp) :: tol_rel = 10.0_cp**(-10.0_cp)  ! relative tolerance for iterative solver
         real(cp) :: tol_abs = 10.0_cp**(-10.0_cp)  ! absolute tolerance for iterative solver

         ! smoothes out tanh curve to determine n_skip_check_res
         real(cp) :: smooth                         ! (must be > 0) higher -> more step-like

         real(cp) :: buffer                         ! percentage of iter_last to start checking more frequently
         real(cp) :: scale                          ! scale amplitude to allow decreasing predictions
         integer :: n_skip_check_res = 1            ! number of iterations to skip before checking residual
         integer :: n_skip_check_res_max = 1        ! number of iterations to skip before checking residual
         integer :: un = 0                          ! file unit
         type(string) :: dir,name                   ! directory / filename
         integer :: iter_last = 1                   ! Number of iterations used last solve
       end type

       interface init;              module procedure init_ISP;              end interface
       interface init;              module procedure init_copy_ISP;         end interface
       interface delete;            module procedure delete_ISP;            end interface
       interface export;            module procedure export_ISP;            end interface
       interface import;            module procedure import_ISP;            end interface
       interface display;           module procedure display_ISP;           end interface
       interface print;             module procedure print_ISP;             end interface

       interface check_res;         module procedure check_res_ISP;         end interface
       interface exit_loop;         module procedure exit_loop_ISP;         end interface
       interface update_check_res;  module procedure update_check_res_ISP;  end interface
       interface update_last_iter;  module procedure update_last_iter_ISP;  end interface

       interface boost;             module procedure boost_ISP;             end interface
       interface reset;             module procedure reset_ISP;             end interface

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
         ISP%iter_last = 1
         ISP%smooth = i_smooth
         ISP%buffer = i_buffer
         ISP%scale = i_scale
         ISP%n_skip_check_res = n_skip_check_res
         ISP%n_skip_check_res_max = n_skip_check_res
         call init(ISP%dir,dir)
         call init(ISP%name,name)
       end subroutine

       subroutine init_copy_ISP(ISP,ISP_in)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         type(iter_solver_params),intent(in) :: ISP_in
         ISP%iter_max = ISP_in%iter_max
         ISP%tol_rel = ISP_in%tol_rel
         ISP%tol_abs = ISP_in%tol_abs
         ISP%n_skip_check_res = ISP_in%n_skip_check_res
         ISP%n_skip_check_res_max = ISP_in%n_skip_check_res_max
         ISP%smooth = ISP_in%smooth
         ISP%buffer = ISP_in%buffer
         ISP%scale = ISP_in%scale
         ISP%un = ISP_in%un
         ISP%iter_last = ISP_in%iter_last
         call init(ISP%dir,ISP_in%dir)
         call init(ISP%name,ISP_in%name)
       end subroutine

       subroutine delete_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         ISP%n_skip_check_res = 1
         ISP%n_skip_check_res_max = 1
         ISP%iter_max = 1
         ISP%iter_last = 1
         ISP%smooth = i_smooth
         ISP%buffer = i_buffer
         ISP%scale = i_scale
         ISP%tol_rel = 0.1_cp
         ISP%tol_abs = 10.0_cp**(-10.0_cp)
         call delete(ISP%dir)
         call delete(ISP%name)
       end subroutine

       subroutine export_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer :: un
         un = new_and_open(str(ISP%dir),str(ISP%name))
         write(un,*) 'iter_max = ';             write(un,*) ISP%iter_max
         write(un,*) 'tol_rel = ';              write(un,*) ISP%tol_rel
         write(un,*) 'tol_abs = ';              write(un,*) ISP%tol_abs
         write(un,*) 'iter_last = ';            write(un,*) ISP%iter_last
         write(un,*) 'n_skip_check_res = ';     write(un,*) ISP%n_skip_check_res
         write(un,*) 'n_skip_check_res_max = '; write(un,*) ISP%n_skip_check_res_max
         write(un,*) 'smooth = ';               write(un,*) ISP%smooth
         write(un,*) 'buffer = ';               write(un,*) ISP%buffer
         write(un,*) 'scale = ';                write(un,*) ISP%scale
         close(un)
       end subroutine

       subroutine import_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer :: un
         un = open_to_read(str(ISP%dir),str(ISP%name))
         read(un,*); read(un,*) ISP%iter_max
         read(un,*); read(un,*) ISP%tol_rel
         read(un,*); read(un,*) ISP%tol_abs
         read(un,*); read(un,*) ISP%iter_last
         read(un,*); read(un,*) ISP%n_skip_check_res
         read(un,*); read(un,*) ISP%n_skip_check_res_max
         read(un,*); read(un,*) ISP%smooth
         read(un,*); read(un,*) ISP%buffer
         read(un,*); read(un,*) ISP%scale
         close(un)
       end subroutine

       subroutine display_ISP(ISP,un)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: un
         write(un,*) 'iter_max = ',ISP%iter_max
         write(un,*) 'tol_rel = ',ISP%tol_rel
         write(un,*) 'tol_abs = ',ISP%tol_abs
         write(un,*) 'iter_last = ',ISP%iter_last
         write(un,*) 'n_skip_check_res = ',ISP%n_skip_check_res
         write(un,*) 'n_skip_check_res_max = ',ISP%n_skip_check_res_max
         write(un,*) 'smooth = ',ISP%smooth
         write(un,*) 'buffer = ',ISP%buffer
         write(un,*) 'scale = ',ISP%scale
       end subroutine

       subroutine print_ISP(ISP)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         call display(ISP,6)
       end subroutine

       function check_res_ISP(ISP,iter) result(L)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         integer,intent(in) :: iter
         logical :: L
         L = mod(iter,ISP%n_skip_check_res).eq.0
       end function

       subroutine update_check_res_ISP(ISP,iter)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: iter
         real(cp) :: n_skip_check,theta,f,amplitude
         theta = real(iter,cp) - real(ISP%iter_last,cp)*ISP%buffer
         ! f is a down-ramp from 1 to zero with a shift (buffer) and slope (smoooth)
         f = 0.5_cp*(1.0_cp-tanh(theta*ISP%smooth))
         ! Modify amplitude, within bounds, based off last number of iterations
         ! Also, the amplitude should be a fraction of the previous step, so
         ! that the number of iterations can decrease.
         amplitude = ISP%scale*minval((/real(ISP%n_skip_check_res_max,cp),real(ISP%iter_last,cp)/))
         ! n_skip_check is a down-ramp from amplitude to 1
         n_skip_check = 1.0_cp + f*(amplitude-1.0_cp)
         ! Final result must be > 0
         ISP%n_skip_check_res = maxval((/1,ceiling(n_skip_check)/))
       end subroutine

       subroutine update_last_iter_ISP(ISP,iter)
         implicit none
         type(iter_solver_params),intent(inout) :: ISP
         integer,intent(in) :: iter
         ISP%iter_last = iter
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

       function exit_loop_ISP(ISP,res,res0) result(L)
         implicit none
         type(iter_solver_params),intent(in) :: ISP
         real(cp),intent(in) :: res,res0
         logical :: L
         L = (res/res0.lt.ISP%tol_rel).or.(res.lt.ISP%tol_abs)
       end function

       end module