       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module iter_solver_params_mod
       use current_precision_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: iter_solver_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_iter_solver_params;    end interface
       interface delete;       module procedure delete_iter_solver_params;       end interface
       interface display;      module procedure display_iter_solver_params;      end interface
       interface display_short;module procedure display_short_iter_solver_params;end interface
       interface display;      module procedure display_wrap_iter_solver_params; end interface
       interface print;        module procedure print_iter_solver_params;        end interface
       interface print_short;  module procedure print_short_iter_solver_params;  end interface
       interface export;       module procedure export_iter_solver_params;       end interface
       interface import;       module procedure import_iter_solver_params;       end interface
       interface export;       module procedure export_wrap_iter_solver_params;  end interface
       interface import;       module procedure import_wrap_iter_solver_params;  end interface
       interface export;       module procedure export_DN_iter_solver_params;    end interface
       interface import;       module procedure import_DN_iter_solver_params;    end interface

       type iter_solver_params
         integer :: un = 0
         type(string) :: dir
         type(string) :: name
         integer :: iter_max = 0
         real(cp) :: tol_abs = 0.0_cp
         real(cp) :: tol_rel = 0.0_cp
         integer :: iter_total = 0
         integer :: iter_per_call = 0
         integer :: n_skip_check_res = 0
         logical :: export_convergence = .false.
         logical :: export_heavy = .false.
         logical,dimension(3) :: exit_loop = .false.
       end type

       contains

       subroutine init_copy_iter_solver_params(this,that)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         type(iter_solver_params),intent(in) :: that
         call delete(this)
         this%un = that%un
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         this%iter_max = that%iter_max
         this%tol_abs = that%tol_abs
         this%tol_rel = that%tol_rel
         this%iter_total = that%iter_total
         this%iter_per_call = that%iter_per_call
         this%n_skip_check_res = that%n_skip_check_res
         this%export_convergence = that%export_convergence
         this%export_heavy = that%export_heavy
         this%exit_loop = that%exit_loop
       end subroutine

       subroutine delete_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         this%un = 0
         call delete(this%dir)
         call delete(this%name)
         this%iter_max = 0
         this%tol_abs = 0.0_cp
         this%tol_rel = 0.0_cp
         this%iter_total = 0
         this%iter_per_call = 0
         this%n_skip_check_res = 0
         this%export_convergence = .false.
         this%export_heavy = .false.
         this%exit_loop = .false.
       end subroutine

       subroutine display_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un                 = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_max           = ',this%iter_max
         write(un,*) 'tol_abs            = ',this%tol_abs
         write(un,*) 'tol_rel            = ',this%tol_rel
         write(un,*) 'iter_total         = ',this%iter_total
         write(un,*) 'iter_per_call      = ',this%iter_per_call
         write(un,*) 'n_skip_check_res   = ',this%n_skip_check_res
         write(un,*) 'export_convergence = ',this%export_convergence
         write(un,*) 'export_heavy       = ',this%export_heavy
         write(un,*) 'exit_loop          = ',this%exit_loop
       end subroutine

       subroutine display_short_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un                 = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_max           = ',this%iter_max
         write(un,*) 'tol_abs            = ',this%tol_abs
         write(un,*) 'tol_rel            = ',this%tol_rel
         write(un,*) 'iter_total         = ',this%iter_total
         write(un,*) 'iter_per_call      = ',this%iter_per_call
         write(un,*) 'n_skip_check_res   = ',this%n_skip_check_res
         write(un,*) 'export_convergence = ',this%export_convergence
         write(un,*) 'export_heavy       = ',this%export_heavy
         write(un,*) 'exit_loop          = ',this%exit_loop
       end subroutine

       subroutine print_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un                  = ';write(un,*) this%un
         call export(this%dir,un)
         call export(this%name,un)
         write(un,*) 'iter_max            = ';write(un,*) this%iter_max
         write(un,*) 'tol_abs             = ';write(un,*) this%tol_abs
         write(un,*) 'tol_rel             = ';write(un,*) this%tol_rel
         write(un,*) 'iter_total          = ';write(un,*) this%iter_total
         write(un,*) 'iter_per_call       = ';write(un,*) this%iter_per_call
         write(un,*) 'n_skip_check_res    = ';write(un,*) this%n_skip_check_res
         write(un,*) 'export_convergence  = ';write(un,*) this%export_convergence
         write(un,*) 'export_heavy        = ';write(un,*) this%export_heavy
         write(un,*) 'exit_loop           = ';write(un,*) this%exit_loop
       end subroutine

       subroutine import_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%un
         call import(this%dir,un)
         call import(this%name,un)
         read(un,*); read(un,*) this%iter_max
         read(un,*); read(un,*) this%tol_abs
         read(un,*); read(un,*) this%tol_rel
         read(un,*); read(un,*) this%iter_total
         read(un,*); read(un,*) this%iter_per_call
         read(un,*); read(un,*) this%n_skip_check_res
         read(un,*); read(un,*) this%export_convergence
         read(un,*); read(un,*) this%export_heavy
         read(un,*); read(un,*) this%exit_loop
       end subroutine

       subroutine display_wrap_iter_solver_params(this,dir,name)
         implicit none
         type(iter_solver_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_iter_solver_params(this,dir,name)
         implicit none
         type(iter_solver_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_iter_solver_params(this,dir,name)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       end module