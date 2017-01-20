       module solver_settings_mod
       implicit none

       private
       public :: solver_settings
       public :: init,delete,export,import,display,print

       public :: get_SS

       type solver_settings
         logical :: initialize = .false.
         logical :: solve = .false.
         logical :: restart = .false.
         integer :: solve_method = 0
       end type

       interface init;    module procedure init_SS;      end interface
       interface init;    module procedure init_copy_SS; end interface
       interface delete;  module procedure delete_SS;    end interface
       interface export;  module procedure export_SS;    end interface
       interface import;  module procedure import_SS;    end interface
       interface display; module procedure display_SS;   end interface
       interface print;   module procedure print_SS;     end interface

       interface get_SS;  module procedure get_SS_SS;    end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_SS(SS,initialize,solve,restart,solve_method)
         implicit none
         type(solver_settings),intent(inout) :: SS
         logical,intent(in) :: initialize,solve,restart
         integer,intent(in) :: solve_method
         SS%initialize = initialize
         SS%solve = solve
         SS%restart = restart
         SS%solve_method = solve_method
       end subroutine

       subroutine init_copy_SS(SS,SS_in)
         implicit none
         type(solver_settings),intent(inout) :: SS
         type(solver_settings),intent(in) :: SS_in
         SS%initialize = SS_in%initialize
         SS%solve = SS_in%solve
         SS%restart = SS_in%restart
         SS%solve_method = SS_in%solve_method
       end subroutine

       subroutine delete_SS(SS)
         implicit none
         type(solver_settings),intent(inout) :: SS
         SS%initialize = .false.
         SS%solve = .false.
         SS%restart = .false.
         SS%solve_method = 0
       end subroutine

       subroutine export_SS(SS,un)
         implicit none
         type(solver_settings),intent(in) :: SS
         integer,intent(in) :: un
         write(un,*) SS%initialize
         write(un,*) SS%solve
         write(un,*) SS%restart
         write(un,*) SS%solve_method
       end subroutine

       subroutine import_SS(SS,un)
         implicit none
         type(solver_settings),intent(inout) :: SS
         integer,intent(in) :: un
         read(un,*) SS%initialize
         read(un,*) SS%solve
         read(un,*) SS%restart
         read(un,*) SS%solve_method
       end subroutine

       subroutine display_SS(SS,un)
         implicit none
         type(solver_settings),intent(in) :: SS
         integer,intent(in) :: un
         write(un,*) 'initialize   = ',SS%initialize
         write(un,*) 'solve        = ',SS%solve
         write(un,*) 'restart      = ',SS%restart
         write(un,*) 'solve_method = ',SS%solve_method
       end subroutine

       subroutine print_SS(SS)
         implicit none
         type(solver_settings),intent(inout) :: SS
         call display(SS,6)
       end subroutine

       function get_SS_SS(initialize,solve,restart,solve_method) result(SS)
         implicit none
         type(solver_settings) :: SS
         logical,intent(in) :: initialize,solve,restart
         integer,intent(in) :: solve_method
         call init(SS,initialize,solve,restart,solve_method)
       end function

       end module