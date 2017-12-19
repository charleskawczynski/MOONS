       module solver_settings_extend_mod
       use solver_settings_mod
       implicit none

       private
       public :: init
       public :: get_SS

       interface init;    module procedure init_SS;      end interface
       interface get_SS;  module procedure get_SS_SS;    end interface

       contains

       subroutine init_SS(SS,initialize,solve,restart,prescribed_BCs,solve_method)
         implicit none
         type(solver_settings),intent(inout) :: SS
         logical,intent(in) :: initialize,solve,restart,prescribed_BCs
         integer,intent(in) :: solve_method
         SS%initialize = initialize
         SS%solve = solve
         SS%restart = restart
         SS%solve_method = solve_method
         SS%prescribed_BCs = prescribed_BCs
       end subroutine

       function get_SS_SS(initialize,solve,restart,prescribed_BCs,solve_method) result(SS)
         implicit none
         type(solver_settings) :: SS
         logical,intent(in) :: initialize,solve,restart,prescribed_BCs
         integer,intent(in) :: solve_method
         call init(SS,initialize,solve,restart,prescribed_BCs,solve_method)
       end function

       end module