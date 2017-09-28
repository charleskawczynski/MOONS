       module var_extend_mod
       use var_mod
       implicit none

       private
       public :: init_IC_BC
       public :: print_info

       interface init_IC_BC;  module procedure init_IC_BC_V;  end interface
       interface print_info;  module procedure print_info_V;  end interface
       interface display_info;module procedure display_info_V;end interface

       contains

       subroutine init_IC_BC_V(V,IC,BC)
         implicit none
         type(var),intent(inout) :: V
         integer,intent(in) :: IC,BC
         V%IC = IC
         V%BC = BC
       end subroutine

       subroutine display_info_V(V,un)
         implicit none
         type(var),intent(in) :: V
         integer,intent(in) :: un
         write(un,*) 'solve_method = ',V%SS%solve_method
         write(un,*) 'alpha        = ',V%MFP%alpha
         write(un,*) 'dt           = ',V%TMP%TS%dt
         write(un,*) 't            = ',V%TMP%t
         write(un,*) 'tol_rel      = ',V%ISP%EC%tol_rel
         write(un,*) 'tol_abs      = ',V%ISP%EC%tol_abs
         write(un,*) 'iter_max     = ',V%ISP%EC%iter_max
       end subroutine

       subroutine print_info_V(V)
         implicit none
         type(var),intent(in) :: V
         call display_info(V,6)
       end subroutine

       end module