       module MOONS_export_full_restart_mod
       use string_mod
       use path_extend_mod
       use MOONS_mod
       implicit none

       private
       public :: MOONS_export_full_restart
       interface MOONS_export_full_restart; module procedure MOONS_export_full_restart_M; end interface

       contains

       subroutine MOONS_export_full_restart_M(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) 'Exporting restart in MOONS_solver, this may take a few minutes...'
         call export_structured(M%C)
         call export_structured(M%GE)
         call alternate_restart_path_MOONS(M)
       end subroutine

       subroutine alternate_restart_path_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         if (identical(M%C%DT%restart%relative,M%C%DT%restart1%relative)) then
           call init(M%C%DT%restart,M%C%DT%restart2)
         elseif (identical(M%C%DT%restart%relative,M%C%DT%restart2%relative)) then
           call init(M%C%DT%restart,M%C%DT%restart1)
         endif
       end subroutine

       end module