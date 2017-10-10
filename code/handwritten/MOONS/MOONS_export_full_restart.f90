       module MOONS_export_full_restart_mod
       use string_mod
       use path_extend_mod
       use MOONS_mod
       implicit none

       private
       public :: MOONS_export_full_restart
       public :: MOONS_prep_full_restart
       interface MOONS_export_full_restart; module procedure MOONS_export_full_restart_M; end interface
       interface MOONS_prep_full_restart; module procedure MOONS_prep_full_restart_M; end interface

       contains

       subroutine MOONS_prep_full_restart_M(M)
         implicit none
         type(MOONS),intent(inout) :: M
         call export_structured(M%C,str(M%C%DT%restart))
         call export_structured(M%GE,str(M%C%DT%restart))
         call alternate_restart_path_MOONS(M)
         call set_IO_dir(M%C,str(M%C%DT%restart))
         call set_IO_dir(M%GE,str(M%C%DT%restart))
         call make_IO_dir(M%C,str(M%C%DT%restart))
         call make_IO_dir(M%GE,str(M%C%DT%restart))
         call MOONS_export_full_restart(M)
       end subroutine

       subroutine MOONS_export_full_restart_M(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) 'Exporting entire MOONS restart to ',str(M%C%DT%restart)
         write(*,*) 'This may take a few minutes...'
         call export_structured(M%C,str(M%C%DT%restart))
         call export_structured(M%GE,str(M%C%DT%restart))
         call alternate_restart_path_MOONS(M)
         call set_IO_dir(M%C,str(M%C%DT%restart))
         call set_IO_dir(M%GE,str(M%C%DT%restart))
       end subroutine

       subroutine alternate_restart_path_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) 'alternating path: '
         write(*,*) 'From M%C%DT%restart = ',str(M%C%DT%restart)
         if (identical(M%C%DT%restart%relative,M%C%DT%restart1%relative)) then
           call init(M%C%DT%restart,M%C%DT%restart2)
         elseif (identical(M%C%DT%restart%relative,M%C%DT%restart2%relative)) then
           call init(M%C%DT%restart,M%C%DT%restart1)
         else
           write(*,*) 'Warning: alternate_restart_path_MOONS'
           write(*,*) 'is not alternating the restart path.'
         endif
         write(*,*) 'To   M%C%DT%restart = ',str(M%C%DT%restart)
       end subroutine

       end module