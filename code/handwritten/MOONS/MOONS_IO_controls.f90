       module MOONS_IO_controls_mod
       use MOONS_mod
       implicit none

       private
       public :: MOONS_export_controls
       public :: MOONS_import_controls
       interface MOONS_export_controls;  module procedure export_controls_MOONS;  end interface
       interface MOONS_import_controls;  module procedure import_controls_MOONS;  end interface

       contains

       subroutine export_controls_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         write(*,*) 'GH 1'; call export_structured(M%C%SP%DP)
         write(*,*) 'GH 2'; call export_structured(M%GE%mom%PCG_U%ISP%EC)
         write(*,*) 'GH 3'; call export_structured(M%GE%mom%PCG_P%ISP%EC)
         write(*,*) 'GH 4'; call export_structured(M%GE%ind%PCG_B%ISP%EC)
         write(*,*) 'GH 5'; call export_structured(M%GE%ind%PCG_B%ISP%EC)
         write(*,*) 'GH 6'; call export_structured(M%GE%ind%PCG_cleanB%ISP%EC)
         write(*,*) 'GH 7'; call export_exit_criteria(M%C%SP%VS)
         write(*,*) 'GH 8'; call export_TMP_dt(M%C%SP%VS)
         write(*,*) 'GH 9'; call export_structured(M%C%SP%coupled%TS)
         write(*,*) 'GH 10'; if (M%C%SP%SCP%couple_time_steps) call couple_time_step(M%C%SP%VS,M%C%SP%coupled)
         write(*,*) 'GH 11'; call export_structured(M%C%EN)
       end subroutine

       subroutine import_controls_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         call import_structured(M%C%SP%DP)
         call import_structured(M%GE%mom%PCG_U%ISP%EC)
         call import_structured(M%GE%mom%PCG_P%ISP%EC)
         call import_structured(M%GE%ind%PCG_B%ISP%EC)
         call import_structured(M%GE%ind%PCG_B%ISP%EC)
         call import_structured(M%GE%ind%PCG_cleanB%ISP%EC)
         call import_exit_criteria(M%C%SP%VS)
         call import_TMP_dt(M%C%SP%VS)
         call import_structured(M%C%SP%coupled%TS)
         if (M%C%SP%SCP%couple_time_steps) call couple_time_step(M%C%SP%VS,M%C%SP%coupled)
         call import_structured(M%C%EN)
       end subroutine

       end module