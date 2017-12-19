       module MOONS_simulate_crash_mod
       use MOONS_mod
       implicit none

       private
       public :: simulate_crash
       interface simulate_crash;   module procedure simulate_crash_MOONS;    end interface

       contains

       subroutine simulate_crash_MOONS(M)
         implicit none
         type(MOONS),intent(inout) :: M
         if (M%C%SP%FCL%simulate_crash) then
         if (M%C%SP%coupled%n_step.eq.447) then ! crash simulator
           stop 'Done in MOONS_solver'
         endif
         endif
         if (M%C%SP%FCL%restart_simulated_crash) then
         if (M%C%SP%coupled%n_step.eq.947) then ! after crash simulator
           stop 'Done in MOONS_solver'
         endif
         endif
       end subroutine
       end module