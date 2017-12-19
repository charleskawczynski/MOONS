       module clock_extend_mod
       use clock_mod
       use current_precision_mod
       implicit none

       private
       public :: init
       public :: tic,toc

       interface init;         module procedure init_clock;          end interface
       interface tic;          module procedure tic_clock;           end interface
       interface toc;          module procedure toc_clock;           end interface

       contains

       subroutine init_clock(c)
         implicit none
         type(clock),intent(inout) :: c
         call delete(c)
       end subroutine

       subroutine tic_clock(c)
         implicit none
         type(clock),intent(inout) :: c
         call system_clock(c%i_start,c%count_rate)
         c%t_start = real(c%i_start,cp)

         call cpu_time(c%t_start_computational)
       end subroutine

       subroutine toc_clock(c)
         implicit none
         type(clock),intent(inout) :: c
         call system_clock(c%i_stop,c%count_rate)
         c%t_stop = real(c%i_stop,cp)
         c%t_elapsed = (c%t_stop - c%t_start)/real(c%count_rate,cp)

         call cpu_time(c%t_stop_computational)
         c%t_elapsed_computational = (c%t_stop_computational - c%t_start_computational)
       end subroutine

       end module