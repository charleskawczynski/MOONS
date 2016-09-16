       module time_with_units_mod
       use current_precision_mod
       use unit_conversion_mod
       implicit none

       private
       public :: time_with_units,init

       interface init;         module procedure init_clock;          end interface

       type time_with_units
         type(unit_conversion) :: uc
         real(cp) :: seconds
         real(cp) :: minutes
         real(cp) :: hours
         real(cp) :: days
         real(cp) :: years
       end type

       contains

      subroutine init_time(t,seconds)
        implicit none
        type(time_with_units),intent(inout) :: t
        call init(t%uc)
        t%seconds = seconds
        t%minutes = seconds/t%uc%seconds_per_minute
        t%hours   = seconds/t%uc%seconds_per_hour
        t%days    = seconds/t%uc%seconds_per_day
        t%years   = seconds/t%uc%seconds_per_year
      end subroutine

       end module