       module unit_conversion_extend_mod
       use unit_conversion_mod
       use current_precision_mod
       implicit none

       private
       public :: init
       interface init;   module procedure init_uc;       end interface

       contains

       subroutine init_uc(uc)
         implicit none
         type(unit_conversion),intent(inout) :: uc
         uc%days_per_year      = 365.2422_cp ! Includes leap year
         uc%seconds_per_second = 1.0_cp

         uc%seconds_per_minute = 60.0_cp
         uc%seconds_per_hour   = 60.0_cp*60.0_cp
         uc%seconds_per_day    = 60.0_cp*60.0_cp*24.0_cp
         uc%seconds_per_year   = uc%seconds_per_day*uc%days_per_year

         uc%minute_per_seconds = 1.0_cp/uc%seconds_per_minute
         uc%hour_per_seconds   = 1.0_cp/uc%seconds_per_hour
         uc%day_per_seconds    = 1.0_cp/uc%seconds_per_day
         uc%year_per_seconds   = 1.0_cp/uc%seconds_per_year
       end subroutine

       end module