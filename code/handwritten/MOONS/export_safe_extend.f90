       module export_safe_extend_mod
       use export_safe_mod
       use current_precision_mod
       implicit none

       private
       public :: export_safe
       public :: init,delete,display,export,import,print
       public :: update

       interface init;      module procedure init_ES;         end interface
       interface update;    module procedure update_ES;       end interface

       contains

       subroutine init_ES(ES,export_period_sec)
         implicit none
         type(export_safe),intent(inout) :: ES
         real(cp),intent(in) :: export_period_sec
         ES%export_period_sec = export_period_sec
         ES%export_now = .false.
         ES%mod_period = 0.0_cp
         ES%mod_period_last = 0.0_cp
       end subroutine

       subroutine update_ES(ES,t_sec)
         implicit none
         type(export_safe),intent(inout) :: ES
         real(cp),intent(in) :: t_sec
         ES%mod_period_last = ES%mod_period
         ES%mod_period = mod(t_sec,ES%export_period_sec)
         ES%export_now = ES%mod_period/ES%mod_period_last.lt.1.0_cp
       end subroutine

       end module
