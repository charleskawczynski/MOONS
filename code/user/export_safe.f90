       module export_safe_mod
       use current_precision_mod
       implicit none

       private
       public :: export_safe
       public :: init,delete,display,export,import,print
       public :: update

       type export_safe
         real(cp) :: export_period_sec
         real(cp) :: mod_period
         real(cp) :: mod_period_last
         logical :: export_now
       end type

       interface init;      module procedure init_ES;         end interface
       interface delete;    module procedure delete_ES;       end interface
       interface print;     module procedure print_ES;        end interface
       interface display;   module procedure display_ES;      end interface
       interface export;    module procedure export_ES;       end interface
       interface import;    module procedure import_ES;       end interface

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

       subroutine delete_ES(ES)
         implicit none
         type(export_safe),intent(inout) :: ES
         ES%export_period_sec = 10.0_cp**(8.0_cp)
         ES%export_now = .false.
         ES%mod_period = 0.0_cp
         ES%mod_period_last = 0.0_cp
       end subroutine

       subroutine display_ES(ES,un)
         implicit none
         type(export_safe),intent(in) :: ES
         integer,intent(in) :: un
         write(un,*) 'export_period_sec = ',ES%export_period_sec
         write(un,*) 'export_now        = ',ES%export_now
         write(un,*) 'mod_period        = ',ES%mod_period
         write(un,*) 'mod_period_last   = ',ES%mod_period_last
       end subroutine

       subroutine print_ES(ES)
         implicit none
         type(export_safe),intent(in) :: ES
         call display(ES,6)
       end subroutine

       subroutine export_ES(ES,un)
         implicit none
         type(export_safe),intent(in) :: ES
         integer,intent(in) :: un
         write(un,*) 'export_period_sec = '; write(un,*) ES%export_period_sec
         write(un,*) 'export_now        = '; write(un,*) ES%export_now
         write(un,*) 'mod_period        = '; write(un,*) ES%mod_period
         write(un,*) 'mod_period_last   = '; write(un,*) ES%mod_period_last
       end subroutine

       subroutine import_ES(ES,un)
         implicit none
         type(export_safe),intent(inout) :: ES
         integer,intent(in) :: un
         read(un,*); read(un,*) ES%export_period_sec
         read(un,*); read(un,*) ES%export_now
         read(un,*); read(un,*) ES%mod_period
         read(un,*); read(un,*) ES%mod_period_last
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
