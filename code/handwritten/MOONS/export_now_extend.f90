       module export_now_extend_mod
       use export_now_mod
       use step_mod
       use step_extend_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: export_now
       public :: init,delete,display,export,import
       public :: update

       interface init;     module procedure init_EN;     end interface
       interface update;   module procedure update_EN;   end interface

       contains

       subroutine init_EN(EN,dir,name)
         implicit none
         type(export_now),intent(inout) :: EN
         character(len=*),intent(in) :: dir,name
         call delete(EN%T)
         call delete(EN%rho)
         call delete(EN%U)
         call delete(EN%B)
         call delete(EN%all)
         EN%any_next = .false.
         call init(EN%dir,dir)
         call init(EN%name,name)
       end subroutine

       subroutine update_EN(EN,export_safe)
         implicit none
         type(export_now),intent(inout) :: EN
         logical,intent(in) :: export_safe
         if (export_safe) EN%all%next = .true.
         EN%any_next = any((/EN%T%next,EN%rho%next,EN%U%next,EN%B%next,EN%all%next/))
         EN%any_now  = any((/EN%T%this,EN%rho%this,EN%U%this,EN%B%this,EN%all%this/))
         call update(EN%T)
         call update(EN%rho)
         call update(EN%U)
         call update(EN%B)
         call update(EN%all)
       end subroutine

       end module