       module step_extend_mod
       use step_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: step
       public :: init,delete,display,export,import
       public :: update
       interface update;   module procedure update_step;   end interface

       contains

       subroutine update_step(s)
         implicit none
         type(step),intent(inout) :: s
         s%this = s%next
         s%next = .false.
       end subroutine

       end module