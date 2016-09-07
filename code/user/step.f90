       module step_mod
       implicit none
       private
       public :: step
       public :: init,delete

       interface init;      module procedure init_step;      end interface
       interface delete;    module procedure delete_step;    end interface

       type step
         logical :: this,next
       end type

       contains

       subroutine init_step(s)
         implicit none
         type(step),intent(inout) :: s
         call delete(s)
       end subroutine

       subroutine delete_step(s)
         implicit none
         type(step),intent(inout) :: s
         s%this = .false.
         s%next = .false.
       end subroutine

       end module