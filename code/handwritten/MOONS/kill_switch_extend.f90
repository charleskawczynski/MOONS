       module kill_switch_extend_mod
       use kill_switch_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: kill_switch
       public :: init,delete,export,import

       interface init;               module procedure init_KS;             end interface

       contains

       subroutine init_KS(KS,dir,name)
         implicit none
         type(kill_switch),intent(inout) :: KS
         character(len=*),intent(in) :: dir,name
         KS%terminate_loop = .false.
         call init(KS%dir,dir)
         call init(KS%name,name)
         call export(KS)
       end subroutine

       end module