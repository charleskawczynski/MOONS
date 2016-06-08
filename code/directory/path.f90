      module path_mod
      use IO_tools_mod
      use string_mod
      implicit none

      private
      public :: path
      public :: init,make,str,rel,full,delete
      
      interface init;     module procedure init_P;     end interface
      interface make;     module procedure make_P;     end interface
      interface rel;      module procedure rel_P;      end interface
      interface str;      module procedure str_P;      end interface
      interface full;     module procedure full_P;     end interface
      interface delete;   module procedure delete_P;   end interface

      type path
        type(string) :: a,r ! a (Absolute), r (Relative)
      end type

      contains

      subroutine init_P(P,root,rel,PS)
        implicit none
        type(path),intent(inout) :: P
        character(len=*),intent(in) :: root,rel,PS
        call init(P%a,root//rel//PS)
        call init(P%r,rel//PS)
      end subroutine

      subroutine make_P(P)
        implicit none
        type(path),intent(in) :: P
        call make_dir(str(P%a))
      end subroutine

      function str_P(P) result (s)
        implicit none
        type(path),intent(in) :: P
        character(len=len(P%r)) :: s
        s = str(P%r)
      end function

      function rel_P(P) result (s)
        implicit none
        type(path),intent(in) :: P
        character(len=len(P%r)) :: s
        s = str(P%r)
      end function

      function full_P(P) result (s)
        implicit none
        type(path),intent(in) :: P
        character(len=len(P%a)) :: s
        s = str(P%a)
      end function

      subroutine delete_P(P)
        implicit none
        type(path),intent(inout) :: P
        call delete(P%a)
        call delete(P%r)
      end subroutine

      end module