      module string_fmt_mod
      use string_mod
      implicit none

      private
      public :: string_fmt
      public :: init,delete
      interface init;     module procedure init_string_fmt;   end interface
      interface delete;   module procedure delete_string_fmt; end interface

       type string_fmt
         type(string) :: s
         type(string) :: fmt
       end type

      contains

      subroutine init_string_fmt(DS,s)
        implicit none
        type(string_fmt),intent(inout) :: DS
        character(len=*),intent(in) :: s
        call init(DS%s,s)
        call init(DS%fmt,'(A'//int2str(str(DS%s))//')')
      end subroutine

      subroutine init_string_fmt(DS)
        implicit none
        type(string_fmt),intent(inout) :: DS
        call delete(DS%fmt)
        call delete(DS%s)
      end subroutine

      end module