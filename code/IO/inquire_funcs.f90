      module inquire_funcs_mod
      use string_mod
      implicit none

      private

      public :: file_exists,unit_exists
      public :: file_open,  unit_open
      public :: file_closed,unit_closed
      public :: file_iostat_error,unit_iostat_error
      public :: file_iostat,unit_iostat

      character(len=4),parameter :: dot_dat = '.dat'

      contains

      function file_exists(dir,name) result(ex)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(string) :: s
        logical :: ex
        call init(s,dir//name//dot_dat)
        inquire(file=str(s),exist=ex)
        call delete(s)
      end function

      function unit_exists(un) result(ex)
        implicit none
        integer,intent(in) :: un
        logical :: ex
        inquire(unit=un,exist=ex)
      end function

      function file_open(dir,name) result(op)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(string) :: s
        logical :: op
        call init(s,dir//name//dot_dat)
        inquire(file=str(s),opened=op)
        call delete(s)
      end function

      function unit_open(un) result(op)
        implicit none
        integer,intent(in) :: un
        logical :: op
        inquire(unit=un,opened=op)
      end function

      function file_closed(dir,name) result(not_op)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(string) :: s
        logical :: op,not_op
        call init(s,dir//name//dot_dat)
        inquire(file=str(s),opened=op)
        call delete(s)
        not_op = .not.op
      end function

      function unit_closed(un) result(not_op)
        implicit none
        integer,intent(in) :: un
        logical :: op,not_op
        inquire(unit=un,opened=op)
        not_op = .not.op
      end function

      function file_iostat_error(dir,name) result(L)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(string) :: s
        logical :: L
        integer :: i
        call init(s,dir//name//dot_dat)
        inquire(file=str(s),iostat=i)
        call delete(s)
        L = .not.i.eq.0
      end function

      function unit_iostat_error(un) result(L)
        implicit none
        integer,intent(in) :: un
        logical :: L
        integer :: i
        inquire(unit=un,iostat=i)
        L = .not.i.eq.0
      end function

      function file_iostat(dir,name) result(i)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(string) :: s
        integer :: i
        call init(s,dir//name//dot_dat)
        inquire(file=str(s),iostat=i)
        call delete(s)
      end function

      function unit_iostat(un) result(i)
        implicit none
        integer,intent(in) :: un
        integer :: i
        inquire(unit=un,iostat=i)
      end function

      end module