      module IO_tools_mod
      use current_precision_mod
      use string_mod
      implicit none

      private

      public :: new_and_open
      public :: openToRead,openToAppend
      public :: close_and_message

      character(len=4),parameter :: dot_dat = '.dat'

      contains

      function new_and_open(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU
        type(string) :: s
        call init(s,dir//name//dot_dat)
        NU = newUnit()
        open(NU,file=str(s),pad='YES')
        call delete(s)
      end function

      function openToRead(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        type(string) :: s
        call init(s,dir//name//dot_dat)
        NU = newUnit()
        inquire(file=str(s),number=NU,exist=ex,opened=op,action=act)

        if (.not.op) then
          NU = newUnit()
        endif
        if (ex) then
          open(NU,file=str(s), status = 'old', action = 'read',iostat=ok)
        else
          write(*,*) 'The file ' // str(s) // ' does not exist. Terminating execution.'; stop
        endif
        if (ok.ne.0) then
          write(*,*) 'The file ' // str(s) // ' was not opened successfully.'; stop
        endif
        call delete(s)
      end function

      function openToAppend(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        type(string) :: s
        NU = newUnit()
        call init(s,dir//name//dot_dat)
        inquire(file=str(s),number=NU,exist=ex,opened=op,action=act)

        NU = newUnit()
        if (.not.op) then
          if (ex) then
            open(NU,file=str(s),&
            status = 'old', action = 'readwrite',iostat=ok,position='append')
          else
            write(*,*) 'The file ' // str(s) // ' does not exist. Terminating execution.'; stop
          endif
          if (ok.ne.0) then
            write(*,*) 'The file ' // str(s) // ' was not opened successfully.'; stop
          endif
        endif
        call delete(s)
      end function

      subroutine close_and_message(u,dir,name)
        implicit none
        integer,intent(in) :: u
        character(len=*),intent(in) :: dir,name
        logical :: exist
        inquire(file=dir//name//'.dat', exist=exist)
        close(u)
        if (exist) then
          write(*,*) '+++ Closed file ' // dir // name
        else; write(*,*) '+++ Attempted to close non-existant file ' // dir // name
        endif
      end subroutine

      function newUnit() result(nu)
        implicit none
        ! local
        integer, parameter :: LUN_MIN=10, LUN_MAX=1000
        logical :: opened
        integer :: lun,nu
        ! begin
        nu=-1
        do lun=LUN_MIN,LUN_MAX
          inquire(unit=lun,opened=opened)
          if (.not. opened) then
            nu=lun
          exit
          endif
        enddo
      end function

      end module