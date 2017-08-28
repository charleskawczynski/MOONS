      module IO_check_mod
      use string_mod
      use inquire_funcs_mod
      implicit none

      private

      public :: check_file_exists,check_unit_exists
      public :: check_file_open,  check_unit_open
      public :: check_file_closed,check_unit_closed

      contains

      ! ************************* DEBUGGING *************************
      ! ************************* DEBUGGING *************************
      ! ************************* DEBUGGING *************************

      subroutine check_file_exists(dir,name,caller)
        implicit none
        character(len=*),intent(in) :: dir,name,caller
        if ((.not.file_exists(dir,name)).or.file_iostat_error(dir,name)) then
          write(*,*) 'Error: file does not exist in '//caller//' in IO_tools.f90'
          write(*,*) 'dir = ',dir
          write(*,*) 'name = ',name
          write(*,*) 'len(dir) = ',len(dir)
          write(*,*) 'len(name) = ',len(name)
          write(*,*) 'ex = ',file_exists(dir,name)
          write(*,*) 'ios = ',file_iostat(dir,name)
          stop 'Terminating execution.'
        endif
      end subroutine

      subroutine check_unit_exists(un,caller)
        implicit none
        integer,intent(in) :: un
        character(len=*),intent(in) :: caller
        if ((.not.unit_exists(un)).or.unit_iostat_error(un)) then
          write(*,*) 'Error: unit does not exist in '//caller//' in IO_tools.f90'
          write(*,*) 'ex = ',unit_exists(un)
          write(*,*) 'ios = ',unit_iostat(un)
          stop 'Terminating execution.'
        endif
      end subroutine

      subroutine check_file_open(dir,name,caller)
        implicit none
        character(len=*),intent(in) :: dir,name,caller
        if ((.not.file_open(dir,name)).or.file_iostat_error(dir,name)) then
          write(*,*) 'Error: file not open in '//caller//' in IO_tools.f90'
          write(*,*) 'dir = ',dir
          write(*,*) 'name = ',name
          write(*,*) 'len(dir) = ',len(dir)
          write(*,*) 'len(name) = ',len(name)
          write(*,*) 'ex = ',file_exists(dir,name)
          write(*,*) 'ios = ',file_iostat(dir,name)
          stop 'Terminating execution.'
        endif
      end subroutine

      subroutine check_unit_open(un,caller)
        implicit none
        integer,intent(in) :: un
        character(len=*),intent(in) :: caller
        if ((.not.unit_open(un)).or.unit_iostat_error(un)) then
          write(*,*) 'Error: unit not open in '//caller//' in IO_tools.f90'
          write(*,*) 'ex = ',unit_exists(un)
          write(*,*) 'ios = ',unit_iostat(un)
          stop 'Terminating execution.'
        endif
      end subroutine

      subroutine check_file_closed(dir,name,caller)
        implicit none
        character(len=*),intent(in) :: dir,name,caller
        if ((.not.file_closed(dir,name)).or.file_iostat_error(dir,name)) then
          write(*,*) 'Error: file not closed in '//caller//' in IO_tools.f90'
          write(*,*) 'dir = ',dir
          write(*,*) 'name = ',name
          write(*,*) 'len(dir) = ',len(dir)
          write(*,*) 'len(name) = ',len(name)
          write(*,*) 'ex = ',file_exists(dir,name)
          write(*,*) 'ios = ',file_iostat(dir,name)
          stop 'Terminating execution.'
        endif
      end subroutine

      subroutine check_unit_closed(un,caller)
        implicit none
        integer,intent(in) :: un
        character(len=*),intent(in) :: caller
        if ((.not.unit_closed(un)).or.unit_iostat_error(un)) then
          write(*,*) 'Error: unit not closed in '//caller//' in IO_tools.f90'
          write(*,*) 'ex = ',unit_exists(un)
          write(*,*) 'ios = ',unit_iostat(un)
          stop 'Terminating execution.'
        endif
      end subroutine

      end module