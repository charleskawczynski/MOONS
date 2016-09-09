      module IO_tools_mod
      use current_precision_mod
      use string_mod
      use inquire_funcs_mod
      use IO_check_mod
      implicit none

      private

      public :: new_and_open,close_and_message
      public :: rewind_file,rewind_unit
      public :: open_to_read,open_to_write,open_to_append

      interface open_to_read;    module procedure open_to_read_file;    end interface
      interface open_to_read;    module procedure open_to_read_unit;    end interface
      interface open_to_write;   module procedure open_to_write_file;   end interface
      interface open_to_write;   module procedure open_to_write_unit;   end interface
      interface open_to_append;  module procedure open_to_app_file;     end interface
      interface open_to_append;  module procedure open_to_app_unit;     end interface

      character(len=4),parameter :: dot_dat = '.dat'

      contains

      ! ************************* NEW UNIT *************************
      ! ************************* NEW UNIT *************************
      ! ************************* NEW UNIT *************************

      function new_unit() result(nu)
        implicit none
        integer,parameter :: lun_min=10,lun_max=1000
        logical :: opened
        integer :: lun,nu
        nu=-1
        do lun=lun_min,lun_max
          if (.not.unit_open(lun)) then; nu=lun; exit; endif
        enddo
      end function

      function new_and_open(dir,name) result(un)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: un
        type(string) :: s
        call init(s,dir//name//dot_dat)
        un = new_unit()
        open(un,file=str(s),pad='YES')
        call delete(s)
      end function

      ! ************************* CLOSE UNIT *************************
      ! ************************* CLOSE UNIT *************************
      ! ************************* CLOSE UNIT *************************

      subroutine close_and_message(un,dir,name)
        implicit none
        integer,intent(in) :: un
        character(len=*),intent(in) :: dir,name
#ifdef _DEBUG_IO_TOOLS_
        call check_unit_exists(un,'close_and_message')
        call check_unit_open(un,'close_and_message')
#endif
        close(un)
        ! #ifndef _OPTIMIZE_IO_TOOLS_
        write(*,*) '+++ Closed file ' // dir // name
        ! #endif
      end subroutine

      ! **************************** REWIND ****************************
      ! **************************** REWIND ****************************
      ! **************************** REWIND ****************************

      subroutine rewind_file(dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(string) :: s
        call init(s,dir//name//dot_dat)
#ifdef _DEBUG_IO_TOOLS_
        call check_file_exists(dir,name,'rewind_file')
        call check_file_open(dir,name,'rewind_file')
#endif
        rewind(get_file_unit(dir,name))
        call delete(s)
      end subroutine

      subroutine rewind_unit(un)
        implicit none
        integer,intent(in) :: un
#ifdef _DEBUG_IO_TOOLS_
        call check_unit_exists(un,'rewind_unit')
        call check_unit_open(un,'rewind_unit')
#endif
        rewind(un)
      end subroutine

      ! *************************** OPEN UNIT ***************************
      ! *************************** OPEN UNIT ***************************
      ! *************************** OPEN UNIT ***************************

      ! ------------------------------------------- unit

      subroutine open_to_read_unit(un)
        implicit none
        integer,intent(in) :: un
#ifdef _DEBUG_IO_TOOLS_
        call check_unit_exists(un,'open_to_read_unit')
        call check_unit_open(un,'open_to_read_unit')
#endif
        open(unit=un,status='old',action='read')
      end subroutine

      subroutine open_to_write_unit(un)
        implicit none
        integer,intent(in) :: un
#ifdef _DEBUG_IO_TOOLS_
        call check_unit_exists(un,'open_to_write_unit')
        call check_unit_open(un,'open_to_write_unit')
#endif
        open(unit=un,status='old',action='write')
      end subroutine

      subroutine open_to_app_unit(un)
        implicit none
        integer,intent(in) :: un
#ifdef _DEBUG_IO_TOOLS_
        call check_unit_exists(un,'open_to_app_unit')
        call check_unit_open(un,'open_to_app_unit')
#endif
        open(unit=un,status='old',action='write',position='append')
      end subroutine

      ! ------------------------------------------- file

      subroutine open_to_read_file(dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
#ifdef _DEBUG_IO_TOOLS_
        call check_file_exists(dir,name,'open_to_read_file')
        call check_file_open(dir,name,'open_to_read_file')
#endif
        open(unit=get_file_unit(dir,name),status='old',action='read')
        call delete(s)
      end subroutine

      subroutine open_to_write_file(dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
#ifdef _DEBUG_IO_TOOLS_
        call check_file_exists(dir,name,'open_to_write_file')
        call check_file_open(dir,name,'open_to_write_file')
#endif
        open(unit=get_file_unit(dir,name),status='old',action='write')
      end subroutine

      subroutine open_to_app_file(dir,name)
        implicit none
        character(len=*),intent(in) :: dir,name
#ifdef _DEBUG_IO_TOOLS_
        call check_file_exists(dir,name,'open_to_app_file')
        call check_file_open(dir,name,'open_to_app_file')
#endif
        open(unit=get_file_unit(dir,name),status='old',action='write',position='append')
      end subroutine

      end module