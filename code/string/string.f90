      module string_mod
      implicit none
      ! Implimentation:

      ! program test_string
      ! use string_mod
      ! implicit none
      ! type(string) :: s
      ! call init(s,'This is');            write(*,*) 'string = ',str(s)
      ! call append(s,' a variable');      write(*,*) 'string = ',str(s)
      ! call append(s,' sized string!');   write(*,*) 'string = ',str(s)
      ! call compress(s);                  write(*,*) 'string, no spaces = ',str(s)
      ! call delete(s)
      ! end program

      private
      public :: string
      public :: init,delete
      public :: get_str,str ! str does not require length
      public :: len
      public :: compress,append
      public :: print,export

      interface init;      module procedure init_size;            end interface
      interface init;      module procedure init_string;          end interface
      interface init;      module procedure init_copy;            end interface
      interface append;    module procedure app_string_char;      end interface
      interface append;    module procedure app_string_string;    end interface
      interface compress;  module procedure compress_string;      end interface
      interface len;       module procedure str_len_string;       end interface
      interface str;       module procedure get_str_short;        end interface
      interface get_str;   module procedure get_str_string;       end interface
      interface delete;    module procedure delete_string;        end interface
      interface print;     module procedure print_string;         end interface
      interface export;    module procedure export_string;        end interface

      type char
        private
        character(len=1) :: c
      end type

      type string
        private
        type(char),dimension(:),allocatable :: s ! string
        integer :: n                             ! string length
      end type

      contains

      subroutine init_size(st,n)
        implicit none
        type(string),intent(inout) :: st
        integer,intent(in) :: n
        if (n.lt.1) stop 'Error: string must be of size > 1 in string.f90'
        call delete(st)
        allocate(st%s(n))
        st%n = n
      end subroutine

      subroutine init_string(st,s)
        implicit none
        type(string),intent(inout) :: st
        character(len=*),intent(in) :: s
        integer :: i
        call init(st,len(s))
        do i=1,st%n
          call init_char(st%s(i),s(i:i))
        enddo
      end subroutine

      subroutine init_copy(a,b)
        implicit none
        type(string),intent(inout) :: a
        type(string),intent(in) :: b
        integer :: i
        call check_allocated(b,'init_copy')
        call init(a,b%n)
        do i=1,b%n
        call init_copy_char(a%s(i),b%s(i))
        enddo
        a%n = b%n
      end subroutine

      subroutine check_allocated(st,s)
        implicit none
        type(string),intent(in) :: st
        character(len=*),intent(in) :: s
        if (.not.allocated(st%s)) then
          write(*,*) 'Error: string must be allocated in '//s//' in string.f90'
        endif
      end subroutine

      subroutine delete_string(st)
        implicit none
        type(string),intent(inout) :: st
        if (allocated(st%s)) deallocate(st%s)
        st%n = 0
      end subroutine

      subroutine print_string(st)
        implicit none
        type(string),intent(in) :: st
        call export(st,6)
      end subroutine

      subroutine export_string(st,un)
        implicit none
        type(string),intent(in) :: st
        integer,intent(in) :: un
        integer :: i
        call check_allocated(st,'export_string')
        do i=1,st%n
          write(un,'(A1)',advance='no') st%s(i)%c
        enddo
      end subroutine

      subroutine app_string_char(st,s)
        implicit none
        type(string),intent(inout) :: st
        character(len=*),intent(in) :: s
        type(string) :: temp
        integer :: i,n
        n = len(s)
        call init(temp,st)
        call init(st,temp%n+n)
        do i=1,temp%n
          call init_copy_char(st%s(i),temp%s(i))
        enddo
        do i=1,n
          call init_char(st%s(temp%n+i),s(i:i))
        enddo
        call delete(temp)
      end subroutine

      subroutine app_string_string(a,b)
        implicit none
        type(string),intent(inout) :: a
        type(string),intent(in) :: b
        call append(a,str(b))
      end subroutine

      subroutine compress_string(st)
        implicit none
        type(string),intent(inout) :: st
        type(string) :: temp
        integer :: i,n_spaces
        if (st%n.lt.1) stop 'Error: input string must be > 1 in string.f90'
        n_spaces = 0
        do i=1,st%n
          if (st%s(i)%c.eq.' ') n_spaces = n_spaces + 1
        enddo
        call init(temp,st%n-n_spaces)
        if (temp%n.lt.1) stop 'Error: output string must be > 1 in string.f90'
        do i=1,temp%n
          if (st%s(i)%c.ne.' ') temp%s(i)%c = st%s(i)%c
        enddo
        call init(st,temp)
        call delete(temp)
      end subroutine

      function get_str_short(st) result(str)
        type(string),intent(in) :: st
        character(len=st%n) :: str
        str = get_str_string(st,st%n)
      end function

      function str_len_string(s) result(n)
        type(string),intent(in) :: s
        integer :: n
        n = s%n
      end function

      function get_str_string(st,n) result(str)
        implicit none
        type(string),intent(in) :: st
        integer,intent(in) :: n
        character(len=n) :: str
        integer :: i
        call check_allocated(st,'get_str_string')
        do i=1,st%n
          str(i:i) = st%s(i)%c
        enddo
      end function

      subroutine init_char(CH,c)
        implicit none
        type(char),intent(inout) :: CH
        character(len=1),intent(in) :: c
        CH%c = c
      end subroutine

      subroutine init_copy_char(a,b)
        implicit none
        type(char),intent(inout) :: a
        type(char),intent(in) :: b
        a%c = b%c
      end subroutine

      end module