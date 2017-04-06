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
      public :: init,delete,display,print,export,import ! Essentials

      public :: write_formatted
      public :: get_str,str ! str does not require length
      public :: len,match,match_index
      public :: compress,append,prepend
      public :: get_char,set_char
      public :: remove_element
      public :: identical

      interface init;                 module procedure init_size;                      end interface
      interface init;                 module procedure init_string;                    end interface
      interface init;                 module procedure init_copy;                      end interface
      interface delete;               module procedure delete_string;                  end interface
      interface display;              module procedure display_string;                 end interface
      interface print;                module procedure print_string;                   end interface
      interface export;               module procedure export_string;                  end interface
      interface import;               module procedure import_string;                  end interface

      interface write_formatted;      module procedure write_formatted_string;         end interface

      interface append;               module procedure app_string_char;                end interface
      interface append;               module procedure app_string_string;              end interface
      interface prepend;              module procedure prep_string_char;               end interface
      interface prepend;              module procedure prep_string_string;             end interface
      interface compress;             module procedure compress_string;                end interface
      interface len;                  module procedure str_len_string;                 end interface
      interface str;                  module procedure get_str_short;                  end interface
      interface get_str;              module procedure get_str_string;                 end interface
      interface match;                module procedure substring_in_string;            end interface
      interface match_index;          module procedure index_substring_in_string;      end interface
      interface get_char;             module procedure get_char_string;                end interface
      interface set_char;             module procedure set_char_string;                end interface
      interface remove_element;       module procedure remove_element_string;          end interface
      interface identical;            module procedure identical_string_string;        end interface
      interface identical;            module procedure identical_string_char;          end interface

      interface insist_allocated;     module procedure insist_allocated_string;        end interface

      type char
        private
        character(len=1) :: c
      end type

      type string
        private
        type(char),dimension(:),allocatable :: s ! string
        integer :: n = 0                         ! string length
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
        call insist_allocated(b,'init_copy')
        call init(a,b%n)
        do i=1,b%n
        call init_copy_char(a%s(i),b%s(i))
        enddo
        a%n = b%n
      end subroutine

      subroutine delete_string(st)
        implicit none
        type(string),intent(inout) :: st
        if (allocated(st%s)) deallocate(st%s)
        st%n = 0
      end subroutine

      subroutine display_string(st,un)
        implicit none
        type(string),intent(in) :: st
        integer,intent(in) :: un
        call export(st,un)
      end subroutine

      subroutine print_string(st)
        implicit none
        type(string),intent(in) :: st
        call display(st,6)
        write(6,*) ''
      end subroutine

      subroutine export_string(st,un)
        implicit none
        type(string),intent(in) :: st
        integer,intent(in) :: un
        call insist_allocated(st,'export_string')
        write(un,*) str(st)
      end subroutine

      subroutine import_string(s,un)
        implicit none
        type(string),intent(inout) :: s
        integer,intent(in) :: un
        character(len=1) :: c
        logical :: first_iteration,continue_loop
        integer :: ReadCode
        ReadCode = 0; continue_loop = .true.
        call delete(s); first_iteration = .true.
        do while (continue_loop)
          if (ReadCode.eq.0) then
            read(un,'(A)',advance='no',iostat=ReadCode) c
            if (first_iteration) then; call init(s,c); else; call append(s,c); endif
          else; continue_loop = .false.; exit
          endif; first_iteration = .false.
        enddo
        if (s%s(s%n)%c.eq.' ') call remove_element(s,s%n)
        if (s%s(1)%c.eq.' ') call remove_element(s,1)
      end subroutine

      subroutine write_formatted_string(s,un)
        implicit none
        integer,intent(in) :: un
        type(string),intent(in) :: s
        write(un,'('//int2str(len(s))//'A)') str(s)
      end subroutine

      function int2Str(i) result(s)
        implicit none
        integer,intent(in) :: i
        character(len=15) :: s
        write(s,'(I15.15)') i
        s = trim(adjustl(s))
      end function

      ! **********************************************************
      ! **********************************************************
      ! **********************************************************

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

      subroutine prep_string_char(a,b)
        implicit none
        type(string),intent(inout) :: a
        character(len=*),intent(in) :: b
        type(string) :: temp
        call init(temp,b)
        call append(temp,a)
        call init(a,temp)
        call delete(temp)
      end subroutine

      subroutine prep_string_string(a,b)
        implicit none
        type(string),intent(inout) :: a
        type(string),intent(in) :: b
        call prepend(a,str(b))
      end subroutine

      subroutine compress_string(st)
        implicit none
        type(string),intent(inout) :: st
        type(string) :: temp
        integer :: i,n_spaces,k
        if (st%n.lt.1) stop 'Error: input string must be > 1 in string.f90'
        n_spaces = 0
        do i=1,st%n
          if (st%s(i)%c.eq.' ') n_spaces = n_spaces + 1
        enddo
        if (n_spaces.ne.0) then
          if (st%n-n_spaces.lt.1) stop 'Error: only spaces in string in compress_string in string.f90'
          call init(temp,st%n-n_spaces)
          k = 0
          do i=1,st%n
            if (st%s(i)%c.ne.' ') then
              temp%s(i-k)%c = st%s(i)%c
            else; k = k+1
            endif
          enddo
          call init(st,temp)
          call delete(temp)
        endif
      end subroutine

      subroutine remove_element_string(st,i)
        implicit none
        type(string),intent(inout) :: st
        integer,intent(in) :: i
        type(string) :: temp
        integer :: j,k
        if (st%n.lt.1) stop 'Error: input string must be > 1 in remove_element_string in string.f90'
        if ((i.lt.1).or.(i.gt.st%n)) stop 'Error: element out of bounds in remove_element_string in string.f90'
        k = 0
        call init(temp,st%n-1)
        do j=1,st%n
          if (i.ne.j) then
            temp%s(j-k)%c = st%s(j)%c
          else; k = 1
          endif
        enddo
        call init(st,temp)
        call delete(temp)
      end subroutine

      function identical_string_string(A,B) result(L)
        implicit none
        type(string),intent(in) :: A,B
        logical :: L
        integer :: i
        call insist_allocated(A,'A identical_string_string')
        call insist_allocated(B,'B identical_string_string')
        L = .false.
        if (A%n.eq.B%n) then
          L = .true.
          do i=1,A%n
            if (A%s(i)%c.eq.B%s(i)%c) L = .false.
          enddo
        endif
      end function

      function identical_string_char(A,B) result(L)
        implicit none
        type(string),intent(in) :: A
        character(len=*),intent(in) :: B
        type(string) :: temp
        logical :: L
        call insist_allocated(A,'A identical_string_string')
        call init(temp,B)
        L = identical(A,temp)
        call delete(temp)
      end function

      function get_char_string(st,i) result(c)
        implicit none
        type(string),intent(in) :: st
        integer,intent(in) :: i
        character(len=1) :: c
        c = st%s(i)%c
      end function

      subroutine set_char_string(st,c,i)
        implicit none
        type(string),intent(inout) :: st
        integer,intent(in) :: i
        character(len=1),intent(in) :: c
        st%s(i)%c = c
      end subroutine

      function get_str_short(st) result(str)
        type(string),intent(in) :: st
        character(len=st%n) :: str
        str = get_str_string(st,st%n)
      end function

      pure function str_len_string(s) result(n)
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
        call insist_allocated(st,'get_str_string')
        if (st%n.lt.1) stop 'Error: st%n.lt.0 in get_str_string in string.f90'
        if (n.lt.1) stop 'Error: n.lt.1 in get_str_string in string.f90'
        do i=1,st%n
          str(i:i) = st%s(i)%c
        enddo
      end function

      function substring_in_string(str,substr) result(L)
        implicit none
        type(string),intent(in) :: str
        character(len=*),intent(in) :: substr
        logical :: L,cond
        integer :: i,j,s
        L = .false.
        s = len(substr)
        if (s.lt.1) stop 'Error: len(substr) must be > 1 in substring_in_string in string.f90'
        do i=1,len(str)-s
          cond = all((/(str%s(i+j-1:i+j-1)%c .eq. substr(j:j),j=1,s)/))
          if (cond) then
            L = .true.
            exit
          endif
        enddo
      end function

      function index_substring_in_string(str,substr) result(index)
        implicit none
        type(string),intent(in) :: str
        character(len=*),intent(in) :: substr
        logical :: cond
        integer :: index,i,j,s
        s = len(substr)
        cond = .false.
        index = 1
        if (s.lt.1) stop 'Error: len(substr) must be > 1 in index_substring_in_string in string.f90'
        do i=1,len(str)-s
          cond = all((/(str%s(i+j-1:i+j-1)%c .eq. substr(j:j),j=1,s)/))
          if (cond) then
            index = i
            exit
          endif
        enddo
        if (.not.cond) stop 'Error: substring not found in index_substring_in_string in string.f90'
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

      function string_allocated(st) result(L)
        implicit none
        type(string),intent(in) :: st
        logical :: L
        L = allocated(st%s)
      end function

      function valid_length(st) result(L)
        implicit none
        type(string),intent(in) :: st
        logical :: L
        L = st%n.gt.0
      end function

      ! function valid_string(st) result(L)
      !   implicit none
      !   type(string),intent(in) :: st
      !   logical :: L
      !   L = string_allocated(st).and.valid_length(st)
      ! end function

      subroutine insist_allocated_string(st,s)
        implicit none
        type(string),intent(in) :: st
        character(len=*),intent(in) :: s
        if (.not.string_allocated(st)) then
          write(*,*) 'Error: string must be allocated in '//s//' in string.f90'
          stop 'Done'
        elseif (.not.valid_length(st)) then
          write(*,*) 'Error: string must have a valid length in '//s//' in string.f90'
          stop 'Done'
        endif
      end subroutine

      end module