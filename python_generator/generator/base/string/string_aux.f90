      module string_aux_mod
      use string_mod
      implicit none

      private

      public :: read_line
      public :: remove_substring

      public :: get_first_int
      public :: remove_leading_zeros
      public :: get_first_int_indexes

      interface read_line;                   module procedure read_line_string;               end interface
      interface remove_substring;            module procedure remove_substring_string;        end interface

      interface get_first_int;               module procedure get_first_int_string;           end interface
      interface remove_leading_zeros;        module procedure remove_leading_zeros_string;    end interface
      interface get_first_int_indexes;       module procedure get_first_int_indexes_string;   end interface

      contains

      function read_line_string(un,n_chars) result(line)
        ! Returns a string of the line, in file unit=un, up to n_chars or EOL.
        implicit none
        integer,intent(in) :: un,n_chars
        character(len=1) :: c
        type(string) :: line
        integer :: i,ReadCode
        ReadCode = 0
        do i=1,n_chars
          if (ReadCode.eq.0) then
            read(un,'(A)',advance='no',iostat=ReadCode) c
            if (i.eq.1) then; call init(line,c); else; call append(line,c); endif
          else; exit
          endif
        enddo
      end function

      function remove_substring_string(st,substr) result(s)
        implicit none
        type(string),intent(in) :: st
        character(len=*),intent(in) :: substr
        type(string) :: s
        integer :: j,first_index
        call init(s,st)
        if (match(st,substr)) then
        first_index = match_index(st,substr)
        do j=1,len(substr)
        call remove_element(s,first_index)
        enddo
        else; call init(s,st)
        endif
      end function

      ! ************************************************************************************
      ! ************************************************************************************
      ! ************************************************************************************

      function is_element_09(st,j) result(L)
        implicit none
        type(string),intent(in) :: st
        integer,intent(in) :: j
        logical,dimension(10) :: any_num
        logical :: L
        any_num(1)  = get_char(st,j).eq.'0'
        any_num(2)  = get_char(st,j).eq.'1'
        any_num(3)  = get_char(st,j).eq.'2'
        any_num(4)  = get_char(st,j).eq.'3'
        any_num(5)  = get_char(st,j).eq.'4'
        any_num(6)  = get_char(st,j).eq.'5'
        any_num(7)  = get_char(st,j).eq.'6'
        any_num(8)  = get_char(st,j).eq.'7'
        any_num(9)  = get_char(st,j).eq.'8'
        any_num(10) = get_char(st,j).eq.'9'
        L = any(any_num)
      end function

      ! ************************************************************************************
      ! ************************************************************************************
      ! ************************************************************************************

      subroutine remove_leading_zeros_string(st)
        implicit none
        type(string),intent(inout) :: st
        type(string) :: temp
        integer :: i,n_zeros,n
        n = len(st)
        if (n.lt.1) stop 'Error: input string must be > 1 in string.f90'
        n_zeros = 0
        do i=1,n-1 ! do not remove the last character even if zero
          if (get_char(st,i).eq.'0') then; n_zeros = n_zeros + 1; else; exit; endif
        enddo
        call init(temp,n-n_zeros)
        do i=1+n_zeros,n
          call set_char(temp,get_char(st,i),i-n_zeros)
        enddo
        call init(st,temp)
        call delete(temp)
      end subroutine

      function get_first_int_string(st) result(i)
        implicit none
        type(string),intent(in) :: st
        type(string) :: i
        integer j,k,n
        logical :: active_number
        active_number = .false.
        n = len(st)
        do j=1,n
          if (is_element_09(st,j)) then
            call init(i,get_char(st,j))
            if (j.lt.n) then
              do k=j+1,n
                if (is_element_09(st,k)) then
                  call append(i,get_char(st,k))
                else; exit
                endif
              enddo
            else; exit
            endif
            exit
          endif
        enddo
      end function

      function get_first_int_indexes_string(st) result(i)
        implicit none
        type(string),intent(in) :: st
        integer,dimension(2) :: firstLast
        type(string) :: i
        integer j,k,n
        logical :: active_number
        active_number = .false.
        n = len(st)
        firstLast(1) = 1
        firstLast(2) = n
        do j=1,n
          if (is_element_09(st,j)) then
            firstLast(1) = j
            if (j.lt.n) then
              do k=j+1,n
                if (is_element_09(st,k)) then
                  call append(i,get_char(st,k))
                else; firstLast(2) = k;exit
                endif
              enddo
              firstLast(2) = n
            else; firstLast(2) = n;exit
            endif
            exit
          endif
        enddo
      end function

      end module