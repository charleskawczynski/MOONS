      module datatype_conversion_mod
      ! NOTE: the string length and the fmt must match!
      use current_precision_mod
      use string_mod
      implicit none

      private

      public :: log2Str
      public :: int2Str
      interface int2Str;      module procedure int2Str_li;       end interface
      interface int2Str;      module procedure int2Str_dynamic;  end interface
      ! interface int2Str;      module procedure int2Str_reg;      end interface

      public :: int2Str2
      public :: str2int
      public :: intLen
      public :: cp2str
      interface cp2str;      module procedure cp2Str_string;   end interface
      ! interface cp2Str;      module procedure cp2Str_raw;      end interface

      ! public :: int2Str,int2Str2,str2int,intLen ! change to this eventually

      contains

      function log2Str(L) result(s)
        implicit none
        logical,intent(in) :: L
        character(len=2) :: s
        write(s,'(L2)') L
        s = trim(adjustl(s))
      end function

      ! function int2Str_reg(i) result(s)
      !   implicit none
      !   integer,intent(in) :: i
      !   character(len=15) :: s
      !   write(s,'(I15.15)') i
      !   s = trim(adjustl(s))
      ! end function

      function int2Str_dynamic(i) result(s)
        implicit none
        integer,intent(in) :: i
        real(cp),parameter :: denom = log(10.0_cp)
        character(len=maxval((/ceiling(log(real(i+1,cp))/denom),1/))) :: s
        integer :: i_places
        i_places = maxval((/ceiling(log(real(i+1,cp))/denom),1/))
        write(s,'(I'//int2Str_dumn(i_places)//')') i
        s = trim(adjustl(s))
      end function

      function int2Str_dumn(i) result(s)
        implicit none
        integer,intent(in) :: i
        character(len=50) :: s
        write(s,'(I50)') i
        s = trim(adjustl(s))
      end function

      function int2Str_li(i) result(s)
        implicit none
        integer(li),intent(in) :: i
        character(len=15) :: s
        write(s,'(I15.15)') i
        s = trim(adjustl(s))
      end function

      ! function cp2Str_raw(f) result(s)
      !   implicit none
      !   real(cp),intent(in) :: f
      !   character(len=100) :: s
      !   write(s,'(F40.20)') f
      !   s = trim(adjustl(s))
      ! end function

      function cp2str_string(f) result(s)
        implicit none
        real(cp),intent(in) :: f
        type(string) :: s
        character(len=100) :: a
        write(a,'(F40.20)') f
        call init(s,trim(adjustl(a)))
      end function

      function int2Str2(i) result(s)
        implicit none
        integer,intent(in) :: i
        character(len=15) :: s
        write(s,'(I15)') i
        s = trim(adjustl(s))
      end function

      function str2int(s) result(i)
        implicit none
        character(len=*),intent(in) :: s
        integer :: i
        read(s,*) i
      end function

      function intLen(i) result(n)
        implicit none
        integer,intent(in) :: i
        integer :: n
        type(string) :: s
        call init(s,int2str(i))
        n = len(s)
      end function

      end module