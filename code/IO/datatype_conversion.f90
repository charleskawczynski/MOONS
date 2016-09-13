      module datatype_conversion_mod
      use current_precision_mod
      use string_mod
      implicit none

      private

      public :: log2Str,int2Str,int2Str2,num2Str,str2int,intLen
      public :: cp2str
      ! public :: logical2Str,int2Str,int2Str2,float2Str,str2int,intLen ! change to this eventually

      contains

      function log2Str(L) result(s) ! NOTE: the string length and the fmt must match!
        implicit none
        logical,intent(in) :: L
        character(len=2) :: s
        write(s,'(L2)') L
        s = trim(adjustl(s))
      end function

      function int2Str(i) result(s) ! NOTE: the string length and the fmt must match!
        implicit none
        integer,intent(in) :: i
        character(len=15) :: s
        write(s,'(I15.15)') i
        s = trim(adjustl(s))
      end function

      function cp2Str(f) result(s) ! NOTE: the string length and the fmt must match!
        implicit none
        real(cp),intent(in) :: f
        character(len=15) :: s
        write(s,'(F15.15)') f
        s = trim(adjustl(s))
      end function

      function int2Str2(i) result(s) ! NOTE: the string length and the fmt must match!
        implicit none
        integer,intent(in) :: i
        character(len=15) :: s
        write(s,'(I15)') i
        s = trim(adjustl(s))
      end function

      function num2Str(i) result(s) ! NOTE: the string length and the fmt must match!
        implicit none
        real(cp),intent(in) :: i
        character(len=15) :: s
        write(s,'(F15.15)') i
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