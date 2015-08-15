      module varStr_mod
      implicit none

      private
      public :: varStr
      public :: init,delete
      public :: compress,append

      interface init;      module procedure init_size;        end interface
      interface init;      module procedure init_copy;        end interface
      interface delete;    module procedure delete_varStr;    end interface
      interface append;    module procedure app_varStr;       end interface
      interface compress;  module procedure compress_varStr;  end interface

      type varStr                         ! string of variable size
        character(len=:),allocatable :: s ! string
        integer :: n                      ! string length
      end type

      contains

      subroutine init_size(VS,n)
        implicit none
        type(varStr),intent(inout) :: VS
        integer,intent(in) :: n
        if (allocated(VS%s)) deallocate(VS%s)
        allocate(character(len=n) :: VS%s); VS%n = n
      end subroutine

      subroutine init_copy(VS,s)
        implicit none
        type(varStr),intent(inout) :: VS
        character(len=*),intent(in) :: s
        call init(VS,len(s))
        VS%s = s
      end subroutine

      subroutine delete_varStr(VS)
        implicit none
        type(varStr),intent(inout) :: VS
        if (allocated(VS%s)) deallocate(VS%s)
      end subroutine

      subroutine app_varStr(VS,s)
        implicit none
        type(varStr),intent(inout) :: VS
        character(len=*),intent(in) :: s
        character(len=:),allocatable :: temp
        integer :: n
        n = len(s)
        allocate(character(len=VS%n+n) :: temp)
        temp = VS%s//s
        call init(VS,VS%n+n) ! Re-size
        VS%s = temp
        deallocate(temp)
      end subroutine

      subroutine compress_varStr(VS)
        implicit none
        type(varStr),intent(inout) :: VS
        character(len=:),allocatable :: temp
        integer :: n
        n = len(VS%s)
        allocate(character(len=n) :: temp)
        temp = strcompress(VS%s,n) ! To get n
        call init(VS,n) ! Re-size in case spaces were given in input
        VS%s = strcompress(temp,n) ! To get s
        deallocate(temp)
      end subroutine

      FUNCTION strcompress( input_string, n ) RESULT ( output_string ) 
        !------------------------------------------------------------------------------ 
        ! NAME: 
        !       strcompress 
        ! 
        ! PURPOSE: 
        !       Function to return a copy of an input string with all whitespace 
        !       (spaces and tabs) removed. 
        ! 
        ! CATEGORY: 
        !       Utility 
        ! 
        ! LANGUAGE: 
        !       Fortran-90 
        ! 
        ! CALLING SEQUENCE: 
        !       output_string = strcompress( input_string,  &  ! Input 
        !                                    n              )  ! Output 
        ! 
        ! INPUT ARGUMENTS: 
        !       input_string:  Character string to be compressed. 
        !                      UNITS:      None 
        !                      TYPE:       Character 
        !                      DIMENSION:  Scalar, LEN = * 
        !                      ATTRIBUTES: INTENT( IN ) 
        ! 
        ! OUTPUT ARGUMENTS: 
        !       n:             Number of useful characters in output string 
        !                      after compression. From character n+1 -> LEN( input_string ) 
        !                      the output is padded with blanks. 
        !                      UNITS:      None 
        !                      TYPE:       Integer 
        !                      DIMENSION:  Scalar 
        !                      ATTRIBUTES: INTENT( OUT ) 
        ! 
        ! FUNCTION RESULT: 
        !       Input character string with internal white space (spaces and tabs) 
        !       removed. Returned string length is still the length of the input 
        !       string, but compressed and padded with blanks. 
        ! 
        ! EXAMPLE: 
        !       input_string = '  This is a string with spaces in it.' 
        !       output_string = strcompress( input_string, n ) 
        !       WRITE( *, '( a )' ) '>',output_string( 1:n ),'<' 
        !   >Thisisastringwithspacesinit.< 
        ! 
        ! PROCEDURE: 
        !       Definitions of a space and a tab character are made for the 
        !       ASCII collating sequence. Each single character of the input 
        !       string is checked against these definitions using the IACHAR() 
        !       intrinsic. If the input string character DOES NOT correspond 
        !       to a space or tab, it is not copied to the output string. 
        ! 
        !       Note that for input that ONLY has spaces or tabs BEFORE the first 
        !       useful character, the output of this function is the same as the 
        !       ADJUSTL() instrinsic.
        ! 
        ! CREATION HISTORY: 
        !       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999 
        !------------------------------------------------------------------------------ 
        CHARACTER( * ), INTENT( IN )  :: input_string 
        INTEGER,        INTENT( OUT ) :: n 
        ! -- Function result 
        CHARACTER( LEN( input_string ) ) :: output_string 
        ! -- Local parameters 
        INTEGER,        PARAMETER :: IACHAR_SPACE = 32, & 
                                     IACHAR_TAB   = 9 
        ! -- Local variables 
        INTEGER :: i 
        INTEGER :: iachar_character 
        ! -- Initialise output string 
        output_string = ' ' 
        ! -- Initialise output string "useful" length counter 
        n = 0 
        ! -- Loop over string elements 
        DO i = 1, LEN( input_string ) 
          ! -- Convert the current character to its position 
          ! -- in the ASCII collating sequence 
          iachar_character = IACHAR( input_string( i:i ) ) 
          ! -- If the character is NOT a space ' ' or a tab '->|' 
          ! -- copy it to the output string. 
          IF ( iachar_character /= IACHAR_SPACE .AND. & 
               iachar_character /= IACHAR_TAB         ) THEN 
            n = n + 1 
            output_string( n:n ) = input_string( i:i ) 
          END IF 
        END DO 
      END FUNCTION strcompress

      end module