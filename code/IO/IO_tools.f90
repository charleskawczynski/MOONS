      module IO_tools_mod
      use current_precision_mod
      use string_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

      private

      public :: make_dir,rmDir
      public :: newUnit
      public :: getUnit,closeExisting
      public :: newAndOpen,newAndOpenBinary,openToRead,openToAppend
      public :: closeAndMessage
      public :: int2Str,int2Str2,num2Str,intLen
      public :: arrfmt,rarrfmt,logfmt,intfmt

       ! This website is a good reference for formatting:
       ! http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
       ! leading digit + . + precision + E + exponent + signs + spaces between
       !       1         1      12       1      3         2          3
       ! rarrfmt is for reading (possible old formats)
       !  arrfmt is for writing (current format)
      character(len=8),parameter :: rarrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=8),parameter ::  arrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=3),parameter ::  intfmt = 'I15'       ! Make sure length is correct when adjusting
      character(len=3),parameter ::  logfmt = 'L1'        ! Make sure length is correct when adjusting

      character(len=4),parameter :: fileType = '.dat'

      contains

      subroutine make_dir(d1,d2,d3,d4)
        character(len=*),intent(in) :: d1
        character(len=*),intent(in),optional :: d2,d3,d4
        type(string) :: s
        logical :: ex
        if (present(d2).and.present(d3).and.present(d4)) then
          call init(s,d1//d2//d3//d4)
        elseif (present(d3)) then
          call init(s,d1//d2//d3)
        elseif (present(d2)) then
          call init(s,d1//d2)
        else
          call init(s,d1)
        endif

        INQUIRE (file=str(s), EXIST=ex)
        if (.not.ex) then
          call system('mkdir ' // str(s) )
          write(*,*) 'Directory ' // str(s) // ' created.'
        else 
          write(*,*) 'Directory ' // str(s) // ' already exists.'
        endif
        call delete(s)
      end subroutine

      subroutine rmDir(d)
        character(len=*),intent(in) :: d
        call system('rm -r /' // d )
        write(*,*) 'Directory ' // d // ' removed.'
      end subroutine

      function newAndOpen(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU
        type(string) :: s
        call init(s,dir//name//fileType)
        NU = newUnit()
        open(NU,file=str(s),pad='YES')
        call delete(s)
      end function

      function newAndOpenBinary(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU
        type(string) :: s
        call init(s,dir//name//fileType)
        NU = newUnit()
        open(NU,file=str(s),form='unformatted')
        call delete(s)
      end function

      function getUnit(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU
        NU = openToRead(dir,name)
      end function

      function openToAppend(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        type(string) :: s
        NU = newUnit()
        call init(s,dir//name//fileType)
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
        else
          ! write(*,*) 'dir/name/ext = ',trim(adjustl(dir)) // trim(adjustl(name)) // fileType
          ! write(*,*) 'NU = ',NU
          ! write(*,*) 'ex = ',ex
          ! write(*,*) 'op = ',op
          ! write(*,*) 'act = ',act
          ! stop 'Error: file is already open, no need to call, just write to file'
        endif
        call delete(s)
      end function

      function openToRead(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        type(string) :: s
        call init(s,dir//name//fileType)
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

      subroutine closeAndMessage(u,name,dir)
        ! BONEHEAD MOVE: should be dir,name,u
        implicit none
        integer,intent(in) :: u
        character(len=*),intent(in) :: name,dir
        close(u)
        write(*,*) '+++ Data for ' // name // ' written to ' // dir //' +++'
      end subroutine

      subroutine closeExisting(u,name,dir)
        implicit none
        integer,intent(in) :: u
        character(len=*),intent(in) :: name,dir
        close(u)
        write(*,*) '+++ Data for ' // name // ' read from ' // dir //' +++'
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

      function int2Str(i) result(s)
        ! NOTE: the string length and the fmt must match!
        implicit none
        integer,intent(in) :: i
        character(len=15) :: s
        write(s,'(I15.15)') i
        s = trim(adjustl(s))
      end function

      function int2Str2(i) result(s)
        ! NOTE: the string length and the fmt must match!
        implicit none
        integer,intent(in) :: i
        character(len=15) :: s
        write(s,'(I15)') i
        s = trim(adjustl(s))
      end function

      function num2Str(i) result(s)
        ! NOTE: the string length and the fmt must match!
        implicit none
        real(cp),intent(in) :: i
        character(len=15) :: s
        write(s,'(F15.15)') i
        s = trim(adjustl(s))
      end function

      function intLen(i) result(n)
        implicit none
        integer,intent(in) :: i
        integer :: n
        type(string) :: s
        call init(s,int2str(i))
        n = len(s)
      end function

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