      module IO_tools_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


      private

      public :: makeDir,rmDir
      public :: newUnit
      public :: getUnit,closeExisting
      public :: newAndOpen,newAndOpenBinary,openToRead,openToAppend
      public :: closeAndMessage
      public :: int2Str,num2Str
      public :: arrfmt,rarrfmt,logfmt,intfmt

       ! This website is a good reference for formatting:
       ! http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
       ! leading digit + . + precision + E + exponent + signs + spaces between
       !       1         1      12       1      3         2          3
       ! rarrfmt is for reading (possible old formats)
       !  arrfmt is for writing (current format)
      character(len=8),parameter :: rarrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=8),parameter ::  arrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=3),parameter ::  intfmt = 'I10'       ! Make sure length is correct when adjusting
      character(len=3),parameter ::  logfmt = 'L1'        ! Make sure length is correct when adjusting

      character(len=4),parameter :: fileType = '.dat'

      contains

      subroutine makeDir(d1,d2,d3)
        character(len=*),intent(in) :: d1
        character(len=*),intent(in),optional :: d2,d3
        character(len=30) :: d
        logical :: ex
        if (present(d2).and.present(d3)) then
          d = adjustl(trim(d1)) // adjustl(trim(d2)) // adjustl(trim(d3))
        elseif (present(d2)) then
          d = adjustl(trim(d1)) // adjustl(trim(d2))
        else
          d = adjustl(trim(d1))
        endif

        INQUIRE (file=adjustl(trim(d)), EXIST=ex)
        if (.not.ex) then
          call system('mkdir ' // adjustl(trim(d)) )
          write(*,*) 'Directory ' // adjustl(trim(d)) // ' created.'
        else 
          write(*,*) 'Directory ' // adjustl(trim(d)) // ' already exists.'
        endif
      end subroutine

      subroutine rmDir(d)
        character(len=*),intent(in) :: d
          call system('rm /' // adjustl(trim(d)) )
          write(*,*) 'Directory ' // adjustl(trim(d)) // ' removed.'
      end subroutine

      function newAndOpen(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,n
        NU = newUnit()
        open(NU,file=trim(strcompress(dir,n)) // trim(strcompress(name,n)) // fileType,pad='YES')
      end function

      function newAndOpenBinary(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,n
        NU = newUnit()
        open(NU,file=trim(strcompress(dir,n)) // trim(strcompress(name,n)) // fileType,&
          form='unformatted')
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
        NU = newUnit()
        inquire(file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType, &
        number=NU,exist=ex,opened=op,action=act)

        if (.not.op) then
          NU = newUnit()
        endif
        if (ex) then
          open(NU,file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType,&
          status = 'old', action = 'readwrite',iostat=ok,position='append')
        else
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' does not exist. Terminating execution.'
        endif
        if (ok.ne.0) then
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' was not opened successfully.'
        endif
      end function

      function openToRead(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        NU = newUnit()
        inquire(file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType, &
        number=NU,exist=ex,opened=op,action=act)

        if (.not.op) then
          NU = newUnit()
        endif
        if (ex) then
          open(NU,file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType,&
          status = 'old', action = 'read',iostat=ok)
        else
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' does not exist. Terminating execution.'
        endif
        if (ok.ne.0) then
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' was not opened successfully.'
        endif
      end function

      subroutine closeAndMessage(u,name,dir)
        ! BONEHEAD MOVE: should be dir,name,u
        implicit none
        integer,intent(in) :: u
        character(len=*),intent(in) :: name,dir
        close(u)
        write(*,*) '+++ Data for ' // trim(adjustl(name)) // ' written to ' // trim(adjustl(dir)) //' +++'
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
        implicit none
        integer,intent(in) :: i
        character(len=10) :: s
        write(s,'(I10.10)') i
        s = trim(adjustl(s))
      end function

      function num2Str(i) result(s)
        implicit none
        real(cp),intent(in) :: i
        character(len=8) :: s
        write(s,'(F3.5)') i
        s = trim(adjustl(s))
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