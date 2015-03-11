      module pieceWise_mod
        ! This module helps define piecewise functions with
        ! 
        ! Compiler flags: (_DEBUG_PIECEWISE_)
        ! 
        ! call init(pw,20)
        ! call setStep(pw,100,10)
        ! call setStep(pw,200,15)
        ! 
        ! value = pw%get(n)
        ! 
        ! will result in
        ! 
        !       value
        !        ^
        !        |
        !    20  |__________
        !        |          |
        !    15  |          |         o_________ infinity
        !        |          |         |
        !    10  |          o_________|
        !        |     
        !        |     
        !        |___________________________ n
        !                  100      200
        ! 
        ! delete with
        ! 
        ! call delete(pw)
        ! 
        ! NOTE: The 'o' signifies that the value is taken there (at the 'o')
        !       The only information stored is the CHANGE in the base value
        !       so this routine does not require lots of memory.
        ! 
      implicit none

      private
      public :: pieceWise,init,set,get

      interface init;     module procedure initPieceWise;     end interface
      interface delete;   module procedure deletePieceWise;   end interface

      type pieceWise
        integer :: baseVal
        integer,dimension(:),allocatable :: stepLoc,stepVal
        procedure, nopass :: get
      end type

      contains

      subroutine initPieceWise(pw,baseVal)
        implicit none
        type(piecewise),intent(inout) :: pw
        integer,intent(in) :: baseVal
        pw%baseVal = baseVal
        if (allocated(pw%stepLoc)) deallocate(stepLoc)
        if (allocated(pw%stepVal)) deallocate(stepVal)
      end subroutine

      subroutine deletePieceWise(pw)
        implicit none
        type(piecewise),intent(inout) :: pw
        if (allocated(pw%stepLoc)) deallocate(stepLoc)
        if (allocated(pw%stepVal)) deallocate(stepVal)
      end subroutine

      subroutine setStep(pw,stepLoc,stepVal)
        implicit none
        type(piecewise),intent(inout) :: pw
        integer,intent(in) :: stepLoc,stepVal
        integer,dimension(:),allocatable :: tempStepLoc,tempStepVal
        if (allocated(pw%stepLoc)) then
          s = size(pw%stepLoc); allocate(tempStepLoc(s+1))
          tempStepLoc(1:s) = pw%stepLoc; deallocate(pw%stepLoc)
          allocate(pw%stepLoc(s+1)); pw%stepLoc = (/tempStepLoc,stepLoc/)
          deallocate(tempStepLoc)
        else; allocate(pw%stepLoc(1)); pw%stepLoc(1) = stepLoc
        endif
        if (allocated(pw%stepVal)) then
          s = size(pw%stepVal); allocate(tempStepVal(s+1))
          tempStepVal(1:s) = pw%stepVal; deallocate(pw%stepVal)
          allocate(pw%stepVal(s+1)); pw%stepVal = (/tempStepVal,stepVal/)
          deallocate(tempStepVal)
        else; allocate(pw%stepVal(1)); pw%stepVal(1) = stepVal
        endif
      end subroutine

      function get(pw,n) result (val)
        implicit none
        type(piecewise),intent(in) :: pw
        integer,intent(in) :: n
        integer :: val,s,i
#ifdef _DEBUG_PIECEWISE_
        if (.not.allocated(pw%stepLoc)) stop 'Error: pieceWise not allocated yet.'
#endif
        val = pw%baseVal
        if (allocated(pw%stepLoc)) then
          s = size(pw%stepLoc)
          do i=1,s
            if (n.ge.pw%stepLoc(i)) then
              val = pw%stepVal(i)
            endif
          enddo
        endif
      end function

      end module