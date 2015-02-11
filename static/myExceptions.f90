      module myExceptions_mod
      implicit none

     ! Fixes / Improvements:

      private
      
      public :: propertyNotAssigned
      public :: alreadyAssociated
      public :: notNullified

      logical,parameter :: debug = .true.
      integer,parameter :: propNameLength = 2
      integer,parameter :: funcNameLength = 30

      contains

       subroutine propertyNotAssigned(name,func)
         implicit none
         character(len=*),intent(in) :: name
         character(len=*),intent(in) :: func
         write(*,*) '------------------------------ ERROR ------------------------------'
         write(*,*) 'The property "' // name //'" of this derived type was not defined '
         write(*,*) 'inside '//adjustl(trim(func)) // & 
                    ' and is therefore unaccessable. Terminating execution.'
         write(*,*) '-------------------------------------------------------------------'
         stop
       end subroutine

       subroutine notNullified(name,func)
         implicit none
         character(len=*),intent(in) :: name
         character(len=*),intent(in) :: func
         write(*,*) '------------------------------ ERROR ------------------------------'
         write(*,*) 'The property "' // name //'" of this derived type was not NULLIFIED '
         write(*,*) 'inside '//adjustl(trim(func)) //  ' and must nullified before'
         write(*,*) 'using the associated() intrinsic before deallocating and allocating.' 
         write(*,*) 'This may lead to unknown behavior. Terminating execution.' 
         write(*,*) '-------------------------------------------------------------------'
         stop
       end subroutine

       subroutine alreadyAssociated(name,func)
         implicit none
         character(len=*),intent(in) :: name
         character(len=*),intent(in) :: func
         write(*,*) '------------------------------ ERROR ------------------------------'
         write(*,*) 'The "' // name //'" component of this scalar field was already '
         write(*,*) 'associated inside '//adjustl(trim(func)) // '. Terminating execution.'
         write(*,*) '-------------------------------------------------------------------'
         stop
       end subroutine

      end module