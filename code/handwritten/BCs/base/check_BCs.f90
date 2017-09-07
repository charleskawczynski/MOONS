       module check_BCs_mod
       use SF_extend_mod
       use VF_extend_mod
       implicit none

       private
       public :: check_defined

       contains

       subroutine check_defined(U)
         implicit none
         type(SF),intent(in) :: U
         integer :: i
         do i=1,U%s
         if (.not.U%BF(i)%BCs%BCL%defined) stop 'Error: bad bctype in check_BCs in check_BCs.f90'
         enddo
       end subroutine

       end module