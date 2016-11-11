       module block_stencil_mod
       use current_precision_mod
       use stencil_1D_mod
       use stencil_3D_mod
       implicit none

       private
       public :: modify
       interface modify;   module procedure modify_BS_SF;   end interface
       interface modify;   module procedure modify_BS_VF;   end interface

       contains

       subroutine modify_BS_SF(SF,multiply_by,then_add_to)
         implicit none
         type(stencil_3D),intent(inout) :: SF
         real(cp),intent(in) :: multiply_by,then_add_to
         call multiply_diag(SF,multiply_by)
         call add_to_diag(SF,then_add_to)
       end subroutine

       subroutine modify_BS_VF(VF,multiply_by,then_add_to)
         implicit none
         type(stencil_3D),dimension(3),intent(inout) :: VF
         real(cp),intent(in) :: multiply_by,then_add_to
         integer :: i
         do i=1,3; call multiply_diag(VF(i),multiply_by); enddo
         do i=1,3; call add_to_diag(VF(i),then_add_to); enddo
       end subroutine

       end module