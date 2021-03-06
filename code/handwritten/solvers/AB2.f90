       module AB2_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       implicit none

       private
       public :: AB2,AB2_overwrite
       interface AB2_overwrite;       module procedure AB2_overwrite_SF;       end interface
       interface AB2_overwrite;       module procedure AB2_overwrite_VF;       end interface
       interface AB2;                 module procedure AB2_SF;                 end interface
       interface AB2;                 module procedure AB2_VF;                 end interface

       contains

       subroutine AB2_overwrite_SF(Fn,Fnm1)
         ! Computes
         !
         !    Fn = 0.5 (3 Fn - Fnm1)
         !
         implicit none
         type(SF),intent(inout) :: Fn
         type(SF),intent(in) :: Fnm1
         call multiply(Fn,1.5_cp)
         call add_product(Fn,Fnm1,-0.5_cp)
       end subroutine

       subroutine AB2_overwrite_VF(Fn,Fnm1)
         ! Computes
         !
         !    Fn = 0.5 (3 Fn - Fnm1)
         !
         implicit none
         type(VF),intent(inout) :: Fn
         type(VF),intent(in) :: Fnm1
         call multiply(Fn,1.5_cp)
         call add_product(Fn,Fnm1,-0.5_cp)
       end subroutine

       subroutine AB2_SF(AB2,Fn,Fnm1)
         ! Computes
         !
         !    AB2 = 0.5 (3 Fn - Fnm1)
         !
         implicit none
         type(SF),intent(inout) :: AB2
         type(SF),intent(in) :: Fn,Fnm1
         call multiply(AB2,Fn,1.5_cp)
         call add_product(AB2,Fnm1,-0.5_cp)
       end subroutine

       subroutine AB2_VF(AB2,Fn,Fnm1)
         ! Computes
         !
         !    AB2 = 0.5 (3 Fn - Fnm1)
         !
         implicit none
         type(VF),intent(inout) :: AB2
         type(VF),intent(in) :: Fn,Fnm1
         call multiply(AB2,Fn,1.5_cp)
         call add_product(AB2,Fnm1,-0.5_cp)
       end subroutine

       end module