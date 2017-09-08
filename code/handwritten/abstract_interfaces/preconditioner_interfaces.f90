      module preconditioner_interfaces_mod
      use current_precision_mod
      use mesh_extend_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_extend_mod
      implicit none

      private
      public :: preconditioner_SF
      public :: preconditioner_VF

      abstract interface
        subroutine preconditioner_SF(Minv,m,sig,c,temp_Minv)
          import mesh,SF,TF,cp
          implicit none
          type(SF),intent(inout) :: Minv,temp_Minv
          type(mesh),intent(in) :: m
          type(TF),intent(in) :: sig
          real(cp),intent(in) :: c
        end subroutine
      end interface

      abstract interface
        subroutine preconditioner_VF(Minv,m,sig,c,temp_Minv)
          import mesh,VF,TF,cp
          implicit none
          type(VF),intent(inout) :: Minv,temp_Minv
          type(mesh),intent(in) :: m
          type(TF),intent(in) :: sig
          real(cp),intent(in) :: c
        end subroutine
      end interface

      end module