       module AB2_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       implicit none

       private
       public :: AB2,AB2_pure
       interface AB2;         module procedure AB2_SF;        end interface
       interface AB2;         module procedure AB2_VF;        end interface
       interface AB2_pure;    module procedure AB2_SF_pure;   end interface
       interface AB2_pure;    module procedure AB2_VF_pure;   end interface


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine AB2_SF(Fn,Fnm1,m)
         ! Computes
         ! 
         !    Fn = 0.5 (3 Fn - Fnm1)
         ! 
         implicit none
         type(SF),intent(inout) :: Fn
         type(SF),intent(in) :: Fnm1
         type(mesh),intent(in) :: m
         call multiply(Fn,1.5_cp)
         call add_product(Fn,Fnm1,-1.5_cp)
       end subroutine

       subroutine AB2_VF(Fn,Fnm1,m)
         ! Computes
         ! 
         !    Fn = 0.5 (3 Fn - Fnm1)
         ! 
         implicit none
         type(VF),intent(inout) :: Fn
         type(VF),intent(in) :: Fnm1
         type(mesh),intent(in) :: m
         call multiply(Fn,1.5_cp)
         call add_product(Fn,Fnm1,-1.5_cp)
       end subroutine

       subroutine AB2_SF_pure(AB2,Fn,Fnm1,m)
         ! Computes
         ! 
         !    AB2 = 0.5 (3 Fn - Fnm1)
         ! 
         implicit none
         type(SF),intent(inout) :: AB2
         type(SF),intent(in) :: Fn,Fnm1
         type(mesh),intent(in) :: m
         call multiply(AB2,Fn,1.5_cp)
         call add_product(AB2,Fnm1,-1.5_cp)
       end subroutine

       subroutine AB2_VF_pure(AB2,Fn,Fnm1,m)
         ! Computes
         ! 
         !    AB2 = 0.5 (3 Fn - Fnm1)
         ! 
         implicit none
         type(VF),intent(inout) :: AB2
         type(VF),intent(in) :: Fn,Fnm1
         type(mesh),intent(in) :: m
         call multiply(AB2,Fn,1.5_cp)
         call add_product(AB2,Fnm1,-1.5_cp)
       end subroutine

       end module