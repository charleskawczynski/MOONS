       module ind_advect_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod

       implicit none

       private
       public :: advect
       interface advect;    module procedure advectB;    end interface
       interface advect;    module procedure advectBtot; end interface

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

       subroutine advectB(curlUcrossB,U,B,m,temp_F,temp_E_TF)
         ! Computes:
         !             ∇x(uxB)
         implicit none
         type(VF),intent(inout) :: curlUcrossB
         type(VF),intent(in) :: U
         type(VF),intent(in) :: B
         type(TF),intent(inout) :: temp_E_TF
         type(mesh),intent(in) :: m
         call add(temp_F,B0,B)
         call face2Edge(temp_E_TF,temp_F,m)
         call cross(tmp_E_VF,U,temp_E_TF)
         call curl(temp_F,tmp_E_VF,m)
       end subroutine

       subroutine advectBtot(curlUcrossB,U,B,B0,m,temp_F,temp_E_TF)
         ! Computes:
         !             ∇x(ux(B+B⁰))
         implicit none
         type(VF),intent(inout) :: curlUcrossB
         type(VF),intent(in) :: U
         type(VF),intent(in) :: B,B0
         type(TF),intent(inout) :: temp_E_TF
         type(mesh),intent(in) :: m
         call add(temp_F,B0,B)
         call face2Edge(temp_E_TF,temp_F,m)
         call cross(tmp_E_VF,U,temp_E_TF)
         call curl(temp_F,tmp_E_VF,m)
       end subroutine

       end module