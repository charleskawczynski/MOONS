       module diffuse_mod
       use mesh_mod
       use VF_mod
       use ops_discrete_mod
       use ops_physics_mod
       
       implicit none
       private

       public :: diffuse

#ifdef _DIFFUSE_CRANK_NICOLSON_
       interface diffuse;      module procedure diffuse_Crank_Nicolson;    end interface
#endif
#ifdef _DIFFUSE_EXPLICIT_
       interface diffuse;      module procedure diffuse_explicit;          end interface
#endif

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

#ifdef _DIFFUSE_CRANK_NICOLSON_
       subroutine diffuse_Crank_Nicolson(diff,U,m,nstep)
         ! Computes
         !          (2Re)⁻¹ ∇²uⁿ
         ! 
         ! Since
         !           diffuse_total = 0.5 Re⁻¹ ( ∇²uⁿ + ∇²uⁿ⁺¹ )
         implicit none
         type(VF),intent(inout) :: diff
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Re
         integer,intent(in) :: nstep
         if (nstep.gt.1) then
           call lap(diff,U,m)
           call multiply(diff,0.5_cp/Re)
         else
           call lap(diff,U,m)
           call multiply(diff,1.0_cp/Re)
         endif
       end subroutine
#endif

#ifdef _DIFFUSE_EXPLICIT_
       subroutine diffuse_explicit(diff,U,m)
         ! Computes
         !          Re⁻¹ ∇²uⁿ
         implicit none
         type(VF),intent(inout) :: U,temp_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_E1,temp_E2
         call lap(diff,U,m)
         call multiply(diff,1.0_cp/Re)
       end subroutine
#endif

       end module