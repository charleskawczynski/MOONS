       module energy_solver_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use domain_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use apply_BCs_mod

       implicit none

       private
       public :: explicitEuler

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

       subroutine explicitEuler(T,U_F,dt,Re,Pr,m,temp_CC1,temp_CC2,temp_F)
         implicit none
         type(SF),intent(inout) :: T,temp_CC1,temp_CC2
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_F
         real(cp),intent(in) :: dt,Re,Pr
         type(VF),intent(inout) :: temp_F
         call cellCenter2Face(temp_F,T,m)
         call multiply(temp_F,U_F)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-1.0_cp)
         call lap(temp_CC2,T,m)
         call multiply(temp_CC2,1.0_cp/(Re*Pr))
         call add(temp_CC1,temp_CC2)
         call multiply(temp_CC1,dt)
         call add(T,temp_CC1)
         call apply_BCs(T,m)
       end subroutine

       end module