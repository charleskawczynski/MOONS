       module ind_methods_mod
       ! Constrained Transport (CT) Method reference:
       ! "Tóth, G. The divergence Constraint in Shock-Capturing 
       ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use applyBCs_mod
       use solverSettings_mod
       use norms_mod
       use SOR_mod

       implicit none

       private
       public :: FRM_CT
       public :: LRM_CT

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

       subroutine FRM_CT(B,B0,U,J,E,sigmaInv,m,dt,Rem,tmp_F,tmp_E)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Constrained Transport (CT)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigmaInv
         type(TF),intent(in) :: U
         type(VF),intent(inout) :: tmp_E,tmp_F
         type(grid),intent(in) :: m
         real(cp),intent(in) :: dt,Rem
         call faceCrossFace_E(tmp_E,U,B0,m,tmp_F)
         call multiply(E,tmp_E,-1.0_cp)
         call curl(J,B,m)
         call multiply(tmp_E,J,sigmaInv)
         call multiply(tmp_E,1.0_cp/Rem)
         call add(E,tmp_E)
         call curl(tmp_F,E,m)
         call multiply(tmp_F,-dt)
         call add(B,tmp_F)
         call applyAllBCs(B,m)
       end subroutine

       subroutine LRM_CT(B,B0,U,J,E,sigmaInv,m,dt,NmaxB,tmp_F,tmp_E)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = ∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - ∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Constrained Transport (CT)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Low Rem
         !             Quasi-static B
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigmaInv
         type(TF),intent(in) :: U
         type(VF),intent(inout) :: tmp_F,tmp_E
         type(grid),intent(in) :: m
         real(cp),intent(in) :: dt
         integer,intent(in) :: NmaxB
         integer :: i
         do i=1,NmaxB
           call faceCrossFace_E(tmp_E,U,B0,m,tmp_F)
           call multiply(E,tmp_E,-1.0_cp)
           call curl(J,B,m)
           call multiply(tmp_E,J,sigmaInv)
           call add(E,tmp_E)
           call curl(tmp_F,E,m)
           call multiply(tmp_F,-dt)
           call add(B,tmp_F)
           call applyAllBCs(B,m)
         enddo
       end subroutine

       subroutine LRM_SOR(ind,U,m,n,compute_norms)
         ! Solves:
         !             ∇²B = ∇x(uxB⁰)
         ! Method:
         !             Pseudo-time stepping
         ! Info:
         !             Low Rem
         !             Quasi-static B
         implicit none
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         logical,intent(inout) :: compute_norms
         integer,intent(in) :: n
         call CCBfieldAdvect(temp_CC,U,B0,m)
         call solve(SOR,B%x,temp_CC%x,m,n,compute_norms)
         call solve(SOR,B%y,temp_CC%y,m,n,compute_norms)
         call solve(SOR,B%z,temp_CC%z,m,n,compute_norms)
       end subroutine

       subroutine LRM_PSE(ind,U,m)
         ! Solves:
         !             ∇²B = ∇x(uxB⁰)
         ! Method:
         !             Pseudo-time stepping
         ! Info:
         !             Low Rem
         !             Quasi-static B
         implicit none
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,NmaxB
           call assign(Bstar,zero)
           call lap(temp_CC,B,m)
           call multiply(temp_CC,dTime)
           call add(Bstar,temp_CC)
           ! call CCBfieldAdvect(temp_CC,U,B0,m)
           call multiply(temp_CC,dTime)
           call subtract(Bstar,temp_CC)
           call add(B,Bstar)
           call applyAllBCs(B,m)
         enddo
       end subroutine

       end module