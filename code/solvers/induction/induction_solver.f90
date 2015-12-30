       module ind_solver_mod
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

       ! CT methods
       public :: Finite_Rem_CT
       public :: Low_Rem_CT

       ! Diffusion implicit methods
       public :: Finite_Rem_CG_semi
       public :: Finite_Rem_CG_implicit ! not yet developed...
       public :: Finite_Rem_CN_AB2_CG

       ! Uniform σ methods: ∇x∇x = ∇(∇•) - ∇² = - ∇²
       public :: Low_Rem_SOR
       public :: Low_Rem_PSE

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

       subroutine Finite_Rem_CT(B,B0,U,J,E,sigmaInv,m,dt,Rem,tmp_F,tmp_E)
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
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt,Rem
         call faceCrossFace_E(tmp_E,U,B0,m,tmp_F)
         call multiply(E,tmp_E,-1.0_cp)
         call curl(J,B,m)
         call multiply(J,1.0_cp/Rem)
         call multiply(tmp_E,J,sigmaInv)
         call add(E,tmp_E)
         call curl(tmp_F,E,m)
         call multiply(tmp_F,-dt)
         call add(B,tmp_F)
         call applyAllBCs(B,m)
       end subroutine

       subroutine Low_Rem_CT(B,B0,U,J,E,sigmaInv,m,dt,NmaxB,tmp_F,tmp_E)
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
         type(mesh),intent(in) :: m
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

       subroutine Finite_Rem_CG_implicit(B,B0,U,sigmaInv,m,n,compute_norms,&
         tmp_F1,tmp_E_TF,tmp_E_VF)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0,sigmaInv
         type(TF),intent(in) :: U
         type(VF),intent(inout) :: tmp_F1,tmp_E_VF
         type(TF),intent(inout) :: tmp_E_TF
         type(mesh),intent(in) :: m
         integer,intent(in) :: n
         logical,intent(in) :: compute_norms
         call add(tmp_F1,B0,B)
         call face2Edge(tmp_E_TF,tmp_F1,m)
         call cross(tmp_E_VF,U,tmp_E_TF)
         call curl(tmp_F1,tmp_E_VF,m)
         call add(tmp_F1,B)

         call solve(CG,B,tmp_F1,m,n,compute_norms)
         call applyAllBCs(B,m)
       end subroutine

       subroutine Finite_Rem_CN_AB2_CG(B,B0,Bnm1,B0nm1,U,Unm1,sigmaInv,m,n,compute_norms,&
         tmp_F1,tmp_E_TF,tmp_E_VF)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0,sigmaInv
         type(TF),intent(in) :: U,Unm1
         type(VF),intent(inout) :: tmp_F1,tmp_E_VF
         type(TF),intent(inout) :: tmp_E_TF
         type(mesh),intent(in) :: m
         integer,intent(in) :: n
         logical,intent(in) :: compute_norms
         call add(tmp_F1,B0,B)
         call face2Edge(tmp_E_TF,tmp_F1,m)
         call cross(tmp_E_VF,U,tmp_E_TF)
         call curl(tmp_F1,tmp_E_VF,m)
         call add(tmp_F1,B)

         call solve(CG,B,tmp_F1,m,n,compute_norms)
         call applyAllBCs(B,m)
       end subroutine

       subroutine Finite_Rem_CG_semi(B,B0,U,sigmaInv,m,n,dt,Rem,theta,compute_norms,&
         tmp_F1,tmp_F2,tmp_E_TF,tmp_E_VF)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion semi-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0,sigmaInv
         type(TF),intent(in) :: U
         type(VF),intent(inout) :: tmp_F1,tmp_F2,tmp_E_VF
         type(TF),intent(inout) :: tmp_E_TF
         type(mesh),intent(in) :: m
         integer,intent(in) :: n
         real(cp),intent(in) :: dt,Rem,theta
         logical,intent(in) :: compute_norms
         call add(tmp_F1,B0,B)
         call face2Edge(tmp_E_TF,tmp_F1,m)
         call cross(tmp_E_VF,U,tmp_E_TF)
         call curl(tmp_F1,tmp_E_VF,m)
         call add(tmp_F1,B)

         call curl(tmp_E_VF,B,m)
         call multiply(tmp_E_VF,sigmaInv)
         call curl(tmp_F2,tmp_E_VF,m)
         call multiply(tmp_F2,dt/Rem*(1.0_cp-theta))

         call add(tmp_F1,tmp_F2)
         call solve(CG,B,tmp_F1,m,n,compute_norms)
         call applyAllBCs(B,m)
       end subroutine

       subroutine Finite_Rem_O2(B,B0,Bnm1,B0nm1,U,Unm1,sigmaInv,m,n,dt,Rem,compute_norms,&
         tmp_F1,tmp_F2,tmp_E_TF,tmp_E_VF)
         ! Solves:
         !             ∂B/∂t = -∇xE , E = j/σ - uxB⁰ , j = Rem⁻¹∇xB
         ! or
         !             ∂B/∂t = ∇x(uxB⁰) - Rem⁻¹∇x(σ⁻¹∇xB)
         ! Computes:
         !             B,J,E
         ! Method:
         !             Diffusion semi-implicit with Conjugate Gradient method (CG)
         ! Info:
         !             cell face => B
         !             cell edge => J,E,sigmaInv,U
         !             Finite Rem
         implicit none
         type(CG_solver_VF),intent(inout) :: CG
         type(VF),intent(inout) :: B
         type(VF),intent(in) :: B0,sigmaInv
         type(TF),intent(in) :: U
         type(VF),intent(inout) :: tmp_F1,tmp_F2,tmp_E_VF
         type(TF),intent(inout) :: tmp_E_TF
         type(mesh),intent(in) :: m
         integer,intent(in) :: n
         real(cp),intent(in) :: dt,Rem
         logical,intent(in) :: compute_norms
         if (n.gt.1) then
           call add(tmp_F1,B0,B)
           call face2Edge(tmp_E_TF,tmp_F1,m)
           call cross(tmp_E_VF,U,tmp_E_TF)
           call curl(tmp_F1,tmp_E_VF,m)

           call add(tmp_F1,B)

           call curl(tmp_E_VF,B,m)
           call multiply(tmp_E_VF,sigmaInv)
           call curl(tmp_F2,tmp_E_VF,m)
           call multiply(tmp_F2,dt/Rem*0.5_cp)

           call add(tmp_F1,tmp_F2)
           call solve(CG,B,tmp_F1,m,n,compute_norms)
           call applyAllBCs(B,m)
         else
           call add(tmp_F1,B0,B)
           call face2Edge(tmp_E_TF,tmp_F1,m)
           call cross(tmp_E_VF,U,tmp_E_TF)
           call curl(tmp_F1,tmp_E_VF,m)
           call add(tmp_F1,B)

           call curl(tmp_E_VF,B,m)
           call multiply(tmp_E_VF,sigmaInv)
           call curl(tmp_F2,tmp_E_VF,m)
           call multiply(tmp_F2,dt/Rem*0.5_cp)

           call add(tmp_F1,tmp_F2)
           call solve(CG,B,tmp_F1,m,n,compute_norms)
           call applyAllBCs(B,m)
         endif
       end subroutine

       subroutine Low_Rem_SOR(ind,U,m,n,compute_norms)
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

       subroutine Low_Rem_PSE(ind,U,m)
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