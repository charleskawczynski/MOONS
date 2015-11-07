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
       public :: solve,clean

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

       subroutine CT_FRM_source_B_CC(B,B0,U_E,F_CC,J,E,sigInv_E,m,dTime,Rem,tmp_F,Bstar,tmp_CC)
         ! Computes (CT method):
         !    E = j/(Rem*sig) - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigInv_E,F_CC
         type(TF),intent(in) :: U_E
         type(VF),intent(inout) :: tmp_F,tmp_CC,Bstar
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime,Rem
         call add(Bstar,B,B0)                                  ! E = uxB
         call edgeCrossCC_E(E,U_E%x,U_E%y,U_E%z,Bstar,m,tmp_F)
         call cellCenter2Face(tmp_F,B,m)                       ! J = Rem^-1 curl(B_face)_edge, assumes curl(B0) = 0
         call curl(J,tmp_F,m)
         call divide(J,Rem)
         call multiply(J,sigInv_E)
         call subtract(E,J)                                    ! -E = ( uxB - j/sig )_edge
         call curl(tmp_F,E,m)                                  ! dBdt = -Curl(E_edge)_face
         call face2CellCenter(tmp_CC,tmp_F,m)                  ! dBdt_cc = interp(dBdt)_face->cc
         call add(tmp_CC,F_CC)                                 ! dBdt = dBdt + F
         call multiply(tmp_CC,dTime)                           ! B^n+1 = B^n + {-curl(E) + F}
         call add(B,tmp_CC)
         call applyAllBCs(B,m)                                 ! Impose BCs
       end subroutine

       subroutine CT_FRM_B_CC(B,B0,U_E,J,E,sigInv_E,m,dTime,Rem,tmp_F,Bstar,tmp_CC)
         ! Computes (CT method):
         !    E = j/(Rem*sig) - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigInv_E
         type(TF),intent(in) :: U_E
         type(VF),intent(inout) :: Bstar,tmp_F,tmp_CC
         type(grid),intent(in) :: m
         real(cp),intent(in) :: dTime,Rem
         call add(Bstar,B,B0)                                  ! E = uxB
         call edgeCrossCC_E(E,U_E%x,U_E%y,U_E%z,Bstar,m,tmp_F)
         call cellCenter2Face(tmp_F,B,m)                       ! J = Rem^-1 curl(B_face)_edge, assumes curl(B0) = 0
         call curl(J,tmp_F,m)
         call divide(J,Rem)
         call multiply(J,sigInv_E)
         call subtract(E,J)                                    ! -E = ( uxB - j/sig )_edge
         call curl(tmp_F,E,m)                                  ! dBdt = -Curl(E_edge)_face
         call face2CellCenter(tmp_CC,tmp_F,m)                  ! dBdt_cc = interp(dBdt)_face->cc
         call multiply(tmp_CC,dTime)                           ! B^n+1 = B^n + {-curl(E) + F}
         call add(B,tmp_CC)
         call applyAllBCs(B,m)                                 ! Impose BCs
       end subroutine

       subroutine CT_LRM_B_CC(B_CC,B0_CC,U_E,J_E,E_E,sigInv_E,m,dTime,NmaxB,tmp_F,tmp_CC)
         ! Computes (CT method):
         !    E = j/sig - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         implicit none
         type(VF),intent(inout) :: B_CC,J_E,E_E
         type(VF),intent(in) :: B0_CC,sigInv_E
         type(TF),intent(in) :: U_E
         type(VF),intent(inout) :: tmp_F,tmp_CC
         type(grid),intent(in) :: m
         real(cp),intent(in) :: dTime
         integer,intent(in) :: NmaxB
         integer :: i
         do i=1,NmaxB ! Begin pseudo time-stepping
           call edgeCrossCC_E(E_E,U_E%x,U_E%y,U_E%z,B0_CC,m,tmp_F) ! Compute fluxes of u cross B0
           call subtract(0.0_cp,E_E)                               ! E = - uxB
           call cellCenter2Face(tmp_F,B_CC,m)                      ! J = curl(B_face)_edge, assumes curl(B0) = 0
           call curl(J_E,tmp_F,m)
           call multiply(J_E,sigInv_E)
           call add(E_E,J_E)                                       ! E = j/sig - uxB
           call curl(tmp_F,E_E,m)                                  ! F = curl(E_edge)_face
           call face2CellCenter(tmp_CC,tmp_F,m)                    ! tmpVF = interp(F)_face->cc
           call multiply(tmp_CC,dTime)                             ! Add induced field of previous time step (B^n)
           call subtract(B_CC,tmp_CC)
           call applyAllBCs(B_CC,m)                                ! Impose BCs
         enddo
       end subroutine

       subroutine collocatedB_lowRem_SOR(ind,U,m,printNorms)
         ! Computes (CT method):
         ! ∇²B = 
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         logical,intent(inout) :: printNorms
         ! call CCBfieldAdvect(temp_CC,U,B0,m)
         call poisson(SOR_B,B%x,temp_CC%x,m,ss_ind,err_residual,printNorms)
         call poisson(SOR_B,B%y,temp_CC%y,m,ss_ind,err_residual,printNorms)
         call poisson(SOR_B,B%z,temp_CC%z,m,ss_ind,err_residual,printNorms)
       end subroutine

       subroutine lowRemPoisson(U,m,printNorms)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         logical,intent(inout) :: printNorms
         call cellCenter2Face(temp_F2,B0,m)
         call faceCurlCross_F(temp_F,U_Ft,temp_F2,m,temp_E1,temp_E2,temp_F2)
         call poisson(SOR_B,B_face%x,temp_F%x,m,ss_ind,err_residual,printNorms)
         call poisson(SOR_B,B_face%y,temp_F%y,m,ss_ind,err_residual,printNorms)
         call poisson(SOR_B,B_face%z,temp_F%z,m,ss_ind,err_residual,printNorms)
         call face2CellCenter(B,B_face,m)
       end subroutine

       subroutine collocatedB_lowRem_PseudoTimeStep_Uniform_sigma(ind,U,m)
         ! This routine assumed div(B)=0 (which requires uniform properties), 
         ! which is how it differs from lowRemPseudoTimeStep(). This was an 
         ! important case to test against the Poisson solution, since the 
         ! terms are essentially the same, but a different iterative method 
         ! is applied.
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,NmaxB
           call assign(Bstar,zero)
           call lap(temp_CC,B,m)                 ! Diffusion term
           call multiply(temp_CC,dTime)
           call add(Bstar,temp_CC)
           ! call CCBfieldAdvect(temp_CC,U,B0,m) ! Advective term
           call multiply(temp_CC,dTime)
           call subtract(Bstar,temp_CC)
           call add(B,Bstar)                     ! Add induced field of previous time step (B^n)
           call applyAllBCs(B,m)                 ! Impose BCs
         enddo
       end subroutine

       subroutine lowRemPseudoTimeStep(ind,U,m)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,NmaxB
           call assign(Bstar,zero)
           ! call CCBfieldDiffuse(temp_CC,B,sigmaInv_face,m) ! Diffusion term
           call multiply(temp_CC,dTime)
           call subtract(Bstar,temp_CC)
           ! call CCBfieldAdvect(temp_CC,U,B0,m)             ! Advection term
           call multiply(temp_CC,dTime)
           call subtract(Bstar,temp_CC)
           call add(B,Bstar)                                 ! Add induced field of previous time step (B^n)
           call applyAllBCs(B,m)                             ! Impose BCs
         enddo
       end subroutine

       subroutine clean_SOR_B_F(SOR,B_F,phi_CC,m,norm,ss,TF,tmp_F,tmp_CC)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: B_F,tmp_F
         type(SF),intent(inout) :: phi_CC,tmp_CC
         type(grid),intent(in) :: m
         type(solverSettings),intent(inout) :: ss
         type(norms),intent(inout) :: norm
         logical,intent(in) :: TF
         call div(tmp_CC,B_F,m)
         call solve(SOR,phi_CC,tmp_CC,m,ss,norm,TF)
         call grad(tmp_F,phi_CC,m)
         call subtract(B_F,tmp_F)
         call applyAllBCs(B_F,m)
       end subroutine

       subroutine clean_SOR_B_CC(SOR,B_CC,phi_CC,m,norm,ss,TF,tmp_F,tmp_CC,tmp_CC_VF)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: B_CC,tmp_F,tmp_CC_VF
         type(SF),intent(inout) :: phi_CC,tmp_CC
         type(grid),intent(in) :: m
         type(solverSettings),intent(inout) :: ss
         type(norms),intent(inout) :: norm
         logical,intent(in) :: TF
         call div(tmp_CC,B_CC,m)
         call solve(SOR,phi_CC,tmp_CC,m,ss,norm,TF)
         call grad(tmp_F,phi_CC,m)
         call face2CellCenter(tmp_CC_VF,tmp_F,m)
         call subtract(B_CC,tmp_CC_VF)
         call applyAllBCs(B_CC,m)
       end subroutine

       end module