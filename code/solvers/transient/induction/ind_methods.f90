       module ind_methods_mod
       use grid_mod
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
       ! use MG_mod

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

       interface solve;  module procedure CT_FRM_source_B_CC; end interface
       interface solve;  module procedure CT_FRM_B_CC;        end interface
       interface solve;  module procedure CT_LRM_B_CC;        end interface
       interface clean;  module procedure clean_SOR_B_F;      end interface
       interface clean;  module procedure clean_SOR_B_CC;     end interface

       contains

       subroutine CT_FRM_source_B_CC(B,B0,U_E,F_CC,J,E,sigInv_E,g,dTime,Rem,tmp_F,Bstar,tmp_CC)
         ! Computes
         !    E = j/(Rem*sig) - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         ! 
         ! using the using the Constrained Transport (CT) Method. 
         ! 
         ! Reference:
         ! "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigInv_E,F_CC
         type(TF),intent(in) :: U_E
         type(VF),intent(inout) :: tmp_F,tmp_CC,Bstar
         type(grid),intent(in) :: g
         real(cp),intent(in) :: dTime,Rem

         ! E = uxB
         call add(Bstar,B,B0)
         call edgeCrossCC_E(E,U_E%x,U_E%y,U_E%z,Bstar,g,tmp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(tmp_F,B,g)
         call curl(J,tmp_F,g)
         call divide(J,Rem)
         call multiply(J,sigInv_E)

         ! -E = ( uxB - j/sig )_edge
         call subtract(E,J)

         ! dBdt = -Curl(E_edge)_face
         call curl(tmp_F,E,g)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(tmp_CC,tmp_F,g)

         ! dBdt = dBdt + F
         call add(tmp_CC,F_CC)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(tmp_CC,dTime)
         call add(B,tmp_CC)

         ! Impose BCs:
         call applyAllBCs(B,g)
       end subroutine

       subroutine CT_FRM_B_CC(B,B0,U_E,J,E,sigInv_E,g,dTime,Rem,tmp_F,Bstar,tmp_CC)
         ! Computes
         !    E = j/(Rem*sig) - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         ! 
         ! using the using the Constrained Transport (CT) Method. 
         ! 
         ! Reference:
         ! "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         implicit none
         type(VF),intent(inout) :: B,J,E
         type(VF),intent(in) :: B0,sigInv_E
         type(TF),intent(in) :: U_E
         type(VF),intent(inout) :: Bstar,tmp_F,tmp_CC
         type(grid),intent(in) :: g
         real(cp),intent(in) :: dTime,Rem

         ! E = uxB
         call add(Bstar,B,B0)
         call edgeCrossCC_E(E,U_E%x,U_E%y,U_E%z,Bstar,g,tmp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(tmp_F,B,g)
         call curl(J,tmp_F,g)
         call divide(J,Rem)
         call multiply(J,sigInv_E)

         ! -E = ( uxB - j/sig )_edge
         call subtract(E,J)

         ! dBdt = -Curl(E_edge)_face
         call curl(tmp_F,E,g)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(tmp_CC,tmp_F,g)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(tmp_CC,dTime)
         call add(B,tmp_CC)

         ! Impose BCs:
         call applyAllBCs(B,g)
       end subroutine

       subroutine CT_LRM_B_CC(B_CC,B0_CC,U_E,J_E,E_E,sigInv_E,g,dTime,NmaxB,tmp_F,tmp_CC)
         ! Computes
         !    E = j/sig - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         ! 
         ! using the using the Constrained Transport (CT) Method. 
         ! 
         ! Reference:
         ! "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         implicit none
         type(VF),intent(inout) :: B_CC,J_E,E_E
         type(VF),intent(in) :: B0_CC,sigInv_E
         type(TF),intent(in) :: U_E
         type(VF),intent(inout) :: tmp_F,tmp_CC
         type(grid),intent(in) :: g
         real(cp),intent(in) :: dTime
         integer,intent(in) :: NmaxB
         integer :: i

         do i=1,NmaxB

           ! Compute fluxes of u cross B0
           call edgeCrossCC_E(E_E,U_E%x,U_E%y,U_E%z,B0_CC,g,tmp_F)
           ! E = - uxB
           call subtract(0.0_cp,E_E)

           ! J = curl(B_face)_edge ! Assumes curl(B0) = 0
           call cellCenter2Face(tmp_F,B_CC,g)
           call curl(J_E,tmp_F,g)
           call multiply(J_E,sigInv_E)

           ! E = j/sig - uxB
           call add(E_E,J_E)

           ! F = curl(E_edge)_face
           call curl(tmp_F,E_E,g)

           ! tmpVF = interp(F)_face->cc
           call face2CellCenter(tmp_CC,tmp_F,g)

           ! Add induced field of previous time step (B^n)
           ! B = B - dTime*tmp_CC
           call multiply(tmp_CC,dTime)
           call subtract(B_CC,tmp_CC)

           ! Impose BCs:
           call applyAllBCs(B_CC,g)
         enddo
       end subroutine

       ! subroutine MG_LRM(MG,B_CC,B0_CC,U_CC,g,ss,tmp_CC)
       !   implicit none
       !   type(multiGrid),dimension(:),intent(in) :: MG
       !   type(VF),intent(inout) :: B_CC
       !   type(VF),intent(in) :: U_CC,B0_CC
       !   type(VF),intent(inout) :: tmp_CC
       !   type(grid),intent(in) :: g
       !   type(norms),intent(inout) :: norm
       !   call CCBfieldAdvect(tmp_CC,U_CC,B0_CC,g)
       !   call solve(MG,B_CC%x,tmp_CC%x,g,ss,norm,.false.)
       !   call solve(MG,B_CC%y,tmp_CC%y,g,ss,norm,.false.)
       !   call solve(MG,B_CC%z,tmp_CC%z,g,ss,norm,.false.)
       !   call applyAllBCs(B_CC,g)
       ! end subroutine

       subroutine clean_SOR_B_F(SOR,B_F,phi_CC,g,norm,ss,TF,tmp_F,tmp_CC)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: B_F,tmp_F
         type(SF),intent(inout) :: phi_CC,tmp_CC
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss
         type(norms),intent(inout) :: norm
         logical,intent(in) :: TF
         call div(tmp_CC,B_F,g)
         call solve(SOR,phi_CC,tmp_CC,g,ss,norm,TF)
         call grad(tmp_F,phi_CC,g)
         call subtract(B_F,tmp_F)
         call applyAllBCs(B_F,g)
       end subroutine

       subroutine clean_SOR_B_CC(SOR,B_CC,phi_CC,g,norm,ss,TF,tmp_F,tmp_CC,tmp_CC_VF)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: B_CC,tmp_F,tmp_CC_VF
         type(SF),intent(inout) :: phi_CC,tmp_CC
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss
         type(norms),intent(inout) :: norm
         logical,intent(in) :: TF
         call div(tmp_CC,B_CC,g)
         call solve(SOR,phi_CC,tmp_CC,g,ss,norm,TF)
         call grad(tmp_F,phi_CC,g)
         call face2CellCenter(tmp_CC_VF,tmp_F,g)
         call subtract(B_CC,tmp_CC_VF)
         call applyAllBCs(B_CC,g)
       end subroutine

       ! subroutine clean_MG(MG,B,phi,tmp_CC,tmp_F,g,norm,ss,TF)
       !   implicit none
       !   type(MG),dimension(:),intent(in) :: MG
       !   type(VF),intent(inout) :: B
       !   type(grid),intent(in) :: tmp_CC
       !   type(solverSettings),intent(in) :: ss
       !   type(norms),intent(inout) :: norm
       !   logical,intent(in) :: TF
       !   call div(tmp_CC,B,g)
       !   call solve(MG,phi,tmp_CC,g,ss,err_cleanB,TF)
       !   call grad(tmp_F,phi,g)
       !   call subtract(B,tmp_F)
       !   call face2CellCenter(B,B_face,g)
       !   call applyAllBCs(B,g)
       ! end subroutine

       end module