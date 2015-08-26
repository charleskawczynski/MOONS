       module ind_CTM_mod
       use grid_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use applyBCs_mod

       implicit none

       private
       public :: ind_CTM,init,delete,solve

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type ind_CTM
         type(VF) :: B,Bstar,B0,temp_CC ! CC data
         type(VF) :: sigmaInv_edge,J,E  ! Edge data
         type(VF) :: temp_F             ! Face data
       end type

       interface init;              module procedure initind_CTM;                 end interface
       interface delete;            module procedure deleteind_CTM;               end interface
       interface solve;             module procedure solve_FiniteRem_withSource;  end interface
       interface solve;             module procedure solve_FiniteRem_noSource;    end interface
       interface solve;             module procedure solve_LowRem;                end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initind_CTM(CTM,g,sigma)
         implicit none
         type(ind_CTM),intent(inout) :: CTM
         type(grid),intent(in) :: g
         type(SF),intent(in) :: sigma
         type(SF) :: temp
         write(*,*) 'Initializing ind_CTM:'

         ! --- Vector Fields ---
         call init_CC(CTM%B,g)
         call init_CC(CTM%Bstar,g)
         call init_CC(CTM%B0,g)
         call init_CC(CTM%temp_CC,g)

         call init_Edge(CTM%J,g)
         call init_Edge(CTM%E,g)
         call init_Edge(CTM%sigmaInv_edge,g)

         call init_Face(CTM%temp_F,g)
         call init(temp,sigma)
         call divide(1.0_cp,temp)
         call cellCenter2Edge(CTM%sigmaInv_edge,temp,g,CTM%temp_F)
         call delete(temp)
         write(*,*) '     Sigma edge defined'
         call treatInterface(CTM%sigmaInv_edge)
         write(*,*) '     Materials initialized'

         write(*,*) '     Finished'
       end subroutine

       subroutine deleteind_CTM(CTM)
         implicit none
         type(ind_CTM),intent(inout) :: CTM

         call delete(CTM%B)
         call delete(CTM%Bstar)
         call delete(CTM%B0)
         call delete(CTM%temp_CC)

         call delete(CTM%sigmaInv_edge)
         call delete(CTM%J)
         call delete(CTM%E)

         call delete(CTM%temp_F)

         write(*,*) 'ind_CTM object deleted'
       end subroutine

       subroutine solve_FiniteRem_withSource(CTM,F_CC,U_E,g,Rem,dTime)
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
         type(ind_CTM),intent(inout) :: CTM
         type(VF),intent(in) :: F_CC
         type(TF),intent(in) :: U_E
         type(grid),intent(in) :: g
         real(cp),intent(in) :: Rem,dTime

         ! E = uxB
         call add(CTM%Bstar,CTM%B,CTM%B0)
         call edgeCrossCC_E(CTM%E,U_E%x,U_E%y,U_E%z,CTM%Bstar,g,CTM%temp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(CTM%temp_F,CTM%B,g)
         call curl(CTM%J,CTM%temp_F,g)
         call divide(CTM%J,Rem)

         ! -E = ( uxB - j/sig )_edge
         call multiply(CTM%J,CTM%sigmaInv_edge)
         call subtract(CTM%E,CTM%J)

         ! dBdt = -Curl(E_edge)_face
         call curl(CTM%temp_F,CTM%E,g)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(CTM%temp_CC,CTM%temp_F,g)

         ! dBdt = dBdt + F
         call add(CTM%temp_CC,F_CC)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(CTM%temp_CC,dTime)
         call add(CTM%B,CTM%temp_CC)

         ! Impose BCs:
         call applyAllBCs(CTM%B,g)
       end subroutine

       subroutine solve_FiniteRem_noSource(CTM,U_E,g,Rem,dTime)
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
         type(ind_CTM),intent(inout) :: CTM
         type(TF),intent(in) :: U_E
         type(grid),intent(in) :: g
         real(cp),intent(in) :: Rem,dTime

         ! E = uxB
         call add(CTM%Bstar,CTM%B,CTM%B0)
         call edgeCrossCC_E(CTM%E,U_E%x,U_E%y,U_E%z,CTM%Bstar,g,CTM%temp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(CTM%temp_F,CTM%B,g)
         call curl(CTM%J,CTM%temp_F,g)
         call divide(CTM%J,Rem)

         ! -E = ( uxB - j/sig )_edge
         call multiply(CTM%J,CTM%sigmaInv_edge)
         call subtract(CTM%E,CTM%J)

         ! dBdt = -Curl(E_edge)_face
         call curl(CTM%temp_F,CTM%E,g)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(CTM%temp_CC,CTM%temp_F,g)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(CTM%temp_CC,dTime)
         call add(CTM%B,CTM%temp_CC)

         ! Impose BCs:
         call applyAllBCs(CTM%B,g)
       end subroutine

       subroutine solve_LowRem(CTM,U_E,g,Rem,dTime,NmaxB)
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
         type(ind_CTM),intent(inout) :: CTM
         type(TF),intent(in) :: U_E
         type(grid),intent(in) :: g
         real(cp),intent(in) :: Rem,dTime
         integer,intent(in) :: NmaxB
         integer :: i

         do i=1,NmaxB

           ! J = curl(B_face)_edge
           call cellCenter2Face(CTM%temp_F,CTM%B,g)
           call curl(CTM%J,CTM%temp_F,g)

           ! Compute fluxes of u cross B0
           call edgeCrossCC_E(CTM%E,U_E%x,U_E%y,U_E%z,CTM%B0,g,CTM%temp_F)

           ! E = j/sig - uxB
           ! E = J*sigmaInv_edge - E
           call multiply(CTM%J,CTM%sigmaInv_edge)
           call subtract(0.0_cp,CTM%E)
           call add(CTM%E,CTM%J)

           ! F = curl(E_edge)_face
           call curl(CTM%temp_F,CTM%E,g)

           ! tempVF = interp(F)_face->cc
           call face2CellCenter(CTM%temp_CC,CTM%temp_F,g)

           ! Add induced field of previous time step (B^n)
           ! B = B - dTime*temp_CC
           call multiply(CTM%temp_CC,dTime)
           call subtract(CTM%B,CTM%temp_CC)

           ! Impose BCs:
           call applyAllBCs(CTM%B,g)
         enddo
       end subroutine

       end module