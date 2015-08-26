       module inductionSolver_mod
       use simParams_mod
       use SF_mod
       use VF_mod
       use ops_embedExtract_mod
       use grid_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use ops_physics_mod
       use applyBCs_mod
       use solverSettings_mod
       use SOR_mod

       implicit none

       private
       public :: induction,init,delete,solve
       public :: setDTime,setNmaxB,setNmaxCleanB

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type induction
         type(SF) :: divB,divJ,phi,temp               ! CC data
         type(solverSettings) :: ss_ind,ss_cleanB,ss_ADI
         type(SORSolver) :: SOR_B, SOR_cleanB
         type(grid) :: g
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
       end type

       interface init;                 module procedure initInduction;                 end interface
       interface delete;               module procedure deleteInduction;               end interface
       interface solve;                module procedure inductionSolver;               end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initInduction(ind,g,SD,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing induction:'

         ind%g = g
         ind%SD = SD
         ! --- Vector Fields ---
         call init_CC(ind%B,g)
         call init_CC(ind%Bstar,g)
         call init_CC(ind%B0,g)
         call init_CC(ind%J_cc,g)
         call init_CC(ind%U_cct,g)
         call init_CC(ind%temp_CC,g)
         call init_CC(ind%dB0dt,g)

         call init_Edge(ind%J,g)
         call init_Edge(ind%E,g)
         call init_Edge(ind%U_E,g)
         call init_Edge(ind%V_E,g)
         call init_Edge(ind%W_E,g)
         call init_Edge(ind%temp_E,g)
         call init_Edge(ind%temp_E1,g)
         call init_Edge(ind%temp_E2,g)
         call init_Edge(ind%sigmaInv_edge,g)

         call init_Face(ind%U_Ft,g)
         call init_Face(ind%temp_F,g)
         call init_Face(ind%sigmaInv_face,g)
         call init_Face(ind%temp_F2,g)
         call init_Face(ind%jCrossB_F,g)
         call init_Face(ind%B_face,g)

         ! --- Scalar Fields ---
         call init_CC(ind%sigma,g)
         call init_CC(ind%mu,g)
         call init_CC(ind%phi,g)
         call init_CC(ind%temp,g)

         call init_CC(ind%divB,g)
         call init_CC(ind%divJ,g)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call initBBCs(ind%B,g,cleanB)
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,g,dir)
         write(*,*) '     B-field initialized'

         call applyAllBCs(ind%B,g)
         write(*,*) '     BCs applied'

         call initSigmaMu(ind%sigma,ind%mu,ind%SD,g)
         call divide(one,ind%sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigma,g,ind%temp_F)
         write(*,*) '     Sigma edge defined'
         call treatInterface(ind%sigmaInv_edge)

         call cellCenter2Face(ind%sigmaInv_face,ind%sigma,g)
         write(*,*) '     Sigma face defined'
         call initSigmaMu(ind%sigma,ind%mu,ind%SD,g)

         write(*,*) '     Materials initialized'

         write(*,*) '     Finished'
       end subroutine

       subroutine deleteInduction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%B)
         call delete(ind%Bstar)
         call delete(ind%B0)
         call delete(ind%dB0dt)

         call delete(ind%U_cct)
         call delete(ind%U_Ft)
         call delete(ind%U_E)
         call delete(ind%V_E)
         call delete(ind%W_E)

         call delete(ind%J)
         call delete(ind%J_cc)
         call delete(ind%E)

         call delete(ind%temp_CC)
         call delete(ind%temp_E1)
         call delete(ind%temp_E2)
         call delete(ind%temp_F1)
         call delete(ind%temp_F2)
         call delete(ind%jCrossB_F)
         call delete(ind%B_face)
         call delete(ind%temp)

         call delete(ind%sigmaInv_edge)
         call delete(ind%sigmaInv_face)
         
         call delete(ind%sigma)
         call delete(ind%mu)

         call delete(ind%divB)
         call delete(ind%divJ)

         call delete(ind%phi)

         ! call delete(ind%B_bcs)
         ! call delete(ind%phi_bcs)

         call delete(ind%g)

         ! call delete(ind%SOR_B)
         ! if (cleanB) call delete(ind%MG)

         write(*,*) 'Induction object deleted'
       end subroutine

       subroutine cleanBSolution(ind,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         call div(ind%temp,ind%B_face,g)
         call poisson(ind%SOR_cleanB,ind%phi,ind%temp,g,ind%ss_cleanB,&
          ind%err_cleanB,getExportErrors(ss_MHD))
         call grad(ind%temp_F2,ind%phi,g)
         call subtract(ind%B_face,ind%temp_F2)
         call face2CellCenter(ind%B,ind%B_face,g)
         call applyAllBCs(ind%B,g)
       end subroutine

       end module