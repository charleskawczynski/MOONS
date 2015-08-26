       module inductionSolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use export_SF_mod
       use export_VF_mod
       use SF_mod
       use VF_mod
       use IO_SF_mod
       use IO_VF_mod

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
       ! use MG_mod

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
         character(len=9) :: name = 'induction'
         ! --- Vector fields ---
         type(VF) :: B,dB0dt,Bstar,B0,B_face          ! CC data
         type(VF) :: J,J_cc,E,temp_E                  ! Edge data
         ! type(VF),dimension(0:1) :: B                 ! CC data - Applied, and induced fields

         type(VF) :: U_Ft                             ! Face data
         type(VF) :: U_cct                            ! Cell Center data
         type(VF) :: U_E,V_E,W_E                      ! Edge data

         type(VF) :: temp_E1,temp_E2                  ! Edge data
         type(VF) :: temp_F,temp_F2
         type(VF) :: jCrossB_F                        ! Face data
         type(VF) :: temp_CC                          ! CC data

         type(VF) :: sigmaInv_edge,sigmaInv_face

         ! --- Scalar fields ---
         type(SF) :: sigma,mu          ! CC data
         type(SF) :: divB,divJ,phi,temp               ! CC data
         ! BCs:
         ! type(BCs) :: phi_bcs
         ! Solver settings
         type(solverSettings) :: ss_ind,ss_cleanB,ss_ADI

         type(SORSolver) :: SOR_B, SOR_cleanB

         type(grid) :: g

         integer :: nstep             ! Nth time step
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Ha               ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: omega            ! Intensity of time changing magnetic field
       end type

       interface init;                 module procedure initInduction;                 end interface
       interface setPiGroups;          module procedure setPiGroupsInduction;          end interface
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


       subroutine lowRemPoisson_CC(ind,U,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

         call poisson(ind%SOR_B,ind%B%x,ind%temp_CC%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%y,ind%temp_CC%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%z,ind%temp_CC%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       subroutine lowRemPoisson_Face(ind,U,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call cellCenter2Face(ind%temp_F2,ind%B0,g)

         call faceCurlCross_F(ind%temp_F,ind%U_Ft,ind%temp_F2,g,&
         ind%temp_E1,ind%temp_E2,ind%temp_F2)

         call poisson(ind%SOR_B,ind%B_face%x,ind%temp_F%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B_face%y,ind%temp_F%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B_face%z,ind%temp_F%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call face2CellCenter(ind%B,ind%B_face,g)
       end subroutine

       ! subroutine lowRemMultigrid(ind,U,g)
       !   implicit none
       !   type(induction),intent(inout) :: ind
       !   type(VF),intent(in) :: U
       !   type(grid),intent(in) :: g
       !   type(multiGrid),dimension(2) :: MG
       !   call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)
       !   call poisson(MG,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,ind%ss_ind,&
       !   ind%err_residual,.false.)
       !   call poisson(MG,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,ind%ss_ind,&
       !   ind%err_residual,.false.)
       !   call poisson(MG,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,ind%ss_ind,&
       !   ind%err_residual,.false.)
       ! end subroutine

       ! ******************* CLEANING **************************

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