       module inductionSolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use export_SF_mod
       use export_VF_mod
       use myTime_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use IO_SF_mod
       use IO_VF_mod

       use init_BBCs_mod
       use init_Bfield_mod
       use init_Sigma_mod
       use ops_embedExtract_mod

       use domain_mod
       use grid_mod
       use mesh_mod
       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use ops_physics_mod
       use apply_BCs_mod
       use solverSettings_mod
       use SOR_mod
       use CG_mod
       use PCG_mod
       ! use ADI_mod
       ! use MG_mod
       use probe_base_mod
       use probe_transient_mod

       implicit none

       private
       public :: induction,init,delete,solve

       public :: setDTime,setNmaxB,setNmaxCleanB
       public :: setPiGroups

       public :: export,exportRaw,exportTransient
       public :: printExportBCs,exportMaterial
       public :: computeAddJCrossB,computeJCrossB

       public :: computeDivergence

       logical :: finite_Rem = .false.

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
         type(VF) :: B,dB0dt,Bstar,B0                 ! CC data
         type(VF) :: J,J_cc,E,temp_E                  ! Edge data
         type(VF) :: Bnm1,curlUCrossB,B_face

         type(VF) :: U_Ft                             ! Face data
         type(VF) :: U_cct                            ! Cell Center data
         type(TF) :: U_E                              ! Edge data

         type(VF) :: temp_E1,temp_E2                  ! Edge data
         type(VF) :: temp_F,temp_F2
         type(VF) :: jCrossB_F                        ! Face data
         type(VF) :: temp_CC                          ! CC data

         type(VF) :: sigmaInv_edge,sigmaInv_face,sigmaInv

         ! --- Scalar fields ---
         type(SF) :: sigma                            ! CC data
         type(SF) :: divB,divJ,phi,temp               ! CC data
         ! BCs:
         ! Solver settings
         type(solverSettings) :: ss_ind,ss_cleanB,ss_ADI
         ! Errors
         type(norms) :: err_divB,err_DivJ
         type(norms) :: err_cleanB,err_residual
         type(norms),dimension(3) :: norm_B
         type(myTime) :: time_CP

         type(SORSolver) :: SOR_B, SOR_cleanB
         type(CG_solver) :: CG_B
         type(PCG_solver) :: PCG_B

         type(indexProbe) :: probe_Bx,probe_By,probe_Bz
         type(indexProbe) :: probe_J
         type(errorProbe) :: probe_divB,probe_divJ
         type(probe) :: KB_energy,KB0_energy,KBi_energy
         type(probe) :: KB_f_energy,KB0_f_energy,KBi_f_energy
         type(mesh) :: m
         type(domain) :: D_fluid,D_sigma ! Latter for vacuum case

         integer :: nstep             ! Nth time step
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Ha               ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: omega            ! Intensity of time changing magnetic field
       end type

       interface init;                 module procedure init_induction;                end interface
       interface setPiGroups;          module procedure setPiGroups_induction;         end interface
       interface set_timeMarching;     module procedure set_timeMarching_induction;    end interface
       interface solve;                module procedure solve_induction;               end interface
       interface delete;               module procedure delete_induction;              end interface


       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initInduction(ind,m,D_fluid,D_sigma,dir,Ha,Rem,dt,NmaxB,NmaxCleanB)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid,D_sigma
         character(len=*),intent(in) :: dir
         real(cp),intent(in) :: Ha,Rem,dt
         integer,intent(in) :: NmaxB,NmaxCleanB
         write(*,*) 'Initializing induction:'
         ind%Ha = Ha
         ind%Rem = Rem
         ind%NmaxB = NmaxB
         ind%dTime = dt
         ind%NmaxCleanB = NmaxCleanB

         call init(ind%m,m)
         call init(ind%D_fluid,D_fluid)
         call init(ind%D_sigma,D_sigma)
         ! --- tensor,vector and scalar fields ---
         call init_Edge(ind%U_E,m,0.0_cp)
         call init_CC(ind%B,m,0.0_cp)
         call init_CC(ind%Bstar,m,0.0_cp)
         call init_CC(ind%B0,m,0.0_cp)
         call init_CC(ind%J_cc,m,0.0_cp)
         call init_CC(ind%U_cct,m,0.0_cp)
         call init_CC(ind%temp_CC,m,0.0_cp)
         call init_CC(ind%dB0dt,m,0.0_cp)
         call init_Edge(ind%J,m,0.0_cp)
         call init_Edge(ind%E,m,0.0_cp)
         call init_Edge(ind%temp_E,m,0.0_cp)
         call init_Edge(ind%temp_E1,m,0.0_cp)
         call init_Edge(ind%temp_E2,m,0.0_cp)
         call init_Edge(ind%sigmaInv_edge,m,0.0_cp)
         call init_Face(ind%U_Ft,m,0.0_cp)
         call init_Face(ind%B_face,m,0.0_cp)
         call init_Face(ind%temp_F,m,0.0_cp)
         call init_Face(ind%sigmaInv_face,m,0.0_cp)
         call init_Face(ind%temp_F2,m,0.0_cp)
         call init_Face(ind%jCrossB_F,m,0.0_cp)
         call init_Face(ind%curlUCrossB,m,0.0_cp)
         call init_CC(ind%sigma,m,0.0_cp)
         call init_CC(ind%Bnm1,m,0.0_cp)
         call init_CC(ind%sigmaInv,m,0.0_cp)
         call init_CC(ind%phi,m,0.0_cp)
         call init_CC(ind%temp,m,0.0_cp)
         call init_CC(ind%divB,m,0.0_cp)
         call init_CC(ind%divJ,m,0.0_cp)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call initBBCs(ind%B,m)
         call initBBCs(ind%B_face,m)
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,m,dir)
         write(*,*) '     B-field initialized'

         call apply_BCs(ind%B,m)
         write(*,*) '     BCs applied'

         call initSigma(ind%sigma,ind%D_sigma,m) ! If sigma changes across wall
         call divide(ind%sigmaInv%x,1.0_cp,ind%sigma)
         call divide(ind%sigmaInv%y,1.0_cp,ind%sigma)
         call divide(ind%sigmaInv%z,1.0_cp,ind%sigma)
         write(*,*) '     Materials initialized'

         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigmaInv,m,ind%temp_F)
         write(*,*) '     Sigma edge defined'
         call cellCenter2Face(ind%sigmaInv_face,ind%sigmaInv,m)
         write(*,*) '     Sigma face defined'

         call treatInterface(ind%sigmaInv_edge)
         call treatInterface(ind%sigmaInv_face)
         write(*,*) '     Interface treated'

         ! init(CG,m,x,k)
         call init(ind%CG_B,ind%m,ind%B_face,ind%sigmaInv_edge)
         call init(ind%PCG_B,ind%m,ind%B_face,ind%sigmaInv_edge,ind%dTime,ind%Rem)

         call init(ind%probe_Bx,dir//'Bfield/','transient_Bx',&
         .not.restartB,ind%B%x%RF(1)%s,(ind%B%x%RF(1)%s+1)/2*2/3,m)

         call init(ind%probe_By,dir//'Bfield/','transient_By',&
         .not.restartB,ind%B%y%RF(1)%s,(ind%B%y%RF(1)%s+1)/2*2/3,m)

         call init(ind%probe_Bz,dir//'Bfield/','transient_Bz',&
         .not.restartB,ind%B%z%RF(1)%s,(ind%B%z%RF(1)%s+1)/2*2/3,m)

         call init(ind%probe_J,dir//'Jfield/','transient_Jx',&
         .not.restartB,ind%J_cc%x%RF(1)%s,(ind%J_cc%x%RF(1)%s+1)*2/3,m)

         call init(ind%probe_divB,dir//'Bfield/','transient_divB',.not.restartB)
         call init(ind%probe_divJ,dir//'Jfield/','transient_divJ',.not.restartB)

         call export(ind%probe_Bx)
         call export(ind%probe_By)
         call export(ind%probe_Bz)
         call export(ind%probe_J)
         call export(ind%probe_divB)
         call export(ind%probe_divJ)
         call init(ind%KB_energy,dir//'Bfield\','KB',.not.restartB)
         call init(ind%KBi_energy,dir//'Bfield\','KBi',.not.restartB)
         call init(ind%KB0_energy,dir//'Bfield\','KB0',.not.restartB)
         call init(ind%KB_f_energy,dir//'Bfield\','KB_f',.not.restartB)
         call init(ind%KBi_f_energy,dir//'Bfield\','KBi_f',.not.restartB)
         call init(ind%KB0_f_energy,dir//'Bfield\','KB0_f',.not.restartB)
         write(*,*) '     B/J probes initialized'

         call init(ind%err_divB)
         call init(ind%err_DivJ)
         call init(ind%err_cleanB)
         call init(ind%err_residual)
         call init(ind%norm_B(1))
         call init(ind%norm_B(2))
         call init(ind%norm_B(3))

         call init(ind%SOR_B,ind%B%x,ind%m)

         call cellCenter2Face(ind%temp_F,ind%B,ind%m)

         ! Initialize solver settings
         call init(ind%ss_ind)
         call setName(ind%ss_ind,'SS B equation       ')
         call setMaxIterations(ind%ss_ind,ind%NmaxB)
         write(*,*) '     Solver settings for B initialized'

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************
         call init(ind%ss_cleanB)
         call setName(ind%ss_cleanB,'cleaning B          ')
         call setMaxIterations(ind%ss_cleanB,ind%NmaxCleanB)
         write(*,*) '     Solver settings for cleaning initialized'

         ! Initialize multigrid
         call inductionInfo(ind,newAndOpen(dir//'parameters/','info_ind'))

         if (restartB) then
         call readLastStepFromFile(ind%nstep,dir//'parameters/','n_ind')
         else; ind%nstep = 0
         endif
         ind%t = 0.0_cp
         ind%omega = 1.0_cp
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

         call delete(ind%J)
         call delete(ind%J_cc)
         call delete(ind%E)

         call delete(ind%temp_CC)
         call delete(ind%temp_E)
         call delete(ind%temp_E1)
         call delete(ind%temp_E2)
         call delete(ind%temp_F)
         call delete(ind%B_face)
         call delete(ind%temp_F2)
         call delete(ind%jCrossB_F)
         call delete(ind%Bnm1)
         call delete(ind%curlUCrossB)
         call delete(ind%temp)

         call delete(ind%sigma)
         call delete(ind%sigmaInv)
         call delete(ind%sigmaInv_edge)
         call delete(ind%sigmaInv_face)

         call delete(ind%divB)
         call delete(ind%divJ)

         call delete(ind%phi)
         call delete(ind%SOR_B)

         call delete(ind%probe_Bx)
         call delete(ind%probe_By)
         call delete(ind%probe_Bz)
         call delete(ind%probe_J)
         call delete(ind%probe_divB)
         call delete(ind%probe_divJ)
         call delete(ind%KB_energy)
         call delete(ind%KBi_energy)
         call delete(ind%KB0_energy)
         call delete(ind%KB_f_energy)
         call delete(ind%KBi_f_energy)
         call delete(ind%KB0_f_energy)
         call delete(ind%m)
         call delete(ind%D_fluid)
         call delete(ind%D_sigma)
         call delete(ind%CG_B)
         call delete(ind%PCG_B)

         write(*,*) 'Induction object deleted'
       end subroutine

       subroutine solve_induction(ind,U,ss_MHD,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(solverSettings),intent(inout) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow

         ! call embedVelocity_E(ind,U)
         call embedVelocity_F(ind,U)
         ! call embedVelocity_CC(ind,U)


         if (finite_Rem) then
           call add(ind%Bstar,ind%B0,ind%B) ! Finite Rem
           call cellCenter2Face(ind%temp_F,ind%Bstar,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,&
           ind%dTime,ind%Rem,ind%m,5,ind%norm_B,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         else
           call cellCenter2Face(ind%temp_F,ind%B0,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,&
           ind%dTime,1.0_cp,ind%m,5,ind%norm_B,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif

         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call computeCurrent(ind)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         call computeTotalMagneticEnergy(ind,ss_MHD)
         call computeTotalMagneticEnergyFluid(ind,ss_MHD)

         call exportTransient(ind,ss_MHD)

         ! call inductionExportTransientFull(ind,ind%m,dir) ! VERY Expensive

         if (getExportErrors(ss_MHD)) call computeDivergence(ind,ind%m)
         if (getExportErrors(ss_MHD)) call exportTransientFull(ind,ind%m,dir)

         if (getPrintParams(ss_MHD)) then
           call inductionInfo(ind,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowB')
         else; exportNow = .false.
         endif

         if (getExportRawSolution(ss_MHD).or.exportNow) then
           call exportRaw(ind,ind%m,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(ind,ind%m,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
       end subroutine



       end module