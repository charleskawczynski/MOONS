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
         type(VF) :: B,B0,Bstar                       ! Face data
         type(VF) :: J,E,temp_E                       ! Edge data
         type(TF) :: U_E                              ! Edge data

         type(VF) :: temp_F
         type(VF) :: jCrossB_F                        ! Face data
         type(VF) :: temp_CC                          ! CC data

         type(VF) :: sigmaInv_edge

         type(SF) :: divB,divJ

         type(CG_solver) :: CG_B
         type(PCG_solver) :: PCG_B

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
         type(SF) :: sigma
         write(*,*) 'Initializing induction:'
         ind%Ha = Ha
         ind%Rem = Rem
         ind%NmaxB = NmaxB
         ind%dTime = dt
         ind%NmaxCleanB = NmaxCleanB

         call init(ind%m,m)
         call init(ind%D_fluid,D_fluid)
         call init(ind%D_sigma,D_sigma)

         ! --- Tensor Fields ---
         call init_Edge(ind%U_E,m);           call assign(ind%U_E,0.0_cp)
         ! --- Vector Fields ---
         call init_CC(ind%B,m);               call assign(ind%B,0.0_cp)
         call init_CC(ind%Bstar,m);           call assign(ind%Bstar,0.0_cp)
         call init_CC(ind%B0,m);              call assign(ind%B0,0.0_cp)
         call init_CC(ind%J_cc,m);            call assign(ind%J_cc,0.0_cp)
         call init_CC(ind%temp_CC,m);         call assign(ind%temp_CC,0.0_cp)

         call init_Edge(ind%J,m);             call assign(ind%J,0.0_cp)
         call init_Edge(ind%E,m);             call assign(ind%E,0.0_cp)
         call init_Edge(ind%temp_E,m);        call assign(ind%temp_E,0.0_cp)
         call init_Edge(ind%sigmaInv_edge,m); call assign(ind%sigmaInv_edge,0.0_cp)

         call init_Face(ind%temp_F,m);        call assign(ind%temp_F,0.0_cp)
         call init_Face(ind%jCrossB_F,m);     call assign(ind%jCrossB_F,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(ind%divB,m);            call assign(ind%divB,0.0_cp)
         call init_CC(ind%divJ,m);            call assign(ind%divJ,0.0_cp)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call initBBCs(ind%B,m)
         call initBBCs(ind%B_face,m)
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,m,dir)
         write(*,*) '     B-field initialized'

         call apply_BCs(ind%B,m)
         write(*,*) '     BCs applied'

         call initSigma(sigma,ind%D_sigma,m) ! If sigma changes across wall
         call invert(sigma)
         write(*,*) '     Materials initialized'

         call cellCenter2Edge(ind%sigmaInv_edge,sigma,m,ind%temp_F)
         write(*,*) '     Sigma edge defined'

         call treatInterface(ind%sigmaInv_edge)
         write(*,*) '     Interface treated'

         call init(ind%CG_B,ind%m,ind%B_face,ind%sigmaInv_edge)
         call init(ind%PCG_B,ind%m,ind%B_face,ind%sigmaInv_edge,ind%dTime,ind%Rem)

         call init(ind%probe_divB,dir//'Bfield/','transient_divB',.not.restartB)
         call init(ind%probe_divJ,dir//'Jfield/','transient_divJ',.not.restartB)

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

         call inductionInfo(ind,newAndOpen(dir//'parameters/','info_ind'))

         if (restartB) then
         call readLastStepFromFile(ind%nstep,dir//'parameters/','n_ind')
         else; ind%nstep = 0
         endif
         ind%t = 0.0_cp
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteInduction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%B)
         call delete(ind%Bstar)
         call delete(ind%B0)

         call delete(ind%U_E)

         call delete(ind%J)
         call delete(ind%J_cc)
         call delete(ind%E)

         call delete(ind%temp_CC)
         call delete(ind%temp_E)
         call delete(ind%temp_F)
         call delete(ind%jCrossB_F)

         call delete(ind%sigmaInv_edge)

         call delete(ind%divB)
         call delete(ind%divJ)


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
         logical :: exportNow,lowRem

         ! call embedVelocity_E(ind,U)
         call embedVelocity_F(ind,U)

         lowRem = .true.
         if (lowRem) then
           call cellCenter2Face(ind%temp_F,ind%B0,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,&
           ind%dTime,1.0_cp,ind%m,5,ind%norm_B,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         else
           call add(ind%Bstar,ind%B0,ind%B) ! Finite Rem
           call cellCenter2Face(ind%temp_F,ind%Bstar,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,&
           ind%dTime,ind%Rem,ind%m,5,ind%norm_B,getExportErrors(ss_MHD))

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