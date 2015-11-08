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
       use applyBCs_mod
       use solverSettings_mod
       use SOR_mod
       ! use ADI_mod
       ! use MG_mod
       use poisson_mod
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
       public :: embedVelocity

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: zero = 0.0_cp
       real(cp),parameter :: one = 1.0_cp
       real(cp),parameter :: PI = 3.14159265358979_cp

       type induction
         character(len=9) :: name = 'induction'
         ! --- Vector fields ---
         type(VF) :: B,dB0dt,Bstar,B0,B_face          ! CC data
         type(VF) :: J,J_cc,E,temp_E                  ! Edge data
         ! type(VF),dimension(0:1) :: B                 ! CC data - Applied, and induced fields

         type(VF) :: U_Ft                             ! Face data
         type(VF) :: U_cct                            ! Cell Center data
         type(TF) :: U_E                              ! Edge data

         type(VF) :: temp_E1,temp_E2                  ! Edge data
         type(VF) :: temp_F,temp_F2
         type(VF) :: jCrossB_F                        ! Face data
         type(VF) :: temp_CC                          ! CC data

         type(VF) :: sigmaInv_edge,sigmaInv_face

         ! --- Scalar fields ---
         type(SF) :: sigma                            ! CC data
         type(SF) :: divB,divJ,phi,temp               ! CC data
         ! BCs:
         ! type(BCs) :: phi_bcs
         ! Solver settings
         type(solverSettings) :: ss_ind,ss_cleanB,ss_ADI
         ! Errors
         type(norms) :: err_divB,err_DivJ,err_ADI
         type(norms) :: err_cleanB,err_residual
         type(myTime) :: time_CP

         type(SORSolver) :: SOR_B, SOR_cleanB
         ! type(myADI) :: ADI_B
         ! type(multiGrid),dimension(2) :: MG ! For cleaning procedure

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

       interface init;                 module procedure initInduction;                 end interface
       interface setPiGroups;          module procedure setPiGroupsInduction;          end interface
       interface delete;               module procedure deleteInduction;               end interface
       interface solve;                module procedure inductionSolver;               end interface
       interface printExportBCs;       module procedure inductionPrintExportBCs;       end interface
       interface export;               module procedure inductionExport;               end interface
       interface exportRaw;            module procedure inductionExportRaw;            end interface
       interface exportTransient;      module procedure inductionExportTransient;      end interface
       interface exportTransientFull;  module procedure inductionExportTransientFull;  end interface
       interface computeDivergence;    module procedure computeDivergenceInduction;    end interface
       interface exportMaterial;       module procedure inductionExportMaterial;       end interface

       interface setDTime;             module procedure setDTimeInduction;             end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initInduction(ind,m,D_fluid,D_sigma,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid,D_sigma
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing induction:'

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
         call init_CC(ind%U_cct,m);           call assign(ind%U_cct,0.0_cp)
         call init_CC(ind%temp_CC,m);         call assign(ind%temp_CC,0.0_cp)
         call init_CC(ind%dB0dt,m);           call assign(ind%dB0dt,0.0_cp)

         call init_Edge(ind%J,m);             call assign(ind%J,0.0_cp)
         call init_Edge(ind%E,m);             call assign(ind%E,0.0_cp)
         call init_Edge(ind%temp_E,m);        call assign(ind%temp_E,0.0_cp)
         call init_Edge(ind%temp_E1,m);       call assign(ind%temp_E1,0.0_cp)
         call init_Edge(ind%temp_E2,m);       call assign(ind%temp_E2,0.0_cp)
         call init_Edge(ind%sigmaInv_edge,m); call assign(ind%sigmaInv_edge,0.0_cp)

         call init_Face(ind%U_Ft,m)
         call init_Face(ind%temp_F,m);        call assign(ind%temp_F,0.0_cp)
         call init_Face(ind%sigmaInv_face,m); call assign(ind%sigmaInv_face,0.0_cp)
         call init_Face(ind%temp_F2,m);       call assign(ind%temp_F2,0.0_cp)
         call init_Face(ind%jCrossB_F,m);     call assign(ind%jCrossB_F,0.0_cp)
         call init_Face(ind%B_face,m);        call assign(ind%B_face,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(ind%sigma,m);           call assign(ind%sigma,0.0_cp)
         call init_CC(ind%phi,m);             call assign(ind%phi,0.0_cp)
         call init_CC(ind%temp,m);            call assign(ind%temp,0.0_cp)

         call init_CC(ind%divB,m);            call assign(ind%divB,0.0_cp)
         call init_CC(ind%divJ,m);            call assign(ind%divJ,0.0_cp)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call initBBCs(ind%B,m)
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,m,dir)
         write(*,*) '     B-field initialized'

         call applyBCs(ind%B,m)
         write(*,*) '     BCs applied'

         ! call initSigma(ind%sigma,ind%D_fluid,m) ! If sigma changes across fluid
         call initSigma(ind%sigma,ind%D_sigma,m) ! If sigma changes across wall
         call divide(one,ind%sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigma,m,ind%temp_F)
         write(*,*) '     Sigma edge defined'
         call treatInterface(ind%sigmaInv_edge)
         ! call Neumanize(ind%sigmaInv_edge%x,6)
         ! call Neumanize(ind%sigmaInv_edge%y,6)
         ! call Neumanize(ind%sigmaInv_edge%z,6)

         call cellCenter2Face(ind%sigmaInv_face,ind%sigma,m)
         write(*,*) '     Sigma face defined'
         call initSigma(ind%sigma,ind%D_sigma,m)

         write(*,*) '     Materials initialized'

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
         ! if (cleanB) call init(ind%MG,ind%phi%s,ind%phi_bcs,ind%m,ind%ss_cleanB,.false.)
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
         call delete(ind%temp_F2)
         call delete(ind%jCrossB_F)
         call delete(ind%B_face)
         call delete(ind%temp)

         call delete(ind%sigmaInv_edge)
         call delete(ind%sigmaInv_face)
         
         call delete(ind%sigma)

         call delete(ind%divB)
         call delete(ind%divJ)

         call delete(ind%phi)

         ! call delete(ind%B_bcs)
         ! call delete(ind%phi_bcs)

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

         ! call delete(ind%SOR_B)
         ! if (cleanB) call delete(ind%MG)

         write(*,*) 'Induction object deleted'
       end subroutine

       subroutine setDTimeInduction(ind,dt)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: dt
         ind%dTime = dt
       end subroutine

       subroutine setNmaxB(ind,NmaxB)
         implicit none
         type(induction),intent(inout) :: ind
         integer,intent(in) :: NmaxB
         ind%NmaxB = NmaxB
       end subroutine

       subroutine setNmaxCleanB(ind,NmaxCleanB)
         implicit none
         type(induction),intent(inout) :: ind
         integer,intent(in) :: NmaxCleanB
         ind%NmaxCleanB = NmaxCleanB
       end subroutine

       subroutine setPiGroupsInduction(ind,Ha,Rem)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Ha,Rem
         ind%Ha = Ha
         ind%Rem = Rem
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine inductionPrintExportBCs(ind,dir)
         implicit none
         type(induction),intent(in) :: ind
         character(len=*),intent(in) :: dir
         ! if (solveInduction) call printVectorBCs(ind%B%RF(1)%b,'Bx','By','Bz')
         ! if (cleanB)         call printAllBoundaries(ind%phi%RF(1)%b,'phi')
         ! if (solveInduction) call writeVectorBCs(ind%B_bcs,dir//'parameters/','Bx','By','Bz')
         ! if (cleanB)         call writeAllBoundaries(ind%phi%RF(1)%b,dir//'parameters/','phi')
       end subroutine

       subroutine inductionExportTransient(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         if ((getExportTransient(ss_MHD))) then
           call apply(ind%probe_Bx,ind%nstep,ind%B%x%RF(1)%f)
           call apply(ind%probe_By,ind%nstep,ind%B%y%RF(1)%f)
           call apply(ind%probe_Bz,ind%nstep,ind%B%z%RF(1)%f)
           call apply(ind%probe_J,ind%nstep,ind%J_cc%x%RF(1)%f)
         endif

         if (getExportErrors(ss_MHD)) then
           call apply(ind%probe_divB,ind%nstep,ind%divB,ind%m)
           call apply(ind%probe_divJ,ind%nstep,ind%divJ,ind%m)
         endif
       end subroutine

       subroutine inductionExportRaw(ind,m,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting RAW Solutions for B'
             call export_3D_3C(m,ind%B0   ,dir//'Bfield/','B0ct',0)
             call export_3D_3C(m,ind%B    ,dir//'Bfield/','Bct',0)
             call export_3D_3C(m,ind%J_cc ,dir//'Jfield/','Jct',0)
             call export_3D_1C(m,ind%sigma,dir//'material/','sigmac',0)
             call export_3D_1C(m,ind%sigma,dir//'Bfield/','divBct',0)
             call export_3D_1C(m,ind%sigma,dir//'Jfield/','divJct',0)

             call export_3D_1C(m,ind%U_E%x%x ,dir//'Ufield/','Uet',0)
             call export_3D_1C(m,ind%U_E%y%y ,dir//'Ufield/','Vet',0)
             call export_3D_1C(m,ind%U_E%z%z ,dir//'Ufield/','Wet',0)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine inductionExport(ind,m,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(SF) :: tempN,tempCC
         type(VF) :: tempVFn,tempVFn2

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           write(*,*) 'Exporting PROCESSED solutions for B'

           ! B
           call init_Node(tempVFn,m)
           call init_Node(tempVFn2,m)
           call cellCenter2Node(tempVFn,ind%B,m,ind%temp_F,ind%temp_E)
           call export_3D_3C(m,tempVFn,dir//'Bfield/','Bnt',0)
           call export_3D_3C(m,tempVFn,dir//'Bfield/','Bnt_phys',1)

           ! call cellCenter2Node(tempVFn,ind%B0,m,ind%temp_F,ind%temp_E)
           ! call export_3D_3C(m,tempVFn,dir//'Bfield/','B0nt',0)

           ! B0
           call cellCenter2Node(tempVFn,ind%B,m,ind%temp_F,ind%temp_E)
           call cellCenter2Node(tempVFn2,ind%B0,m,ind%temp_F,ind%temp_E)
           call add(tempVFn,tempVFn2)
           call export_3D_3C(m,tempVFn,dir//'Bfield/','Btotnt',0)
           call export_3D_3C(m,tempVFn,dir//'Bfield/','Btotnt_phys',1)

           ! J
           call cellCenter2Node(tempVFn,ind%J_cc,m,ind%temp_F,ind%temp_E)
           call export_3D_3C(m,tempVFn,dir//'Jfield/','Jtotnt_phys',0)
           call delete(tempVFn)
           call delete(tempVFn2)

           ! JxB
           ! call export_3D_1C(m,ind%jCrossB_F%x,dir//'Jfield/','jCrossB_Fx',0)
           ! call export_3D_1C(m,ind%jCrossB_F%y,dir//'Jfield/','jCrossB_Fy',0)
           ! call export_3D_1C(m,ind%jCrossB_F%z,dir//'Jfield/','jCrossB_Fz',0)

           ! sigma
           call init_Node(tempN,m)
           call cellCenter2Node(tempN,ind%sigma,m,ind%temp_F%x,ind%temp_E%z)
           call treatInterface(tempN)
           call export_3D_1C(m,tempN,dir//'material/','sigman',1)
           call delete(tempN)

           ! U_induction
           call init_CC(tempCC,m)
           call div(tempCC,ind%U_cct,m)
           call export_3D_3C(m,ind%U_cct,dir//'Ufield/','Ucct',0)
           call export_3D_1C(m,tempCC,dir//'Ufield/','divUct',0)
           call delete(tempCC)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine inductionExportMaterial(ind,dir)
         implicit none
         type(induction),intent(inout) :: ind
         character(len=*),intent(in) :: dir
         type(SF) :: tempN
         if (solveInduction) then
           call init_Node(tempN,ind%m)
           call cellCenter2Node(tempN,ind%sigma,ind%m,ind%temp_F%x,ind%temp_E%z)
           call treatInterface(tempN)
           call export_3D_1C(ind%m,ind%sigma,dir//'material/','sigmac',0)
           call export_3D_1C(ind%m,tempN,dir//'material/','sigman',1)
           call delete(tempN)
         endif
       end subroutine

       subroutine inductionInfo(ind,un)
         ! Use un = 6 to print to screen
         implicit none
         type(induction),intent(in) :: ind
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MAGNETIC **************************'
         write(un,*) '**************************************************************'
         write(un,*) '(Rem) = ',ind%Rem
         write(un,*) '(t,dt) = ',ind%t,ind%dTime
         write(un,*) '(nstep) = ',ind%nstep
         write(un,*) ''
         call export(ind%m,un)
         write(un,*) ''
         call printPhysicalMinMax(ind%B,'B')
         call printPhysicalMinMax(ind%B0,'B0')
         call printPhysicalMinMax(ind%divB,'divB')
         call printPhysicalMinMax(ind%divJ,'divJ')
       end subroutine

       subroutine inductionExportTransientFull(ind,m,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(VF) :: tempVFn,tempVFn2

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           call init_Node(tempVFn,m)
           call init_Node(tempVFn2,m)

           ! call cellCenter2Node(tempVFn,ind%B,m)

           call cellCenter2Node(tempVFn,ind%B,m,ind%temp_F,ind%temp_E)
           call cellCenter2Node(tempVFn2,ind%B0,m,ind%temp_F,ind%temp_E)
           call add(tempVFn,tempVFn2)

           ! call cellCenter2Node(tempVFn,ind%J_cc,m)

           ! call cellCenter2Node(tempVFn,ind%J_cc,m)

           call delete(tempVFn)
           call delete(tempVFn2)
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine inductionSolver(ind,U,g_mom,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U
         type(mesh),intent(in) :: g_mom
         type(solverSettings),intent(inout) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow
         ! ********************** LOCAL VARIABLES ***********************
         ! ind%B0%x = exp(-ind%omega*ind%t)
         ! ind%B0%y = exp(-ind%omega*ind%t)
         ! ind%B0%z = 1.0_cp

         ! ind%B0%x = exp(-ind%omega*ind%t)
         ! ind%B0%y = 0.0_cp
         ! ind%B0%z = 0.0_cp

         ! ind%B0%x = 0.0_cp
         ! ind%B0%y = 0.0_cp
         ! ind%B0%z = exp(-ind%omega*ind%t)


         call embedVelocity(ind,U)

         ! call assign(ind%dB0dt,0.0_cp)
         ! call assignZ(ind%dB0dt,ind%omega*exp(-ind%omega*ind%t))

         select case (solveBMethod)
         ! case (1); call lowRemPoisson(ind,ind%U_cct,ind%m,ss_MHD)
         ! case (2); call lowRemPseudoTimeStepUniform(ind,ind%U_cct,ind%m)
         ! case (3); call lowRemPseudoTimeStep(ind,ind%U_cct,ind%m)
         case (4); call lowRemCTmethod(ind,ind%m)
         ! case (5); call finiteRemCTmethod(ind,ind%dB0dt,ind%m)
         case (5); call finiteRemCTmethod(ind,ind%m)
         ! case (6); call lowRem_ADI(ind,ind%U_cct,ind%m,ss_MHD)
         ! case (7); call lowRemMultigrid(ind,ind%U_cct,ind%m)
         end select

         if (cleanB) call cleanBSolution(ind,ind%m,ss_MHD)

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

       subroutine lowRemPoissonOld(ind,U,m,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(solverSettings),intent(inout) :: ss_MHD
         ! call CCBfieldAdvect(ind%temp_CC,U,ind%B0,m)

         call poisson(ind%SOR_B,ind%B%x,ind%temp_CC%x,m,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%y,ind%temp_CC%y,m,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%z,ind%temp_CC%z,m,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       subroutine lowRemPoisson(ind,U,m,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(solverSettings),intent(inout) :: ss_MHD

         call cellCenter2Face(ind%temp_F2,ind%B0,m)

         call faceCurlCross_F(ind%temp_F,ind%U_Ft,ind%temp_F2,m, &
         ind%temp_E1,ind%temp_E2,ind%temp_F2)

         call poisson(ind%SOR_B,ind%B_face%x,ind%temp_F%x,m,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B_face%y,ind%temp_F%y,m,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B_face%z,ind%temp_F%z,m,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call face2CellCenter(ind%B,ind%B_face,m)
       end subroutine

       subroutine lowRemPseudoTimeStepUniform(ind,U,m)
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
         
         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)
           ! ------------- diffusion term -------------
           call lap(ind%temp_CC,ind%B,m)
           call multiply(ind%temp_CC,ind%dTime)
           call add(ind%Bstar,ind%temp_CC)
           
           ! ------------- source term -------------
           ! call CCBfieldAdvect(ind%temp_CC,U,ind%B0,m)
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)

           ! Add induced field of previous time step (B^n)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyBCs(ind%B,m)
         enddo
       end subroutine

       subroutine lowRemPseudoTimeStep(ind,U,m)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer :: i

         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)

           ! ------------- diffusion term -------------
           ! call CCBfieldDiffuse(ind%temp_CC,ind%B,ind%sigmaInv_face,m)

           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)
           
           ! ------------- source term -------------
           ! call CCBfieldAdvect(ind%temp_CC,U,ind%B0,m)
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)

           ! Add induced field of previous time step (B^n)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyBCs(ind%B,m)
         enddo
       end subroutine

       subroutine lowRemCTmethod(ind,m)
         ! inductionSolverCT solves the induction equation using the
         ! Constrained Transport (CT) Method. The magnetic field is
         ! stored and collocated at the cell center. The magnetic
         ! field is updated using Faraday's Law, where the electric
         ! field is solved for using appropriate fluxes as described
         ! in "Tóth, m. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         ! The velocity field is assumed to be cell centered.
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         integer :: i

         do i=1,ind%NmaxB

           ! Compute current from appropriate fluxes:

           ! J = curl(B_face)_edge
           call cellCenter2Face(ind%temp_F,ind%B,m)
           call curl(ind%J,ind%temp_F,m)

           ! Compute fluxes of u cross B0
           call edgeCrossCC_E(ind%E,ind%U_E%x,ind%U_E%y,ind%U_E%z,ind%B0,m,ind%temp_F)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! F = curl(E_edge)_face
           call curl(ind%temp_F,ind%E,m)

           ! tempVF = interp(F)_face->cc
           call face2CellCenter(ind%temp_CC,ind%temp_F,m)

           ! Add induced field of previous time step (B^n)
           ! ind%B = ind%B - ind%dTime*ind%temp_CC
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%B,ind%temp_CC)

           ! Impose BCs:
           call applyBCs(ind%B,m)
         enddo
       end subroutine

       subroutine finiteRemCTmethod(ind,m)
         ! Computes
         !    E = j/(Rem*sig) - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         ! 
         ! using the using the Constrained Transport (CT) Method. 
         ! 
         ! Reference:
         ! "Tóth, m. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m

         ! E = uxB
         call add(ind%Bstar,ind%B,ind%B0)
         call edgeCrossCC_E(ind%E,ind%U_E%x,ind%U_E%y,ind%U_E%z,ind%Bstar,m,ind%temp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(ind%temp_F,ind%B,m)
         call curl(ind%J,ind%temp_F,m)
         call divide(ind%J,ind%Rem)

         ! -E = ( uxB - j/sig )_edge
         call multiply(ind%J,ind%sigmaInv_edge)
         call subtract(ind%E,ind%J)

         ! dBdt = -Curl(E_edge)_face
         call curl(ind%temp_F,ind%E,m)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(ind%temp_CC,ind%temp_F,m)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(ind%temp_CC,ind%dTime)
         call add(ind%B,ind%temp_CC)

         ! Impose BCs:
         call applyBCs(ind%B,m)
       end subroutine

       subroutine finiteRemCTmethod_with_source(ind,F_CC,m)
         ! Computes
         !    E = j/(Rem*sig) - uxB
         !    dBdt = -curl(E) + F
         !    B^n+1 = B^n + {-curl(E) + F}
         ! 
         ! using the using the Constrained Transport (CT) Method. 
         ! 
         ! Reference:
         ! "Tóth, m. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: F_CC
         type(mesh),intent(in) :: m

         ! E = uxB
         call add(ind%Bstar,ind%B,ind%B0)
         call edgeCrossCC_E(ind%E,ind%U_E%x,ind%U_E%y,ind%U_E%z,ind%Bstar,m,ind%temp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(ind%temp_F,ind%B,m)
         call curl(ind%J,ind%temp_F,m)
         call divide(ind%J,ind%Rem)

         ! -E = ( uxB - j/sig )_edge
         call multiply(ind%J,ind%sigmaInv_edge)
         call subtract(ind%E,ind%J)

         ! dBdt = -Curl(E_edge)_face
         call curl(ind%temp_F,ind%E,m)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(ind%temp_CC,ind%temp_F,m)

         ! dBdt = dBdt + F
         call add(ind%temp_CC,F_CC)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(ind%temp_CC,ind%dTime)
         call add(ind%B,ind%temp_CC)

         ! Impose BCs:
         call applyBCs(ind%B,m)
       end subroutine

!        subroutine lowRem_ADI(ind,U,m,ss_MHD)
!          ! inductionSolverCT solves the induction equation using the
!          ! Constrained Transport (CT) Method. The magnetic field is
!          ! stored and collocated at the cell center. The magnetic
!          ! field is updated using Faraday's Law, where the electric
!          ! field is solved for using appropriate fluxes as described
!          ! in "Tóth, m. The divergence Constraint in Shock-Capturing 
!          ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
!          ! The velocity field is assumed to be cell centered.
!          implicit none
!          ! ********************** INPUT / OUTPUT ************************
!          type(induction),intent(inout) :: ind
!          type(VF),intent(in) :: U
!          type(mesh),intent(in) :: m
!          type(solverSettings),intent(inout) :: ss_MHD

!          ! J = curl(B_face)_edge
!          call cellCenter2Face(ind%temp_F,ind%B,m)
!          call curl(ind%J,ind%temp_F,m)

!          ! Compute fluxes of u cross B0
!          call cross(ind%temp_CC,U,ind%B0)
!          call cellCenter2Edge(ind%E,ind%temp_CC,m)

!          ! E = j/sig - uxB
!          ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
!          call multiply(ind%J,ind%sigmaInv_edge)
!          call subtract(zero,ind%E)
!          call add(ind%E,ind%J)

!          ! F = Curl(E)
!          call curl(ind%temp_F,ind%E,m)

!          ! tempVF = interp(F)_face->cc
!          call face2CellCenter(ind%temp_CC,ind%temp_F,m)

!          ! Subtract laplacian from B^n
!          call lap(ind%Bstar,ind%B,ind%sigmaInv_face,m)

!          ! ind%temp_CC = ind%temp_CC - ind%Bstar
!          call subtract(ind%temp_CC,ind%Bstar)

!          ! Solve with semi-implicit ADI
!          call setDt(ind%ADI_B,ind%dTime)
!          call setAlpha(ind%ADI_B,1.0_cp)

!          call init(ind%ss_ADI)
!          call setName(ind%ss_ADI,'ADI for B-field     ')

!          call apply(ind%ADI_B,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,m,&
!             ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
!          call apply(ind%ADI_B,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,m,&
!             ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
!          call apply(ind%ADI_B,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,m,&
!             ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))

!          ! Impose BCs:
!          call applyBCs(ind%B,ind%B_bcs,m)
!        end subroutine

!        subroutine lowRemMultigrid(ind,U,m)
!          implicit none
!          ! ********************** INPUT / OUTPUT ************************
!          type(induction),intent(inout) :: ind
!          type(VF),intent(in) :: U
!          type(mesh),intent(in) :: m
!          type(multiGrid),dimension(2) :: MG

!          call CCBfieldAdvect(ind%temp_CC,U,ind%B0,m)

!          call poisson(MG,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,m,ind%ss_ind,&
!          ind%err_residual,.false.)

!          call poisson(MG,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,m,ind%ss_ind,&
!          ind%err_residual,.false.)

!          call poisson(MG,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,m,ind%ss_ind,&
!          ind%err_residual,.false.)
!        end subroutine

       ! ******************* CLEANING **************************

       subroutine cleanBSolution(ind,m,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(solverSettings),intent(in) :: ss_MHD
         call div(ind%temp,ind%B_face,m)

         call poisson(ind%SOR_cleanB,ind%phi,ind%temp,m,ind%ss_cleanB,&
          ind%err_cleanB,getExportErrors(ss_MHD))

         call grad(ind%temp_F2,ind%phi,m)
         call subtract(ind%B_face,ind%temp_F2)

         call face2CellCenter(ind%B,ind%B_face,m)

         call applyBCs(ind%B,m)
       end subroutine

       ! ********************* COMPUTE *****************************

       subroutine computeAddJCrossB(jcrossB,ind,Ha,Re,Rem)
         ! addJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the applied or total mangetic
         ! field, depending on the solveBMethod.
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Ha,Re,Rem
         type(VF) :: temp
         call init(temp,jcrossB)
         call assign(temp,0.0_cp)
         call computeJCrossB(temp,ind,Ha,Re,Rem)
         call add(jcrossB,temp)
         call delete(temp)
       end subroutine

       subroutine computeJCrossB(jcrossB,ind,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J_cc,ind%B,ind%m)
           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%m)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_functional(jcrossB,B,B0,J_cc,m,D_fluid,Ha,Re,Rem,Bstar,temp_CC,jCrossB_F)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(VF),intent(in) :: B,B0
         type(VF),intent(inout) :: J_cc,Bstar,temp_CC,jCrossB_F
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(Bstar,B,B0)
           call curl(J_cc,B,m)
           call cross(temp_CC,J_cc,Bstar)
           call cellCenter2Face(jCrossB_F,temp_CC,m)
           call extractFace(jcrossB,jCrossB_F,D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(J_cc,B,m)
           call cross(temp_CC,J_cc,B0)
           call cellCenter2Face(jCrossB_F,temp_CC,m)
           call extractFace(jcrossB,jCrossB_F,D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_Bface(jcrossB,ind,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J,ind%B,ind%m)
           ! call edge2Face(ind%temp_F,ind%J,ind%m)

           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%m)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_new(jcrossB,ind,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Ha,Re,Rem

         ! Magnetic Pressure (not yet done)
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call square(ind%Bstar)
           call divide(ind%Bstar,2.0_cp)
           ! call grad(ind%jCrossB_F,ind%Bstar,ind%m)
           call multiply(ind%jCrossB_F,-1.0_cp)
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%m)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select

         ! Magnetic Stress (not yet done)
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J_cc,ind%B,ind%m)
           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%m)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%m)
           call extractFace(jcrossB,ind%jCrossB_F,ind%D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeDivergenceInduction(ind,m)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         if (solveInduction) then
           select case (solveBMethod)
           case (4:5)
             ! CT method enforces div(b) = 0, (result is in CC), 
             ! when computed from FACE-centered data:
             call div(ind%divB,ind%temp_F,m)
           case default
             call div(ind%divB,ind%B,m)
           end select
         endif
         call div(ind%divJ,ind%J_cc,m)
       end subroutine

       subroutine computeCurrent(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call add(ind%Bstar,ind%B0,ind%B)
         call curl(ind%J_cc,ind%Bstar,ind%m)
       end subroutine

       subroutine computeTotalMagneticEnergyFluid(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
          if (computeKB.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call assign(ind%Bstar,ind%B)
           call add(ind%Bstar,ind%B0)
           call totalEnergy(K_energy,ind%Bstar,ind%D_fluid)
           call set(ind%KB_f_energy,ind%nstep,K_energy)
           call apply(ind%KB_f_energy)
          endif
          if (computeKBi.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call assign(ind%Bstar,ind%B)
           call totalEnergy(K_energy,ind%Bstar,ind%D_fluid)
           call set(ind%KBi_f_energy,ind%nstep,K_energy)
           call apply(ind%KBi_f_energy)
          endif
          if (computeKB0.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call totalEnergy(K_energy,ind%Bstar,ind%D_fluid)
           call set(ind%KB0_f_energy,ind%nstep,K_energy)
           call apply(ind%KB0_f_energy)
          endif
       end subroutine

       subroutine computeTotalMagneticEnergy(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
          if (computeKB.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call assign(ind%Bstar,ind%B)
           call add(ind%Bstar,ind%B0)
           call totalEnergy(K_energy,ind%Bstar,ind%m)
           call set(ind%KB_energy,ind%nstep,K_energy)
           call apply(ind%KB_energy)
          endif
          if (computeKBi.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call totalEnergy(K_energy,ind%B,ind%m)
           call set(ind%KBi_energy,ind%nstep,K_energy)
           call apply(ind%KBi_energy)
          endif
          if (computeKB0.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call totalEnergy(K_energy,ind%B0,ind%m)
           call set(ind%KB0_energy,ind%nstep,K_energy)
           call apply(ind%KB0_energy)
          endif
       end subroutine

       ! ********************* AUX *****************************

       subroutine embedVelocity(ind,U_E)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U_E ! Momentum edge velocity
         call embedEdge(ind%U_E%x,U_E%x,ind%D_fluid)
         call embedEdge(ind%U_E%y,U_E%y,ind%D_fluid)
         call embedEdge(ind%U_E%z,U_E%z,ind%D_fluid)
         ! call Neumanize(ind%U_E%x%x,6)
         ! call Neumanize(ind%U_E%x%y,6)
         ! call Neumanize(ind%U_E%x%z,6)
         ! call Neumanize(ind%U_E%y%x,6)
         ! call Neumanize(ind%U_E%y%y,6)
         ! call Neumanize(ind%U_E%y%z,6)
         ! call Neumanize(ind%U_E%z%x,6)
         ! call Neumanize(ind%U_E%z%y,6)
         ! call Neumanize(ind%U_E%z%z,6)
       end subroutine

       subroutine Neumanize(f,face)
         implicit none
         type(SF),intent(inout) :: f
         integer,intent(in) :: face
         integer :: i
         select case (face)
         case (1); do i=1,f%s; f%RF(i)%f(1,:,:) = f%RF(i)%f(2,:,:)                        ; enddo
         case (2); do i=1,f%s; f%RF(i)%f(f%RF(i)%s(1),:,:) = f%RF(i)%f(f%RF(i)%s(1)-1,:,:); enddo
         case (3); do i=1,f%s; f%RF(i)%f(:,1,:) = f%RF(i)%f(:,2,:)                        ; enddo
         case (4); do i=1,f%s; f%RF(i)%f(:,f%RF(i)%s(2),:) = f%RF(i)%f(:,f%RF(i)%s(2)-1,:); enddo
         case (5); do i=1,f%s; f%RF(i)%f(:,:,1) = f%RF(i)%f(:,:,2)                        ; enddo
         case (6); do i=1,f%s; f%RF(i)%f(:,:,f%RF(i)%s(3)) = f%RF(i)%f(:,:,f%RF(i)%s(3)-1); enddo
         case default; stop 'Error: face must = 1:6 in Neumanize in inductionSolver.f90'
         end select
       end subroutine

       ! subroutine embedVelocity_old(ind,U_fi,m)
       !   implicit none
       !   type(induction),intent(inout) :: ind
       !   type(VF),intent(in) :: U_fi ! Raw momentum velocity
       !   type(mesh),intent(in) :: m ! Momentum mesh
       !   type(VF) :: temp
       !   logical,dimension(4) :: usedVelocity
       !   usedVelocity = (/.true.,.false.,.false.,.false./)
       !   if (usedVelocity(1)) then ! Edge - 2 interpolations
       !     call init_Edge(temp,m)
       !     call face2Edge(temp%x,U_fi%x,m,1,1)
       !     call face2Edge(temp%y,U_fi%x,m,1,2)
       !     call face2Edge(temp%z,U_fi%x,m,1,3)
       !     call embedEdge(ind%U_E,temp,ind%D_fluid,m)
       !     call face2Edge(temp%x,U_fi%y,m,2,1)
       !     call face2Edge(temp%y,U_fi%y,m,2,2)
       !     call face2Edge(temp%z,U_fi%y,m,2,3)
       !     call embedEdge(ind%V_E,temp,ind%D_fluid,m)
       !     call face2Edge(temp%x,U_fi%z,m,3,1)
       !     call face2Edge(temp%y,U_fi%z,m,3,2)
       !     call face2Edge(temp%z,U_fi%z,m,3,3)
       !     call embedEdge(ind%W_E,temp,ind%D_fluid,m)
       !     call delete(temp)
       !     ! call printPhysicalMinMax(ind%U_E,'U_E')
       !     ! call printPhysicalMinMax(ind%V_E,'V_E')
       !     ! call printPhysicalMinMax(ind%W_E,'W_E')
       !   endif
       !   if (usedVelocity(2)) then ! CC - 1 interpolation
       !     call init_CC(temp,m)
       !     call face2CellCenter(temp%x,U_fi%x,m,1)
       !     call face2CellCenter(temp%y,U_fi%y,m,2)
       !     call face2CellCenter(temp%z,U_fi%z,m,3)
       !     call embedCC(ind%U_cct,temp,ind%D_fluid,m)
       !     call delete(temp)
       !   endif
       !   if (usedVelocity(3)) then ! Face - no interpolations
       !     call embedFace(ind%U_Ft,U_Fi,ind%D_fluid,m)
       !   endif
       !   ! if (usedVelocity(4)) then ! Node - 3 interpolations (not needed for any solvers)
       !     ! call allocateX(temp,m%c(1)%sc,m%c(2)%sn,m%c(3)%sn)
       !     ! call allocateY(temp,m%c(1)%sn,m%c(2)%sc,m%c(3)%sn)
       !     ! call allocateZ(temp,m%c(1)%sn,m%c(2)%sn,m%c(3)%sc)
       !     ! call face2Node(temp%x,U_fi%x,m,1)
       !     ! call face2Node(temp%y,U_fi%x,m,1)
       !     ! call face2Node(temp%z,U_fi%x,m,1)
       !     ! call embedNode(ind%U_N,temp,Nin1,Nin2,m)
       !     ! call face2Node(temp%x,U_fi%y,m,2)
       !     ! call face2Node(temp%y,U_fi%y,m,2)
       !     ! call face2Node(temp%z,U_fi%y,m,2)
       !     ! call embedNode(ind%V_N,temp,Nin1,Nin2,m)
       !     ! call face2Node(temp%x,U_fi%z,m,3)
       !     ! call face2Node(temp%y,U_fi%z,m,3)
       !     ! call face2Node(temp%z,U_fi%z,m,3)
       !     ! call embedNode(ind%W_N,temp,Nin1,Nin2,m)
       !     ! call delete(temp)
       !   ! endif
       ! end subroutine

!        subroutine perturbAll(f,m)
!          implicit none
!          real(cp),dimension(:,:,:),intent(inout) :: f
!          type(mesh),intent(in) :: m
!          real(cp),dimension(3) :: wavenum,eps
!          integer,dimension(3) :: s
!          integer :: i,j,k
!          s = shape(f)
!          wavenum = 0.1_cp
!          eps = 0.01_cp
!          if (all((/(s(i).eq.m%c(i)%sn,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(1)*sin(wavenum(1)*PI*m%c(1)%hn(i)) +&
!                                            eps(2)*sin(wavenum(2)*PI*m%c(2)%hn(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hn(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          elseif (all((/(s(i).eq.m%c(i)%sc,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(1)*sin(wavenum(1)*PI*m%c(1)%hc(i)) +&
!                                            eps(2)*sin(wavenum(2)*PI*m%c(2)%hc(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hc(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          else
!           stop 'Error: unmatched case in perturbAll in inductionSolver.f90'
!          endif
!        end subroutine

!        subroutine perturb(f,m)
!          implicit none
!          real(cp),dimension(:,:,:),intent(inout) :: f
!          type(mesh),intent(in) :: m
!          real(cp),dimension(3) :: wavenum,eps
!          integer,dimension(3) :: s
!          integer :: i,j,k
!          s = shape(f)
!          wavenum = 10.0_cp
!          eps = 0.1_cp
!          if (all((/(s(i).eq.m%c(i)%sn,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(2)*sin(wavenum(2)*PI*m%c(2)%hn(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hn(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          elseif (all((/(s(i).eq.m%c(i)%sc,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(2)*sin(wavenum(2)*PI*m%c(2)%hc(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hc(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          else
!           stop 'Error: unmatched case in perturb in inductionSolver.f90'
!          endif
!        end subroutine

       end module