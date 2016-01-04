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
       use export_raw_processed_mod

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
       use BCs_mod
       use apply_BCs_mod
       use solverSettings_mod
       use SOR_mod
       use CG_mod
       use preconditioners_mod
       use PCG_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       ! use ADI_mod
       ! use MG_mod
       use probe_base_mod
       use probe_transient_mod

       implicit none

       private
       public :: induction,init,delete,solve

       public :: setDTime,setNmaxB,setNmaxCleanB
       public :: setPiGroups

       public :: export,exportTransient
       public :: printExportBCs,exportMaterial
       public :: computeAddJCrossB,computeJCrossB

       public :: computeDivergence

       logical :: lowRem = .false.
       logical :: finiteRem = .true.
       logical :: semi_implicit = .false.


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
         ! type(TF) :: temp_E_TF                        ! Edge data

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
         type(myTime) :: time_CP

         type(SORSolver) :: SOR_B, SOR_cleanB
         type(CG_solver_VF) :: CG_B
         type(PCG_solver_VF) :: PCG_B

         type(CG_solver_SF) :: CG_cleanB

         type(indexProbe) :: probe_Bx,probe_By,probe_Bz
         type(indexProbe) :: probe_J
         type(errorProbe) :: probe_divB,probe_divJ
         type(probe) :: KB_energy,KB0_energy,KBi_energy
         type(probe) :: KB_f_energy,KB0_f_energy,KBi_f_energy
         type(mesh) :: m
         type(domain) :: D_fluid,D_sigma ! Latter for vacuum case

         type(matrix_free_params) :: MFP_B
         type(matrix_free_params) :: MFP_cleanB

         integer :: nstep             ! Nth time step
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Ha               ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: omega            ! Intensity of time changing magnetic field
         real(cp) :: theta
       end type

       interface init;                 module procedure initInduction;                 end interface
       interface setPiGroups;          module procedure setPiGroupsInduction;          end interface
       interface delete;               module procedure deleteInduction;               end interface
       interface solve;                module procedure inductionSolver;               end interface
       interface printExportBCs;       module procedure inductionPrintExportBCs;       end interface
       interface export;               module procedure export_induction;              end interface
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

         ! call treatInterface(ind%sigmaInv_edge)
         ! call treatInterface(ind%sigmaInv_face)
         write(*,*) '     Interface treated'

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

         ! ind%theta = 1.0_cp - ind%m%dhmin_min*ind%Rem*(1.0_cp/max(ind%sigmaInv))
         ind%theta = 0.9_cp
         if ((ind%theta.le.0.0_cp).or.(ind%theta.ge.1.0_cp)) then
           stop 'Error: 0 < theta < 1 violated in inductionSolver.f90'
         endif
         if (lowRem) ind%MFP_B%c_ind = ind%dTime
         if (finiteRem) ind%MFP_B%c_ind = ind%dTime/ind%Rem
         if (semi_implicit) ind%MFP_B%c_ind = ind%dTime/ind%Rem*ind%theta

         ! init(CG,m,x,k)

         call export_raw(m,ind%sigmaInv_edge,'out/LDC/','sigmaInv_edge',0)

         ! CG,operator,m,MFP,x,k,dir,name,testSymmetry,vizualizeOperator
         call init(ind%CG_B,ind_diffusion,ind_diffusion_explicit,ind%m,ind%MFP_B,ind%B_face,&
         ind%sigmaInv_edge,dir//'Bfield/','B',.false.,.false.)

         call init(ind%PCG_B,ind_diffusion,ind_diffusion_explicit,prec_curl_curl_VF,ind%m,&
         ind%MFP_B,ind%B_face,ind%sigmaInv_edge,dir//'Bfield/','B',.true.,.true.,.true.)
         write(*,*) '     CG Solver initialized'
         stop 'done'

         call init_BC_mesh(ind%phi,ind%m)
         call init_Dirichlet(ind%phi%RF(1)%b)
         call init_BCs(ind%phi,0.0_cp)

         call init(ind%CG_cleanB,Lap_uniform_props,Lap_uniform_props_explicit,ind%m,&
         ind%MFP_B,ind%phi,ind%temp_F,dir//'Bfield/','phi',.false.,.false.)
         write(*,*) '     PCG Solver initialized for phi'

         ! write(*,*) 'dhmin = ',ind%m%dhmin_min
         ! write(*,*) 'Rem = ',ind%Rem
         ! write(*,*) 'min(sigma) = ',1.0_cp/max(ind%sigmaInv)
         ! write(*,*) 'theta = ',ind%theta

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
         call delete(ind%CG_cleanB)
         call delete(ind%PCG_B)

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
         if (solveInduction) call print_BCs(ind%B,'B')
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

       subroutine export_induction(ind,m,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting Solutions for B'
             call export_raw(m,ind%B0   ,dir//'Bfield/','B0',0)
             call export_raw(m,ind%B    ,dir//'Bfield/','B',0)
             call export_raw(m,ind%J_cc ,dir//'Jfield/','J',0)
             call export_raw(m,ind%U_cct,dir//'Bfield/','U',0)
             call export_raw(m,ind%U_E%x%x ,dir//'Bfield/','U',0)
             call export_raw(m,ind%U_E%y%y ,dir//'Bfield/','V',0)
             call export_raw(m,ind%U_E%z%z ,dir//'Bfield/','W',0)
             call export_raw(m,ind%divB,dir//'Bfield/','divB',0)
             call export_raw(m,ind%divJ,dir//'Jfield/','divJ',0)

             call export_processed(m,ind%B0   ,dir//'Bfield/','B0',1)
             call export_processed(m,ind%B    ,dir//'Bfield/','B',1)
             call export_processed(m,ind%J_cc ,dir//'Jfield/','J',1)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine inductionExportMaterial(ind,dir)
         implicit none
         type(induction),intent(inout) :: ind
         character(len=*),intent(in) :: dir
         if (solveInduction) then
             call export_raw(ind%m,ind%sigma,dir//'material/','sigma',0)
             call export_processed(ind%m,ind%sigma,dir//'material/','sigma',1)
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
         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           ! call init_Node(tempVFn,m)
           ! call init_Node(tempVFn2,m)

           ! call cellCenter2Node(tempVFn,ind%B,m)

           ! call cellCenter2Node(tempVFn,ind%B,m,ind%temp_F,ind%temp_E)
           ! call cellCenter2Node(tempVFn2,ind%B0,m,ind%temp_F,ind%temp_E)
           ! call add(tempVFn,tempVFn2)

           ! call cellCenter2Node(tempVFn,ind%J_cc,m)

           ! call cellCenter2Node(tempVFn,ind%J_cc,m)

           ! call delete(tempVFn)
           ! call delete(tempVFn2)
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine inductionSolver(ind,U,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
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


         ! call embedVelocity_E(ind,U)
         call embedVelocity_F(ind,U)
         ! call embedVelocity_CC(ind,U)

         ! call assign(ind%dB0dt,0.0_cp)
         ! call assignZ(ind%dB0dt,ind%omega*exp(-ind%omega*ind%t))

         select case (solveBMethod)
         ! case (1); call lowRemPoisson(ind,ind%U_cct,ind%m,ss_MHD)
         ! case (2); call lowRemPseudoTimeStepUniform(ind,ind%U_cct,ind%m)
         ! case (3); call lowRemPseudoTimeStep(ind,ind%U_cct,ind%m)
         case (4); call lowRemCTmethod(ind,ind%m)
         ! case (5); call finiteRemCTmethod(ind,ind%dB0dt,ind%m)
         case (5); call finiteRemCTmethod(ind,ind%m)

         ! call CG_ind(x,xnm1,b,sigmaInv,Rem,dt,m,n,norm,displayTF)
         case (6); 
         ! call faceCurlCross_F(div,U,B,m,temp_E1,temp_E2,temp_F) ! interface
         ! call export_3D_3C(ind%m,ind%U_cct,'out/LDC/','U_cct',0)
         ! call export_3D_3C(ind%m,ind%B0,'out/LDC/','B0',0)

         ! call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F2,&
         !                      ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F)
         ! call export_3D_1C(ind%m,ind%curlUCrossB%x,'out/LDC/','curlUCrossB_x',0)
         ! call export_3D_1C(ind%m,ind%curlUCrossB%y,'out/LDC/','curlUCrossB_y',0)
         ! call export_3D_1C(ind%m,ind%curlUCrossB%z,'out/LDC/','curlUCrossB_z',0)

         if (lowRem) then
           call cellCenter2Face(ind%temp_F,ind%B0,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,ind%m,5,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif
         if (finiteRem) then
           ! call Finite_Rem_CG_implicit(ind%B,ind%B0,ind%U_E,ind%sigmaInv_edge,&
           ! ind%m,5,getExportErrors(ss_MHD),ind%temp_F,ind%temp_E_TF,ind%temp_E)

           call add(ind%Bstar,ind%B0,ind%B) ! Finite Rem
           call cellCenter2Face(ind%temp_F,ind%Bstar,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative (in divergence form)

           call add(ind%curlUCrossB,ind%B_face)
           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,ind%m,10,getExportErrors(ss_MHD))

           ! Clean B
           call div(ind%divB,ind%B_face,ind%m)
           call solve(ind%CG_cleanB,ind%phi,ind%divB,ind%m,5,getExportErrors(ss_MHD))
           call grad(ind%temp_F,ind%phi,ind%m)
           call subtract(ind%B_face,ind%temp_F)
           call apply_BCs(ind%B_face,ind%m)

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif
         if (semi_implicit) then

           ! call Finite_Rem_CG_semi(ind%B,ind%B0,ind%U_E,ind%sigmaInv_edge,&
           ! ind%m,5,ind%dTime,ind%Rem,ind%theta,getExportErrors(ss_MHD),&
           ! ind%temp_F,ind%temp_F2,ind%temp_E_TF,ind%temp_E)

           call add(ind%Bstar,ind%B0,ind%B) ! Finite Rem
           call cellCenter2Face(ind%temp_F,ind%Bstar,ind%m)

           call faceCurlCross_F(ind%curlUCrossB,ind%U_Ft,ind%temp_F,&
                                ind%m,ind%temp_E1,ind%temp_E2,ind%temp_F2)
           call multiply(ind%curlUCrossB,-ind%dTime) ! Must be negative (in divergence form)

           call add(ind%curlUCrossB,ind%B_face)

           call curl(ind%temp_E,ind%B_face,ind%m)
           call multiply(ind%temp_E,ind%sigmaInv_edge)
           call curl(ind%temp_F,ind%temp_E,ind%m)
           call multiply(ind%temp_F,ind%dTime/ind%Rem*(1.0_cp-ind%theta))
           call add(ind%curlUCrossB,ind%temp_F)

           call solve(ind%CG_B,ind%B_face,ind%curlUCrossB,ind%m,5,getExportErrors(ss_MHD))

           call face2cellCenter(ind%B,ind%B_face,ind%m)
         endif


         ! call cross(ind%temp_CC,ind%U_cct,ind%B0)
         ! call curl(ind%curlUCrossB,ind%temp_CC,ind%m)
         ! call subtract(0.0_cp,ind%curlUCrossB)
         ! call poisson(ind%SOR_B,ind%B%x,ind%curlUCrossB%x,ind%m,ind%ss_ind,&
         ! ind%err_residual,getExportErrors(ss_MHD))
         ! call poisson(ind%SOR_B,ind%B%y,ind%curlUCrossB%y,ind%m,ind%ss_ind,&
         ! ind%err_residual,getExportErrors(ss_MHD))
         ! call poisson(ind%SOR_B,ind%B%z,ind%curlUCrossB%z,ind%m,ind%ss_ind,&
         ! ind%err_residual,getExportErrors(ss_MHD))

         ! case (6); call lowRem_ADI(ind,ind%U_cct,ind%m,ss_MHD)
         ! case (7); call lowRemMultigrid(ind,ind%U_cct,ind%m)
         end select

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
         ! if (getExportErrors(ss_MHD)) call exportTransientFull(ind,ind%m,dir)

         if (getPrintParams(ss_MHD)) then
           call inductionInfo(ind,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowB')
         else; exportNow = .false.
         endif

         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(ind,ind%m,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
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
           call subtract(0.0_cp,ind%E)
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
           call apply_BCs(ind%B,m)
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
         call apply_BCs(ind%B,m)
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
           case (6)
             call div(ind%divB,ind%B_face,m)
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

       subroutine embedVelocity_E(ind,U_E)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U_E ! Momentum edge velocity
         call embedEdge(ind%U_E%x,U_E%x,ind%D_fluid)
         call embedEdge(ind%U_E%y,U_E%y,ind%D_fluid)
         call embedEdge(ind%U_E%z,U_E%z,ind%D_fluid)
       end subroutine

       subroutine embedVelocity_F(ind,U_F)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U_F ! Momentum edge velocity
         call embedFace(ind%U_Ft,U_F,ind%D_fluid)
       end subroutine

       subroutine embedVelocity_CC(ind,U_CC)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U_CC ! Momentum edge velocity
         call embedCC(ind%U_cct,U_CC,ind%D_fluid)
       end subroutine


       end module