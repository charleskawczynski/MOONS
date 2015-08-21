       module inductionSolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use export_SF_mod
       use export_VF_mod
       use myTime_mod
       use SF_mod
       use VF_mod
       use IO_SF_mod
       use IO_VF_mod

       use initializeBBCs_mod
       use initializeBfield_mod
       use initializeSigmaMu_mod
       use ops_embedExtract_mod

       use grid_mod
       use norms_mod
       use del_mod
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
         type(grid) :: g
         type(subdomain) :: SD

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

       subroutine initInduction(ind,g,SD,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
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
         call treatInterface(ind%sigmaInv_edge)

         call cellCenter2Face(ind%sigmaInv_face,ind%sigma,g)
         call initSigmaMu(ind%sigma,ind%mu,ind%SD,g)

         write(*,*) '     Materials initialized'

         call init(ind%probe_Bx,dir//'Bfield/','transient_Bx',&
         .not.restartB,shape(ind%B%x),(shape(ind%B%x)+1)/2*2/3,g)

         call init(ind%probe_By,dir//'Bfield/','transient_By',&
         .not.restartB,shape(ind%B%y),(shape(ind%B%y)+1)/2*2/3,g)

         call init(ind%probe_Bz,dir//'Bfield/','transient_Bz',&
         .not.restartB,shape(ind%B%z),(shape(ind%B%z)+1)/2*2/3,g)

         call init(ind%probe_J,dir//'Jfield/','transient_Jx',&
         .not.restartB,shape(ind%J_cc%x),(shape(ind%J_cc%x)+1)*2/3,g)

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
         write(*,*) '     B/J probes initialized'

         call init(ind%err_divB)
         call init(ind%err_DivJ)
         call init(ind%err_cleanB)
         call init(ind%err_residual)

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
         ! if (cleanB) call init(ind%MG,ind%phi%s,ind%phi_bcs,ind%g,ind%ss_cleanB,.false.)
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
         call delete(ind%V_E)
         call delete(ind%W_E)

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
         call delete(ind%mu)

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
         call delete(ind%g)

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
           ! call apply(ind%probe_Bx,ind%nstep,ind%B%x)
           ! call apply(ind%probe_By,ind%nstep,ind%B%y)
           ! call apply(ind%probe_Bz,ind%nstep,ind%B%z)
           ! call apply(ind%probe_J,ind%nstep,ind%J_cc%x)
         endif

         if (getExportErrors(ss_MHD)) then
           ! call apply(ind%probe_divB,ind%nstep,ind%divB)
           ! call apply(ind%probe_divJ,ind%nstep,ind%divJ)
         endif
       end subroutine

       subroutine inductionExportRaw(ind,g,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting RAW Solutions for B'
             call export_3C_VF(g,ind%B0   ,dir//'Bfield/','B0ct',0)
             call export_3C_VF(g,ind%B    ,dir//'Bfield/','Bct',0)
             call export_3C_VF(g,ind%J_cc ,dir//'Jfield/','Jct',0)
             call export_1C_SF(g,ind%sigma,dir//'material/','sigmac',0)
             call export_1C_SF(g,ind%sigma,dir//'Bfield/','divBct',0)
             call export_1C_SF(g,ind%sigma,dir//'Jfield/','divJct',0)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine inductionExport(ind,g,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         type(SF) :: tempN,tempCC
         type(VF) :: tempVFn,tempVFn2

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           write(*,*) 'Exporting PROCESSED solutions for B'

           ! B
           call init_Node(tempVFn,g)
           call init_Node(tempVFn2,g)
           call cellCenter2Node(tempVFn,ind%B,g,ind%temp_F,ind%temp_E)
           call export_3C_VF(g,tempVFn,dir//'Bfield/','Bnt',0)

           call cellCenter2Node(tempVFn,ind%B0,g,ind%temp_F,ind%temp_E)
           call export_3C_VF(g,tempVFn,dir//'Bfield/','B0nt',0)

           ! B0
           call cellCenter2Node(tempVFn,ind%B,g,ind%temp_F,ind%temp_E)
           call cellCenter2Node(tempVFn2,ind%B0,g,ind%temp_F,ind%temp_E)
           call add(tempVFn,tempVFn2)
           call export_3C_VF(g,tempVFn,dir//'Bfield/','Btotnt',0)

           ! J
           call cellCenter2Node(tempVFn,ind%J_cc,g,ind%temp_F,ind%temp_E)
           call export_3C_VF(g,tempVFn,dir//'Jfield/','Jtotnt_phys',0)
           call delete(tempVFn)
           call delete(tempVFn2)

           ! sigma
           call init_Node(tempN,g)
           call cellCenter2Node(tempN,ind%sigma,g,ind%temp_F%x,ind%temp_E%z)
           call export_1C_SF(g,tempN,dir//'material/','sigman',0)
           call delete(tempN)

           ! U_induction
           call init_CC(tempCC,g)
           call div(tempCC,ind%U_cct,g)
           call export_1C_SF(g,tempCC,dir//'Ufield/','divUct',0)
           call delete(tempCC)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine inductionExportMaterial(ind,dir)
         implicit none
         type(induction),intent(inout) :: ind
         character(len=*),intent(in) :: dir
         type(SF) :: tempN
         integer :: i
         if (solveInduction) then
           call init_Node(tempN,ind%g)
           call cellCenter2Node(tempN,ind%sigma,ind%g,ind%temp_F%x,ind%temp_E%z)
           call treatInterface(tempN)
           call export_1C_SF(ind%g,tempN,dir//'material/','sigman',0)
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
         write(un,*) ''
         write(un,*) 'N_cells = ',(/ind%g%c(1)%N,ind%g%c(2)%N,ind%g%c(3)%N/)
         write(un,*) 'volume = ',ind%g%volume
         write(un,*) 'min/max(h)_x = ',(/ind%g%c(1)%hmin,ind%g%c(1)%hmax/)
         write(un,*) 'min/max(h)_y = ',(/ind%g%c(2)%hmin,ind%g%c(2)%hmax/)
         write(un,*) 'min/max(h)_z = ',(/ind%g%c(3)%hmin,ind%g%c(3)%hmax/)
         write(un,*) 'min/max(dh)_x = ',(/ind%g%c(1)%dhMin,ind%g%c(1)%dhMax/)
         write(un,*) 'min/max(dh)_y = ',(/ind%g%c(2)%dhMin,ind%g%c(2)%dhMax/)
         write(un,*) 'min/max(dh)_z = ',(/ind%g%c(3)%dhMin,ind%g%c(3)%dhMax/)
         write(un,*) 'stretching_x = ',ind%g%c(1)%dhMax-ind%g%c(1)%dhMin
         write(un,*) 'stretching_y = ',ind%g%c(2)%dhMax-ind%g%c(2)%dhMin
         write(un,*) 'stretching_z = ',ind%g%c(3)%dhMax-ind%g%c(3)%dhMin
         write(un,*) ''
         call printPhysicalMinMax(ind%B,'Bx','By','Bz')
         call printPhysicalMinMax(ind%B0,'B0x','B0y','B0z')
         call printPhysicalMinMax(ind%divB,'divB')
         call printPhysicalMinMax(ind%divJ,'divJ')
       end subroutine

       subroutine inductionExportTransientFull(ind,g,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         type(VF) :: tempVFn,tempVFn2

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           call init(tempVFn,Nx,Ny,Nz)
           call init(tempVFn2,tempVFn)

           ! call cellCenter2Node(tempVFn,ind%B,g)

           call cellCenter2Node(tempVFn,ind%B,g,ind%temp_F,ind%temp_E)
           call cellCenter2Node(tempVFn2,ind%B0,g,ind%temp_F,ind%temp_E)
           call add(tempVFn,tempVFn2)

           ! call cellCenter2Node(tempVFn,ind%J_cc,g)

           ! call cellCenter2Node(tempVFn,ind%J_cc,g)

           call delete(tempVFn)
           call delete(tempVFn2)
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine inductionSolver(ind,U,g_mom,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g_mom
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

         call embedVelocity(ind,U,g_mom)

         call assign(ind%dB0dt,0.0_cp)
         call assignZ(ind%dB0dt,ind%omega*exp(-ind%omega*ind%t))

         select case (solveBMethod)
         case (1); call lowRemPoisson(ind,ind%U_cct,ind%g,ss_MHD)
         case (2); call lowRemPseudoTimeStepUniform(ind,ind%U_cct,ind%g)
         case (3); call lowRemPseudoTimeStep(ind,ind%U_cct,ind%g)
         case (4); call lowRemCTmethod(ind,ind%g)
         case (5); call finiteRemCTmethod(ind,ind%dB0dt,ind%g)
         ! case (6); call lowRem_ADI(ind,ind%U_cct,ind%g,ss_MHD)
         ! case (7); call lowRemMultigrid(ind,ind%U_cct,ind%g)
         end select
         if (cleanB) then
           call cleanBSolution(ind,ind%g,ss_MHD)
           ! call cleanBMultigrid(ind,g,ss_MHD)
         endif
         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call computeCurrent(ind)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         call computeTotalMagneticEnergy(ind,ss_MHD)
         ! call computeTotalMagneticEnergyFluid(ind,g_mom,ss_MHD)
         call exportTransient(ind,ss_MHD)

         ! call inductionExportTransientFull(ind,ind%g,dir) ! VERY Expensive

         if (getExportErrors(ss_MHD)) call computeDivergence(ind,ind%g)
         if (getExportErrors(ss_MHD)) call exportTransientFull(ind,ind%g,dir)

         if (getPrintParams(ss_MHD)) then
           call inductionInfo(ind,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowB')
         else; exportNow = .false.
         endif

         if (getExportRawSolution(ss_MHD).or.exportNow) then
           call exportRaw(ind,ind%g,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(ind,ind%g,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
       end subroutine

       subroutine lowRemPoissonOld(ind,U,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD
         ! call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

         call poisson(ind%SOR_B,ind%B%x,ind%temp_CC%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%y,ind%temp_CC%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%z,ind%temp_CC%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       subroutine lowRemPoisson(ind,U,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call cellCenter2Face(ind%temp_F2,ind%B0,g)

         call faceCurlCross_F(ind%temp_F,ind%U_Ft,ind%temp_F2,ind%temp_E1,ind%temp_E2,g)

         call poisson(ind%SOR_B,ind%B_face%x,ind%temp_F%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B_face%y,ind%temp_F%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B_face%z,ind%temp_F%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call face2CellCenter(ind%B,ind%B_face,g)
       end subroutine

       subroutine lowRemPseudoTimeStepUniform(ind,U,g)
         ! This routine assumed div(B)=0 (which requires uniform properties), 
         ! which is how it differs from lowRemPseudoTimeStep(). This was an 
         ! important case to test against the Poisson solution, since the 
         ! terms are essentially the same, but a different iterative method 
         ! is applied.
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i
         
         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)
           ! ------------- diffusion term -------------
           call lap(ind%temp_CC,ind%B,g)
           call multiply(ind%temp_CC,ind%dTime)
           call add(ind%Bstar,ind%temp_CC)
           
           ! ------------- source term -------------
           ! call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)

           ! Add induced field of previous time step (B^n)
           call multiply(ind%B,ind%mu)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyAllBCs(ind%B,g)
         enddo
       end subroutine

       subroutine lowRemPseudoTimeStep(ind,U,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i

         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)

           ! ------------- diffusion term -------------
           call divide(ind%B,ind%mu)
           ! call CCBfieldDiffuse(ind%temp_CC,ind%B,ind%sigmaInv_face,g)
           call multiply(ind%B,ind%mu)

           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)
           
           ! ------------- source term -------------
           ! call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)

           ! Add induced field of previous time step (B^n)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyAllBCs(ind%B,g)
         enddo
       end subroutine

       subroutine lowRemCTmethod(ind,g)
         ! inductionSolverCT solves the induction equation using the
         ! Constrained Transport (CT) Method. The magnetic field is
         ! stored and collocated at the cell center. The magnetic
         ! field is updated using Faraday's Law, where the electric
         ! field is solved for using appropriate fluxes as described
         ! in "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         ! The velocity field is assumed to be cell centered.
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         integer :: i

         do i=1,ind%NmaxB

           ! Compute current from appropriate fluxes:

           ! J = curl(B_face)_edge
           call cellCenter2Face(ind%temp_F,ind%B,g)
           call curl(ind%J,ind%temp_F,g)

           ! Compute fluxes of u cross B0
           call edgeCrossCC_E(ind%E,ind%U_E,ind%V_E,ind%W_E,ind%B0,g,ind%temp_F)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! F = curl(E_edge)_face
           call curl(ind%temp_F,ind%E,g)

           ! tempVF = interp(F)_face->cc
           call face2CellCenter(ind%temp_CC,ind%temp_F,g)

           ! Add induced field of previous time step (B^n)
           ! ind%B = ind%B - ind%dTime*ind%temp_CC
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%B,ind%temp_CC)

           ! Impose BCs:
           call applyAllBCs(ind%B,g)
         enddo
       end subroutine

       subroutine finiteRemCTmethod(ind,F_CC,g)
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
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: F_CC
         type(grid),intent(in) :: g

         ! E = uxB
         call add(ind%Bstar,ind%B,ind%B0)
         call edgeCrossCC_E(ind%E,ind%U_E,ind%V_E,ind%W_E,ind%Bstar,g,ind%temp_F)

         ! J = Rem^-1 curl(B_face)_edge ! Assumes curl(B0) = 0
         call cellCenter2Face(ind%temp_F,ind%B,g)
         call curl(ind%J,ind%temp_F,g)
         call divide(ind%J,ind%Rem)

         ! -E = ( uxB - j/sig )_edge
         call multiply(ind%J,ind%sigmaInv_edge)
         call subtract(ind%E,ind%J)

         ! dBdt = -Curl(E_edge)_face
         call curl(ind%temp_F,ind%E,g)

         ! dBdt_cc = interp(dBdt)_face->cc
         call face2CellCenter(ind%temp_CC,ind%temp_F,g)

         ! dBdt = dBdt + F
         call add(ind%temp_CC,F_CC)

         ! B^n+1 = B^n + {-curl(E) + F}
         call multiply(ind%temp_CC,ind%dTime)
         call add(ind%B,ind%temp_CC)

         ! Impose BCs:
         call applyAllBCs(ind%B,g)
       end subroutine

!        subroutine lowRem_ADI(ind,U,g,ss_MHD)
!          ! inductionSolverCT solves the induction equation using the
!          ! Constrained Transport (CT) Method. The magnetic field is
!          ! stored and collocated at the cell center. The magnetic
!          ! field is updated using Faraday's Law, where the electric
!          ! field is solved for using appropriate fluxes as described
!          ! in "Tóth, G. The divergence Constraint in Shock-Capturing 
!          ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
!          ! The velocity field is assumed to be cell centered.
!          implicit none
!          ! ********************** INPUT / OUTPUT ************************
!          type(induction),intent(inout) :: ind
!          type(VF),intent(in) :: U
!          type(grid),intent(in) :: g
!          type(solverSettings),intent(inout) :: ss_MHD

!          ! J = curl(B_face)_edge
!          call cellCenter2Face(ind%temp_F,ind%B,g)
!          call curl(ind%J,ind%temp_F,g)

!          ! Compute fluxes of u cross B0
!          call cross(ind%temp_CC,U,ind%B0)
!          call cellCenter2Edge(ind%E,ind%temp_CC,g)

!          ! E = j/sig - uxB
!          ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
!          call multiply(ind%J,ind%sigmaInv_edge)
!          call subtract(zero,ind%E)
!          call add(ind%E,ind%J)

!          ! F = Curl(E)
!          call curl(ind%temp_F,ind%E,g)

!          ! tempVF = interp(F)_face->cc
!          call face2CellCenter(ind%temp_CC,ind%temp_F,g)

!          ! Subtract laplacian from B^n
!          call lap(ind%Bstar,ind%B,ind%sigmaInv_face,g)

!          ! ind%temp_CC = ind%temp_CC - ind%Bstar
!          call subtract(ind%temp_CC,ind%Bstar)

!          ! Solve with semi-implicit ADI
!          call setDt(ind%ADI_B,ind%dTime)
!          call setAlpha(ind%ADI_B,1.0_cp)

!          call init(ind%ss_ADI)
!          call setName(ind%ss_ADI,'ADI for B-field     ')

!          call apply(ind%ADI_B,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,&
!             ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
!          call apply(ind%ADI_B,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,&
!             ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
!          call apply(ind%ADI_B,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,&
!             ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))

!          ! Impose BCs:
!          call applyAllBCs(ind%B,ind%B_bcs,g)
!        end subroutine

!        subroutine lowRemMultigrid(ind,U,g)
!          implicit none
!          ! ********************** INPUT / OUTPUT ************************
!          type(induction),intent(inout) :: ind
!          type(VF),intent(in) :: U
!          type(grid),intent(in) :: g
!          type(multiGrid),dimension(2) :: MG

!          call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

!          call poisson(MG,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,ind%ss_ind,&
!          ind%err_residual,.false.)

!          call poisson(MG,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,ind%ss_ind,&
!          ind%err_residual,.false.)

!          call poisson(MG,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,ind%ss_ind,&
!          ind%err_residual,.false.)
!        end subroutine

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

       ! ********************* COMPUTE *****************************

       subroutine computeAddJCrossB(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! addJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the applied or total mangetic
         ! field, depending on the solveBMethod.
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem
         type(VF) :: temp
         call init(temp,jcrossB)
         call assign(temp,0.0_cp)
         call computeJCrossB(temp,ind,g_mom,Ha,Re,Rem)
         call add(jcrossB,temp)
         call delete(temp)
       end subroutine

       subroutine computeJCrossB(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_Bface(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J,ind%B,ind%g)
           ! call edge2Face(ind%temp_F,ind%J,ind%g)

           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_new(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem

         ! Magnetic Pressure (not yet done)
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call square(ind%Bstar)
           call divide(ind%Bstar,2.0_cp)
           ! call grad(ind%jCrossB_F,ind%Bstar,ind%g)
           call multiply(ind%jCrossB_F,-1.0_cp)
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select

         ! Magnetic Stress (not yet done)
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeDivergenceInduction(ind,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         if (solveInduction) then
           select case (solveBMethod)
           case (4:5)
             ! CT method enforces div(b) = 0, (result is in CC), 
             ! when computed from FACE-centered data:
             call div(ind%divB,ind%temp_F,g)
           case default
             call div(ind%divB,ind%B,g)
           end select
         endif
         call div(ind%divJ,ind%J_cc,g)
       end subroutine

       subroutine computeCurrent(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call add(ind%Bstar,ind%B0,ind%B)
         ! call divide(ind%Bstar,ind%mu)
         call curl(ind%J_cc,ind%Bstar,ind%g)
       end subroutine

!        subroutine computeTotalMagneticEnergyFluid(ind,g,ss_MHD)
!          implicit none
!          type(induction),intent(inout) :: ind
!          type(grid),intent(in) :: g
!          type(solverSettings),intent(in) :: ss_MHD
!          real(cp) :: K_energy
!          integer,dimension(3) :: Nici1,Nici2
!          Nici1 = ind%SD%Nici1; Nici2 = ind%SD%Nici2
!           if (computeKB.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
!            call assign(ind%Bstar,ind%B)
!            call add(ind%Bstar,ind%B0)
!            call totalEnergy(K_energy,&
!              ind%Bstar%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              ind%Bstar%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              ind%Bstar%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              g)
!            call set(ind%KB_energy,ind%nstep,K_energy)
!            call apply(ind%KB_energy)
!           endif
!           if (computeKBi.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
!            call assign(ind%Bstar,ind%B)
!            call totalEnergy(K_energy,&
!              ind%Bstar%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              ind%Bstar%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              ind%Bstar%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              g)
!            call set(ind%KBi_energy,ind%nstep,K_energy)
!            call apply(ind%KBi_energy)
!           endif
!           if (computeKB0.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
!            call totalEnergy(K_energy,&
!              ind%B0%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              ind%B0%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              ind%B0%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
!              g)
!            call set(ind%KB0_energy,ind%nstep,K_energy)
!            call apply(ind%KB0_energy)
!           endif
!        end subroutine

       subroutine computeTotalMagneticEnergy(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
          if (computeKB.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call assign(ind%Bstar,ind%B)
           call add(ind%Bstar,ind%B0)
           call totalEnergy(K_energy,ind%Bstar,ind%g)
           call set(ind%KB_energy,ind%nstep,K_energy)
           call apply(ind%KB_energy)
          endif
          if (computeKBi.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call totalEnergy(K_energy,ind%B,ind%g)
           call set(ind%KBi_energy,ind%nstep,K_energy)
           call apply(ind%KBi_energy)
          endif
          if (computeKB0.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           call totalEnergy(K_energy,ind%B0,ind%g)
           call set(ind%KB0_energy,ind%nstep,K_energy)
           call apply(ind%KB0_energy)
          endif
       end subroutine

       ! ********************* AUX *****************************

       subroutine embedVelocity(ind,U_fi,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U_fi ! Raw momentum velocity
         type(grid),intent(in) :: g ! Momentum grid
         type(VF) :: temp
         logical,dimension(4) :: usedVelocity

         usedVelocity = (/.true.,.true.,.false.,.false./)

         if (usedVelocity(1)) then ! Edge - 2 interpolations
           call allocateX(temp,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
           call allocateY(temp,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
           call allocateZ(temp,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
           call face2Edge(temp%x,U_fi%x,g,1,1)
           call face2Edge(temp%y,U_fi%x,g,1,2)
           call face2Edge(temp%z,U_fi%x,g,1,3)
           call embedEdge(ind%U_E,temp,ind%SD,g)
           call face2Edge(temp%x,U_fi%y,g,2,1)
           call face2Edge(temp%y,U_fi%y,g,2,2)
           call face2Edge(temp%z,U_fi%y,g,2,3)
           call embedEdge(ind%V_E,temp,ind%SD,g)
           call face2Edge(temp%x,U_fi%z,g,3,1)
           call face2Edge(temp%y,U_fi%z,g,3,2)
           call face2Edge(temp%z,U_fi%z,g,3,3)
           call embedEdge(ind%W_E,temp,ind%SD,g)
           call delete(temp)
         endif

         if (usedVelocity(2)) then ! CC - 1 interpolation
           call allocateX(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
           call allocateY(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
           call allocateZ(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
           call face2CellCenter(temp%x,U_fi%x,g,1)
           call face2CellCenter(temp%y,U_fi%y,g,2)
           call face2CellCenter(temp%z,U_fi%z,g,3)
           call embedCC(ind%U_cct,temp,ind%SD,g)
           call delete(temp)
         endif

         if (usedVelocity(3)) then ! Face - no interpolations
           call embedFace(ind%U_Ft,U_Fi,ind%SD,g)
         endif
         ! if (usedVelocity(4)) then ! Node - 3 interpolations (not needed for any solvers)
           ! call allocateX(temp,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
           ! call allocateY(temp,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
           ! call allocateZ(temp,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
           ! call face2Node(temp%x,U_fi%x,g,1)
           ! call face2Node(temp%y,U_fi%x,g,1)
           ! call face2Node(temp%z,U_fi%x,g,1)
           ! call embedNode(ind%U_N,temp,Nin1,Nin2,g)
           ! call face2Node(temp%x,U_fi%y,g,2)
           ! call face2Node(temp%y,U_fi%y,g,2)
           ! call face2Node(temp%z,U_fi%y,g,2)
           ! call embedNode(ind%V_N,temp,Nin1,Nin2,g)
           ! call face2Node(temp%x,U_fi%z,g,3)
           ! call face2Node(temp%y,U_fi%z,g,3)
           ! call face2Node(temp%z,U_fi%z,g,3)
           ! call embedNode(ind%W_N,temp,Nin1,Nin2,g)
           ! call delete(temp)
         ! endif
       end subroutine

       subroutine perturbAll(f,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(3) :: wavenum,eps
         integer,dimension(3) :: s
         integer :: i,j,k
         s = shape(f)
         wavenum = 0.1_cp
         eps = 0.01_cp
         if (all((/(s(i).eq.g%c(i)%sn,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(1.0_cp + eps(1)*sin(wavenum(1)*PI*g%c(1)%hn(i)) +&
                                           eps(2)*sin(wavenum(2)*PI*g%c(2)%hn(j)) +&
                                           eps(3)*sin(wavenum(3)*PI*g%c(3)%hn(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (all((/(s(i).eq.g%c(i)%sc,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(1.0_cp + eps(1)*sin(wavenum(1)*PI*g%c(1)%hc(i)) +&
                                           eps(2)*sin(wavenum(2)*PI*g%c(2)%hc(j)) +&
                                           eps(3)*sin(wavenum(3)*PI*g%c(3)%hc(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         else
          stop 'Error: unmatched case in perturbAll in inductionSolver.f90'
         endif
       end subroutine

       subroutine perturb(f,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(3) :: wavenum,eps
         integer,dimension(3) :: s
         integer :: i,j,k
         s = shape(f)
         wavenum = 10.0_cp
         eps = 0.1_cp
         if (all((/(s(i).eq.g%c(i)%sn,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(1.0_cp + eps(2)*sin(wavenum(2)*PI*g%c(2)%hn(j)) +&
                                           eps(3)*sin(wavenum(3)*PI*g%c(3)%hn(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (all((/(s(i).eq.g%c(i)%sc,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(1.0_cp + eps(2)*sin(wavenum(2)*PI*g%c(2)%hc(j)) +&
                                           eps(3)*sin(wavenum(3)*PI*g%c(3)%hc(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         else
          stop 'Error: unmatched case in perturb in inductionSolver.f90'
         endif
       end subroutine

       end module