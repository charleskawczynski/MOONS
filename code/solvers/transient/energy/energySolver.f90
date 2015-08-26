       module energySolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_Auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod
       use myTime_mod
       use SF_mod
       use VF_mod

       use initializeTBCs_mod
       use initializeTfield_mod
       use initializeK_mod

       use ops_embedExtract_mod
       use grid_mod
       use norms_mod
       use del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod
       use BCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use probe_base_mod

       implicit none

       private
       public :: energy,init,delete,solve

       public :: setDTime,setNmaxT,setPiGroups
       public :: computeAddBuoyancy,computeAddGravity
       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeDivergence
       public :: embedVelocityEnergy

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

       type energy
         character(len=6) :: name = 'energy'
         ! --- Vector fields ---
         type(SF) :: T,Tstar,Ttemp         ! CC data
         type(VF) :: temp_F,k              ! Face data
         type(VF) :: U_cct,temp_CC         ! CC data
         type(VF) :: U_ft                  ! Face data
         type(VF) :: gravity               ! CC data
         type(VF) :: buoyancy              ! CC data
         ! --- Scalar fields ---
         type(SF) :: k_cc                  ! CC data
         type(SF) :: divQ                  ! CC data
         ! BCs:
         type(BCs) :: T_bcs
         ! Solver settings
         type(solverSettings) :: ss_energy
         ! Errors
         type(norms) :: err_divQ,err_residual

         type(indexProbe) :: probe_T
         type(errorProbe) :: probe_divQ
         type(grid) :: g
         type(subdomain) :: SD

         integer :: nstep             ! Nth time step
         integer :: NmaxT             ! Maximum number iterations in solving T (if iterative)
         real(cp) :: dTime            ! Time step
         real(cp) :: time             ! Time

         real(cp) :: Re,Pr,Ec,Al,Rem,Pe  ! Reynolds, Prandtl, Eckert, Alfen, Magnetic Reynolds, Peclet
       end type

       interface init;               module procedure initenergy;              end interface
       interface setPiGroups;        module procedure setPiGroupsENG;          end interface
       interface delete;             module procedure deleteenergy;            end interface
       interface solve;              module procedure solveEnergyEquation;     end interface
       interface printExportBCs;     module procedure printExportenergyBCs;    end interface
       interface export;             module procedure energyExport;            end interface
       interface exportRaw;          module procedure energyExportRaw;         end interface
       interface exportTransient;    module procedure energyExportTransient;   end interface
       interface computeDivergence;  module procedure computeDivergenceEnergy; end interface
       interface embedVelocity;      module procedure embedVelocityEnergy;     end interface

       interface setDTime;           module procedure setDTimeEnergy;          end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initenergy(nrg,g,SD,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing energy:'

         nrg%g = g
         nrg%SD = SD
         call init_CC(nrg%T,g)
         call init_CC(nrg%Tstar,g)
         call init_CC(nrg%Ttemp,g)
         call init_CC(nrg%k_cc,g)

         call init_Face(nrg%temp_F,g)
         call init_Face(nrg%k,g)
         call init_Face(nrg%U_ft,g)

         call init_CC(nrg%U_cct,g)
         call init_CC(nrg%temp_CC,g)
         call init_CC(nrg%gravity,g)
         call init_CC(nrg%buoyancy,g)

         call assign(nrg%gravity,0.0_cp)
         call assign(nrg%buoyancy,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(nrg%divQ,g)
         write(*,*) '     Fields allocated'


         ! --- Initialize Fields ---
         call initTBCs(nrg%T,nrg%g)
         write(*,*) '     BCs initialized'

         call initTfield(nrg%T,g,dir)
         write(*,*) '     T-field initialized'

         call applyAllBCs(nrg%T,g)
         write(*,*) '     BCs applied'

         call initK(nrg%k_cc,nrg%SD,g)
         call cellCenter2Face(nrg%k,nrg%k_cc,g)
         write(*,*) '     Materials initialized'

         call init(nrg%probe_T,dir//'Tfield/','transient_T',&
         .not.restartT,nrg%T%RF(1)%s,(nrg%T%RF(1)%s+1)/2,g)

         call init(nrg%probe_divQ,dir//'Tfield/','transient_divQ',.not.restartT)

         call export(nrg%probe_T)
         call export(nrg%probe_divQ)
         write(*,*) '     probes initialized'

         call init(nrg%err_divQ)
         call init(nrg%err_residual)

         ! Initialize solver settings
         call init(nrg%ss_energy)
         call setName(nrg%ss_energy,'SS B equation       ')
         call setMaxIterations(nrg%ss_energy,nrg%NmaxT)
         write(*,*) '     Solver settings for T initialized'

         if (restartT) then
         call readLastStepFromFile(nrg%nstep,dir//'parameters/','n_nrg')
         else; nrg%nstep = 0
         endif
         nrg%time = 0.0_cp
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteEnergy(nrg)
         implicit none
         type(energy),intent(inout) :: nrg

         call delete(nrg%T)
         call delete(nrg%Tstar)
         call delete(nrg%Ttemp)

         call delete(nrg%temp_F)
         call delete(nrg%k)
         
         call delete(nrg%U_cct)
         call delete(nrg%temp_CC)

         call delete(nrg%U_ft)
         call delete(nrg%gravity)
         call delete(nrg%buoyancy)

         call delete(nrg%k_cc)
         call delete(nrg%divQ)

         call delete(nrg%T_bcs)

         call delete(nrg%probe_T)
         call delete(nrg%probe_divQ)
         call delete(nrg%g)

         write(*,*) 'energy object deleted'
       end subroutine

       subroutine setDTimeEnergy(nrg,dt)
         implicit none
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: dt
         nrg%dTime = dt
       end subroutine

       subroutine setNmaxT(nrg,NmaxT)
         implicit none
         type(energy),intent(inout) :: nrg
         integer,intent(in) :: NmaxT
         nrg%NmaxT = NmaxT
       end subroutine

       subroutine setPiGroupsENG(nrg,Re,Pr,Ec,Al,Rem)
         implicit none
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Re,Pr,Ec,Al,Rem
         nrg%Re = Re
         nrg%Pr = Pr
         nrg%Ec = Ec
         nrg%Al = Al
         nrg%Rem = Rem
         nrg%Pe = Re*Pr
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine printExportenergyBCs(nrg,dir)
         implicit none
         type(energy),intent(in) :: nrg
         character(len=*),intent(in) :: dir
         if (solveenergy) call printAllBoundaries(nrg%T_bcs,'T')
         if (solveenergy) call writeAllBoundaries(nrg%T_bcs,dir//'parameters/','T')
       end subroutine

       subroutine energyExportTransient(nrg,ss_MHD)
         implicit none
         type(energy),intent(inout) :: nrg
         type(solverSettings),intent(in) :: ss_MHD
         if ((getExportTransient(ss_MHD))) then
           call apply(nrg%probe_T,nrg%nstep,nrg%T%RF(1)%f)
         endif

         if (getExportErrors(ss_MHD)) then
           ! call apply(nrg%probe_divQ,nrg%nstep,nrg%divQ)
         endif
       end subroutine

       subroutine energyExportRaw(nrg,g,dir)
         implicit none
         type(energy),intent(in) :: nrg
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         if (restartT.and.(.not.solveEnergy)) then
           ! This preserves the initial data
         else
           write(*,*) 'Exporting RAW Solutions for T'
           call export_3C_VF(g,nrg%U_cct,dir//'Tfield/','U_cct',0)
           call export_1C_SF(g,nrg%T,dir//'Tfield/','Tct',0)
           call export_1C_SF(g,nrg%U_ft%x,dir//'Tfield/','U_ft',0)
           call export_1C_SF(g,nrg%U_ft%y,dir//'Tfield/','V_ft',0)
           call export_1C_SF(g,nrg%U_ft%z,dir//'Tfield/','W_ft',0)
           call export_1C_SF(g,nrg%divQ,dir//'Tfield/','divQct',0)
           call export_1C_SF(g,nrg%k_cc,dir//'Tfield/','kct',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine energyExport(nrg,g,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         type(SF) :: tempN,tempE
         if (solveEnergy) then
           write(*,*) 'Exporting PROCESSED Solutions for T'
           call init_Node(tempN,g)
           call init_Edge(tempE,g,3)
           call cellCenter2Node(tempN,nrg%T,g,nrg%temp_F%x,tempE)
           call export_1C_SF(g,tempN,dir//'Tfield/','Tnt',0)
         ! ----------------------- MATERIAL PROPERTIES AT NODES ------------------------
           call cellCenter2Node(tempN,nrg%k_cc,g,nrg%temp_F%x,tempE)
           call export_1C_SF(g,tempN,dir//'material/','knt',0)
           call delete(tempN)
           call delete(tempE)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine energyInfo(nrg,ss_MHD,un)
         implicit none
         type(energy),intent(in) :: nrg
         type(solverSettings),intent(in) :: ss_MHD
         integer,intent(in) :: un
         if (getPrintParams(ss_MHD)) then
           write(un,*) '**************************************************************'
           write(un,*) '*************************** ENERGY ***************************'
           write(un,*) '**************************************************************'
           write(un,*) '(Re,Pr) = ',nrg%Re,nrg%Pr
           write(un,*) '(Ec,Al) = ',nrg%Ec,nrg%Al
           write(un,*) '(Rem) = ',nrg%Rem
           write(un,*) '(t,dt) = ',nrg%time,nrg%dTime
           write(un,*) ''
           write(un,*) 'N_cells = ',(/nrg%g%c(1)%N,nrg%g%c(2)%N,nrg%g%c(3)%N/)
           write(un,*) 'volume = ',nrg%g%volume
           write(un,*) 'min/max(h)_x = ',(/nrg%g%c(1)%hmin,nrg%g%c(1)%hmax/)
           write(un,*) 'min/max(h)_y = ',(/nrg%g%c(2)%hmin,nrg%g%c(2)%hmax/)
           write(un,*) 'min/max(h)_z = ',(/nrg%g%c(3)%hmin,nrg%g%c(3)%hmax/)
           write(un,*) 'min/max(dh)_x = ',(/nrg%g%c(1)%dhMin,nrg%g%c(1)%dhMax/)
           write(un,*) 'min/max(dh)_y = ',(/nrg%g%c(2)%dhMin,nrg%g%c(2)%dhMax/)
           write(un,*) 'min/max(dh)_z = ',(/nrg%g%c(3)%dhMin,nrg%g%c(3)%dhMax/)
           write(un,*) 'stretching_x = ',nrg%g%c(1)%dhMax-nrg%g%c(1)%dhMin
           write(un,*) 'stretching_y = ',nrg%g%c(2)%dhMax-nrg%g%c(2)%dhMin
           write(un,*) 'stretching_z = ',nrg%g%c(3)%dhMax-nrg%g%c(3)%dhMin
           write(un,*) ''
           call printPhysicalMinMax(nrg%T,'T')
           call printPhysicalMinMax(nrg%divQ,'divQ')
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solveEnergyEquation(nrg,U,g_mom,ss_MHD,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g_mom
         type(solverSettings),intent(inout) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow

         call assign(nrg%gravity%y,1.0_cp)

         call embedVelocity(nrg,U,g_mom)
         select case (solveTMethod)
         case (1); call ExplicitEuler(nrg,nrg%g)
         end select
         nrg%nstep = nrg%nstep + 1
         nrg%time = nrg%time + nrg%dTime ! This only makes sense for finite Rem

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call computeFlux(nrg,nrg%g)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         call exportTransient(nrg,ss_MHD)
         if (getExportErrors(ss_MHD)) call computeDivergence(nrg,nrg%g)
         ! if (getExportErrors(ss_MHD)) call exportTransientFull(nrg,nrg%g,dir)

         ! call computeMagneticEnergy(nrg,nrg%B,nrg%B0,g_mom,ss_MHD) ! Maybe thermal energy?

         if (getPrintParams(ss_MHD)) then
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowT')
         else; exportNow = .false.
         endif

         if (getExportRawSolution(ss_MHD).or.exportNow) then
           call exportRaw(nrg,nrg%g,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowT')
         endif
         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(nrg,nrg%g,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowT')
         endif
         call energyInfo(nrg,ss_MHD,6)
       end subroutine

       subroutine explicitEuler(nrg,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g

         ! Advection
         call assign(nrg%Ttemp,0.0_cp)
         call cellCenter2Face(nrg%temp_F,nrg%T,g)
         call multiply(nrg%temp_F,nrg%U_ft)
         call div(nrg%Ttemp,nrg%temp_F,g)
         call multiply(nrg%Ttemp,-1.0_cp)

         ! Diffusion
         call assign(nrg%Tstar,nrg%Ttemp)
         call lap(nrg%Ttemp,nrg%T,g)
         call divide(nrg%Ttemp,nrg%Re*nrg%Pr)

         ! Explicit Euler
         call add(nrg%Tstar,nrg%Ttemp)
         call multiply(nrg%Tstar,nrg%dTime)
         call add(nrg%T,nrg%Tstar)

         ! Impose BCs:
         call applyAllBCs(nrg%T,g)
       end subroutine

       ! ********************* AUX *****************************

       subroutine computeBuoyancy(buoyancy,nrg,g_mom,Gr,Re)
         ! Computes
         ! 
         !            Gr
         !           ---  T g
         !           Re^2
         implicit none
         type(VF),intent(inout) :: buoyancy
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Gr,Re
         type(grid),intent(in) :: g_mom
         call assign(nrg%buoyancy,nrg%T)
         call multiply(nrg%buoyancy,Gr/(Re**2.0_cp))
         call multiply(nrg%buoyancy,nrg%gravity)
         call cellCenter2Face(nrg%temp_F,nrg%buoyancy,nrg%g)
         call extractFace(buoyancy,nrg%temp_F,nrg%SD,g_mom)
       end subroutine

       subroutine computeAddBuoyancy(buoyancy,nrg,g_mom,Gr,Re)
         implicit none
         type(VF),intent(inout) :: buoyancy
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Gr,Re
         type(grid),intent(in) :: g_mom
         type(VF) :: temp
         call init(temp,buoyancy)
         call assign(temp,0.0_cp)
         call computeBuoyancy(temp,nrg,g_mom,Gr,Re)
         call add(buoyancy,temp)
         call delete(temp)
       end subroutine

       subroutine computeGravity(gravity,nrg,g_mom,Fr)
         ! Computes
         ! 
         !            1   
         !           --- g
         !           Fr^2 
         implicit none
         type(VF),intent(inout) :: gravity
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Fr
         type(grid),intent(in) :: g_mom
         call assign(nrg%temp_CC,nrg%gravity)
         call divide(nrg%temp_CC,Fr**2.0_cp)
         call cellCenter2Face(nrg%temp_F,nrg%temp_CC,nrg%g)
         call extractFace(gravity,nrg%temp_F,nrg%SD,g_mom)
       end subroutine

       subroutine computeAddGravity(gravity,nrg,g_mom,Fr)
         implicit none
         type(VF),intent(inout) :: gravity
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Fr
         type(grid),intent(in) :: g_mom
         type(VF) :: temp
         call init(temp,gravity)
         call assign(temp,0.0_cp)
         call computeGravity(temp,nrg,g_mom,Fr)
         call add(gravity,temp)
         call delete(temp)
       end subroutine

       subroutine computeFlux(nrg,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         if (solveEnergy) then
           call grad(nrg%temp_F,nrg%T,g)
           call multiply(nrg%temp_F,nrg%k)
           call multiply(nrg%temp_F,-1.0_cp)
         endif
       end subroutine

       subroutine computeDivergenceEnergy(nrg,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         if (solveEnergy) then
           call div(nrg%divQ,nrg%temp_F,g)
         endif
       end subroutine

       subroutine embedVelocityEnergy(nrg,U_fi,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(VF),intent(in) :: U_fi ! Raw momentum velocity
         type(grid),intent(in) :: g ! Momentum grid
         type(VF) :: temp
         integer :: i
         logical,dimension(2) :: usedVelocity

         usedVelocity = (/.true.,.true./)

         if (usedVelocity(1)) then ! Face
           call embedFace(nrg%U_ft,U_fi,nrg%SD,g)
         endif

         if (usedVelocity(2)) then ! Cell Center
           call init(temp,(/(g%c(i)%sc,i=1,3)/))
           call face2CellCenter(temp%x,U_fi%x,g,1)
           call face2CellCenter(temp%y,U_fi%y,g,2)
           call face2CellCenter(temp%z,U_fi%z,g,3)
           call embedCC(nrg%U_cct,temp,nrg%SD,g)
           call delete(temp)
         endif
       end subroutine

       end module