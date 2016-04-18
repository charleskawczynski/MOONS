       module energy_mod
       use current_precision_mod
       use simParams_mod
       use IO_tools_mod
       use IO_Auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod
       use export_raw_processed_mod
       use myTime_mod
       use SF_mod
       use VF_mod

       use init_TBCs_mod
       use init_Tfield_mod
       use init_K_mod

       use ops_embedExtract_mod
       use domain_mod
       use mesh_mod
       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod
       use BCs_mod
       use apply_BCs_mod
       use solverSettings_mod
       use probe_base_mod

       implicit none

       private
       public :: energy,init,delete,solve

       public :: setDTime,setNmaxT,setPiGroups
       public :: computeAddBuoyancy,computeAddGravity
       public :: export,exportTransient
       public :: printExportBCs
       public :: computeDivergence
       public :: embedVelocityEnergy

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
         type(mesh) :: m
         type(domain) :: D

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
       interface export;             module procedure export_energy;           end interface
       interface exportTransient;    module procedure energyExportTransient;   end interface
       interface computeDivergence;  module procedure computeDivergenceEnergy; end interface
       interface embedVelocity;      module procedure embedVelocityEnergy;     end interface

       interface setDTime;           module procedure setDTimeEnergy;          end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initenergy(nrg,m,D,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing energy:'

         call init(nrg%m,m)
         call init(nrg%D,D)

         call init_CC(nrg%T,m,0.0_cp)
         call init_CC(nrg%Tstar,m,0.0_cp)
         call init_CC(nrg%Ttemp,m,0.0_cp)
         call init_CC(nrg%k_cc,m,0.0_cp)
         call init_Face(nrg%temp_F,m,0.0_cp)
         call init_Face(nrg%k,m,0.0_cp)
         call init_Face(nrg%U_ft,m,0.0_cp)
         call init_CC(nrg%U_cct,m,0.0_cp)
         call init_CC(nrg%temp_CC,m,0.0_cp)
         call init_CC(nrg%gravity,m,0.0_cp)
         call init_CC(nrg%buoyancy,m,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(nrg%divQ,m)
         write(*,*) '     Fields allocated'


         ! --- Initialize Fields ---
         call initTBCs(nrg%T,nrg%m)
         write(*,*) '     BCs initialized'

         call initTfield(nrg%T,m,dir)
         write(*,*) '     T-field initialized'

         call apply_BCs(nrg%T,m)
         write(*,*) '     BCs applied'

         call initK(nrg%k_cc,nrg%D)
         call cellCenter2Face(nrg%k,nrg%k_cc,m)
         write(*,*) '     Materials initialized'

         call init(nrg%probe_T,dir//'Tfield/','transient_T',&
         .not.restartT,nrg%T%RF(1)%s,(nrg%T%RF(1)%s+1)/2,m)

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
         call delete(nrg%m)

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
         if (solveEnergy) call print_BCs(nrg%T,'T')
         if (solveEnergy) call export_BCs(nrg%T,dir//'parameters/','T')
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

       subroutine export_energy(nrg,m,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         if (solveEnergy) then
           write(*,*) 'Exporting Solutions for T'
           call export_raw(m,nrg%U_cct,dir//'Tfield/','U',0)
           call export_raw(m,nrg%T,dir//'Tfield/','T',0)
           call export_raw(m,nrg%U_ft,dir//'Tfield/','U',0)
           call export_raw(m,nrg%divQ,dir//'Tfield/','divQ',0)
           call export_raw(m,nrg%k_cc,dir//'Tfield/','k',0)

           call export_processed(m,nrg%T,dir//'Tfield/','T',0)
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
           call print(nrg%m)
           write(un,*) ''
           call printPhysicalMinMax(nrg%T,'T')
           call printPhysicalMinMax(nrg%divQ,'divQ')
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solveEnergyEquation(nrg,U,m_mom,ss_MHD,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m_mom
         type(solverSettings),intent(inout) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow

         call assign(nrg%gravity%y,1.0_cp)

         call embedVelocity(nrg,U,m_mom)
         select case (solveTMethod)
         case (1); call ExplicitEuler(nrg,nrg%m)
         end select
         nrg%nstep = nrg%nstep + 1
         nrg%time = nrg%time + nrg%dTime ! This only makes sense for finite Rem

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call computeFlux(nrg,nrg%m)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         call exportTransient(nrg,ss_MHD)
         if (getExportErrors(ss_MHD)) call computeDivergence(nrg,nrg%m)
         ! if (getExportErrors(ss_MHD)) call exportTransientFull(nrg,nrg%m,dir)

         ! call computeMagneticEnergy(nrg,nrg%B,nrg%B0,m_mom,ss_MHD) ! Maybe thermal energy?

         if (getPrintParams(ss_MHD)) then
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowT')
         else; exportNow = .false.
         endif

         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(nrg,nrg%m,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowT')
         endif
         call energyInfo(nrg,ss_MHD,6)
       end subroutine

       subroutine explicitEuler(nrg,m)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m

         ! Advection
         call assign(nrg%Ttemp,0.0_cp)
         call cellCenter2Face(nrg%temp_F,nrg%T,m)
         call multiply(nrg%temp_F,nrg%U_ft)
         call div(nrg%Ttemp,nrg%temp_F,m)
         call multiply(nrg%Ttemp,-1.0_cp)

         ! Diffusion
         call assign(nrg%Tstar,nrg%Ttemp)
         call lap(nrg%Ttemp,nrg%T,m)
         call divide(nrg%Ttemp,nrg%Re*nrg%Pr)

         ! Explicit Euler
         call add(nrg%Tstar,nrg%Ttemp)
         call multiply(nrg%Tstar,nrg%dTime)
         call add(nrg%T,nrg%Tstar)

         ! Impose BCs:
         call apply_BCs(nrg%T,m)
       end subroutine

       ! ********************* AUX *****************************

       subroutine computeBuoyancy(buoyancy,nrg,Gr,Re)
         ! Computes
         ! 
         !            Gr
         !           ---  T g
         !           Re^2
         implicit none
         type(VF),intent(inout) :: buoyancy
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Gr,Re
         call assign(nrg%buoyancy,nrg%T)
         call multiply(nrg%buoyancy,Gr/(Re**2.0_cp))
         call multiply(nrg%buoyancy,nrg%gravity)
         call cellCenter2Face(nrg%temp_F,nrg%buoyancy,nrg%m)
         call extractFace(buoyancy,nrg%temp_F,nrg%D)
       end subroutine

       subroutine computeAddBuoyancy(buoyancy,nrg,Gr,Re)
         implicit none
         type(VF),intent(inout) :: buoyancy
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Gr,Re
         type(VF) :: temp
         call init(temp,buoyancy)
         call assign(temp,0.0_cp)
         call computeBuoyancy(temp,nrg,Gr,Re)
         call add(buoyancy,temp)
         call delete(temp)
       end subroutine

       subroutine computeGravity(gravity,nrg,Fr)
         ! Computes
         ! 
         !            1   
         !           --- g
         !           Fr^2 
         implicit none
         type(VF),intent(inout) :: gravity
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Fr
         call assign(nrg%temp_CC,nrg%gravity)
         call divide(nrg%temp_CC,Fr**2.0_cp)
         call cellCenter2Face(nrg%temp_F,nrg%temp_CC,nrg%m)
         call extractFace(gravity,nrg%temp_F,nrg%D)
       end subroutine

       subroutine computeAddGravity(gravity,nrg,Fr)
         implicit none
         type(VF),intent(inout) :: gravity
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Fr
         type(VF) :: temp
         call init(temp,gravity)
         call assign(temp,0.0_cp)
         call computeGravity(temp,nrg,Fr)
         call add(gravity,temp)
         call delete(temp)
       end subroutine

       subroutine computeFlux(nrg,m)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         if (solveEnergy) then
           call grad(nrg%temp_F,nrg%T,m)
           call multiply(nrg%temp_F,nrg%k)
           call multiply(nrg%temp_F,-1.0_cp)
         endif
       end subroutine

       subroutine computeDivergenceEnergy(nrg,m)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         if (solveEnergy) then
           call div(nrg%divQ,nrg%temp_F,m)
         endif
       end subroutine

       subroutine embedVelocityEnergy(nrg,U_fi,m)
         implicit none
         type(energy),intent(inout) :: nrg
         type(VF),intent(in) :: U_fi ! Raw momentum velocity
         type(mesh),intent(in) :: m ! Momentum mesh
         type(VF) :: temp
         logical,dimension(2) :: usedVelocity

         usedVelocity = (/.true.,.true./)

         if (usedVelocity(1)) then ! Face
           call embedFace(nrg%U_ft,U_fi,nrg%D)
         endif

         if (usedVelocity(2)) then ! Cell Center
           call init_CC(temp,m)
           call face2CellCenter(temp%x,U_fi%x,m,1)
           call face2CellCenter(temp%y,U_fi%y,m,2)
           call face2CellCenter(temp%z,U_fi%z,m,3)
           call embedCC(nrg%U_cct,temp,nrg%D)
           call delete(temp)
         endif
       end subroutine

       end module