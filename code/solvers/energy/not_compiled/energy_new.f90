       module energy_mod
       use current_precision_mod
       use simParams_mod
       use IO_tools_mod
       use IO_Auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod
       use export_raw_processed_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       use domain_mod

       use energy_aux_mod
       use init_TBCs_mod
       use init_Tfield_mod
       use init_K_mod

       use ops_embedExtract_mod
       use norms_mod
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
       public :: export,exportTransient

       real(cp),parameter :: zero = 0.0_cp

       type energy
         character(len=6) :: name = 'energy'
         ! --- Vector fields ---
         type(SF) :: T,temp_CC1,temp_CC2   ! CC data
         type(VF) :: temp_F,k              ! Face data
         type(VF) :: U_F                   ! Face data
         type(VF) :: gravity               ! CC data
         type(VF) :: buoyancy              ! CC data
         ! --- Scalar fields ---
         type(SF) :: divQ                  ! CC data

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
       interface delete;             module procedure deleteenergy;            end interface
       interface solve;              module procedure solveEnergyEquation;     end interface
       interface export;             module procedure export_energy;           end interface
       interface exportTransient;    module procedure energyExportTransient;   end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initenergy(nrg,m,D,NmaxT,dt,Re,Pr,Ec,Al,Rem,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         real(cp),intent(in) :: dt,Re,Pr,Ec,Al,Rem
         integer,intent(in) :: NmaxT
         character(len=*),intent(in) :: dir
         type(SF) :: k_cc
         write(*,*) 'Initializing energy:'
         nrg%dTime = dt
         nrg%NmaxT = NmaxT
         nrg%Re = Re
         nrg%Pr = Pr
         nrg%Ec = Ec
         nrg%Al = Al
         nrg%Rem = Rem
         nrg%Pe = Re*Pr

         call init(nrg%m,m)
         call init(nrg%D,D)

         call init_CC(nrg%T,m,0.0_cp)
         call init_CC(nrg%temp_CC2,m,0.0_cp)
         call init_Face(nrg%temp_F,m,0.0_cp)
         call init_Face(nrg%k,m,0.0_cp)
         call init_Face(nrg%U_F,m,0.0_cp)
         call init_CC(nrg%temp_CC1,m,0.0_cp)
         call init_CC(nrg%gravity,m,0.0_cp)
         call init_CC(nrg%buoyancy,m,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(nrg%divQ,m)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call initTBCs(nrg%T,nrg%m)
         if (solveEnergy) call print_BCs(nrg%T,'T')
         if (solveEnergy) call export_BCs(nrg%T,dir//'parameters/','T')
         write(*,*) '     BCs initialized'

         call initTfield(nrg%T,m,dir)
         write(*,*) '     T-field initialized'

         call apply_BCs(nrg%T,m)
         write(*,*) '     BCs applied'

         call init_CC(k_cc,m,0.0_cp)
         call initK(k_cc,nrg%D)
         call cellCenter2Face(nrg%k,k_cc,m)
         call delete(k_cc)
         write(*,*) '     Materials initialized'

         call init(nrg%probe_divQ,dir//'Tfield/','transient_divQ',.not.restartT)

         call export(nrg%probe_divQ)
         write(*,*) '     probes initialized'

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
         call delete(nrg%temp_F)
         call delete(nrg%k)
         call delete(nrg%temp_CC1)
         call delete(nrg%temp_CC2)

         call delete(nrg%U_F)
         call delete(nrg%gravity)
         call delete(nrg%buoyancy)

         call delete(nrg%divQ)

         call delete(nrg%probe_divQ)
         call delete(nrg%m)

         write(*,*) 'energy object deleted'
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine energyExportTransient(nrg,ss_MHD)
         implicit none
         type(energy),intent(inout) :: nrg
         type(solverSettings),intent(in) :: ss_MHD
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
           call export_raw(m,nrg%U_F,dir//'Tfield/','U',0)
           call export_raw(m,nrg%T,dir//'Tfield/','T',0)
           call export_raw(m,nrg%divQ,dir//'Tfield/','divQ',0)
           call export_raw(m,nrg%k,dir//'Tfield/','k',0)

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

       end module