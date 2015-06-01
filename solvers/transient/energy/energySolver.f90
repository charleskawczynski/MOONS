       module energySolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use myTime_mod
       use scalarField_mod
       use vectorField_mod

       use initializeTBCs_mod
       use initializeTfield_mod
       use initializeK_mod

       use ops_embedExtract_mod
       use grid_mod
       use norms_mod
       use del_mod
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

       public :: setDTime,setNmaxT,setPe

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

       real(cp),parameter :: zero = real(0.0,cp)

       type energy
         character(len=6) :: name = 'energy'
         ! --- Vector fields ---
         type(scalarField) :: T,Tstar,tempCC        ! CC data
         type(vectorField) :: q,F,k                 ! Face data
         type(vectorField) :: U_cct                 ! CC data
         real(cp) :: dT,beta
         ! --- Scalar fields ---
         type(scalarField) :: k_cc                  ! CC data
         type(scalarField) :: divQ                  ! CC data
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

         real(cp) :: Pe,Re,Pr,Rem,Ec,Al  ! Reynolds, Prandtl, Magnetic Reynolds, Eckert, Alfen
       end type

       interface init;               module procedure initenergy;              end interface
       interface delete;             module procedure deleteenergy;            end interface
       interface solve;              module procedure solveEnergyEquation;     end interface
       interface printExportBCs;     module procedure printExportenergyBCs;    end interface
       interface export;             module procedure energyExport;            end interface
       interface exportRaw;          module procedure energyExportRaw;         end interface
       interface exportTransient;    module procedure energyExportTransient;   end interface
       interface computeDivergence;  module procedure computeDivergenceEnergy; end interface

       interface setDTime;           module procedure setDTimeEnergy;          end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initenergy(nrg,g,SD,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing energy:'

         nrg%g = g
         nrg%SD = SD
         ! --- Vector Fields ---
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc

         call allocateField(nrg%T,Nx,Ny,Nz)
         call allocateField(nrg%Tstar,nrg%T)
         call allocateField(nrg%k_cc,nrg%T)

         call allocateX(nrg%F,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(nrg%F,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(nrg%F,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)

         call allocateVectorField(nrg%q,nrg%F)
         call allocateVectorField(nrg%k,nrg%F)

         ! --- Scalar Fields ---
         call allocateField(nrg%divQ,Nx,Ny,Nz)
         write(*,*) '     Fields allocated'


         ! --- Initialize Fields ---
         call initTBCs(nrg%T_bcs,nrg%g)
         write(*,*) '     BCs initialized'

         call initTfield(nrg%T%phi,g,dir)
         write(*,*) '     T-field initialized'

         call applyAllBCs(nrg%T_bcs,nrg%T%phi,g)
         write(*,*) '     BCs applied'

         call initK(nrg%k_cc%phi,g)
         call cellCenter2Face(nrg%k%x,nrg%k_cc%phi,g,1)
         call cellCenter2Face(nrg%k%y,nrg%k_cc%phi,g,2)
         call cellCenter2Face(nrg%k%z,nrg%k_cc%phi,g,3)
         write(*,*) '     Materials initialized'

         call init(nrg%probe_T,dir//'Tfield/','transient_T',&
         .not.restartB,shape(nrg%T%phi),(shape(nrg%T%phi)+1)/2,g)

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
         nrg%time = real(0.0,cp)
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteEnergy(nrg)
         implicit none
         type(energy),intent(inout) :: nrg
         call delete(nrg%T)
         call delete(nrg%Tstar)

         call delete(nrg%k_cc)
         call delete(nrg%k)

         call delete(nrg%q)
         call delete(nrg%F)
         
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

       subroutine setPe(nrg,Pe)
         implicit none
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Pe
         nrg%Pe = Pe
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
           call apply(nrg%probe_T,nrg%nstep,nrg%T%phi)
         endif

         if (getExportErrors(ss_MHD)) then
           call apply(nrg%probe_divQ,nrg%nstep,nrg%divQ%phi)
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
           call writeToFile(g,nrg%T%phi,dir//'Tfield/','T')
           call writeToFile(g,nrg%k_cc%phi,dir//'material/','k_cc')
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine energyExport(nrg,g,dir)
         implicit none
         type(energy),intent(in) :: nrg
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         real(cp),dimension(:,:,:),allocatable :: tempn,tempni
         if (solveEnergy) then
           write(*,*) 'Exporting PROCESSED Solutions for T'
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           allocate(tempn(Nx,Ny,Nz))
           call cellCenter2Node(tempn,nrg%T%phi,g)
           call writeToFile(g,tempn,dir//'Tfield/','Tnt')
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           allocate(tempni(Nx-2,Ny-2,Nz-2))
           tempni = tempn(2:Nx-1,2:Ny-1,2:Nz-1)
           call writeToFile(g,tempni,dir//'Tfield/','Tni')
           deallocate(tempni)
         ! ----------------------- MATERIAL PROPERTIES AT NODES ------------------------
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           allocate(tempn(Nx,Ny,Nz))
           call cellCenter2Node(tempn,nrg%k_cc%phi,g)
           call writeToFile(g,tempn,dir//'material/','k_n')
           deallocate(tempn)
           write(*,*) '     finished'
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solveEnergyEquation(nrg,U,g,ss_MHD)
         implicit none
         type(energy),intent(inout) :: nrg
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD
         select case (solveTMethod)
         case (1); call ExplicitEuler(nrg,U,g)
         end select
         nrg%nstep = nrg%nstep + 1
         nrg%time = nrg%time + nrg%dTime
       end subroutine

       subroutine ExplicitEuler(nrg,U,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i

         call assign(nrg%Tstar,zero)

         ! call CCadvect(nrg%Tstar,nrg%U_cct,nrg%T,g)
         call subtract(zero,nrg%Tstar)

         ! call CCdiffuse(nrg%tempCC,nrg%k,nrg%T,g)
         call subtract(nrg%Tstar,nrg%tempCC)

         call multiply(nrg%Tstar,nrg%dTime)
         call add(nrg%T,nrg%Tstar)

         ! Impose BCs:
         call applyAllBCs(nrg%T_bcs,nrg%T%phi,g)
       end subroutine

       ! ********************* AUX *****************************

       subroutine addBuoyancy(buyancy,nrg,g_mom,g_nrg,Gr,Re,Fr)
         ! Computes
         ! 
         !            1        Gr
         !           --- g +  ---  T g
         !           Fr^2     Re^2
         implicit none
         type(vectorField),intent(inout) :: buyancy
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Gr,Re,Fr
         type(grid),intent(in) :: g_mom,g_nrg

          call assign(buyancy,nrg%T)

         call multiply(buyancy,Gr/(Re**real(2.0,cp)))
       end subroutine

       subroutine computeDivergenceEnergy(nrg,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         if (solveEnergy) then
           call div(nrg%divQ%phi,nrg%F%x,nrg%F%y,nrg%F%z,g)
         endif
       end subroutine

       subroutine embedVelocityEnergy(nrg,U_fi,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(vectorField),intent(in) :: U_fi ! Raw momentum velocity
         type(grid),intent(in) :: g ! Momentum grid
         type(vectorField) :: temp

         call allocateX(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call allocateY(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call allocateZ(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call face2CellCenter(temp%x,U_fi%x,g,1)
         call face2CellCenter(temp%y,U_fi%y,g,2)
         call face2CellCenter(temp%z,U_fi%z,g,3)
         call embedCC(nrg%U_cct,temp,nrg%SD,g)
         call delete(temp)
       end subroutine

       end module