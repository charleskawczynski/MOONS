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

       use grid_mod
       use myError_mod
       use interpOps_mod
       use del_mod
       use delOps_mod
       use BCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use probe_base_mod

       implicit none

       private
       public :: energy,init,delete,solve

       public :: setDTime,setNmaxT,setPr

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
         type(scalarField) :: T,Tstar               ! CC data
         type(vectorField) :: q,F,k                 ! Face data
         type(vectorField) :: U_cct                 ! Face data
         real(cp) :: dT,beta
         ! --- Scalar fields ---
         type(scalarField) :: k_cc                  ! CC data
         type(scalarField) :: divQ                  ! CC data
         ! BCs:
         type(BCs) :: T_bcs
         ! Solver settings
         type(solverSettings) :: ss_energy
         ! Errors
         type(myError) :: err_divQ,err_residual

         type(indexProbe) :: probe_T
         type(errorProbe) :: probe_divQ
         type(grid) :: g

         integer :: nstep             ! Nth time step
         integer :: NmaxT             ! Maximum number iterations in solving T (if iterative)
         real(cp) :: dTime            ! Time step
         real(cp) :: time             ! Time

         real(cp) :: Re,Pr,Rem,Ec,Al  ! Reynolds, Prandtl, Magnetic Reynolds, Eckert, Alfen
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

       subroutine initenergy(nrg,g,dir)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing energy:'

         nrg%g = g
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
         call myCellCenter2Face(nrg%k%x,nrg%k_cc%phi,g,1)
         call myCellCenter2Face(nrg%k%y,nrg%k_cc%phi,g,2)
         call myCellCenter2Face(nrg%k%z,nrg%k_cc%phi,g,3)
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

       subroutine setPr(nrg,Pr)
         implicit none
         type(energy),intent(inout) :: nrg
         real(cp),intent(in) :: Pr
         nrg%Pr = Pr
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
           call myCellCenter2Node(tempn,nrg%T%phi,g)
           call writeToFile(g,tempn,dir//'Tfield/','Tnt')
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           allocate(tempni(Nx-2,Ny-2,Nz-2))
           tempni = tempn(2:Nx-1,2:Ny-1,2:Nz-1)
           call writeToFile(g,tempni,dir//'Tfield/','Tni')
           deallocate(tempni)
         ! ----------------------- MATERIAL PROPERTIES AT NODES ------------------------
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           allocate(tempn(Nx,Ny,Nz))
           call myCellCenter2Node(tempn,nrg%k_cc%phi,g)
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
         call assign(nrg%T,zero)

         ! Impose BCs:
         call applyAllBCs(nrg%T_bcs,nrg%T%phi,g)
       end subroutine

       ! ********************* AUX *****************************

       subroutine computeBuoyancy(buyancy,nrg,g_mom,g_nrg,Re,Ha)
         implicit none
         type(vectorField),intent(inout) :: buyancy
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g_mom,g_nrg
         real(cp),intent(in) :: Re,Ha
         integer :: Nx,Ny,Nz,dir

         ! nrgex fluid face source terms:
         ! Excluding wall normal values

         dir = 1
         Nx = g_mom%c(1)%sn; Ny = g_mom%c(2)%sc; Nz = g_mom%c(3)%sc
         call myCellCenter2Face(nrg%F%x,nrg%Tstar%phi,g_nrg,dir)
         buyancy%x = zero ! expensive!
         buyancy%x(3:Nx-2,2:Ny-1,2:Nz-1) = &
         nrg%F%x( Nin1(1)+1: Nin2(1)-1,Nice1(2):Nice2(2),Nice1(3):Nice2(3))
         buyancy%x = buyancy%x*((Ha**real(2.0,cp))/Re)

         dir = 2
         Nx = g_mom%c(1)%sc; Ny = g_mom%c(2)%sn; Nz = g_mom%c(3)%sc
         call myCellCenter2Face(nrg%F%y,nrg%Tstar%phi,g_nrg,dir)
         buyancy%y = zero ! expensive!
         buyancy%y(2:Nx-1,3:Ny-2,2:Nz-1) = &
         nrg%F%y(Nice1(1):Nice2(1), Nin1(2)+1: Nin2(2)-1,Nice1(3):Nice2(3))
         buyancy%y = buyancy%y*((Ha**real(2.0,cp))/Re)

         dir = 3
         Nx = g_mom%c(1)%sc; Ny = g_mom%c(2)%sc; Nz = g_mom%c(3)%sn
         call myCellCenter2Face(nrg%F%z,nrg%Tstar%phi,g_nrg,dir)
         buyancy%z = zero ! expensive!
         buyancy%z(2:Nx-1,2:Ny-1,3:Nz-2) = &
         nrg%F%z(Nice1(1):Nice2(1),Nice1(2):Nice2(2), Nin1(3)+1: Nin2(3)-1)
         buyancy%z = buyancy%z*((Ha**real(2.0,cp))/Re)
       end subroutine

       subroutine computeDivergenceEnergy(nrg,g)
         implicit none
         type(energy),intent(inout) :: nrg
         type(grid),intent(in) :: g
         if (solveEnergy) then
           call div(nrg%divQ%phi,nrg%F%x,nrg%F%y,nrg%F%z,g)
         endif
       end subroutine

       subroutine embedVelocityEnergy(U_fi,U_cct,U_cci,g_mom)
         implicit none
         type(vectorField),intent(in) :: U_fi
         type(vectorField),intent(inout) :: U_cct
         type(scalarField),intent(inout) :: U_cci
         type(grid),intent(in) :: g_mom
         integer,dimension(3) :: Ni

         Ni = (/g_mom%c(1)%sc-2,g_mom%c(2)%sc-2,g_mom%c(3)%sc-2/) ! minus fictitious cells
         ! (exclude fictitious cells)
         call myFace2CellCenter(U_cci%phi,U_fi%x,g_mom,1)

          U_cct%x(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         U_cci%phi(2:Ni(1)+1,2:Ni(2)+1,2:Ni(3)+1)

         call myFace2CellCenter(U_cci%phi,U_fi%y,g_mom,2)
          U_cct%y(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         U_cci%phi(2:Ni(1)+1,2:Ni(2)+1,2:Ni(3)+1)

         call myFace2CellCenter(U_cci%phi,U_fi%z,g_mom,3)
          U_cct%z(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         U_cci%phi(2:Ni(1)+1,2:Ni(2)+1,2:Ni(3)+1)
       end subroutine

       end module