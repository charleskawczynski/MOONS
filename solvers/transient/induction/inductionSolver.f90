       module inductionSolver_mod
       use simParams_mod
       use constants_mod
       use myIO_mod
       use myDebug_mod
       use myTime_mod
       use myAllocate_mod
       use scalarField_mod
       use vectorField_mod

       use initializeBBCs_mod
       use initializeBfield_mod
       use initializeSigmaMu_mod

       use griddata_mod
       use rundata_mod
       use myError_mod
       use myDel_mod
       use vectorOps_mod
       use BCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use mySOR_mod
       use myPoisson_mod
       use baseProbes_mod

       implicit none

       private
       public :: induction,initialize,delete,solve
       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeJCrossB
       public :: computeDivergence


       type induction
         character(len=9) :: name = 'induction'
         ! --- Vector fields ---
         type(vectorField) :: B,Bstar,B0,J_cc,U_cct,tempVF     ! CC data
         type(vectorField) :: J,E,sigmaInv_edge                ! Edge data
         type(vectorField) :: F                                ! Face data
         ! --- Scalar fields ---
         type(scalarField) :: sigma,sigmaInv,mu,muInv          ! CC data
         type(scalarField) :: divB,divJ,phi,temp               ! CC data
         ! BCs:
         type(BCs) :: Bx_bcs,By_bcs,Bz_bcs
         type(BCs) :: phi_bcs
         ! Solver settings
         type(solverSettings) :: ss_ind,ss_cleanB
         ! Errors
         type(myError) :: err_divB,err_DivJ
         type(myError) :: err_cleanB,err_residual
         type(myTime) :: time_CP
         integer :: nstep

         type(mySOR) :: SOR_B, SOR_cleanB

         type(indexProbe) :: probe_B,probe_J
         type(errorProbe) :: probe_divB,probe_divJ

         real(dpn) :: ds = 1.0d-4     ! Pseudo time step
         integer :: NmaxB = 5         ! Maximum number of pseudo steps
         ! real(dpn) :: ds = 1.0d-6     ! S = 100
         ! real(dpn) :: ds = 1.0d-7     ! S = 1000
         ! integer :: NmaxB = 50        ! Maximum number of pseudo steps
         integer :: NmaxCleanB = 5    ! Maximum number of cleaning steps
       end type

       interface initialize;         module procedure initializeInduction;        end interface
       interface delete;             module procedure deleteInduction;            end interface
       interface solve;              module procedure inductionSolver;            end interface
       interface printExportBCs;     module procedure printExportInductionBCs;    end interface
       interface export;             module procedure inductionExport;            end interface
       interface exportRaw;          module procedure inductionExportRaw;         end interface
       interface exportTransient;    module procedure inductionExportTransient;   end interface
       interface computeDivergence;  module procedure computeDivergenceInduction; end interface

       contains

       subroutine initializeInduction(ind,gd,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing induction:'

         ! --- Vector Fields ---
         call myAllocate(Nx,Ny,Nz,gd,BLoc)
         call allocateVectorField(ind%B,Nx,Ny,Nz)
         call allocateVectorField(ind%Bstar,Nx,Ny,Nz)
         call allocateVectorField(ind%B0,Nx,Ny,Nz)
         call allocateVectorField(ind%J_cc,Nx,Ny,Nz)
         call allocateVectorField(ind%U_cct,Nx,Ny,Nz)
         call allocateVectorField(ind%tempVF,Nx,Ny,Nz)

         call myAllocate(Nx,Ny,Nz,gd,7,1); call allocateX(ind%J,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,2); call allocateY(ind%J,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,3); call allocateZ(ind%J,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,1); call allocateX(ind%E,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,2); call allocateY(ind%E,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,3); call allocateZ(ind%E,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,1); call allocateX(ind%sigmaInv_edge,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,2); call allocateY(ind%sigmaInv_edge,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,7,3); call allocateZ(ind%sigmaInv_edge,Nx,Ny,Nz)

         call myAllocate(Nx,Ny,Nz,gd,4,1); call allocateX(ind%F,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,4,2); call allocateY(ind%F,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,4,3); call allocateZ(ind%F,Nx,Ny,Nz)

         ! --- Scalar Fields ---
         call myAllocate(Nx,Ny,Nz,gd,BLoc)
         call allocateField(ind%sigma,Nx,Ny,Nz)
         call allocateField(ind%sigmaInv,Nx,Ny,Nz)
         call allocateField(ind%mu,Nx,Ny,Nz)
         call allocateField(ind%muInv,Nx,Ny,Nz)
         call allocateField(ind%phi,Nx,Ny,Nz)
         call allocateField(ind%temp,Nx,Ny,Nz)

         call allocateField(ind%divB,Nx,Ny,Nz)
         call allocateField(ind%divJ,Nx,Ny,Nz)
         write(*,*) '     Fields allocated'


         ! --- Initialize Fields ---
         call initializeBBCs(ind%Bx_bcs,ind%By_bcs,ind%Bz_bcs,ind%phi_bcs,gd)
         write(*,*) '     BCs initialized'

         call initializeBfield(ind%B%x,ind%B%y,ind%B%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,dir)
         write(*,*) '     B-field initialized'

         call applyAllBCs(ind%Bx_bcs,ind%B%x,gd)
         call applyAllBCs(ind%By_bcs,ind%B%y,gd)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,gd)
         write(*,*) '     BCs applied'

         call initializeSigmaMu(ind%sigma%phi,ind%mu%phi,gd)
         ind%sigma = one/ind%sigma
         call myCellCenter2Edge(ind%sigmaInv_edge%x,ind%sigma%phi,gd,1)
         call myCellCenter2Edge(ind%sigmaInv_edge%y,ind%sigma%phi,gd,2)
         call myCellCenter2Edge(ind%sigmaInv_edge%z,ind%sigma%phi,gd,3)
         call initializeSigmaMu(ind%sigma%phi,ind%mu%phi,gd)
         write(*,*) '     Materials initialized'

         call initialize(ind%probe_B,dir//'Bfield/','transient_Bx',&
         .not.restartB,shape(ind%B%x),(shape(ind%B%x)+1)*2/3,gd)

         call initialize(ind%probe_J,dir//'Jfield/','transient_Jx',&
         .not.restartB,shape(ind%J_cc%x),(shape(ind%J_cc%x)+1)*2/3,gd)

         call initialize(ind%probe_divB,dir//'Bfield/','transient_divB',.not.restartB)
         call initialize(ind%probe_divJ,dir//'Jfield/','transient_divJ',.not.restartB)

         call export(ind%probe_B)
         call export(ind%probe_J)
         call export(ind%probe_divB)
         call export(ind%probe_divJ)
         write(*,*) '     B/J probes initialized'

         call initialize(ind%err_divB)
         call initialize(ind%err_DivJ)
         call initialize(ind%err_cleanB)
         call initialize(ind%err_residual)

         ! Initialize solver settings
         call initializeSolverSettings(ind%ss_ind)
         call setName(ind%ss_ind,'SS B equation       ')
         call setMaxIterations(ind%ss_ind,ind%NmaxB)
         write(*,*) '     Solver settings for B initialized'
         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************
         call initializeSolverSettings(ind%ss_cleanB)
         call setName(ind%ss_cleanB,'cleaning B          ')
         call setMaxIterations(ind%ss_cleanB,ind%NmaxCleanB)
         write(*,*) '     Solver settings for cleaning initialized'

         write(*,*) '     Finished'
       end subroutine

       subroutine deleteInduction(ind,dir)
         implicit none
         type(induction),intent(inout) :: ind
         character(len=*),intent(in) :: dir
         call delete(ind%B)
         call delete(ind%Bstar)
         call delete(ind%B0)
         call delete(ind%J_cc)
         call delete(ind%U_cct)
         call delete(ind%tempVF)

         call delete(ind%J)
         call delete(ind%E)
         call delete(ind%sigmaInv_edge)

         call delete(ind%F)
         
         call delete(ind%sigma)
         call delete(ind%sigmaInv)
         call delete(ind%mu)
         call delete(ind%muInv)

         call delete(ind%divB)
         call delete(ind%divJ)

         call delete(ind%temp)
         call delete(ind%phi)

         call delete(ind%Bx_bcs)
         call delete(ind%By_bcs)
         call delete(ind%Bz_bcs)
         call delete(ind%phi_bcs)
         call closeTransientUnits(dir)
         write(*,*) 'Induction object deleted'
       end subroutine

       subroutine printExportInductionBCs(ind,dir)
         implicit none
         type(induction),intent(in) :: ind
         character(len=*),intent(in) :: dir
         if (solveInduction) call printAllBoundaries(ind%Bx_bcs,'Bx')
         if (solveInduction) call printAllBoundaries(ind%By_bcs,'By')
         if (solveInduction) call printAllBoundaries(ind%Bz_bcs,'Bz')
         if (cleanB)         call printAllBoundaries(ind%phi_bcs,'phi')
         if (solveInduction) call writeAllBoundaries(ind%Bx_bcs,dir//'parameters/','Bx')
         if (solveInduction) call writeAllBoundaries(ind%By_bcs,dir//'parameters/','By')
         if (solveInduction) call writeAllBoundaries(ind%Bz_bcs,dir//'parameters/','Bz')
         if (cleanB)         call writeAllBoundaries(ind%phi_bcs,dir//'parameters/','phi')
       end subroutine

       subroutine inductionExportTransient(ind,gd,ss_MHD,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         if ((getExportTransient(ss_MHD))) then
           call apply(ind%probe_B,getIteration(ss_MHD),ind%B%x)
           call apply(ind%probe_J,getIteration(ss_MHD),ind%J_cc%x)
         endif

         if (getExportErrors(ss_MHD)) then
           call apply(ind%probe_divB,getIteration(ss_MHD),ind%divB%phi)
           call apply(ind%probe_divJ,getIteration(ss_MHD),ind%divJ%phi)
         endif
       end subroutine

       subroutine inductionExportRaw(ind,gd,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! ------------------------------- B/J FIELD ------------------------------
         if (solveInduction) then
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%B%x,ind%B%y,ind%B%z,dir//'Bfield/','Bxct','Byct','Bzct')
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%J_cc%x,ind%J_cc%y,ind%J_cc%z,dir//'Jfield/','jxct','jyct','jzct')
         ! ---------------------------- SIGMA/MU FIELD ----------------------------
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%sigma%phi,dir//'material/','sigmac')
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%mu%phi,dir//'material/','muc')
         ! ----------------------- DIVERGENCE QUANTITIES --------------------------
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%divB%phi,dir//'Bfield/','divBct')
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%divJ%phi,dir//'Jfield/','divJct')
           write(*,*) 'Exported Raw Solutions for B'
         endif
       end subroutine

       subroutine inductionExport(ind,gd,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         ! Interior
         real(dpn),dimension(:,:,:),allocatable :: tempnx,tempny,tempnz,tempn,tempcc

         ! -------------------------- B/J FIELD AT NODES --------------------------
         ! Magnetic field/currents:
         if (solveInduction) then
           call myAllocate(Nx,Ny,Nz,gd,dom_n_tot); allocate(tempnx(Nx,Ny,Nz))
                                                   allocate(tempny(Nx,Ny,Nz))
                                                   allocate(tempnz(Nx,Ny,Nz))
                                                   allocate(tempn (Nx,Ny,Nz))
           call myCellCenter2Node(tempnx,ind%B%x,gd)
           call myCellCenter2Node(tempny,ind%B%y,gd)
           call myCellCenter2Node(tempnz,ind%B%z,gd)

           call writeToFile(gd%xnt,gd%ynt,gd%znt,tempnx,tempny,tempnz,dir//'Bfield/','Bxnt','Bynt','Bznt')
           call myNodeDiv(tempn,tempnx,tempny,tempnz,gd)
           call writeToFile(gd%xnt,gd%ynt,gd%znt,tempn,dir//'Bfield/','divBnt')
           call myCellCenter2Node(tempnx,ind%J_cc%x,gd)
           call myCellCenter2Node(tempny,ind%J_cc%y,gd)
           call myCellCenter2Node(tempnz,ind%J_cc%z,gd)
           call writeToFile(gd%xnt,gd%ynt,gd%znt,tempnx,tempny,tempnz,dir//'Jfield/','jxnt','jynt','jznt')
           deallocate(tempnx,tempny,tempnz,tempn)

         ! ----------------------- SIGMA/MU FIELD AT NODES ------------------------
           call myAllocate(Nx,Ny,Nz,gd,dom_n_tot); allocate(tempn(Nx,Ny,Nz))
           call myCellCenter2Node(tempn,ind%sigma%phi,gd)
           call writeToFile(gd%xnt,gd%ynt,gd%znt,tempn,dir//'material/','sigman')
           call myCellCenter2Node(tempn,ind%mu%phi,gd)
           call writeToFile(gd%xnt,gd%ynt,gd%znt,tempn,dir//'material/','mun')
           deallocate(tempn)

         ! -------------------------- TOTAL DOMAIN VELOCITY -----------------------
         ! Total velocity
           call writeToFile(gd%xct,gd%yct,gd%zct,ind%U_cct%x,ind%U_cct%y,ind%U_cct%z,dir//'Ufield/','uct','vct','wct')
           call myAllocate(Nx,Ny,Nz,gd,dom_cc_tot); allocate(tempcc(Nx,Ny,Nz))
           call myCC2CCDiv(tempcc,ind%U_cct%x,ind%U_cct%y,ind%U_cct%z,gd)
           call writeToFile(gd%xct,gd%yct,gd%zct,tempcc,dir//'Ufield/','divUct')
           deallocate(tempcc)
         endif
       end subroutine

       subroutine inductionSolver(ind,U,gd,rd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(inout) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         select case (solveBMethod)
         case (1); call lowRemPoisson(ind,U,gd,ss_MHD)
         case (2); call lowRemPseudoTimeStepUniform(ind,U,gd)
         case (3); call lowRemPseudoTimeStep(ind,U,gd)
         case (4); call fullInduction(ind,U,gd,rd)
         case (5); call CTmethod(ind,U,gd)
         end select
         if (cleanB) then
           call cleanBSolution(ind,gd,ss_MHD)
         endif
         ind%nstep = ind%nstep + 1
       end subroutine

       subroutine lowRemPoisson(ind,U,gd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(inout) :: ss_MHD

         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,1)
         call myPoisson(ind%SOR_B,ind%B%x,ind%temp%phi,ind%Bx_bcs,gd,ind%ss_ind,&
         ind%err_residual,1,getExportErrors(ss_MHD))

         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,2)
         call myPoisson(ind%SOR_B,ind%B%y,ind%temp%phi,ind%By_bcs,gd,ind%ss_ind,&
         ind%err_residual,1,getExportErrors(ss_MHD))

         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,3)
         call myPoisson(ind%SOR_B,ind%B%z,ind%temp%phi,ind%Bz_bcs,gd,ind%ss_ind,&
         ind%err_residual,1,getExportErrors(ss_MHD))
       end subroutine

       subroutine lowRemPseudoTimeStepUniform(ind,U,gd)
         ! This routine assumed div(B)=0 (which requires uniform properties), 
         ! which is how it differs from lowRemPseudoTimeStep(). This was an 
         ! important case to test against the Poisson solution, since the 
         ! terms are essentially the same, but a different iterative method 
         ! is applied.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(griddata),intent(in) :: gd
         ! ********************** LOCAL VARIABLES ***********************
         integer :: i
         ! *********************** ALLOCATE DATA ************************
         
         do i=1,ind%NmaxB
           ind%Bstar = zero
           ! ------------- diffusion term -------------
           call myCC2CCLap(ind%temp%phi,ind%B%x,gd)
           ind%Bstar%x = ind%Bstar%x + ind%ds*ind%temp%phi
           call myCC2CCLap(ind%temp%phi,ind%B%y,gd)
           ind%Bstar%y = ind%Bstar%y + ind%ds*ind%temp%phi
           call myCC2CCLap(ind%temp%phi,ind%B%z,gd)
           ind%Bstar%z = ind%Bstar%z + ind%ds*ind%temp%phi
           
           ! ------------- source term -------------
           call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,1)
           ind%Bstar%x = ind%Bstar%x - ind%ds*ind%temp%phi
           call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,2)
           ind%Bstar%y = ind%Bstar%y - ind%ds*ind%temp%phi
           call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,3)
           ind%Bstar%z = ind%Bstar%z - ind%ds*ind%temp%phi

           ! Add induced field of previous time step (B^n)
           ind%B = ind%B*ind%mu + ind%Bstar

           ! Impose BCs:
           call applyAllBCs(ind%Bx_bcs,ind%B%x,gd)
           call applyAllBCs(ind%By_bcs,ind%B%y,gd)
           call applyAllBCs(ind%Bz_bcs,ind%B%z,gd)
         enddo
       end subroutine

       subroutine lowRemPseudoTimeStep(ind,U,gd)
         ! Needs to be fixed (j is not computed correctly)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(griddata),intent(in) :: gd
         ! ********************** LOCAL VARIABLES ***********************
         integer :: i

         do i=1,ind%NmaxB
           ind%Bstar = zero
           ind%B = ind%B/ind%mu
           ! ------------- diffusion term -------------
           call myCCBfieldDiffuse(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,ind%sigmaInv%phi,gd,1)
           ind%Bstar%x = ind%Bstar%x - ind%ds*ind%temp%phi
           call myCCBfieldDiffuse(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,ind%sigmaInv%phi,gd,2)
           ind%Bstar%y = ind%Bstar%y - ind%ds*ind%temp%phi
           call myCCBfieldDiffuse(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,ind%sigmaInv%phi,gd,3)
           ind%Bstar%z = ind%Bstar%z - ind%ds*ind%temp%phi

           ! Try converting to:
           ! call myCCBfieldDiffuse(ind%tempVF%x,ind%B%x,ind%B%y,ind%B%z,ind%sigmaInv%phi,gd,1)
           ! call myCCBfieldDiffuse(ind%tempVF%y,ind%B%x,ind%B%y,ind%B%z,ind%sigmaInv%phi,gd,2)
           ! call myCCBfieldDiffuse(ind%tempVF%z,ind%B%x,ind%B%y,ind%B%z,ind%sigmaInv%phi,gd,3)
           ! ind%Bstar = ind%Bstar - ind%ds*ind%tempVF
           
           ! ------------- source term -------------
           call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,1)
           ind%Bstar%x = ind%Bstar%x - ind%ds*ind%temp%phi
           call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,2)
           ind%Bstar%y = ind%Bstar%y - ind%ds*ind%temp%phi
           call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,3)
           ind%Bstar%z = ind%Bstar%z - ind%ds*ind%temp%phi

           ! Try converting to:
           ! call myCCBfieldAdvect(ind%tempVF%x,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,1)
           ! call myCCBfieldAdvect(ind%tempVF%y,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,2)
           ! call myCCBfieldAdvect(ind%tempVF%z,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,3)
           ! ind%Bstar = ind%Bstar - ind%ds*ind%tempVF

           ! Add induced field of previous time step (B^n)
           ind%B = ind%B*ind%mu + ind%Bstar

           ! Impose BCs:
           call applyAllBCs(ind%Bx_bcs,ind%B%x,gd)
           call applyAllBCs(ind%By_bcs,ind%B%y,gd)
           call applyAllBCs(ind%Bz_bcs,ind%B%z,gd)
         enddo
       end subroutine

       subroutine fullInduction(ind,U,gd,rd)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         ! ********************** LOCAL VARIABLES ***********************
         real(dpn) :: Rem,dt

         dt = getDtime(rd)
         Rem = getRem(rd)
         ind%Bstar = zero

         ! ------------- advection term -------------
         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B%x,ind%B%y,ind%B%z,gd,1)
         ind%Bstar%x = ind%Bstar%x - dt*ind%temp%phi
         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B%x,ind%B%y,ind%B%z,gd,2)
         ind%Bstar%y = ind%Bstar%y - dt*ind%temp%phi
         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B%x,ind%B%y,ind%B%z,gd,3)
         ind%Bstar%z = ind%Bstar%z - dt*ind%temp%phi

         ! Try converting to:
         ! call myCCBfieldAdvect(ind%tempVF%x,U%x,U%y,U%z,ind%B%x,ind%B%y,ind%B%z,gd,1)
         ! call myCCBfieldAdvect(ind%tempVF%y,U%x,U%y,U%z,ind%B%x,ind%B%y,ind%B%z,gd,2)
         ! call myCCBfieldAdvect(ind%tempVF%z,U%x,U%y,U%z,ind%B%x,ind%B%y,ind%B%z,gd,3)
         ! ind%Bstar = ind%Bstar - dt*ind%tempVF

         ! ------------- diffusion term -------------
         ind%B = ind%B/ind%mu
         call myCCBfieldDiffuse(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,ind%sigma%phi,gd,1)
         ind%Bstar%x = ind%Bstar%x - dt/Rem*ind%temp%phi
         call myCCBfieldDiffuse(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,ind%sigma%phi,gd,2)
         ind%Bstar%y = ind%Bstar%y - dt/Rem*ind%temp%phi
         call myCCBfieldDiffuse(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,ind%sigma%phi,gd,3)
         ind%Bstar%z = ind%Bstar%z - dt/Rem*ind%temp%phi
         ind%B = ind%B*ind%mu

         ! Try converting to:
         ! ind%B = ind%B/ind%mu
         ! call myCCBfieldDiffuse(ind%tempVF%x,ind%B%x,ind%B%y,ind%B%z,ind%sigma%phi,gd,1)
         ! call myCCBfieldDiffuse(ind%tempVF%y,ind%B%x,ind%B%y,ind%B%z,ind%sigma%phi,gd,2)
         ! call myCCBfieldDiffuse(ind%tempVF%z,ind%B%x,ind%B%y,ind%B%z,ind%sigma%phi,gd,3)
         ! ind%Bstar = ind%Bstar - dt/Rem*ind%tempVF
         ! ind%B = ind%B*ind%mu

         ! ------------- source term -------------
         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,1)
         ind%Bstar%x = ind%Bstar%x - dt*ind%temp%phi
         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,2)
         ind%Bstar%y = ind%Bstar%y - dt*ind%temp%phi
         call myCCBfieldAdvect(ind%temp%phi,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,3)
         ind%Bstar%z = ind%Bstar%z - dt*ind%temp%phi

         ! Try converting to:
         ! call myCCBfieldAdvect(ind%tempVF%x,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,1)
         ! call myCCBfieldAdvect(ind%tempVF%y,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,2)
         ! call myCCBfieldAdvect(ind%tempVF%z,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,gd,3)
         ! ind%Bstar = ind%Bstar - dt*ind%tempVF

         ! Add induced field of previous time step (B^n)
         ind%B = ind%B + ind%Bstar

         ! Impose BCs:
         call applyAllBCs(ind%Bx_bcs,ind%B%x,gd)
         call applyAllBCs(ind%By_bcs,ind%B%y,gd)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,gd)
       end subroutine

       subroutine CTmethod(ind,U,gd)
         ! inductionSolverCT solves the induction equation using the
         ! Constrained Transport (CT) Method. The magnetic field is
         ! stored, and collocated, at the cell center. The magnetic
         ! field is updated using Faraday's Law, where the electric
         ! field is solved for using appropriate fluxes as described
         ! in "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         ! The velocity field is assumed to be cell centered.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(griddata),intent(in) :: gd
         ! ********************** LOCAL VARIABLES ***********************
         integer :: i

         do i=1,ind%NmaxB

           ! Compute current from appropriate fluxes:
           call myCC2EdgeCurl(ind%J%x,ind%B%x,ind%B%y,ind%B%z,gd,1)
           call myCC2EdgeCurl(ind%J%y,ind%B%x,ind%B%y,ind%B%z,gd,2)
           call myCC2EdgeCurl(ind%J%z,ind%B%x,ind%B%y,ind%B%z,gd,3)

           ! Compute fluxes of u cross B
           call myCollocatedCross(ind%tempVF%x,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,1)
           call myCollocatedCross(ind%tempVF%y,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,2)
           call myCollocatedCross(ind%tempVF%z,U%x,U%y,U%z,ind%B0%x,ind%B0%y,ind%B0%z,3)
           call myCellCenter2Edge(ind%E%x,ind%tempVF%x,gd,1)
           call myCellCenter2Edge(ind%E%y,ind%tempVF%y,gd,2)
           call myCellCenter2Edge(ind%E%z,ind%tempVF%z,gd,3)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! Add induced field of previous time step (B^n)
           call myEdge2FaceCurl(ind%F%x,ind%E%x,ind%E%y,ind%E%z,gd,1)
           call myEdge2FaceCurl(ind%F%y,ind%E%x,ind%E%y,ind%E%z,gd,2)
           call myEdge2FaceCurl(ind%F%z,ind%E%x,ind%E%y,ind%E%z,gd,3)
           call myFace2CellCenter(ind%tempVF%x,ind%F%x,gd,1)
           call myFace2CellCenter(ind%tempVF%y,ind%F%y,gd,2)
           call myFace2CellCenter(ind%tempVF%z,ind%F%z,gd,3)

           ! ind%B = ind%B - ind%ds*ind%tempVF
           call multiply(ind%tempVF,ind%ds)
           call subtract(ind%B,ind%tempVF)

           ! Impose BCs:
           call applyAllBCs(ind%Bx_bcs,ind%B%x,gd)
           call applyAllBCs(ind%By_bcs,ind%B%y,gd)
           call applyAllBCs(ind%Bz_bcs,ind%B%z,gd)
         enddo
       end subroutine

       subroutine cleanBSolution(ind,gd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************

         call myCC2CCDiv(ind%temp%phi,ind%B%x,ind%B%y,ind%B%z,gd)
         call myPoisson(ind%SOR_cleanB,ind%phi%phi,ind%temp%phi,ind%phi_bcs,gd,ind%ss_cleanB,&
          ind%err_CleanB,1,getExportErrors(ss_MHD))

         call myCC2CCDel(ind%temp%phi,ind%phi%phi,gd,1)
         ind%B%x = ind%B%x - ind%temp%phi
         call myCC2CCDel(ind%temp%phi,ind%phi%phi,gd,2)
         ind%B%y = ind%B%y - ind%temp%phi
         call myCC2CCDel(ind%temp%phi,ind%phi%phi,gd,3)
         ind%B%z = ind%B%z - ind%temp%phi

         ! Impose BCs:
         call applyAllBCs(ind%Bx_bcs,ind%B%x,gd)
         call applyAllBCs(ind%By_bcs,ind%B%y,gd)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,gd)
         call stopTime(ind%time_CP,ind%ss_cleanB)
       end subroutine

       subroutine computeJCrossB(jcrossB,ind,gd,Re,Ha)
         ! computeJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the total mangetic
         ! field.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(vectorField),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         real(dpn),intent(in) :: Re,Ha
         ! *********************** LOCAL VARIABLES **********************
         integer :: Nx,Ny,Nz,dir
         ! **************************************************************

         ind%Bstar = ind%B + ind%B0

         ! Index fluid face source terms:
         ! Excluding wall normal values

         dir = 1
         call myAllocate(Nx,Ny,Nz,gd,ULoc,dir)
         call myCollocatedCross(ind%tempVF%x,ind%J_cc%x,ind%J_cc%y,ind%J_cc%z,ind%Bstar%x,ind%Bstar%y,ind%Bstar%z,dir)
         call myCellCenter2Face(ind%F%x,ind%tempVF%x,gd,dir)
         jcrossB%x = zero ! expensive!
         jcrossB%x(2:Nx-1,2:Ny-1,2:Nz-1) = &
         ind%F%x( Nin1(1)+1: Nin2(1)-1,Nice1(2):Nice2(2),Nice1(3):Nice2(3))
         jcrossB%x = jcrossB%x*((Ha**two)/Re)

         dir = 2
         call myAllocate(Nx,Ny,Nz,gd,ULoc,dir)
         call myCollocatedCross(ind%tempVF%y,ind%J_cc%x,ind%J_cc%y,ind%J_cc%z,ind%Bstar%x,ind%Bstar%y,ind%Bstar%z,dir)
         call myCellCenter2Face(ind%F%y,ind%tempVF%y,gd,dir)
         jcrossB%y = zero ! expensive!
         jcrossB%y(2:Nx-1,2:Ny-1,2:Nz-1) = &
         ind%F%y(Nice1(1):Nice2(1), Nin1(2)+1: Nin2(2)-1,Nice1(3):Nice2(3))
         jcrossB%y = jcrossB%y*((Ha**two)/Re)

         dir = 3
         call myAllocate(Nx,Ny,Nz,gd,ULoc,dir)
         call myCollocatedCross(ind%tempVF%z,ind%J_cc%x,ind%J_cc%y,ind%J_cc%z,ind%Bstar%x,ind%Bstar%y,ind%Bstar%z,dir)
         call myCellCenter2Face(ind%F%z,ind%tempVF%z,gd,dir)
         jcrossB%z = zero ! expensive!
         jcrossB%z(2:Nx-1,2:Ny-1,2:Nz-1) = &
         ind%F%z(Nice1(1):Nice2(1),Nice1(2):Nice2(2), Nin1(3)+1: Nin2(3)-1)
         jcrossB%z = jcrossB%z*((Ha**two)/Re)
       end subroutine

       subroutine computeDivergenceInduction(ind,gd)
         implicit none
         type(induction),intent(inout) :: ind
         type(griddata),intent(in) :: gd
         if (solveInduction) then
           select case (solveBMethod)
           case (5)
             ! CT method enforces div(b) = 0, (result is in CC), 
             ! when computed from FACE-centered data:
             call myFaceDiv(ind%divB%phi,ind%F%x,ind%F%y,ind%F%z,gd)
             call myCC2CCDiv(ind%divJ%phi,ind%J_cc%x,ind%J_cc%y,ind%J_cc%z,gd)
           case default
             call myCC2CCDiv(ind%divB%phi,ind%B%x,ind%B%y,ind%B%z,gd)
             call myCC2CCDiv(ind%divJ%phi,ind%J_cc%x,ind%J_cc%y,ind%J_cc%z,gd)
           end select
         endif
       end subroutine



       end module