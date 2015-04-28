       module inductionSolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use myTime_mod
       use scalarField_mod
       use vectorField_mod

       use initializeBBCs_mod
       use initializeBfield_mod
       use initializeSigmaMu_mod

       use grid_mod
       use myError_mod
       use interpOps_mod
       use del_mod
       use ops_discrete_mod
       use ops_physics_mod
       use BCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use mySOR_mod
       use myADI_mod
       use myMG_mod
       use myPoisson_mod
       use probe_base_mod

       implicit none

       private
       public :: induction,init,delete,solve

       public :: setDTime,setNmaxB,setRem,setNmaxCleanB

       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeJCrossB
       public :: computeDivergence
       public :: computeCurrent
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

       real(cp),parameter :: zero = real(0.0,cp)
       real(cp),parameter :: one = real(1.0,cp)

       type induction
         character(len=9) :: name = 'induction'
         ! --- Vector fields ---
         type(vectorField) :: B,Bstar,B0,J_cc,U_cct,tempVF     ! CC data
         type(vectorField) :: J,E,sigmaInv_edge                ! Edge data
         type(vectorField) :: F,sigmaInv_face                  ! Face data
         ! --- Scalar fields ---
         type(scalarField) :: sigma,sigmaInv,mu,muInv          ! CC data
         type(scalarField) :: divB,divJ,phi,temp               ! CC data
         ! BCs:
         type(BCs) :: Bx_bcs,By_bcs,Bz_bcs
         ! type(vectorBCs) :: B_bcs
         type(BCs) :: phi_bcs
         ! Solver settings
         type(solverSettings) :: ss_ind,ss_cleanB,ss_ADI
         ! Errors
         type(myError) :: err_divB,err_DivJ,err_ADI
         type(myError) :: err_cleanB,err_residual
         type(myTime) :: time_CP

         type(mySOR) :: SOR_B, SOR_cleanB
         type(myADI) :: ADI_B
         type(multiGrid),dimension(2) :: MG ! For cleaning procedure

         type(indexProbe) :: probe_Bx,probe_By,probe_Bz
         type(indexProbe) :: probe_J
         type(errorProbe) :: probe_divB,probe_divJ
         type(grid) :: g

         integer :: nstep             ! Nth time step
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: omega            ! Intensity of time changing magnetic field
       end type

       interface init;               module procedure initInduction;              end interface
       interface delete;             module procedure deleteInduction;            end interface
       interface solve;              module procedure inductionSolver;            end interface
       interface printExportBCs;     module procedure printExportInductionBCs;    end interface
       interface export;             module procedure inductionExport;            end interface
       interface exportRaw;          module procedure inductionExportRaw;         end interface
       interface exportTransient;    module procedure inductionExportTransient;   end interface
       interface computeDivergence;  module procedure computeDivergenceInduction; end interface

       interface setDTime;           module procedure setDTimeInduction;          end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initInduction(ind,g,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing induction:'

         ind%g = g
         ! --- Vector Fields ---
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc

         ! CC Data
         call allocateVectorField(ind%B,Nx,Ny,Nz)
         call allocateVectorField(ind%Bstar,ind%B)
         call allocateVectorField(ind%B0,ind%B)
         call allocateVectorField(ind%J_cc,ind%B)
         call allocateVectorField(ind%U_cct,ind%B)
         call allocateVectorField(ind%tempVF,ind%B)

         ! Edge Data
         call allocateX(ind%J,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
         call allocateY(ind%J,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
         call allocateZ(ind%J,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)

         call allocateVectorField(ind%E,ind%J)
         call allocateVectorField(ind%sigmaInv_edge,ind%J)

         ! Face Data
         call allocateX(ind%F,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(ind%F,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(ind%F,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)

         call allocateVectorField(ind%sigmaInv_face,ind%F)

         ! --- Scalar Fields ---
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc

         call allocateField(ind%sigma,Nx,Ny,Nz)
         call allocateField(ind%sigmaInv,ind%sigma)
         call allocateField(ind%mu,ind%sigma)
         call allocateField(ind%muInv,ind%sigma)
         call allocateField(ind%phi,ind%sigma)
         call allocateField(ind%temp,ind%sigma)

         call allocateField(ind%divB,ind%sigma)
         call allocateField(ind%divJ,ind%sigma)
         write(*,*) '     Fields allocated'


         ! --- Initialize Fields ---
         call initBBCs(ind%Bx_bcs,ind%By_bcs,ind%Bz_bcs,ind%phi_bcs,g,cleanB)
         ! call initBBCs(ind%B_bcs,ind%phi_bcs,g,cleanB)
         write(*,*) '     BCs initialized'

         call initBfield(ind%B%x,ind%B%y,ind%B%z,ind%B0%x,ind%B0%y,ind%B0%z,g,dir)
         write(*,*) '     B-field initialized'

         ! call applyAllBCs(ind%B,ind%B_bcs,g)

         call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
         call applyAllBCs(ind%By_bcs,ind%B%y,g)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
         write(*,*) '     BCs applied'

         call initSigmaMu(ind%sigma%phi,ind%mu%phi,g)
         call divide(one,ind%sigma)
         call myCellCenter2Edge(ind%sigmaInv_edge,ind%sigma%phi,g)
         call myCellCenter2Face(ind%sigmaInv_face,ind%sigma%phi,g)
         call initSigmaMu(ind%sigma%phi,ind%mu%phi,g)

         write(*,*) '     Materials initialized'

         call init(ind%probe_Bx,dir//'Bfield/','transient_Bx',&
         .not.restartB,shape(ind%B%x),(shape(ind%B%x)+1)/2,g)

         call init(ind%probe_By,dir//'Bfield/','transient_By',&
         .not.restartB,shape(ind%B%y),(shape(ind%B%y)+1)/2,g)

         call init(ind%probe_Bz,dir//'Bfield/','transient_Bz',&
         .not.restartB,shape(ind%B%z),(shape(ind%B%z)+1)/2,g)

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
         if (cleanB) call init(ind%MG,ind%phi%s,ind%phi_bcs,ind%g,ind%ss_cleanB,.false.)

         if (restartB) then
         call readLastStepFromFile(ind%nstep,dir//'parameters/','n_ind')
         else; ind%nstep = 0
         endif
         ind%t = real(0.0,cp)
         ind%omega = real(1.0,cp)
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteInduction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%B)
         call delete(ind%Bstar)
         call delete(ind%B0)
         call delete(ind%J_cc)
         call delete(ind%U_cct)
         call delete(ind%tempVF)

         call delete(ind%J)
         call delete(ind%E)
         call delete(ind%sigmaInv_edge)
         call delete(ind%sigmaInv_face)

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

         call delete(ind%probe_Bx)
         call delete(ind%probe_By)
         call delete(ind%probe_Bz)
         call delete(ind%probe_J)
         call delete(ind%probe_divB)
         call delete(ind%probe_divJ)
         call delete(ind%g)

         ! call delete(ind%SOR_B)
         if (cleanB) call delete(ind%MG)

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

       subroutine setRem(ind,Rem)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Rem
         ind%Rem = Rem
       end subroutine

       subroutine setNmaxCleanB(ind,NmaxCleanB)
         implicit none
         type(induction),intent(inout) :: ind
         integer,intent(in) :: NmaxCleanB
         ind%NmaxCleanB = NmaxCleanB
       end subroutine

       ! ******************* EXPORT ****************************

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

       subroutine inductionExportTransient(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         if ((getExportTransient(ss_MHD))) then
           call apply(ind%probe_Bx,ind%nstep,ind%B%x)
           call apply(ind%probe_By,ind%nstep,ind%B%y)
           call apply(ind%probe_Bz,ind%nstep,ind%B%z)
           call apply(ind%probe_J,ind%nstep,ind%J_cc%x)
         endif

         if (getExportErrors(ss_MHD)) then
           call apply(ind%probe_divB,ind%nstep,ind%divB%phi)
           call apply(ind%probe_divJ,ind%nstep,ind%divJ%phi)
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
           write(*,*) 'Exporting RAW Solutions for B'
           call writeToFile(g,ind%B0,dir//'Bfield/','B0xct','B0yct','B0zct')
           call writeToFile(g,ind%B,dir//'Bfield/','Bxct','Byct','Bzct')
           call writeToFile(g,ind%J_cc,dir//'Jfield/','jxct','jyct','jzct')
           call writeToFile(g,ind%sigma%phi,dir//'material/','sigmac')
           call writeToFile(g,ind%divB%phi,dir//'Bfield/','divBct')
           call writeToFile(g,ind%divJ%phi,dir//'Jfield/','divJct')
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine inductionExport(ind,g,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         ! Interior
         real(cp),dimension(:,:,:),allocatable :: tempn,tempcc
         type(vectorField) :: tempVFn

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           write(*,*) 'Exporting PROCESSED solutions for B'
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           call allocateVectorField(tempVFn,Nx,Ny,Nz)
           call myCellCenter2Node(tempVFn,ind%B,g)

           call writeVecPhysical(g,tempVFn,dir//'Bfield/','Bxnt','Bynt','Bznt')

           call myCellCenter2Node(tempVFn,ind%B0,g)
           ! call writeToFile(g,tempVFn,dir//'Bfield/','B0xnt','B0ynt','B0znt')
           call writeVecPhysical(g,tempVFn,dir//'Bfield/','B0xnt_phys','B0ynt_phys','B0znt_phys')

           call myCellCenter2Node(tempVFn,ind%J_cc,g)
           ! call writeToFile(g,tempVFn,dir//'Jfield/','jxnt','jynt','jznt')
           call writeVecPhysical(g,tempVFn,dir//'Jfield/','jxnt_phys','jynt_phys','jznt_phys')

         ! ----------------------- SIGMA/MU FIELD AT NODES ------------------------
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           allocate(tempn(Nx,Ny,Nz))
           call myCellCenter2Node(tempn,ind%sigma%phi,g)
           call writeToFile(g,tempn,dir//'material/','sigman')
           ! call myCellCenter2Node(tempn,ind%mu%phi,g)
           ! call writeToFile(g,tempn,dir//'material/','mun')
           deallocate(tempn)

         ! -------------------------- TOTAL DOMAIN VELOCITY -----------------------
           call writeToFile(g,ind%U_cct,dir//'Ufield/','uct','vct','wct')
           Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
           allocate(tempcc(Nx,Ny,Nz))
           call div(tempcc,ind%U_cct%x,ind%U_cct%y,ind%U_cct%z,g)
           call writeToFile(g,tempcc,dir//'Ufield/','divUct')
           deallocate(tempcc)
           call delete(tempVFn)
           write(*,*) '     finished'
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine inductionSolver(ind,U,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         select case (solveBMethod)
         case (1); call lowRemPoisson(ind,U,g,ss_MHD)
         case (2); call lowRemPseudoTimeStepUniform(ind,U,g)
         case (3); call lowRemPseudoTimeStep(ind,U,g)
         case (4); call lowRemCTmethod(ind,U,g)
         case (5); call finiteRemCTmethod(ind,U,g)
         case (6); call LowRem_semi_implicit_ADI(ind,U,g,ss_MHD)
         case (7); call lowRemMultigrid(ind,U,g,ss_MHD)
         end select
         if (cleanB) then
           ! call cleanBSolution(ind,g,ss_MHD)
           call cleanBMultigrid(ind,g,ss_MHD)
         endif
         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem
       end subroutine

       subroutine lowRemPoisson(ind,U,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call CCBfieldAdvect(ind%tempVF,U,ind%B0,g)

         call myPoisson(ind%SOR_B,ind%B%x,ind%tempVF%x,ind%Bx_bcs,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myPoisson(ind%SOR_B,ind%B%y,ind%tempVF%y,ind%By_bcs,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myPoisson(ind%SOR_B,ind%B%z,ind%tempVF%z,ind%Bz_bcs,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       subroutine lowRemPseudoTimeStepUniform(ind,U,g)
         ! This routine assumed div(B)=0 (which requires uniform properties), 
         ! which is how it differs from lowRemPseudoTimeStep(). This was an 
         ! important case to test against the Poisson solution, since the 
         ! terms are essentially the same, but a different iterative method 
         ! is applied.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         ! ********************** LOCAL VARIABLES ***********************
         integer :: i
         ! *********************** ALLOCATE DATA ************************
         
         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)
           ! ------------- diffusion term -------------
           call lap(ind%tempVF,ind%B,g)
           call multiply(ind%tempVF,ind%dTime)
           call add(ind%Bstar,ind%tempVF)
           
           ! ------------- source term -------------
           call CCBfieldAdvect(ind%tempVF,U,ind%B0,g)
           call multiply(ind%tempVF,ind%dTime)
           call subtract(ind%Bstar,ind%tempVF)

           ! Add induced field of previous time step (B^n)
           call multiply(ind%B,ind%mu)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
           call applyAllBCs(ind%By_bcs,ind%B%y,g)
           call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
         enddo
       end subroutine

       subroutine lowRemPseudoTimeStep(ind,U,g)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i

         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)

           ! ------------- diffusion term -------------
           call divide(ind%B,ind%mu)
           call CCBfieldDiffuse(ind%tempVF,ind%B,ind%sigmaInv_face,g)
           call multiply(ind%B,ind%mu)

           call multiply(ind%tempVF,ind%dTime)
           call subtract(ind%Bstar,ind%tempVF)
           
           ! ------------- source term -------------
           call CCBfieldAdvect(ind%tempVF,U,ind%B0,g)
           call multiply(ind%tempVF,ind%dTime)
           call subtract(ind%Bstar,ind%tempVF)

           ! Add induced field of previous time step (B^n)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
           call applyAllBCs(ind%By_bcs,ind%B%y,g)
           call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
         enddo
       end subroutine

       subroutine lowRemCTmethod(ind,U,g)
         ! inductionSolverCT solves the induction equation using the
         ! Constrained Transport (CT) Method. The magnetic field is
         ! stored and collocated at the cell center. The magnetic
         ! field is updated using Faraday's Law, where the electric
         ! field is solved for using appropriate fluxes as described
         ! in "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         ! The velocity field is assumed to be cell centered.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         ! ********************** LOCAL VARIABLES ***********************
         integer :: i

         do i=1,ind%NmaxB

           ! Compute current from appropriate fluxes:

           ! J = curl(B_face)_edge
           call myCellCenter2Face(ind%F,ind%B,g)
           call curl(ind%J,ind%F,g)

           ! Compute fluxes of u cross B0
           call cross(ind%tempVF,U,ind%B0)
           call myCellCenter2Edge(ind%E,ind%tempVF,g)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! F = curl(E_edge)_face
           call curl(ind%F,ind%E,g)

           ! tempVF = interp(F)_face->cc
           call myFace2CellCenter(ind%tempVF,ind%F,g)

           ! Add induced field of previous time step (B^n)
           ! ind%B = ind%B - ind%dTime*ind%tempVF
           call multiply(ind%tempVF,ind%dTime)
           call subtract(ind%B,ind%tempVF)

           ! Impose BCs:
           call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
           call applyAllBCs(ind%By_bcs,ind%B%y,g)
           call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
         enddo
       end subroutine

       subroutine finiteRemCTmethod(ind,U,g)
         ! finiteRemCTmethod solves the induction equation using the
         ! Constrained Transport (CT) Method. The magnetic field is
         ! stored and collocated at the cell center. The magnetic
         ! field is updated using Faraday's Law, where the electric
         ! field is solved for using appropriate fluxes as described
         ! in "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         ! The velocity field is assumed to be cell centered.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g

         ! Compute current from appropriate fluxes:
         ! Assumes curl(B0) = 0 (so B is not added to this)
         ! J = curl(B_face)_edge
         call myCellCenter2Face(ind%F,ind%B,g)
         call curl(ind%J,ind%F,g)

         ! Compute fluxes of u cross (B-B0)
         call add(ind%B,ind%B0)
         call cross(ind%tempVF,U,ind%B)
         call subtract(ind%B,ind%B0)

         call myCellCenter2Edge(ind%E,ind%tempVF,g)

         ! E = 1/Rem*j/sig - uxB
         ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
         call multiply(ind%J,ind%sigmaInv_edge)
         call divide(ind%J,ind%Rem)
         call subtract(zero,ind%E)
         call add(ind%E,ind%J)

         ! F = Curl(E_edge)_face
         call curl(ind%F,ind%E,g)

         ! tempVF = interp(F)_face->cc
         call myFace2CellCenter(ind%tempVF,ind%F,g)

         ! Add induced field of previous time step (B^n)
         ! ind%B = ind%B - ind%dTime*ind%tempVF
         call multiply(ind%tempVF,ind%dTime)
         call subtract(ind%B,ind%tempVF)

         ! Add time changing applied magnetic field
         ind%tempVF%x = -ind%dTime*ind%omega*exp(-ind%omega*ind%t)
         ind%tempVF%y = -ind%dTime*ind%omega*exp(-ind%omega*ind%t)
         ind%tempVF%z = real(0.0,cp)
         call subtract(ind%B,ind%tempVF)

         ! Impose BCs:
         call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
         call applyAllBCs(ind%By_bcs,ind%B%y,g)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
       end subroutine

       subroutine LowRem_semi_implicit_ADI(ind,U,g,ss_MHD)
         ! inductionSolverCT solves the induction equation using the
         ! Constrained Transport (CT) Method. The magnetic field is
         ! stored and collocated at the cell center. The magnetic
         ! field is updated using Faraday's Law, where the electric
         ! field is solved for using appropriate fluxes as described
         ! in "Tóth, G. The divergence Constraint in Shock-Capturing 
         ! MHD Codes. J. Comput. Phys. 161, 605–652 (2000)."
         ! The velocity field is assumed to be cell centered.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         ! J = curl(B_face)_edge
         call myCellCenter2Face(ind%F,ind%B,g)
         call curl(ind%J,ind%F,g)

         ! Compute fluxes of u cross B0
         call cross(ind%tempVF,U,ind%B0)
         call myCellCenter2Edge(ind%E,ind%tempVF,g)

         ! E = j/sig - uxB
         ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
         call multiply(ind%J,ind%sigmaInv_edge)
         call subtract(zero,ind%E)
         call add(ind%E,ind%J)

         ! F = Curl(E)
         call curl(ind%F,ind%E,g)

         ! tempVF = interp(F)_face->cc
         call myFace2CellCenter(ind%tempVF,ind%F,g)

         ! Subtract laplacian from B^n
         call lap(ind%Bstar,ind%B,ind%sigmaInv_face,g)

         ! ind%tempVF = ind%tempVF - ind%Bstar
         call subtract(ind%tempVF,ind%Bstar)

         ! Solve with semi-implicit ADI
         call setDt(ind%ADI_B,ind%dTime)
         call setAlpha(ind%ADI_B,real(1.0,cp))
         ! call setAlpha(ind%ADI_B,ind%sigmaInv)

         call init(ind%ss_ADI)
         call setName(ind%ss_ADI,'ADI for B-field     ')

         call apply(ind%ADI_B,ind%B%x,ind%tempVF%x,ind%Bx_bcs,g,&
            ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
         call apply(ind%ADI_B,ind%B%y,ind%tempVF%y,ind%By_bcs,g,&
            ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
         call apply(ind%ADI_B,ind%B%z,ind%tempVF%z,ind%Bz_bcs,g,&
            ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))

         ! Impose BCs:
         call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
         call applyAllBCs(ind%By_bcs,ind%B%y,g)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
       end subroutine

       subroutine lowRemMultigrid(ind,U,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD
         type(multiGrid),dimension(2) :: MG

         call CCBfieldAdvect(ind%tempVF,U,ind%B0,g)

         call myPoisson(MG,ind%B%x,ind%tempVF%x,ind%Bx_bcs,g,ind%ss_ind,&
         ind%err_residual,.false.)

         call myPoisson(MG,ind%B%y,ind%tempVF%y,ind%By_bcs,g,ind%ss_ind,&
         ind%err_residual,.false.)

         call myPoisson(MG,ind%B%z,ind%tempVF%z,ind%Bz_bcs,g,ind%ss_ind,&
         ind%err_residual,.false.)
       end subroutine

       ! ******************* CLEANING **************************

       subroutine cleanBSolution(ind,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         call div(ind%temp%phi,ind%B,g)

         call myPoisson(ind%SOR_cleanB,ind%phi%phi,ind%temp%phi,ind%phi_bcs,g,ind%ss_cleanB,&
          ind%err_CleanB,getExportErrors(ss_MHD))

         call grad(ind%Bstar,ind%phi%phi,g)
         call subtract(ind%B,ind%Bstar)

         ! Impose BCs:
         call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
         call applyAllBCs(ind%By_bcs,ind%B%y,g)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
       end subroutine

       subroutine cleanBMultigrid(ind,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD

         call div(ind%temp%phi,ind%B,g)
         call myPoisson(ind%MG,ind%phi%phi,ind%temp%phi,ind%phi_bcs,g,ind%ss_cleanB,&
          ind%err_CleanB,getExportErrors(ss_MHD))

         call grad(ind%Bstar,ind%phi%phi,g)
         call subtract(ind%B,ind%Bstar)

         ! Impose BCs:
         call applyAllBCs(ind%Bx_bcs,ind%B%x,g)
         call applyAllBCs(ind%By_bcs,ind%B%y,g)
         call applyAllBCs(ind%Bz_bcs,ind%B%z,g)
       end subroutine

       ! ********************* AUX *****************************

       subroutine computeJCrossB(jcrossB,ind,g_mom,g_ind,Re,Ha)
         ! computeJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the applied or total mangetic
         ! field, depending on the solveBMethod.
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(vectorField),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom,g_ind
         real(cp),intent(in) :: Re,Ha
         ! *********************** LOCAL VARIABLES **********************
         integer :: Nx,Ny,Nz,dir

         call assign(ind%Bstar,ind%B0)

         ! For Finite Rem, B_induced should be added
         select case (solveBMethod)
         case (5,6); call add(ind%Bstar,ind%B)
         end select

         call cross(ind%tempVF,ind%J_cc,ind%Bstar)
         call myCellCenter2Face(ind%F,ind%tempVF,g_ind)

         ! Index fluid face source terms:
         ! Excluding wall normal values
         dir = 1
         Nx = g_mom%c(1)%sn; Ny = g_mom%c(2)%sc; Nz = g_mom%c(3)%sc
         jcrossB%x = zero ! expensive!
         jcrossB%x(3:Nx-2,2:Ny-1,2:Nz-1) = &
         ind%F%x( Nin1(1)+1: Nin2(1)-1,Nice1(2):Nice2(2),Nice1(3):Nice2(3))
         jcrossB%x = jcrossB%x*((Ha**real(2.0,cp))/Re)

         dir = 2
         Nx = g_mom%c(1)%sc; Ny = g_mom%c(2)%sn; Nz = g_mom%c(3)%sc
         jcrossB%y = zero ! expensive!
         jcrossB%y(2:Nx-1,3:Ny-2,2:Nz-1) = &
         ind%F%y(Nice1(1):Nice2(1), Nin1(2)+1: Nin2(2)-1,Nice1(3):Nice2(3))
         jcrossB%y = jcrossB%y*((Ha**real(2.0,cp))/Re)

         dir = 3
         Nx = g_mom%c(1)%sc; Ny = g_mom%c(2)%sc; Nz = g_mom%c(3)%sn
         jcrossB%z = zero ! expensive!
         jcrossB%z(2:Nx-1,2:Ny-1,3:Nz-2) = &
         ind%F%z(Nice1(1):Nice2(1),Nice1(2):Nice2(2), Nin1(3)+1: Nin2(3)-1)
         jcrossB%z = jcrossB%z*((Ha**real(2.0,cp))/Re)
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
             call div(ind%divB%phi,ind%F,g)
           case default
             call div(ind%divB%phi,ind%B,g)
           end select
         endif
         call div(ind%divJ%phi,ind%J_cc,g)
       end subroutine

       subroutine computeCurrent(J,B,B0,mu,g)
         implicit none
         type(vectorField),intent(inout) :: B,J
         type(vectorField),intent(in) :: B0
         type(scalarField),intent(in) :: mu
         type(grid),intent(in) :: g
         ! B = (B + B0)/mu
         call add(B,B0)
         call divide(B,mu)

         ! J_cc = curl(B_cc)
         call curl(J,B,g)

         ! B = B*mu - B0
         call multiply(B,mu)
         call subtract(B,B0)
       end subroutine

       subroutine embedVelocity(U_cct,U_fi,U_cci,g_mom)
         implicit none
         type(vectorField),intent(inout) :: U_cct
         type(vectorField),intent(in) :: U_fi
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

         ! call uniformify(U_cct)
       end subroutine

       subroutine uniformify(U)
         implicit none
         type(vectorField),intent(inout) :: U
         integer,dimension(3) :: s
         integer :: i
         s = shape(U%x)
         do i=1,s(1)
           U%x(i,:,:) = U%x(Nici1(1)+2,:,:)
         enddo
       end subroutine

       end module