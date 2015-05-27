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
       use ops_aux_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use ops_physics_mod
       use BCs_mod
       use vectorBCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use mySOR_mod
       use myADI_mod
       use myMG_mod
       use myPoisson_mod
       use probe_base_mod
       use probe_transient_mod

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
       public :: computeMagneticEnergy
       public :: exportTransientFull

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
         type(vectorField) :: B,Bstar,B0,B_face                ! CC data

         type(vectorField) :: J,J_cc,E                         ! Edge data

         type(vectorField) :: U_Ft                             ! Face data
         type(vectorField) :: U_cct                            ! Cell Center data
         type(vectorField) :: U_E,V_E,W_E                      ! Edge data

         type(vectorField) :: temp_E1,temp_E2                  ! Edge data
         type(vectorField) :: temp_F,temp_F2
         type(vectorField) :: temp_F3,temp_F4                  ! Edge data
         type(vectorField) :: temp_CC                          ! CC data

         type(vectorField) :: sigmaInv_edge,sigmaInv_face

         ! --- Scalar fields ---
         type(scalarField) :: sigma,mu          ! CC data
         type(scalarField) :: divB,divJ,phi,temp               ! CC data
         ! BCs:
         type(vectorBCs) :: B_bcs
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
         type(probe) :: KB_energy,KB0_energy
         type(grid) :: g

         integer :: nstep             ! Nth time step
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: omega            ! Intensity of time changing magnetic field
       end type

       interface init;                 module procedure initInduction;                 end interface
       interface delete;               module procedure deleteInduction;               end interface
       interface solve;                module procedure inductionSolver;               end interface
       interface printExportBCs;       module procedure printExportInductionBCs;       end interface
       interface export;               module procedure inductionExport;               end interface
       interface exportRaw;            module procedure inductionExportRaw;            end interface
       interface exportTransient;      module procedure inductionExportTransient;      end interface
       interface exportTransientFull;  module procedure inductionExportTransientFull;  end interface
       interface computeDivergence;    module procedure computeDivergenceInduction;    end interface

       interface setDTime;             module procedure setDTimeInduction;             end interface

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
         call allocateVectorField(ind%temp_CC,ind%B)

         ! Edge Data
         call allocateX(ind%J,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
         call allocateY(ind%J,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
         call allocateZ(ind%J,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)

         call allocateVectorField(ind%E,ind%J)
         call allocateVectorField(ind%U_E,ind%J)
         call allocateVectorField(ind%V_E,ind%J)
         call allocateVectorField(ind%W_E,ind%J)
         call allocateVectorField(ind%temp_E1,ind%J)
         call allocateVectorField(ind%temp_E2,ind%J)
         call allocateVectorField(ind%sigmaInv_edge,ind%J)

         ! Face Data
         call allocateX(ind%U_Ft,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(ind%U_Ft,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(ind%U_Ft,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)

         call allocateVectorField(ind%temp_F,ind%U_Ft)
         call allocateVectorField(ind%sigmaInv_face,ind%U_Ft)
         call allocateVectorField(ind%temp_F2,ind%U_Ft)
         call allocateVectorField(ind%temp_F3,ind%U_Ft)
         call allocateVectorField(ind%temp_F4,ind%U_Ft)
         call allocateVectorField(ind%B_face,ind%U_Ft)

         ! --- Scalar Fields ---
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc

         call allocateField(ind%sigma,Nx,Ny,Nz)
         call allocateField(ind%mu,ind%sigma)
         call allocateField(ind%phi,ind%sigma)
         call allocateField(ind%temp,ind%sigma)

         call allocateField(ind%divB,ind%sigma)
         call allocateField(ind%divJ,ind%sigma)
         write(*,*) '     Fields allocated'


         ! --- Initialize Fields ---
         call initBBCs(ind%B_bcs,ind%phi_bcs,ind%B,g,cleanB)
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,g,dir)
         write(*,*) '     B-field initialized'

         call applyAllBCs(ind%B,ind%B_bcs,g)
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
         call init(ind%KB_energy,dir//'Bfield\','KB',.not.restartB)
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

         call delete(ind%U_cct)
         call delete(ind%U_Ft)
         call delete(ind%U_E)
         call delete(ind%V_E)
         call delete(ind%W_E)

         call delete(ind%J)
         call delete(ind%J_cc)
         call delete(ind%E)

         call delete(ind%temp_CC)
         call delete(ind%temp_E1)
         call delete(ind%temp_E2)
         call delete(ind%temp_F)
         call delete(ind%temp_F2)
         call delete(ind%temp_F3)
         call delete(ind%B_face)
         call delete(ind%temp)

         call delete(ind%sigmaInv_edge)
         call delete(ind%sigmaInv_face)
         
         call delete(ind%sigma)
         call delete(ind%mu)

         call delete(ind%divB)
         call delete(ind%divJ)

         call delete(ind%phi)

         call delete(ind%B_bcs)
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
         if (solveInduction) call printVectorBCs(ind%B_bcs,'Bx','By','Bz')
         if (cleanB)         call printAllBoundaries(ind%phi_bcs,'phi')
         if (solveInduction) call writeVectorBCs(ind%B_bcs,dir//'parameters/','Bx','By','Bz')
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
           if (cleanB) call writeToFile(g,ind%phi%phi,dir//'Bfield/','phi')
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
           call writeVecPhysical(g,tempVFn,dir//'Bfield/','Bxnt_phys','Bynt_phys','Bznt_phys')

           call myCellCenter2Node(tempVFn,ind%B0,g)
           call writeVecPhysical(g,tempVFn,dir//'Bfield/','B0xnt_phys','B0ynt_phys','B0znt_phys')

           call myCellCenter2Node(tempVFn,ind%J_cc,g)
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

       subroutine inductionExportTransientFull(ind,g,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         type(vectorField) :: tempVFn

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
           call allocateVectorField(tempVFn,Nx,Ny,Nz)

           call myCellCenter2Node(tempVFn,ind%B,g)
           ! call writeVecPhysicalPlane(g,tempVFn,dir//'Bfield/transient/',&
           ! 'Bxnt_phys',&
           ! 'Bynt_phys',&
           ! 'Bznt_phys','_'//int2str(ind%nstep),1,2,ind%nstep)

           call writeScalarPhysicalPlane(g,tempVFn%x,dir//'Bfield/transient/',&
           'Bxnt_phys','_'//int2str(ind%nstep),1,2,ind%nstep)

           call myCellCenter2Node(tempVFn,ind%J_cc,g)
           call writeVecPhysicalPlane(g,tempVFn,dir//'Jfield/transient/',&
            'jxnt_phys',&
            'jynt_phys',&
            'jznt_phys','_'//int2str(ind%nstep),1,2,ind%nstep)

           call delete(tempVFn)
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine inductionSolver(ind,U,g_mom,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g,g_mom
         type(solverSettings),intent(inout) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         call embedVelocity(ind,U,g_mom)

         select case (solveBMethod)
         case (1); call lowRemPoisson(ind,ind%U_cct,g,ss_MHD)
         case (2); call lowRemPseudoTimeStepUniform(ind,ind%U_cct,g)
         case (3); call lowRemPseudoTimeStep(ind,ind%U_cct,g)
         case (4); call lowRemCTmethod(ind,g)
         case (5); call finiteRemCTmethod(ind,g)
         case (6); call LowRem_semi_implicit_ADI(ind,ind%U_cct,g,ss_MHD)
         case (7); call lowRemMultigrid(ind,ind%U_cct,g,ss_MHD)
         case (8); call lowRem_JacksExperiment(ind,ind%U_cct,g)
         end select
         if (cleanB) then
           call cleanBSolution(ind,g,ss_MHD)
           ! call cleanBMultigrid(ind,g,ss_MHD)
         endif
         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem
       end subroutine

       subroutine lowRemPoissonOld(ind,U,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD
         call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

         call myPoisson(ind%SOR_B,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myPoisson(ind%SOR_B,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myPoisson(ind%SOR_B,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       subroutine lowRemPoisson(ind,U,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call myCellCenter2Face(ind%temp_F2,ind%B0,g)

         call faceCurlCross_F(ind%temp_F,ind%U_Ft,ind%temp_F2,ind%temp_E1,ind%temp_E2,ind%temp,g)

         call myPoisson(ind%SOR_B,ind%B_face%x,ind%temp_F%x,ind%B_bcs%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myPoisson(ind%SOR_B,ind%B_face%y,ind%temp_F%y,ind%B_bcs%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myPoisson(ind%SOR_B,ind%B_face%z,ind%temp_F%z,ind%B_bcs%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call myFace2CellCenter(ind%B,ind%B_face,g)
       end subroutine

       subroutine lowRemPseudoTimeStepUniform(ind,U,g)
         ! This routine assumed div(B)=0 (which requires uniform properties), 
         ! which is how it differs from lowRemPseudoTimeStep(). This was an 
         ! important case to test against the Poisson solution, since the 
         ! terms are essentially the same, but a different iterative method 
         ! is applied.
         implicit none
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i
         
         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)
           ! ------------- diffusion term -------------
           call lap(ind%temp_CC,ind%B,g)
           call multiply(ind%temp_CC,ind%dTime)
           call add(ind%Bstar,ind%temp_CC)
           
           ! ------------- source term -------------
           call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)

           ! Add induced field of previous time step (B^n)
           call multiply(ind%B,ind%mu)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyAllBCs(ind%B,ind%B_bcs,g)
         enddo
       end subroutine

       subroutine lowRemPseudoTimeStep(ind,U,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i

         do i=1,ind%NmaxB
           call assign(ind%Bstar,zero)

           ! ------------- diffusion term -------------
           call divide(ind%B,ind%mu)
           call CCBfieldDiffuse(ind%temp_CC,ind%B,ind%sigmaInv_face,g)
           call multiply(ind%B,ind%mu)

           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)
           
           ! ------------- source term -------------
           call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%Bstar,ind%temp_CC)

           ! Add induced field of previous time step (B^n)
           call add(ind%B,ind%Bstar)

           ! Impose BCs:
           call applyAllBCs(ind%B,ind%B_bcs,g)
         enddo
       end subroutine

       subroutine lowRemCTmethodOld(ind,U,g)
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
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i

         do i=1,ind%NmaxB

           ! Compute current from appropriate fluxes:

           ! J = curl(B_face)_edge
           call myCellCenter2Face(ind%temp_F,ind%B,g)
           call curl(ind%J,ind%temp_F,g)

           ! Compute fluxes of u cross B0
           call cross(ind%temp_CC,U,ind%B0)
           call myCellCenter2Edge(ind%E,ind%temp_CC,g)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! F = curl(E_edge)_face
           call curl(ind%temp_F,ind%E,g)

           ! tempVF = interp(F)_face->cc
           call myFace2CellCenter(ind%temp_CC,ind%temp_F,g)

           ! Add induced field of previous time step (B^n)
           ! ind%B = ind%B - ind%dTime*ind%temp_CC
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%B,ind%temp_CC)

           ! Impose BCs:
           call applyAllBCs(ind%B,ind%B_bcs,g)
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
           call myCellCenter2Face(ind%temp_F,ind%B,g)
           call curl(ind%J,ind%temp_F,g)

           ! Compute fluxes of u cross B0
           call edgeCrossCC_E(ind%E,ind%U_E,ind%V_E,ind%W_E,ind%B0,g)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! F = curl(E_edge)_face
           call curl(ind%temp_F,ind%E,g)

           ! tempVF = interp(F)_face->cc
           call myFace2CellCenter(ind%temp_CC,ind%temp_F,g)

           ! Add induced field of previous time step (B^n)
           ! ind%B = ind%B - ind%dTime*ind%temp_CC
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%B,ind%temp_CC)

           ! Impose BCs:
           call applyAllBCs(ind%B,ind%B_bcs,g)
         enddo
       end subroutine

       subroutine lowRem_JacksExperiment(ind,U,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         integer :: i,t,b
         b = 10
         t = 21

         do i=1,ind%NmaxB

           ! Compute current from appropriate fluxes:

           ! J = curl(B_face)_edge
           call myCellCenter2Face(ind%temp_F,ind%B,g)
           call curl(ind%J,ind%temp_F,g)

           ind%J%y(:,1:2,b) = real(1.0,cp)
           ind%J%y(:,1:2,t) = real(-1.0,cp)

           ind%J%y(:,ind%J%sy(2)-1:ind%J%sy(2),b) = real(1.0,cp)
           ind%J%y(:,ind%J%sy(2)-1:ind%J%sy(2),t) = real(-1.0,cp)

           ! Compute fluxes of u cross B0
           call cross(ind%temp_CC,U,ind%B0)
           call myCellCenter2Edge(ind%E,ind%temp_CC,g)

           ! E = j/sig - uxB
           ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
           call multiply(ind%J,ind%sigmaInv_edge)
           call subtract(zero,ind%E)
           call add(ind%E,ind%J)

           ! F = curl(E_edge)_face
           call curl(ind%temp_F,ind%E,g)

           ! tempVF = interp(F)_face->cc
           call myFace2CellCenter(ind%temp_CC,ind%temp_F,g)

           ! Add induced field of previous time step (B^n)
           ! ind%B = ind%B - ind%dTime*ind%temp_CC
           call multiply(ind%temp_CC,ind%dTime)
           call subtract(ind%B,ind%temp_CC)

           ! Impose BCs:
           call applyAllBCs(ind%B,ind%B_bcs,g)
         enddo
       end subroutine

       subroutine finiteRemCTmethod(ind,g)
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
         type(grid),intent(in) :: g

         ! Compute current from appropriate fluxes:
         ! Assumes curl(B0) = 0 (so B is not added to this)
         ! J = curl(B_face)_edge
         call myCellCenter2Face(ind%temp_F,ind%B,g)
         call curl(ind%J,ind%temp_F,g)

         ! Compute fluxes of u cross (B-B0)
         call add(ind%B,ind%B0)
         ! call cross(ind%Bstar,ind%U_cct,ind%B)
         ! call myCellCenter2Edge(ind%E,ind%Bstar,g)
         call edgeCrossCC_E(ind%E,ind%U_E,ind%V_E,ind%W_E,ind%B,g)
         call subtract(ind%B,ind%B0)

         ! E = 1/Rem*j/sig - uxB
         ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
         call multiply(ind%J,ind%sigmaInv_edge)
         call divide(ind%J,ind%Rem)
         call subtract(zero,ind%E)
         call add(ind%E,ind%J)

         ! F = Curl(E_edge)_face
         call curl(ind%temp_F,ind%E,g)

         ! tempVF = interp(F)_face->cc
         call myFace2CellCenter(ind%temp_CC,ind%temp_F,g)

         ! Add induced field of previous time step (B^n)
         ! ind%B = ind%B - ind%dTime*ind%temp_CC
         call multiply(ind%temp_CC,ind%dTime)
         call subtract(ind%B,ind%temp_CC)

         ! Add time changing applied magnetic field
         ! ind%temp_CC%x = -ind%dTime*ind%omega*exp(dble(-ind%omega*ind%t))
         ! ind%temp_CC%y = -ind%dTime*ind%omega*exp(dble(-ind%omega*ind%t))
         ! ind%temp_CC%z = real(0.0,cp)

         ind%temp_CC%x = -ind%dTime*ind%omega*exp(dble(-ind%omega*ind%t))
         ind%temp_CC%y = real(0.0,cp)
         ind%temp_CC%z = real(0.0,cp)
         call subtract(ind%B,ind%temp_CC)

         ! Impose BCs:
         call applyAllBCs(ind%B,ind%B_bcs,g)
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
         call myCellCenter2Face(ind%temp_F,ind%B,g)
         call curl(ind%J,ind%temp_F,g)

         ! Compute fluxes of u cross B0
         call cross(ind%temp_CC,U,ind%B0)
         call myCellCenter2Edge(ind%E,ind%temp_CC,g)

         ! E = j/sig - uxB
         ! ind%E = ind%J*ind%sigmaInv_edge - ind%E
         call multiply(ind%J,ind%sigmaInv_edge)
         call subtract(zero,ind%E)
         call add(ind%E,ind%J)

         ! F = Curl(E)
         call curl(ind%temp_F,ind%E,g)

         ! tempVF = interp(F)_face->cc
         call myFace2CellCenter(ind%temp_CC,ind%temp_F,g)

         ! Subtract laplacian from B^n
         call lap(ind%Bstar,ind%B,ind%sigmaInv_face,g)

         ! ind%temp_CC = ind%temp_CC - ind%Bstar
         call subtract(ind%temp_CC,ind%Bstar)

         ! Solve with semi-implicit ADI
         call setDt(ind%ADI_B,ind%dTime)
         call setAlpha(ind%ADI_B,real(1.0,cp))

         call init(ind%ss_ADI)
         call setName(ind%ss_ADI,'ADI for B-field     ')

         call apply(ind%ADI_B,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,&
            ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
         call apply(ind%ADI_B,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,&
            ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))
         call apply(ind%ADI_B,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,&
            ind%ss_ADI,ind%err_ADI,getExportErrors(ss_MHD))

         ! Impose BCs:
         call applyAllBCs(ind%B,ind%B_bcs,g)
       end subroutine

       subroutine lowRemMultigrid(ind,U,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD
         type(multiGrid),dimension(2) :: MG

         call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

         call myPoisson(MG,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,ind%ss_ind,&
         ind%err_residual,.false.)

         call myPoisson(MG,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,ind%ss_ind,&
         ind%err_residual,.false.)

         call myPoisson(MG,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,ind%ss_ind,&
         ind%err_residual,.false.)
       end subroutine

       ! ******************* CLEANING **************************

       subroutine cleanBSolutionOld(ind,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         call div(ind%temp%phi,ind%B_face,g)

         call myPoisson(ind%SOR_cleanB,ind%phi%phi,ind%temp%phi,ind%phi_bcs,g,ind%ss_cleanB,&
          ind%err_cleanB,getExportErrors(ss_MHD))

         call grad(ind%Bstar,ind%phi%phi,g)
         call subtract(ind%B,ind%Bstar)

         ! Impose BCs:
         call applyAllBCs(ind%B,ind%B_bcs,g)
       end subroutine

       subroutine cleanBSolution(ind,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         call div(ind%temp%phi,ind%B_face,g)

         call myPoisson(ind%SOR_cleanB,ind%phi%phi,ind%temp%phi,ind%phi_bcs,g,ind%ss_cleanB,&
          ind%err_cleanB,getExportErrors(ss_MHD))

         call grad(ind%temp_F2,ind%phi%phi,g)
         call subtract(ind%B_face,ind%temp_F2)

         call myFace2CellCenter(ind%B,ind%B_face,g)
         ! Impose BCs:
         ! call applyAllBCs(ind%B,ind%B_bcs,g)
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
         call applyAllBCs(ind%B,ind%B_bcs,g)
       end subroutine

       ! ********************* COMPUTE *****************************

       subroutine computeJCrossB(jcrossB,ind,g_mom,g_ind,Re,Ha)
         ! computeJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the applied or total mangetic
         ! field, depending on the solveBMethod.
         implicit none
         type(vectorField),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom,g_ind
         real(cp),intent(in) :: Re,Ha


         select case (solveBMethod)
         case (5,6) ! Finite Rem

         call assign(ind%Bstar,ind%B0)
         call add(ind%Bstar,ind%B)
         call curl(ind%J_cc,ind%Bstar,g_ind)

         case default ! Low Rem

         call assign(ind%Bstar,ind%B)
         call curl(ind%J_cc,ind%Bstar,g_ind)
         call assign(ind%Bstar,ind%B0)

         end select

         call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
         call myCellCenter2Face(ind%temp_F,ind%temp_CC,g_ind)

         call extractFace(jcrossB,ind%temp_F,g_mom,Nin1,Nin2,Nici1,Nici2,Nice1,Nice2,2)

         call multiply(jcrossB,Ha**real(2.0,cp)/Re)
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
             call div(ind%divB%phi,ind%temp_F,g)
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
         ! call divide(B,mu)

         ! J_cc = curl(B_cc)
         call curl(J,B,g)

         ! B = B*mu - B0
         ! call multiply(B,mu)
         call subtract(B,B0)
       end subroutine

       ! ********************* AUX *****************************

       subroutine computeMagneticEnergy(ind,B,B0,Nici1,Nici2,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: B,B0
         integer,dimension(3),intent(in) :: Nici1,Nici2
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
          if (computeKB.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           ! call totalEnergy(K_energy,ind%B,ind%g) ! Sergey uses interior...
           call totalEnergy(K_energy,&
             B%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             B%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             B%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             g)
           call set(ind%KB_energy,ind%nstep,K_energy)
           call apply(ind%KB_energy)
          endif
          if (computeKB0.and.getExportTransient(ss_MHD).or.ind%nstep.eq.0) then
           ! call totalEnergy(K_energy,B0,ind%g) ! Sergey uses interior...
           call totalEnergy(K_energy,&
             B0%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             B0%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             B0%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             g)
           call set(ind%KB0_energy,ind%nstep,K_energy)
           call apply(ind%KB0_energy)
          endif
       end subroutine

       subroutine extractFace(face_i,face_t,g,Nn1,Nn2,Ni1,Ni2,Ne1,Ne2,extractType)
         implicit none
         type(vectorField),intent(inout) :: face_i
         type(vectorField),intent(in) :: face_t
         type(grid),intent(in) :: g
         integer,dimension(3),intent(in) :: Nn1,Nn2,Ni1,Ni2,Ne1,Ne2
         integer,intent(in) :: extractType
         integer :: Nx,Ny,Nz

         select case (extractType)
         case (1) ! For duct flows (when U is not zero at boundary)
           ! Including ghost nodes
           ! Including ghost cells
           ! Including boundary values
           face_i%x = face_t%x(Nn1(1)-1:Nn2(1)+1,Ni1(2)  :Ni2(2)  ,Ni1(3)  :Ni2(3)  )
           face_i%y = face_t%y(Ni1(1)  :Ni2(1)  ,Nn1(2)-1:Nn2(2)+1,Ni1(3)  :Ni2(3)  )
           face_i%z = face_t%z(Ni1(1)  :Ni2(1)  ,Ni1(2)  :Ni2(2)  ,Nn1(3)-1:Nn2(3)+1)
         case (2) ! ** Probably the most physically correct one **
           ! For duct flows (when U is not zero at boundary)
           ! Excluding ghost nodes
           ! Excluding ghost cells
           ! Including boundary values
           call zeroGhostPoints(face_i)
           face_i%x(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1) = &
           face_t%x( Nn1(1)-1: Nn2(1)+1,Ne1(2):Ne2(2),Ne1(3):Ne2(3))

           face_i%y(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1) = &
           face_t%y(Ne1(1):Ne2(1), Nn1(2)-1: Nn2(2)+1,Ne1(3):Ne2(3))

           face_i%z(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:) = &
           face_t%z(Ne1(1):Ne2(1),Ne1(2):Ne2(2), Nn1(3)-1: Nn2(3)+1)
         case (3) ! For Lid Driven Cavity, e.g.
           ! Excluding ghost nodes
           ! Excluding ghost cells
           ! Excluding boundary values
           call zeroGhostPoints(face_i)
           face_i%x(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1) = &
           face_t%x( Nn1(1): Nn2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3))

           face_i%y(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1) = &
           face_t%y(Ne1(1):Ne2(1), Nn1(2): Nn2(2),Ne1(3):Ne2(3))

           face_i%z(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1) = &
           face_t%z(Ne1(1):Ne2(1),Ne1(2):Ne2(2), Nn1(3): Nn2(3))
         case default
         stop 'Error: extractType must = 1,2 in extractFace'
         end select
       end subroutine

       subroutine embedVelocity(ind,U_fi,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(vectorField),intent(in) :: U_fi ! Raw momentum velocity
         type(grid),intent(in) :: g ! Momentum grid
         type(vectorField) :: temp
         integer,dimension(3) :: Ni
         integer :: embedType,dir
         logical,dimension(4) :: usedVelocity

         usedVelocity = (/.true.,.true.,.false.,.false./)

         if (usedVelocity(1)) then ! Edge - 2 interpolations
           call allocateX(temp,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
           call allocateY(temp,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
           call allocateZ(temp,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
           call myFace2Edge(temp%x,U_fi%x,g,1,1)
           call myFace2Edge(temp%y,U_fi%x,g,1,2)
           call myFace2Edge(temp%z,U_fi%x,g,1,3)
           call embedEdge(ind%U_E,temp,Nin1,Nin2,Nice1,Nice2,Nici1,Nici2,g)
           call myFace2Edge(temp%x,U_fi%y,g,2,1)
           call myFace2Edge(temp%y,U_fi%y,g,2,2)
           call myFace2Edge(temp%z,U_fi%y,g,2,3)
           call embedEdge(ind%V_E,temp,Nin1,Nin2,Nice1,Nice2,Nici1,Nici2,g)
           call myFace2Edge(temp%x,U_fi%z,g,3,1)
           call myFace2Edge(temp%y,U_fi%z,g,3,2)
           call myFace2Edge(temp%z,U_fi%z,g,3,3)
           call embedEdge(ind%W_E,temp,Nin1,Nin2,Nice1,Nice2,Nici1,Nici2,g)
           call delete(temp)
         endif

         if (usedVelocity(2)) then ! CC - 1 interpolation
           call allocateX(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
           call allocateY(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
           call allocateZ(temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
           call myFace2CellCenter(temp%x,U_fi%x,g,1)
           call myFace2CellCenter(temp%y,U_fi%y,g,2)
           call myFace2CellCenter(temp%z,U_fi%z,g,3)
           call embedCC(ind%U_cct,temp,Nice1,Nice2,Nici1,Nici2,g)
           call delete(temp)
         endif

         if (usedVelocity(3)) then ! Face - no interpolations
           call embedFace(ind%U_Ft,U_Fi,Nin1,Nin2,Nice1,Nice2,g)
         endif
         ! if (usedVelocity(4)) then ! Node - 3 interpolations (not needed for any solvers)
           ! call allocateX(temp,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
           ! call allocateY(temp,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
           ! call allocateZ(temp,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
           ! call myFace2Node(temp%x,U_fi%x,g,1)
           ! call myFace2Node(temp%y,U_fi%x,g,1)
           ! call myFace2Node(temp%z,U_fi%x,g,1)
           ! call embedNode(ind%U_N,temp,Nin1,Nin2,g)
           ! call myFace2Node(temp%x,U_fi%y,g,2)
           ! call myFace2Node(temp%y,U_fi%y,g,2)
           ! call myFace2Node(temp%z,U_fi%y,g,2)
           ! call embedNode(ind%V_N,temp,Nin1,Nin2,g)
           ! call myFace2Node(temp%x,U_fi%z,g,3)
           ! call myFace2Node(temp%y,U_fi%z,g,3)
           ! call myFace2Node(temp%z,U_fi%z,g,3)
           ! call embedNode(ind%W_N,temp,Nin1,Nin2,g)
           ! call delete(temp)
         ! endif
       end subroutine

       subroutine embedFace(U_Ft,U_Fi,Nn1,Nn2,Ne1,Ne2,g)
         implicit none
         type(vectorField),intent(inout) :: U_Ft
         type(vectorField),intent(in) :: U_Fi
         integer,dimension(3),intent(in) :: Nn1,Nn2,Ne1,Ne2
         type(grid),intent(in) :: g
         U_Ft%x(Nn1(1):Nn2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_Fi%x(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         U_Ft%y(Ne1(1):Ne2(1),Nn1(2):Nn2(2),Ne1(3):Ne2(3)) = &
         U_Fi%y(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         U_Ft%z(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Nn1(3):Nn2(3)) = &
         U_Fi%z(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
       end subroutine

       subroutine embedEdge(U_Et,U_Ei,Nn1,Nn2,Ne1,Ne2,Ni1,Ni2,g)
         implicit none
         type(vectorField),intent(inout) :: U_Et
         type(vectorField),intent(in) :: U_Ei
         integer,dimension(3),intent(in) :: Nn1,Nn2,Ne1,Ne2,Ni1,Ni2
         type(grid),intent(in) :: g
         call embedEdgeDir(U_Et,U_Ei,Nn1,Nn2,Ne1,Ne2,Ni1,Ni2,g,1)
       end subroutine

       subroutine embedEdgeExclude(U_Et,U_Ei,Nn1,Nn2,Ne1,Ne2,g)
         implicit none
         type(vectorField),intent(inout) :: U_Et
         type(vectorField),intent(in) :: U_Ei
         integer,dimension(3),intent(in) :: Nn1,Nn2,Ne1,Ne2
         type(grid),intent(in) :: g
         U_Et%x(Ne1(1):Ne2(1),Nn1(2):Nn2(2),Nn1(3):Nn2(3)) = &
         U_Ei%x(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         U_Et%y(Nn1(1):Nn2(1),Ne1(2):Ne2(2),Nn1(3):Nn2(3)) = &
         U_Ei%y(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         U_Et%z(Nn1(1):Nn2(1),Nn1(2):Nn2(2),Ne1(3):Ne2(3)) = &
         U_Ei%z(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
       end subroutine

       subroutine embedEdgeInclude(U_Et,U_Ei,Nn1,Nn2,Ni1,Ni2,g)
         implicit none
         type(vectorField),intent(inout) :: U_Et
         type(vectorField),intent(in) :: U_Ei
         integer,dimension(3),intent(in) :: Nn1,Nn2,Ni1,Ni2
         type(grid),intent(in) :: g
         U_Et%x(Ni1(1):Ni2(1),Nn1(2):Nn2(2),Nn1(3):Nn2(3)) = U_Ei%x(:,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         U_Et%y(Nn1(1):Nn2(1),Ni1(2):Ni2(2),Nn1(3):Nn2(3)) = U_Ei%y(2:g%c(1)%sn-1,:,2:g%c(3)%sn-1)
         U_Et%z(Nn1(1):Nn2(1),Nn1(2):Nn2(2),Ni1(3):Ni2(3)) = U_Ei%z(2:g%c(1)%sn-1,2:g%c(2)%sn-1,:)
       end subroutine

       subroutine embedEdgeDir(U_Et,U_Ei,Nn1,Nn2,Ne1,Ne2,Ni1,Ni2,g,dir)
         implicit none
         type(vectorField),intent(inout) :: U_Et
         type(vectorField),intent(in) :: U_Ei
         integer,dimension(3),intent(in) :: Nn1,Nn2,Ne1,Ne2,Ni1,Ni2
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         select case(dir)
         case (1);U_Et%x(Ni1(1):Ni2(1),Nn1(2):Nn2(2),Nn1(3):Nn2(3)) = &
                  U_Ei%x(:,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
                  U_Et%y(Nn1(1)-1:Nn2(1)+1,Ne1(2):Ne2(2),Nn1(3):Nn2(3)) = &
                  U_Ei%y(:,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
                  U_Et%z(Nn1(1)-1:Nn2(1)+1,Nn1(2):Nn2(2),Ne1(3):Ne2(3)) = &
                  U_Ei%z(:,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         case (2);U_Et%x(Ne1(1):Ne2(1),Nn1(2)-1:Nn2(2)+1,Nn1(3):Nn2(3)) = &
                  U_Ei%x(2:g%c(1)%sc-1,:,2:g%c(3)%sn-1)
                  U_Et%y(Nn1(1):Nn2(1),Ni1(2):Ni2(2),Nn1(3):Nn2(3)) = &
                  U_Ei%y(2:g%c(1)%sn-1,:,2:g%c(3)%sn-1)
                  U_Et%z(Nn1(1):Nn2(1),Nn1(2)-1:Nn2(2)+1,Ne1(3):Ne2(3)) = &
                  U_Ei%z(2:g%c(1)%sn-1,:,2:g%c(3)%sc-1)
         case (3);U_Et%x(Ne1(1):Ne2(1),Nn1(2):Nn2(2),Nn1(3)-1:Nn2(3)+1) = &
                  U_Ei%x(2:g%c(1)%sc-1,2:g%c(2)%sn-1,:)
                  U_Et%y(Nn1(1):Nn2(1),Ne1(2):Ne2(2),Nn1(3)-1:Nn2(3)+1) = &
                  U_Ei%y(2:g%c(1)%sn-1,2:g%c(2)%sc-1,:)
                  U_Et%z(Nn1(1):Nn2(1),Nn1(2):Nn2(2),Ni1(3):Ni2(3)) = &
                  U_Ei%z(2:g%c(1)%sn-1,2:g%c(2)%sn-1,:)
         case default
         stop 'Error: dir must = 1,2,3 in embedEdgeDir in inductionSolver.f90'
         end select
       end subroutine

       subroutine embedCC(U_cct,U_cci,Ne1,Ne2,Ni1,Ni2,g)
         implicit none
         type(vectorField),intent(inout) :: U_cct
         type(vectorField),intent(in) :: U_cci
         integer,dimension(3),intent(in) :: Ne1,Ne2,Ni1,Ni2
         type(grid),intent(in) :: g
         integer,dimension(3) :: Ni
         integer :: dir,embedType
         Ni = (/g%c(1)%sc-1,g%c(2)%sc-1,g%c(3)%sc-1/) ! minus fictitious cells
         embedType = 1
         dir = 1
         call embedCCDir(U_cct%x,U_cci%x,g,Ni,Nici1,Nici2,Nice1,Nice2,embedType,dir)
         call embedCCDir(U_cct%y,U_cci%y,g,Ni,Nici1,Nici2,Nice1,Nice2,embedType,dir)
         call embedCCDir(U_cct%z,U_cci%z,g,Ni,Nici1,Nici2,Nice1,Nice2,embedType,dir)
         U_cct%x(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_cci%x(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         U_cct%y(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_cci%y(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         U_cct%z(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_cci%z(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
       end subroutine

       subroutine embedCCExclude(U_cct,U_cci,Ne1,Ne2,g)
         implicit none
         type(vectorField),intent(inout) :: U_cct
         type(vectorField),intent(in) :: U_cci
         integer,dimension(3),intent(in) :: Ne1,Ne2
         type(grid),intent(in) :: g
         integer,dimension(3) :: Ni
         Ni = (/g%c(1)%sc-1,g%c(2)%sc-1,g%c(3)%sc-1/) ! minus fictitious cells
         U_cct%x(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_cci%x(2:Ni(1),2:Ni(2),2:Ni(3))
         U_cct%y(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_cci%y(2:Ni(1),2:Ni(2),2:Ni(3))
         U_cct%z(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = &
         U_cci%z(2:Ni(1),2:Ni(2),2:Ni(3))
       end subroutine

       subroutine embedCCInclude(U_cct,U_cci,Ni1,Ni2,g)
         implicit none
         type(vectorField),intent(inout) :: U_cct
         type(vectorField),intent(in) :: U_cci
         integer,dimension(3),intent(in) :: Ni1,Ni2
         type(grid),intent(in) :: g
         U_cct%x(Ni1(1):Ni2(1),Ni1(2):Ni2(2),Ni1(3):Ni2(3)) = U_cci%x
         U_cct%y(Ni1(1):Ni2(1),Ni1(2):Ni2(2),Ni1(3):Ni2(3)) = U_cci%y
         U_cct%z(Ni1(1):Ni2(1),Ni1(2):Ni2(2),Ni1(3):Ni2(3)) = U_cci%z
       end subroutine

       subroutine embedCCDir(U_cct,U_cci,g,Ni,Ni1,Ni2,Ne1,Ne2,embedType,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: U_cct
         real(cp),dimension(:,:,:),intent(in) :: U_cci
         type(grid),intent(in) :: g
         integer,dimension(3),intent(in) :: Ni,Ni1,Ni2,Ne1,Ne2
         integer,intent(in) :: embedType,dir

         select case(embedType) ! Depends on method used to solve B and BCs for U

         case (1) ! Good for LDC or when U is zero outside domain
           ! (exclude all fictitious cells)
           U_cct(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = U_cci(2:Ni(1),2:Ni(2),2:Ni(3))

         case (2) ! Good for duct flows
           ! Include along dir (Duct flow - exclude except inlet / outlet)

           select case (dir)
           case (1); U_cct(Ni1(1):Ni2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = U_cci(:,2:Ni(2),2:Ni(3))
           case (2); U_cct(Ne1(1):Ne2(1),Ni1(2):Ni2(2),Ne1(3):Ne2(3)) = U_cci(2:Ni(1),:,2:Ni(3))
           case (3); U_cct(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ni1(3):Ni2(3)) = U_cci(2:Ni(1),2:Ni(2),:)
           case default
           stop 'Error: dir must = 1,2,3'
           end select

         case (3) ! Good for when U is NOT zero outside domain (neumann BCs for U)
           ! (including all fictitious cells)
           U_cct(Ni1(1):Ni2(1),Ni1(2):Ni2(2),Ni1(3):Ni2(3)) = U_cci

         case default
         stop 'Error: embedType must = 1,2,3'
         end select
       end subroutine

       subroutine embedN(U_nt,U_ni,Nn1,Nn2,g)
         implicit none
         type(vectorField),intent(inout) :: U_nt
         type(vectorField),intent(in) :: U_ni
         integer,dimension(3),intent(in) :: Nn1,Nn2
         type(grid),intent(in) :: g
         U_nt%x(Nn1(1):Nn2(1),Nn1(2):Nn2(2),Nn1(3):Nn2(3)) = &
         U_ni%x(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         U_nt%y(Nn1(1):Nn2(1),Nn1(2):Nn2(2),Nn1(3):Nn2(3)) = &
         U_ni%y(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         U_nt%z(Nn1(1):Nn2(1),Nn1(2):Nn2(2),Nn1(3):Nn2(3)) = &
         U_ni%z(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
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

       subroutine uniformifyX(U)
         implicit none
         type(vectorField),intent(inout) :: U
         integer,dimension(3) :: s
         integer :: i
         s = shape(U%x)
         do i=1,s(1)
           U%x(i,:,:) = U%x((Nici1(1)+Nici2(1))/2,:,:)
         enddo
         s = shape(U%y)
         do i=1,s(1)
           U%y(i,:,:) = U%y((Nici1(1)+Nici2(1))/2,:,:)
         enddo
         s = shape(U%z)
         do i=1,s(1)
           U%z(i,:,:) = U%z((Nici1(1)+Nici2(1))/2,:,:)
         enddo
       end subroutine

       ! ********************* OLD *****************************

       subroutine embedVelocityOld(U_cct,U_fi,U_cci,g_mom)
         ! Whether or not the fictitious cells are included must be determined
         ! based on the BCs for the velocity. If no slip / no flow through is
         ! used, then only interior cell velocities should be used. Otherwise,
         ! fictitious cells will affect the solution of B, which will in-tern
         ! affect the solution of U, etc.
         implicit none
         type(vectorField),intent(inout) :: U_cct
         type(vectorField),intent(in) :: U_fi
         type(scalarField),intent(inout) :: U_cci
         type(grid),intent(in) :: g_mom
         integer,dimension(3) :: Ni
         integer :: dir,embedType

         Ni = (/g_mom%c(1)%sc-1,g_mom%c(2)%sc-1,g_mom%c(3)%sc-1/) ! minus fictitious cells
         embedType = 1
         dir = 1
         call myFace2CellCenter(U_cci%phi,U_fi%x,g_mom,1)
         call embedCCOld(U_cct%x,U_cci%phi,g_mom,Ni,Nici1,Nici2,Nice1,Nice2,embedType,dir)
         call myFace2CellCenter(U_cci%phi,U_fi%y,g_mom,2)
         call embedCCOld(U_cct%y,U_cci%phi,g_mom,Ni,Nici1,Nici2,Nice1,Nice2,embedType,dir)
         call myFace2CellCenter(U_cci%phi,U_fi%z,g_mom,3)
         call embedCCOld(U_cct%z,U_cci%phi,g_mom,Ni,Nici1,Nici2,Nice1,Nice2,embedType,dir)
       end subroutine

       subroutine embedCCOld(U_cct,U_cci,g,Ni,Ni1,Ni2,Ne1,Ne2,embedType,dir)
         ! Whether or not the fictitious cells are included must be determined
         ! based on the BCs for the velocity. If no slip / no flow through is
         ! used, then only interior cell velocities should be used. Otherwise,
         ! fictitious cells will affect the solution of B, which will in-tern
         ! affect the solution of U, etc.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: U_cct
         real(cp),dimension(:,:,:),intent(in) :: U_cci
         type(grid),intent(in) :: g
         integer,dimension(3),intent(in) :: Ni,Ni1,Ni2,Ne1,Ne2
         integer,intent(in) :: embedType,dir

         select case(embedType) ! Depends on method used to solve B and BCs for U

         case (1) ! Good for LDC or when U is zero outside domain
           ! (exclude all fictitious cells)
           U_cct(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = U_cci(2:Ni(1),2:Ni(2),2:Ni(3))

         case (2) ! Good for duct flows
           ! Include along dir (Duct flow - exclude except inlet / outlet)

           select case (dir)
           case (1); U_cct(Ni1(1):Ni2(1),Ne1(2):Ne2(2),Ne1(3):Ne2(3)) = U_cci(:,2:Ni(2),2:Ni(3))
           case (2); U_cct(Ne1(1):Ne2(1),Ni1(2):Ni2(2),Ne1(3):Ne2(3)) = U_cci(2:Ni(1),:,2:Ni(3))
           case (3); U_cct(Ne1(1):Ne2(1),Ne1(2):Ne2(2),Ni1(3):Ni2(3)) = U_cci(2:Ni(1),2:Ni(2),:)
           case default
           stop 'Error: dir must = 1,2,3'
           end select

         case (3) ! Good for when U is NOT zero outside domain (neumann BCs for U)
           ! (including all fictitious cells)
           U_cct(Ni1(1):Ni2(1),Ni1(2):Ni2(2),Ni1(3):Ni2(3)) = U_cci

         case default
         stop 'Error: embedType must = 1,2,3'
         end select
       end subroutine

       end module