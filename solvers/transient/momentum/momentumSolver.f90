       module momentumSolver_mod
       use simParams_mod
       use constants_mod
       use myAllocate_mod
       use scalarField_mod
       use vectorField_mod

       use initializeUBCs_mod
       use initializeUField_mod

       use myIO_mod
       use grid_mod
       use griddata_mod
       use rundata_mod
       use myError_mod
       use vectorOps_mod
       use BCs_mod
       use applyBCs_mod
       use solverSettings_mod
       use mySOR_mod
       use myADI_mod
       use myTime_mod
       use myPoisson_mod
       use baseProbes_mod
       use derivedProbes_mod
       
       implicit none
       private
       
       public :: momentum,init,delete,solve
       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeDivergence

       type momentum
         character(len=8) :: name = 'momentum'
         type(vectorField) :: U,Ustar,F,TempVF
         type(scalarField) :: p,Temp,divU

         type(BCs) :: u_bcs,v_bcs,w_bcs,p_bcs
         type(solverSettings) :: ss_mom,ss_ppe,ss_ADI
         type(myError) :: err_PPE,err_DivU,err_ADI
         type(mySOR) :: SOR_p
         type(myADI) :: ADI_p,ADI_u
         integer :: nstep

         real(dpn) :: dt,Re

         ! Transient probes
         type(aveProbe) :: u_center
         type(errorProbe) :: transient_ppe,transient_divU
         type(avePlaneErrorProbe) :: u_symmetry
       end type

       interface init;                module procedure initMomentum;               end interface
       interface delete;              module procedure deleteMomentum;             end interface
       interface solve;               module procedure solveMomentumEquation;      end interface
       interface export;              module procedure momentumExport;             end interface
       interface exportRaw;           module procedure momentumExportRaw;          end interface
       interface exportTransient;     module procedure momentumExportTransient;    end interface
       interface printExportBCs;      module procedure printExportMomentumBCs;     end interface
       interface computeDivergence;   module procedure computeDivergenceMomentum;  end interface

       contains

       subroutine initMomentum(mom,gd,g,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing momentum:'
         ! Initialize temp fields
         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%U,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%U,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%U,Nx,Ny,Nz)


         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%Ustar,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%Ustar,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%Ustar,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%F,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%F,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%F,Nx,Ny,Nz)

         call myAllocate(Nx,Ny,Nz,gd,ULoc,1); call allocateX(mom%TempVF,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,2); call allocateY(mom%TempVF,Nx,Ny,Nz)
         call myAllocate(Nx,Ny,Nz,gd,ULoc,3); call allocateZ(mom%TempVF,Nx,Ny,Nz)
         ! allocate P-Fields
         call myAllocate(Nx,Ny,Nz,gd,PLoc)
         call allocateField(mom%p,Nx,Ny,Nz)
         call allocateField(mom%divU,Nx,Ny,Nz)
         call allocateField(mom%temp,Nx,Ny,Nz)
         
         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call initializeUBCs(mom%u_bcs,mom%v_bcs,mom%w_bcs,mom%p_bcs,gd)
         write(*,*) '     BCs initialized'

         call initializeUfield(mom%U%x,mom%U%y,mom%U%z,mom%p%phi,gd,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         call applyAllBCs(mom%u_bcs,mom%U%x,gd)
         call applyAllBCs(mom%v_bcs,mom%U%y,gd)
         call applyAllBCs(mom%w_bcs,mom%U%z,gd)
         call applyAllBCs(mom%p_bcs,mom%p%phi,gd)
         write(*,*) '     BCs applied'

         call initialize(mom%err_DivU)
         call initialize(mom%err_PPE)

         ! (p,dir,name,TF_freshStart,s,i,gd,component)
         call initialize(mom%u_center,dir//'Ufield/','transient_u',&
         .not.restartU,shape(mom%U%x),(shape(mom%U%x)+1)/2,gd,1)

         write(*,*) '     momentum probes initialized'
         call initialize(mom%transient_divU,dir//'Ufield/','transient_divU',.not.restartU)
         call initialize(mom%transient_ppe,dir//'Ufield/','transient_ppe',.not.restartU)
         write(*,*) '     momentum probes initialized'

         call initialize(mom%u_symmetry,dir//'Ufield/','u_symmetry',&
         .not.restartU,shape(mom%U%z),(shape(mom%U%z)+1)/2,gd,3)
         write(*,*) '     momentum probes initialized'

         call export(mom%u_center)
         call export(mom%transient_ppe)
         call export(mom%transient_divU)
         call export(mom%u_symmetry)
         write(*,*) '     momentum probes initialized'

         ! Initialize solver settings
         call init(mom%ss_ppe)
         call setName(mom%ss_ppe,'pressure poisson    ')
         call setMaxIterations(mom%ss_ppe,5)
         call setSubtractMean(mom%ss_ppe)

         ! Init ADI ss
         call init(mom%ss_ADI)
         call setName(mom%ss_ADI,'momentum ADI        ')
         call setMaxIterations(mom%ss_ADI,1)

         ! call setMinTolerance(mom%ss_ppe,real(1.0**(-6.0),dpn))
         ! call setMixedConditions(mom%ss_ppe)
         mom%nstep = 0
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteMomentum(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U);               call delete(mom%Ustar);   call delete(mom%F)
         call delete(mom%TempVF);          call delete(mom%p);       call delete(mom%Temp)

         call delete(mom%u_bcs);           call delete(mom%v_bcs);   call delete(mom%w_bcs);
         call delete(mom%p_bcs);           call delete(mom%divU)

         call delete(mom%u_center);        call delete(mom%transient_ppe)
         call delete(mom%transient_divU);  call delete(mom%u_symmetry)
         write(*,*) 'Momentum object deleted'
       end subroutine

       subroutine solveMomentumEquation(mom,gd,rd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(in) :: ss_MHD
         select case(solveUMethod)
         case (1); call explicitEuler(mom,gd,rd,ss_MHD)
         case (2); call semi_implicit_ADI(mom,gd,rd,ss_MHD)
         case default
         write(*,*) 'Error: solveUMethod must = 1,2 in solveMomentumEquation.';stop
         end select
         ! This seems to be the best place for increasing the time step
         ! mom%nstep = mom%nstep + 1
       end subroutine

       subroutine semi_implicit_ADI(mom,gd,rd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(dpn) :: Re,dt

         dt = getDtime(rd)
         Re = getRe(rd)

         call setAlpha(mom%ADI_u,one/Re)
         call setDt(mom%ADI_u,dt)

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1)
           ! call myFaceAdvectDonor(mom%TempVF,U,U,gd,1)
           ! call myFaceAdvectDonor(mom%TempVF%x,U,U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectDonor(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (2)
           call myFaceAdvect(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvect(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvect(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (3)
           call myFaceAdvectHybrid(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectHybrid(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectHybrid(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         end select

         ! mom%Ustar = -mom%TempVF
         call multiply(mom%TempVF,(-one))
         call assign(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! mom%Ustar = mom%Ustar + dt*mom%F
         call add(mom%Ustar,mom%F)

         ! Velocity at Previous Time Step Terms --------------------
         ! mom%Ustar = mom%Ustar + mom%U
         call add(mom%Ustar,mom%U)

         ! Solve momentum with ADI --------------------
         ! u_t = 1/Re*(u_xx + u_yy + u_zz) - f
         ! mom%Ustar = mom%Ustar + mom%U
         call setDt(mom%ADI_u,dt)
         call setAlpha(mom%ADI_u,one/Re)

         call apply(mom%ADI_u,mom%Ustar%x,mom%F%x,mom%u_bcs,gd,&
            mom%ss_ADI,mom%err_ADI,1,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%Ustar%y,mom%F%y,mom%v_bcs,gd,&
            mom%ss_ADI,mom%err_ADI,1,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%Ustar%z,mom%F%z,mom%w_bcs,gd,&
            mom%ss_ADI,mom%err_ADI,1,getExportErrors(ss_MHD))
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.1) then
           call myFaceDiv(mom%Temp%phi,mom%Ustar%x,mom%Ustar%y,mom%Ustar%z,gd)
           ! call myFaceDiv(mom%Temp,mom%Ustar,gd)
           ! mom%Temp = (one/dt)*mom%Temp
           call divide(mom%Temp,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,gd,&
            mom%ss_ppe,mom%err_PPE,1,getExportErrors(ss_MHD))

           call myCC2FaceGrad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,gd)

           ! mom%Ustar = mom%Ustar - dt*mom%TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! mom%U = mom%Ustar
         call assign(mom%U,mom%Ustar)

         ! call applyVectorBCs(mom%u_bcs,U,gd)
         call applyAllBCs(mom%u_bcs,mom%U%x,gd)
         call applyAllBCs(mom%v_bcs,mom%U%y,gd)
         call applyAllBCs(mom%w_bcs,mom%U%z,gd)

         mom%nstep = mom%nstep + 1
       end subroutine

       subroutine explicitEuler(mom,gd,rd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(dpn) :: Re,dt

         dt = getDtime(rd)
         Re = getRe(rd)

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1)
           ! call myFaceAdvectDonor(mom%TempVF,U,U,gd,1)
           ! call myFaceAdvectDonor(mom%TempVF%x,U,U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectDonor(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (2)
           call myFaceAdvect(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvect(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvect(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (3)
           call myFaceAdvectHybrid(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectHybrid(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectHybrid(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         end select

         ! mom%Ustar = (-dt)*mom%TempVF
         call multiply(mom%TempVF,(-dt))
         call assign(mom%Ustar,mom%TempVF)

         ! Laplacian Terms -----------------------------------------
         ! call myFaceLap(mom%TempVF,U,gd)

         call myFaceLap(mom%TempVF%x,mom%U%x,gd,1)
         call myFaceLap(mom%TempVF%y,mom%U%y,gd,2)
         call myFaceLap(mom%TempVF%z,mom%U%z,gd,3)

         ! mom%Ustar = mom%Ustar + (dt/Re)*mom%TempVF
         call multiply(mom%TempVF,(dt/Re))
         call add(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! mom%Ustar = mom%Ustar + dt*mom%F
         call multiply(mom%F,dt)
         call add(mom%Ustar,mom%F)

         ! Velocity at Previous Time Step Terms --------------------
         ! mom%Ustar = mom%Ustar + mom%U
         call add(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.1) then
           call myFaceDiv(mom%Temp%phi,mom%Ustar%x,mom%Ustar%y,mom%Ustar%z,gd)
           ! call myFaceDiv(mom%Temp,mom%Ustar,gd)
           ! mom%Temp = (one/dt)*mom%Temp
           call divide(mom%Temp,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           ! call setDt(mom%ADI_p,dt)
           ! call setAlpha(mom%ADI_p,one)
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,gd,&
            mom%ss_ppe,mom%err_PPE,1,getExportErrors(ss_MHD))

           call myCC2FaceGrad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,gd)

           ! mom%Ustar = mom%Ustar - dt*mom%TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! mom%U = mom%Ustar
         call assign(mom%U,mom%Ustar)

         ! call applyVectorBCs(mom%u_bcs,U,gd)
         call applyAllBCs(mom%u_bcs,mom%U%x,gd)
         call applyAllBCs(mom%v_bcs,mom%U%y,gd)
         call applyAllBCs(mom%w_bcs,mom%U%z,gd)

         mom%nstep = mom%nstep + 1
       end subroutine

       subroutine solveMomentumEquationOld(mom,gd,rd,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         type(rundata),intent(in) :: rd
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(dpn) :: Re,dt

         dt = getDtime(rd)
         Re = getRe(rd)

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1)
           ! call myFaceAdvectDonor(mom%TempVF,U,U,gd,1)
           ! call myFaceAdvectDonor(mom%TempVF%x,U,U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectDonor(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectDonor(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (2)
           call myFaceAdvect(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvect(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvect(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         case (3)
           call myFaceAdvectHybrid(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,gd,1)
           call myFaceAdvectHybrid(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,gd,2)
           call myFaceAdvectHybrid(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,gd,3)
         end select

         ! mom%Ustar = (-dt)*mom%TempVF
         call multiply(mom%TempVF,(-dt))
         call assign(mom%Ustar,mom%TempVF)

         ! Laplacian Terms -----------------------------------------
         ! call myFaceLap(mom%TempVF,U,gd)

         call myFaceLap(mom%TempVF%x,mom%U%x,gd,1)
         call myFaceLap(mom%TempVF%y,mom%U%y,gd,2)
         call myFaceLap(mom%TempVF%z,mom%U%z,gd,3)

         ! mom%Ustar = mom%Ustar + (dt/Re)*mom%TempVF
         call multiply(mom%TempVF,(dt/Re))
         call add(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! mom%Ustar = mom%Ustar + dt*mom%F
         call multiply(mom%F,dt)
         call add(mom%Ustar,mom%F)

         ! Velocity at Previous Time Step Terms --------------------
         ! mom%Ustar = mom%Ustar + mom%U
         call add(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.1) then
           call myFaceDiv(mom%Temp%phi,mom%Ustar%x,mom%Ustar%y,mom%Ustar%z,gd)
           ! call myFaceDiv(mom%Temp,mom%Ustar,gd)
           ! mom%Temp = (one/dt)*mom%Temp
           call divide(mom%Temp,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,gd,&
            mom%ss_ppe,mom%err_PPE,1,getExportErrors(ss_MHD))

           call myCC2FaceGrad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,gd)

           ! mom%Ustar = mom%Ustar - dt*mom%TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! mom%U = mom%Ustar
         call assign(mom%U,mom%Ustar)

         ! call applyVectorBCs(mom%u_bcs,U,gd)
         call applyAllBCs(mom%u_bcs,mom%U%x,gd)
         call applyAllBCs(mom%v_bcs,mom%U%y,gd)
         call applyAllBCs(mom%w_bcs,mom%U%z,gd)

         mom%nstep = mom%nstep + 1
       end subroutine

       subroutine printExportMomentumBCs(mom,dir)
         implicit none
         type(momentum),intent(in) :: mom
         character(len=*),intent(in) :: dir
         call printAllBoundaries(mom%u_bcs,'u')
         call printAllBoundaries(mom%v_bcs,'v')
         call printAllBoundaries(mom%w_bcs,'w')
         call printAllBoundaries(mom%p_bcs,'w')
         call writeAllBoundaries(mom%u_bcs,dir//'parameters/','u')
         call writeAllBoundaries(mom%v_bcs,dir//'parameters/','v')
         call writeAllBoundaries(mom%w_bcs,dir//'parameters/','w')
         call writeAllBoundaries(mom%p_bcs,dir//'parameters/','p')
       end subroutine

       subroutine momentumExportTransient(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD

         if ((getExportTransient(ss_MHD))) then
           call apply(mom%u_center,getIteration(ss_MHD),mom%U%x)
         endif

         if (getExportErrors(ss_MHD)) then
           call set(mom%transient_ppe,getIteration(ss_MHD),getL2(mom%err_PPE))
           call apply(mom%transient_ppe)

           call apply(mom%transient_divU,getIteration(ss_MHD),mom%divU%phi)
           call apply(mom%u_symmetry,getIteration(ss_MHD),mom%U%z)
         endif
       end subroutine

       subroutine momentumExportRaw(mom,gd,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! ************************* EXPORT IN RAW FORMAT *************************
         call writeToFile(gd%xni,gd%yci,gd%zci,mom%U%x,dir//'Ufield/','ufi')
         call writeToFile(gd%xci,gd%yni,gd%zci,mom%U%y,dir//'Ufield/','vfi')
         call writeToFile(gd%xci,gd%yci,gd%zni,mom%U%z,dir//'Ufield/','wfi')
         call writeToFile(gd%xci,gd%yci,gd%zci,mom%p%phi,dir//'Ufield/','pci')
         call writeToFile(gd%xci,gd%yci,gd%zci,mom%divU%phi,dir//'Ufield/','divUci')
         write(*,*) 'Exported Raw Solutions for U'
       end subroutine

       subroutine momentumExport(mom,gd,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(griddata),intent(in) :: gd
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         ! Interior
         ! real(dpn),dimension(:,:,:),allocatable :: tempx,tempy,tempz
         real(dpn),dimension(:,:,:),allocatable :: tempccx,tempccy,tempccz
         real(dpn),dimension(:,:,:),allocatable :: tempnx,tempny,tempnz,tempn

         ! ************************ EXPORT IN OTHER FORMATS ***********************

         ! --------------------------- UFIELD AT NODES ----------------------------
         call myAllocate(Nx,Ny,Nz,gd,dom_n_in); allocate(tempnx(Nx,Ny,Nz))
                                                allocate(tempny(Nx,Ny,Nz))
                                                allocate(tempnz(Nx,Ny,Nz))
                                                allocate(tempn (Nx,Ny,Nz))
         call myFace2Node(tempnx,mom%U%x,gd,1)
         call myFace2Node(tempny,mom%U%y,gd,2)
         call myFace2Node(tempnz,mom%U%z,gd,3)

         call myNodeDiv(tempn,tempnx,tempny,tempnz,gd)
         call writeToFile(gd%xni,gd%yni,gd%zni,tempnx,tempny,tempnz,dir//'Ufield/','uni','vni','wni')
         call writeToFile(gd%xni,gd%yni,gd%zni,tempn,dir//'Ufield/','divUni')
         deallocate(tempnx,tempny,tempnz,tempn)

         ! ****************** EXPORT IN CELL CENTERS ************************
         if (outputAlternativeFormats) then
           ! Velocities at cell centers:
           call myAllocate(Nx,Ny,Nz,gd,dom_cc_in); allocate(tempccx(Nx,Ny,Nz))
                                                   allocate(tempccy(Nx,Ny,Nz))
                                                   allocate(tempccz(Nx,Ny,Nz))
           call myFace2CellCenter(tempccx,mom%U%x,gd,1)
           call myFace2CellCenter(tempccy,mom%U%y,gd,2)
           call myFace2CellCenter(tempccz,mom%U%z,gd,3)
           call writeToFile(gd%xci,gd%yci,gd%zci,tempccx,tempccy,tempccz,dir//'Ufield/','uci','vci','wci')
           deallocate(tempccx,tempccy,tempccz)
         endif
       end subroutine

       subroutine computeDivergenceMomentum(mom,gd)
         implicit none
         type(momentum),intent(inout) :: mom
         type(griddata),intent(in) :: gd
         call myFaceDiv(mom%divU%phi,mom%U%x,mom%U%y,mom%U%z,gd)
       end subroutine

       end module