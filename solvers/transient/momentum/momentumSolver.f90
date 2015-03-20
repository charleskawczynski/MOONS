       module momentumSolver_mod
       use simParams_mod
       
       use scalarField_mod
       use vectorField_mod

       use initializeUBCs_mod
       use initializeUField_mod

       use myIO_mod
       use grid_mod

       use myError_mod
       use interpOps_mod
       use del_mod
       use delOps_mod

       use BCs_mod
       use applyBCs_mod

       use solverSettings_mod
       use myTime_mod

       use mySOR_mod
       use myADI_mod
       use myPoisson_mod

       use baseProbes_mod
       use derivedProbes_mod
       
       implicit none
       private
       
       public :: momentum,init,delete,solve
       public :: setDTime,setNmaxPPE,setRe
       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeDivergence


!        logical,parameter :: solveMomentum = .true.
!        logical,parameter :: restartU      = .false.
       
!        integer,parameter :: solveUMethod = 1
!        !                                   1 : Explicit Euler
!        !                                   2 : Semi-Implicit 3D ADI (Douglas)

!        integer :: advectiveUFormulation = 1
!        !                                  1 : Donor-Cell (conservative form)
!        !                                  2 : Advective form
!        !                                  3 : Upwind (not yet implemented)
!        !                                  4 : Hybrid (not yet implemented)

!        real(dpn) :: lambdu = 0.5 ! Upwind blending parameter  ( 0 <= lambdu <= 1 )
!        !                                                       pure         central
!        !                                                      upwind       difference


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type momentum
         ! Field Quantities
         type(vectorField) :: U,Ustar,F,TempVF
         type(scalarField) :: p,Temp,divU

         ! Boundary conditions
         type(BCs) :: u_bcs,v_bcs,w_bcs,p_bcs

         type(solverSettings) :: ss_mom,ss_ppe,ss_ADI
         type(mySOR) :: SOR_p
         type(myADI) :: ADI_p,ADI_u

         ! Residuals
         type(myError) :: err_PPE,err_DivU,err_ADI

         ! Time step, Reynolds number, grid
         integer :: nstep,NmaxPPE
         real(cp) :: dTime,Re,t
         type(grid) :: g

         ! Transient probes
         type(aveProbe) :: u_center,v_center,w_center
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

       interface setDTime;            module procedure setDTimeMomentum;           end interface

       contains

       subroutine initMomentum(mom,g,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         write(*,*) 'Initializing momentum:'

         mom%g = g

         ! Initialize temp fields
         call allocateX(mom%U,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(mom%U,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(mom%U,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)

         call allocateX(mom%Ustar,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(mom%Ustar,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(mom%Ustar,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
         call allocateX(mom%F,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(mom%F,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(mom%F,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)

         call allocateX(mom%TempVF,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(mom%TempVF,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(mom%TempVF,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
         ! allocate P-Fields
         call allocateField(mom%p,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call allocateField(mom%divU,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call allocateField(mom%temp,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call initUBCs(mom%u_bcs,mom%v_bcs,mom%w_bcs,mom%p_bcs,g)
         write(*,*) '     BCs initialized'

         ! Use mom%g later, for no just g
         call initUfield(mom%U%x,mom%U%y,mom%U%z,mom%p%phi,g,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         call applyAllBCs(mom%u_bcs,mom%U%x,g)
         call applyAllBCs(mom%v_bcs,mom%U%y,g)
         call applyAllBCs(mom%w_bcs,mom%U%z,g)
         call applyAllBCs(mom%p_bcs,mom%p%phi,g)
         write(*,*) '     BCs applied'

         call init(mom%err_DivU)
         call init(mom%err_PPE)

         call init(mom%u_center,dir//'Ufield/','transient_u',&
         .not.restartU,shape(mom%U%x),(shape(mom%U%x)+1)/2,g,1)

         ! call init(mom%v_center,dir//'Ufield/','transient_v',&
         ! .not.restartU,shape(mom%U%y),(shape(mom%U%y)+1)/2,g,2)

         ! call init(mom%w_center,dir//'Ufield/','transient_w',&
         ! .not.restartU,shape(mom%U%z),(shape(mom%U%z)+1)/2,g,3)

         write(*,*) '     momentum probes initialized'
         call init(mom%transient_divU,dir//'Ufield/','transient_divU',.not.restartU)
         call init(mom%transient_ppe,dir//'Ufield/','transient_ppe',.not.restartU)
         write(*,*) '     momentum probes initialized'

         call init(mom%u_symmetry,dir//'Ufield/','u_symmetry',&
         .not.restartU,shape(mom%U%z),(shape(mom%U%z)+1)/2,g,3)
         write(*,*) '     momentum probes initialized'

         call export(mom%u_center)
         ! call export(mom%v_center)
         ! call export(mom%w_center)
         call export(mom%transient_ppe)
         call export(mom%transient_divU)
         call export(mom%u_symmetry)
         write(*,*) '     momentum probes initialized'

         ! Initialize solver settings
         call init(mom%ss_ppe)
         call setName(mom%ss_ppe,'pressure poisson    ')
         call setMaxIterations(mom%ss_ppe,mom%NmaxPPE)
         call setSubtractMean(mom%ss_ppe)

         ! Init ADI ss
         call init(mom%ss_ADI)
         call setName(mom%ss_ADI,'momentum ADI        ')
         ! call setMaxIterations(mom%ss_ADI,1) ! Not needed since apply() is used.

         ! call setMinTolerance(mom%ss_ppe,real(1.0**(-6.0),cp))
         ! call setMixedConditions(mom%ss_ppe)
         mom%nstep = 0
         mom%t = real(0.0,cp)
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
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
         call delete(mom%g)
         write(*,*) 'Momentum object deleted'
       end subroutine

       subroutine setDTimeMomentum(mom,dTime)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp),intent(in) :: dTime
         mom%dTime = dTime
       end subroutine

       subroutine setNmaxPPE(mom,NmaxPPE)
         implicit none
         type(momentum),intent(inout) :: mom
         integer,intent(in) :: NmaxPPE
         mom%NmaxPPE = NmaxPPE
       end subroutine

       subroutine setRe(mom,Re)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp),intent(in) :: Re
         mom%Re = Re
       end subroutine

       subroutine solveMomentumEquation(mom,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         select case(solveUMethod)
         case (1); call explicitEuler(mom,g,ss_MHD)
         case (2); call semi_implicit_ADI(mom,g,ss_MHD)
         case default
         write(*,*) 'Error: solveUMethod must = 1,2 in solveMomentumEquation.';stop
         end select
         mom%t = mom%t + mom%dTime
         ! This seems to be the best place for increasing the time step
         ! mom%nstep = mom%nstep + 1
       end subroutine

       subroutine explicitEuler(mom,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(cp) :: Re,dt
         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1)
           call myFaceAdvectDonor(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,g,1)
           call myFaceAdvectDonor(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,g,2)
           call myFaceAdvectDonor(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,g,3)
         case (2)
           call myFaceAdvect(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,g,1)
           call myFaceAdvect(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,g,2)
           call myFaceAdvect(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,g,3)
         case (3)
           call myFaceAdvectHybrid(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,g,1)
           call myFaceAdvectHybrid(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,g,2)
           call myFaceAdvectHybrid(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,g,3)
         end select

         write(*,*) 'maxval(advectX) = ',maxval(mom%TempVF%x)
         write(*,*) 'maxval(advectY) = ',maxval(mom%TempVF%y)
         write(*,*) 'maxval(advectZ) = ',maxval(mom%TempVF%z)

         ! mom%Ustar = (-real(1.0,cp))*mom%TempVF
         call multiply(mom%TempVF,(-real(1.0,cp)))
         call assign(mom%Ustar,mom%TempVF)

         ! Laplacian Terms -----------------------------------------
         ! call myFaceLap(mom%TempVF,U,g)

         call myFaceLap(mom%TempVF%x,mom%U%x,g)
         call myFaceLap(mom%TempVF%y,mom%U%y,g)
         call myFaceLap(mom%TempVF%z,mom%U%z,g)

         write(*,*) 'maxval(LapX) = ',maxval(mom%TempVF%x)
         write(*,*) 'maxval(LapY) = ',maxval(mom%TempVF%y)
         write(*,*) 'maxval(LapZ) = ',maxval(mom%TempVF%z)

         ! mom%Ustar = mom%Ustar + (real(1.0,cp)/Re)*mom%TempVF
         call multiply(mom%TempVF,(real(1.0,cp)/Re))
         call add(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! mom%Ustar = mom%Ustar + real(1.0,cp)*mom%F
         call multiply(mom%F,real(1.0,cp))
         call add(mom%Ustar,mom%F)

         ! Solve with explicit Euler --------------------
         ! mom%Ustar = mom%U + dt*mom%Ustar
         call multiply(mom%Ustar,dt)
         call add(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.0) then
           call myFaceDiv(mom%Temp%phi,mom%Ustar%x,mom%Ustar%y,mom%Ustar%z,g)
           ! call myFaceDiv(mom%Temp,mom%Ustar,g)
           ! mom%Temp = (real(1.0,cp)/dt)*mom%Temp
           call divide(mom%Temp,dt)
           write(*,*) 'maxval(sourceP) = ',maxval(mom%Temp%phi)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           ! call setDt(mom%ADI_p,dt)
           ! call setAlpha(mom%ADI_p,real(1.0,cp))
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,g,&
            mom%ss_ppe,mom%err_PPE,1,getExportErrors(ss_MHD))

           write(*,*) 'maxval(p) = ',maxval(mom%p%phi)
           call myCC2FaceGrad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,g)

         write(*,*) 'maxval(grad(p)_x) = ',maxval(mom%TempVF%x)
         write(*,*) 'maxval(grad(p)_y) = ',maxval(mom%TempVF%y)
         write(*,*) 'maxval(grad(p)_z) = ',maxval(mom%TempVF%z)

           ! mom%Ustar = mom%Ustar - dt*mom%TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! mom%U = mom%Ustar
         call assign(mom%U,mom%Ustar)

         call applyAllBCs(mom%u_bcs,mom%U%x,g)
         call applyAllBCs(mom%v_bcs,mom%U%y,g)
         call applyAllBCs(mom%w_bcs,mom%U%z,g)

         write(*,*) 'maxval(LapX) = ',maxval(mom%TempVF%x)
         write(*,*) 'maxval(LapY) = ',maxval(mom%TempVF%y)
         write(*,*) 'maxval(LapZ) = ',maxval(mom%TempVF%z)

         mom%nstep = mom%nstep + 1
         write(*,*) 'nstep = ',mom%nstep
       end subroutine

       subroutine semi_implicit_ADI(mom,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(cp) :: Re,dt

         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1)
           call myFaceAdvectDonor(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,g,1)
           call myFaceAdvectDonor(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,g,2)
           call myFaceAdvectDonor(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,g,3)
         case (2)
           call myFaceAdvect(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,g,1)
           call myFaceAdvect(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,g,2)
           call myFaceAdvect(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,g,3)
         case (3)
           call myFaceAdvectHybrid(mom%TempVF%x,mom%U%x,mom%U%y,mom%U%z,mom%U%x,g,1)
           call myFaceAdvectHybrid(mom%TempVF%y,mom%U%x,mom%U%y,mom%U%z,mom%U%y,g,2)
           call myFaceAdvectHybrid(mom%TempVF%z,mom%U%x,mom%U%y,mom%U%z,mom%U%z,g,3)
         end select

         ! mom%Ustar = (-real(1.0,cp))*mom%TempVF
         call multiply(mom%TempVF,(-real(1.0,cp)))
         call assign(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! mom%Ustar = mom%Ustar + real(1.0,cp)*mom%F
         call multiply(mom%F,real(1.0,cp))
         call add(mom%Ustar,mom%F)

         ! Solve momentum with ADI --------------------
         ! u_t = 1/Re*(u_xx + u_yy + u_zz) - f
         ! Where f = RHS of NS
         ! mom%Ustar = mom%Ustar + mom%U
         call setDt(mom%ADI_u,dt)
         call setAlpha(mom%ADI_u,real(1.0,cp)/Re)

         call assign(mom%F,mom%Ustar)
         call multiply(mom%F,real(-1.0,cp))

         call apply(mom%ADI_u,mom%U%x,mom%F%x,mom%u_bcs,g,&
            mom%ss_ADI,mom%err_ADI,1,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%U%y,mom%F%y,mom%v_bcs,g,&
            mom%ss_ADI,mom%err_ADI,1,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%U%z,mom%F%z,mom%w_bcs,g,&
            mom%ss_ADI,mom%err_ADI,1,getExportErrors(ss_MHD))

         call assign(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.1) then
           call myFaceDiv(mom%Temp%phi,mom%Ustar%x,mom%Ustar%y,mom%Ustar%z,g)
           ! call myFaceDiv(mom%Temp,mom%Ustar,g)
           ! mom%Temp = (real(1.0,cp)/dt)*mom%Temp
           call divide(mom%Temp,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,g,&
            mom%ss_ppe,mom%err_PPE,1,getExportErrors(ss_MHD))

           call myCC2FaceGrad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,g)

           ! mom%Ustar = mom%Ustar - dt*mom%TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! mom%U = mom%Ustar
         call assign(mom%U,mom%Ustar)

         ! call applyVectorBCs(mom%u_bcs,U,g)
         call applyAllBCs(mom%u_bcs,mom%U%x,g)
         call applyAllBCs(mom%v_bcs,mom%U%y,g)
         call applyAllBCs(mom%w_bcs,mom%U%z,g)

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
           ! call apply(mom%v_center,getIteration(ss_MHD),mom%U%y)
           ! call apply(mom%w_center,getIteration(ss_MHD),mom%U%z)
         endif

         if (getExportErrors(ss_MHD)) then
           call set(mom%transient_ppe,getIteration(ss_MHD),getL2(mom%err_PPE))
           call apply(mom%transient_ppe)

           call apply(mom%transient_divU,getIteration(ss_MHD),mom%divU%phi)
           call apply(mom%u_symmetry,getIteration(ss_MHD),mom%U%z)
         endif
       end subroutine

       subroutine momentumExportRaw(mom,g,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         ! ************************* EXPORT IN RAW FORMAT *************************
         call writeToFile(g%c(1)%hn,g%c(2)%hc,g%c(3)%hc,mom%U%x,dir//'Ufield/','ufi')
         call writeToFile(g%c(1)%hc,g%c(2)%hn,g%c(3)%hc,mom%U%y,dir//'Ufield/','vfi')
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hn,mom%U%z,dir//'Ufield/','wfi')
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,mom%p%phi,dir//'Ufield/','pci')
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,mom%divU%phi,dir//'Ufield/','divUci')
         write(*,*) 'Exported Raw Solutions for U'
       end subroutine

       subroutine momentumExport(mom,g,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         ! Locals
         integer :: Nx,Ny,Nz
         type(del) :: d
         ! Interior
         real(cp),dimension(:,:,:),allocatable :: tempccx,tempccy,tempccz
         real(cp),dimension(:,:,:),allocatable :: tempnx,tempny,tempnz
         real(cp),dimension(:,:,:),allocatable :: tempnix,tempniy,tempniz

         ! ************************ EXPORT IN OTHER FORMATS ***********************

         ! --------------------------- UFIELD AT NODES ----------------------------
         Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
         allocate(tempnx(Nx,Ny,Nz))
         allocate(tempny(Nx,Ny,Nz))
         allocate(tempnz(Nx,Ny,Nz))
         allocate(tempnix(Nx-2,Ny-2,Nz-2))
         allocate(tempniy(Nx-2,Ny-2,Nz-2))
         allocate(tempniz(Nx-2,Ny-2,Nz-2))
         call myFace2Node(tempnx,mom%U%x,g,1)
         call myFace2Node(tempny,mom%U%y,g,2)
         call myFace2Node(tempnz,mom%U%z,g,3)

         tempnix = tempnx(2:Nx-1,2:Ny-1,2:Nz-1)
         tempniy = tempny(2:Nx-1,2:Ny-1,2:Nz-1)
         tempniz = tempnz(2:Nx-1,2:Ny-1,2:Nz-1)

         call writeToFile(g%c(1)%hn(2:Nx-1),g%c(2)%hn(2:Ny-1),g%c(3)%hn(2:Nz-1),&
          tempnix,tempniy,tempniz,dir//'Ufield/','uni','vni','wni')
         deallocate(tempnx,tempny,tempnz)
         deallocate(tempnix,tempniy,tempniz)

         ! ****************** EXPORT IN CELL CENTERS ************************
         ! Velocities at cell centers:
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         allocate(tempccx(Nx,Ny,Nz))
         allocate(tempccy(Nx,Ny,Nz))
         allocate(tempccz(Nx,Ny,Nz))
         call myFace2CellCenter(tempccx,mom%U%x,g,1)
         call myFace2CellCenter(tempccy,mom%U%y,g,2)
         call myFace2CellCenter(tempccz,mom%U%z,g,3)
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,tempccx,tempccy,tempccz,dir//'Ufield/','uci','vci','wci')
         deallocate(tempccx,tempccy,tempccz)

         allocate(tempccx(Nx,Ny,Nz))
         call d%assign(tempccx,mom%U%x,g,1,1,1) ! Padding avoids calcs on fictive cells
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,tempccx,dir//'Ufield/','dudx')

         call d%assign(tempccx,mom%U%y,g,1,2,1) ! Padding avoids calcs on fictive cells
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,tempccx,dir//'Ufield/','dvdy')

         call d%assign(tempccx,mom%U%z,g,1,3,1) ! Padding avoids calcs on fictive cells
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,tempccx,dir//'Ufield/','dwdz')
         deallocate(tempccx)

       end subroutine

       subroutine computeDivergenceMomentum(mom,g)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         call myFaceDiv(mom%divU%phi,mom%U%x,mom%U%y,mom%U%z,g)
         mom%divU%phi(1,:,:) = real(0.0,cp)
         mom%divU%phi(:,1,:) = real(0.0,cp)
         mom%divU%phi(:,:,1) = real(0.0,cp)

         mom%divU%phi(g%c(1)%sc,:,:) = real(0.0,cp)
         mom%divU%phi(:,g%c(2)%sc,:) = real(0.0,cp)
         mom%divU%phi(:,:,g%c(3)%sc) = real(0.0,cp)
       end subroutine

       end module