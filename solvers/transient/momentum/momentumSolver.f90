       module momentumSolver_mod
       use simParams_mod
       
       use scalarField_mod
       use vectorField_mod

       use initializeUBCs_mod
       use initializeUField_mod

       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use grid_mod

       use myError_mod
       use interpOps_mod
       use del_mod
       use ops_discrete_mod
       use ops_physics_mod
       use ops_aux_mod

       use BCs_mod
       use applyBCs_mod

       use probe_transient_mod
       use solverSettings_mod
       use myTime_mod

       use myJacobi_mod
       use mySOR_mod
       use myADI_mod
       use myMG_mod
       use myPoisson_mod

       use probe_base_mod
       use probe_derived_mod
       
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
         ! type(vectorBCs) :: u_bcs

         type(solverSettings) :: ss_mom,ss_ppe,ss_ADI
         type(multiGrid),dimension(3) :: MG
         type(myJacobi) :: Jacobi_p
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

       ! ******************* INIT/DELETE ***********************

       subroutine initMomentum(mom,g,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing momentum:'

         mom%g = g

         ! Initialize temp fields
         call allocateX(mom%U,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
         call allocateY(mom%U,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
         call allocateZ(mom%U,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)

         call allocateVectorField(mom%Ustar,mom%U)
         call allocateVectorField(mom%F,mom%U)
         call allocateVectorField(mom%TempVF,mom%U)

         ! allocate P-Fields
         call allocateField(mom%p,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call allocateField(mom%divU,mom%p)
         call allocateField(mom%temp,mom%p)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call initUBCs(mom%u_bcs,mom%v_bcs,mom%w_bcs,mom%p_bcs,g)
         write(*,*) '     BCs initialized'

         ! Use mom%g later, for no just g
         call initUfield(mom%U%x,mom%U%y,mom%U%z,mom%p%phi,g,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         ! call applyAllBCs(mom%u_bcs,mom%U%x,g)
         ! call applyAllBCs(mom%v_bcs,mom%U%y,g)
         ! call applyAllBCs(mom%w_bcs,mom%U%z,g)
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
         ! call setSubtractMean(mom%ss_ppe)

         ! Init ADI ss
         call init(mom%ss_ADI)
         call setName(mom%ss_ADI,'momentum ADI        ')

         ! Init Multigrid solver
         ! call init(mom%MG,mom%p%s,mom%p_bcs,mom%g,mom%ss_ppe,.false.)
         ! call setMaxIterations(mom%ss_ADI,1) ! Not needed since apply() is used.

         ! call setMinTolerance(mom%ss_ppe,real(1.0**(-6.0),cp))
         ! call setMixedConditions(mom%ss_ppe)
         if (restartU) then
         call readLastStepFromFile(mom%nstep,dir//'parameters/','n_mom')
         else; mom%nstep = 0
         endif

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

         ! call delete(mom%MG)
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

       ! ******************* EXPORT ****************************

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

         if (solveMomentum.and.(getExportTransient(ss_MHD))) then
           call apply(mom%u_center,mom%nstep,mom%U%x)
           ! call apply(mom%v_center,mom%nstep,mom%U%y)
           ! call apply(mom%w_center,mom%nstep,mom%U%z)
         endif

         if (solveMomentum.and.getExportErrors(ss_MHD)) then
           call set(mom%transient_ppe,mom%nstep,getL2(mom%err_PPE))
           call apply(mom%transient_ppe)

           call apply(mom%transient_divU,mom%nstep,mom%divU%phi)
           call apply(mom%u_symmetry,mom%nstep,mom%U%z)
         endif
       end subroutine

       subroutine momentumExportRaw(mom,g,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         if (restartU.and.(.not.solveMomentum)) then
           ! This preserves the initial data
         else
           write(*,*) 'Exporting RAW Solutions for U'
           call writeToFile(g,mom%U%x,dir//'Ufield/','ufi')
           call writeToFile(g,mom%U%y,dir//'Ufield/','vfi')
           call writeToFile(g,mom%U%z,dir//'Ufield/','wfi')
           call writeToFile(g,mom%p%phi,dir//'Ufield/','pci')
           call writeToFile(g,mom%divU%phi,dir//'Ufield/','divUci')
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine momentumExport(mom,g,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         type(vectorField) :: tempCCVF,tempNVF

         write(*,*) 'Exporting PROCESSED Solutions for U'

         ! ********************** EXPORT IN NODES ***************************
         Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
         call allocateVectorField(tempNVF,Nx,Ny,Nz)
         call myFace2Node(tempNVF,mom%u,g)
         call writeToFile(g,tempNVF,dir//'Ufield/','uni','vni','wni')
         call writeVecPhysical(g,tempNVF,dir//'Ufield/','uni_phys','vni_phys','wni_phys')

         ! ******************** EXPORT IN CELL CENTERS **********************
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         call allocateVectorField(tempCCVF,Nx,Ny,Nz)
         call myFace2CellCenter(tempCCVF,mom%U,g)
         call writeToFile(g,tempCCVF,dir//'Ufield/','uci','vci','wci')

         call delete(tempNVF)
         call delete(tempCCVF)
         write(*,*) '     finished'
       end subroutine

       ! ******************* SOLVER ****************************

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
         mom%nstep = mom%nstep + 1
       end subroutine

       subroutine explicitEuler(mom,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(cp) :: Re,dt
         type(del) :: d
         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1); call  faceAdvectDonor(mom%TempVF,mom%U,mom%U,g)
         case (2); call       faceAdvect(mom%TempVF,mom%U,mom%U,g)
         case (3); call faceAdvectHybrid(mom%TempVF,mom%U,mom%U,g)
         end select

         ! Ustar = -TempVF
         call multiply(mom%TempVF,(-real(1.0,cp)))
         call assign(mom%Ustar,mom%TempVF)

         ! Laplacian Terms -----------------------------------------
         call lap(mom%TempVF,mom%U,g)
         call divide(mom%TempVF,Re)
         call add(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         call add(mom%Ustar,mom%F)

         ! Zero wall coincident forcing (may be bad for neumann BCs)
         call zeroWallCoincidentBoundariesVF(mom%Ustar,g)

         ! Solve with explicit Euler --------------------
         ! Ustar = U + dt*Ustar
         call multiply(mom%Ustar,dt)
         call add(mom%Ustar,mom%U)

         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.0) then
           call div(mom%Temp%phi,mom%Ustar,g)
           ! Temp = Temp/dt
           call divide(mom%Temp,dt)
           call zeroGhostPoints(mom%Temp%phi)

           ! Solve lap(p) = div(U)/dt
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,g,&
            mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD))

           call grad(mom%TempVF,mom%p%phi,g)

           ! Ustar = Ustar - dt*dp/dx
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! U = Ustar
         call assign(mom%U,mom%Ustar)

         call applyAllBCs(mom%u_bcs,mom%U%x,g)
         call applyAllBCs(mom%v_bcs,mom%U%y,g)
         call applyAllBCs(mom%w_bcs,mom%U%z,g)
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
         case (1); call  faceAdvectDonor(mom%TempVF,mom%U,mom%U,g)
         case (2); call       faceAdvect(mom%TempVF,mom%U,mom%U,g)
         case (3); call faceAdvectHybrid(mom%TempVF,mom%U,mom%U,g)
         end select

         ! Ustar = -TempVF
         call multiply(mom%TempVF,(-real(1.0,cp)))
         call assign(mom%Ustar,mom%TempVF)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! Ustar = Ustar + F
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
            mom%ss_ADI,mom%err_ADI,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%U%y,mom%F%y,mom%v_bcs,g,&
            mom%ss_ADI,mom%err_ADI,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%U%z,mom%F%z,mom%w_bcs,g,&
            mom%ss_ADI,mom%err_ADI,getExportErrors(ss_MHD))

         call assign(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.0) then
           call div(mom%Temp%phi,mom%Ustar,g)
           ! Temp = Temp/dt
           call divide(mom%Temp,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           call myPoisson(mom%SOR_p,mom%p%phi,mom%Temp%phi,mom%p_bcs,g,&
            mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD))

           call grad(mom%TempVF%x,mom%TempVF%y,mom%TempVF%z,mom%p%phi,g)

           ! Ustar = Ustar - dt*TempVF
           call multiply(mom%TempVF,dt)
           call subtract(mom%Ustar,mom%TempVF)
         endif

         ! U = Ustar
         call assign(mom%U,mom%Ustar)

         call applyAllBCs(mom%u_bcs,mom%U%x,g)
         call applyAllBCs(mom%v_bcs,mom%U%y,g)
         call applyAllBCs(mom%w_bcs,mom%U%z,g)
       end subroutine

       ! ********************* AUX *****************************

       subroutine computeDivergenceMomentum(mom,g)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         call div(mom%divU%phi,mom%U%x,mom%U%y,mom%U%z,g)
         call zeroGhostPoints(mom%divU%phi)
       end subroutine

       subroutine zeroWallCoincidentBoundaries(f,s,g,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         select case (dir)
         case (0)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = real(0.0,cp); f(s(1),:,:) = real(0.0,cp)
             f(2,:,:) = real(0.0,cp); f(s(1)-1,:,:) = real(0.0,cp)
           elseif (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = real(0.0,cp); f(:,s(2),:) = real(0.0,cp)
             f(:,2,:) = real(0.0,cp); f(:,s(2)-1,:) = real(0.0,cp)
           elseif (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = real(0.0,cp); f(:,:,s(3)) = real(0.0,cp)
             f(:,:,2) = real(0.0,cp); f(:,:,s(3)-1) = real(0.0,cp)
           endif
         case (1)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = real(0.0,cp); f(s(1),:,:) = real(0.0,cp)
             f(2,:,:) = real(0.0,cp); f(s(1)-1,:,:) = real(0.0,cp)
           endif
         case (2)
           if (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = real(0.0,cp); f(:,s(2),:) = real(0.0,cp)
             f(:,2,:) = real(0.0,cp); f(:,s(2)-1,:) = real(0.0,cp)
           endif
         case (3)
           if (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = real(0.0,cp); f(:,:,s(3)) = real(0.0,cp)
             f(:,:,2) = real(0.0,cp); f(:,:,s(3)-1) = real(0.0,cp)
           endif
         case default
           stop 'Error: dir must = 0,1,2,3 in zeroWallCoincidentBoundaries'
         end select
       end subroutine

       subroutine zeroWallCoincidentBoundariesVF(f,g)
         implicit none
         type(vectorField),intent(inout) :: f
         type(grid),intent(in) :: g
         call zeroWallCoincidentBoundaries(f%x,f%sx,g,0)
         call zeroWallCoincidentBoundaries(f%y,f%sy,g,0)
         call zeroWallCoincidentBoundaries(f%z,f%sz,g,0)
       end subroutine

       end module