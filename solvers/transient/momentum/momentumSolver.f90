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

       use norms_mod
       use del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod

       use BCs_mod
       use vectorBCs_mod
       use applyBCs_mod

       use solverSettings_mod
       use myTime_mod

       use jacobi_mod
       use SOR_mod
       use ADI_mod
       use MG_mod
       use poisson_mod

       use probe_base_mod
       use probe_transient_mod
       use probe_derived_mod
       
       implicit none
       private
       
       public :: momentum,init,delete,solve
       public :: setDTime,setNmaxPPE
       public :: setPiGroups
       public :: export,exportRaw,exportTransient
       public :: printExportBCs
       public :: computeDivergence
       public :: computeKineticEnergy
       public :: computeTotalKineticEnergy
       public :: exportTransientFull
       public :: computeMomentumstability


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
         type(vectorField) :: U,Ustar
         type(vectorField) :: Unm1,U_CC

         ! type(vectorField) :: vorticity
         type(vectorField) :: temp_F
         type(vectorField) :: temp_E1,temp_E2

         type(scalarField) :: p,divU,temp_CC

         type(scalarField) :: Fo_grid,Co_grid,Re_grid

         type(scalarField) :: KE_adv,KE_diff,KE_pres,KE_transient,KE_jCrossB

         ! Boundary conditions
         type(BCs) :: p_bcs
         type(vectorBCs) :: U_bcs

         type(solverSettings) :: ss_mom,ss_ppe,ss_ADI
         type(multiGrid),dimension(3) :: MG
         type(jacobi) :: Jacobi_p
         type(SORSolver) :: SOR_p
         type(myADI) :: ADI_p,ADI_u
         type(probe) :: KU_energy

         ! Residuals
         type(norms) :: err_PPE,err_DivU,err_ADI
         type(norms),dimension(5) :: e_KE_terms

         ! Time step, Reynolds number, grid
         integer :: nstep,NmaxPPE
         real(cp) :: dTime,t
         real(cp) :: Re,Ha,Gr,Fr
         type(grid) :: g
         real(cp) :: L_eta,U_eta,t_eta ! Kolmogorov Scales

         ! Transient probes
         type(aveProbe) :: u_center,v_center,w_center
         type(errorProbe) :: transient_ppe,transient_divU
         type(avePlaneErrorProbe) :: u_symmetry
       end type

       interface init;                module procedure initMomentum;               end interface
       interface setPiGroups;         module procedure setPiGroupsMomentum;        end interface
       interface delete;              module procedure deleteMomentum;             end interface
       interface solve;               module procedure solveMomentumEquation;      end interface
       interface export;              module procedure momentumExport;             end interface
       interface exportRaw;           module procedure momentumExportRaw;          end interface
       interface exportTransient;     module procedure momentumExportTransient;    end interface
       interface exportTransientFull; module procedure momentumExportTransientFull;end interface
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
         call allocateVectorField(mom%Unm1,mom%U)
         call allocateVectorField(mom%temp_F,mom%U)

         call allocateX(mom%temp_E1,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
         call allocateY(mom%temp_E1,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
         call allocateZ(mom%temp_E1,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)

         call allocateVectorField(mom%temp_E2,mom%temp_E1)

         ! allocate P-Fields
         call allocateField(mom%p,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
         call allocateField(mom%divU,mom%p)
         call allocateVectorField(mom%U_CC,mom%p)
         call allocateField(mom%temp_CC,mom%p)
         call allocateField(mom%Fo_grid,mom%p)
         call allocateField(mom%Co_grid,mom%p)
         call allocateField(mom%Re_grid,mom%p)

         call allocateField(mom%KE_adv,mom%p)
         call allocateField(mom%KE_diff,mom%p)
         call allocateField(mom%KE_pres,mom%p)
         call allocateField(mom%KE_transient,mom%p)
         call allocateField(mom%KE_jCrossB,mom%p)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call initUBCs(mom%U_bcs,mom%p_bcs,g)
         write(*,*) '     BCs initialized'

         ! Use mom%g later, for no just g
         call initUfield(mom%U,mom%p%phi,g,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         if (solveMomentum) call applyAllBCs(mom%U,mom%U_bcs,g)
         if (solveMomentum) call applyAllBCs(mom%p_bcs,mom%p%phi,g)
         write(*,*) '     BCs applied'

         call init(mom%err_DivU)
         call init(mom%err_PPE)

         call init(mom%u_center,dir//'Ufield/','transient_u',&
         .not.restartU,mom%U%sx,(mom%U%sx+1)/2,g,1)

         ! call init(mom%v_center,dir//'Ufield/','transient_v',&
         ! .not.restartU,mom%U%sy,(mom%U%sy+1)/2,g,2)

         ! call init(mom%w_center,dir//'Ufield/','transient_w',&
         ! .not.restartU,mom%U%sz,(mom%U%sz+1)/2,g,3)

         call init(mom%transient_divU,dir//'Ufield/','transient_divU',.not.restartU)
         call init(mom%transient_ppe,dir//'Ufield/','transient_ppe',.not.restartU)
         write(*,*) '     momentum probes initialized'

         call init(mom%u_symmetry,dir//'Ufield/','u_symmetry',&
         .not.restartU,mom%U%sz,(mom%U%sz+1)/2,g,3)
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
         call init(mom%KU_energy,dir//'Ufield\','KU',.not.restartU)
         mom%t = real(0.0,cp)
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine deleteMomentum(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U)
         call delete(mom%Unm1)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%p)
         call delete(mom%temp_CC)

         call delete(mom%p_bcs)
         call delete(mom%divU)
         call delete(mom%U_CC)
         call delete(mom%U_bcs)
         call delete(mom%Fo_grid)
         call delete(mom%Co_grid)
         call delete(mom%Re_grid)

         call delete(mom%KE_adv)
         call delete(mom%KE_diff)
         call delete(mom%KE_pres)
         call delete(mom%KE_transient)
         call delete(mom%KE_jCrossB)

         call delete(mom%u_center);
         call delete(mom%transient_ppe)
         call delete(mom%transient_divU);
         call delete(mom%u_symmetry)
         call delete(mom%g)
         call delete(mom%temp_E1)
         call delete(mom%temp_E2)

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

       subroutine setPiGroupsMomentum(mom,Re,Ha,Gr,Fr)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp),intent(in) :: Re,Ha,Gr,Fr
         mom%Re = Re
         mom%Ha = Ha
         mom%Gr = Gr
         mom%Fr = Fr
         mom%L_eta = Re**real(-3.0/4.0,cp)
         mom%U_eta = Re**real(-1.0/4.0,cp)
         mom%t_eta = Re**real(-1.0/2.0,cp)
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine printExportMomentumBCs(mom,dir)
         implicit none
         type(momentum),intent(in) :: mom
         character(len=*),intent(in) :: dir
         call printVectorBCs(mom%U_bcs,'u','v','w')
         call printAllBoundaries(mom%p_bcs,'p')
         call writeVectorBCs(mom%U_bcs,dir//'parameters/','u','v','w')
         call writeAllBoundaries(mom%p_bcs,dir//'parameters/','p')
       end subroutine

       subroutine momentumExportTransient(mom,ss_MHD,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir

         if (solveMomentum.and.(getExportTransient(ss_MHD))) then
           call apply(mom%u_center,mom%nstep,mom%U%x)
           ! call apply(mom%v_center,mom%nstep,mom%U%y)
           ! call apply(mom%w_center,mom%nstep,mom%U%z)
         endif

         if (solveMomentum.and.getExportErrors(ss_MHD)) then
           call set(mom%transient_ppe,mom%nstep,getL2(mom%err_PPE))
           call apply(mom%transient_ppe)
           ! call export(mom%e_KE_terms,dir//'Ufield/energy/','e_KE_terms'//int2str(mom%nstep),1)

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
         call face2Node(tempNVF,mom%u,g)
         call writeToFile(g,tempNVF,dir//'Ufield/','uni','vni','wni')
         call writeVecPhysical(g,tempNVF,dir//'Ufield/','uni_phys','vni_phys','wni_phys')
         call delete(tempNVF)

         ! ******************** EXPORT IN CELL CENTERS **********************
         ! Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         ! call allocateVectorField(tempCCVF,Nx,Ny,Nz)
         ! call face2CellCenter(tempCCVF,mom%U,g)
         ! call writeToFile(g,tempCCVF,dir//'Ufield/','uci','vci','wci')
         ! call delete(tempCCVF)

         write(*,*) '     finished'
       end subroutine

       subroutine momentumExportTransientFull(mom,g,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         integer :: Nx,Ny,Nz
         type(vectorField) :: tempNVF
         Nx = g%c(1)%sn; Ny = g%c(2)%sn; Nz = g%c(3)%sn
         call allocateVectorField(tempNVF,Nx,Ny,Nz)
         call face2Node(tempNVF,mom%u,g)
         call writeVecPhysicalPlane(g,tempNVF,dir//'Ufield/transient/',&
          'uni_phys',&
          'vni_phys',&
          'wni_phys','_'//int2str(mom%nstep),1,2,mom%nstep)
         call writeScalarPhysicalPlane(g,mom%Re_grid%phi,dir//'Ufield/transient/',&
          'Re_grid','_'//int2str(mom%nstep),1,2,mom%nstep)
         call delete(tempNVF)
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solveMomentumEquation(mom,F,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(vectorField),intent(in) :: F
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir

         select case(solveUMethod)
         case (1); call explicitEuler(mom,F,mom%g,ss_MHD)
         case (2); call semi_implicit_ADI(mom,F,mom%g,ss_MHD)
         case default
         write(*,*) 'Error: solveUMethod must = 1,2 in solveMomentumEquation.';stop
         end select
         mom%t = mom%t + mom%dTime
         mom%nstep = mom%nstep + 1

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%g)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************
         ! call computeKineticEnergy(mom,mom%g,F)
         ! call computeTotalKineticEnergy(mom,ss_MHD)
         ! call computeMomentumStability(mom,ss_MHD)

         if (getExportErrors(ss_MHD)) then
           call computeDivergence(mom,mom%g)
           ! call exportTransientFull(mom,mom%g,dir)
         endif
         if (getExportTransient(ss_MHD)) then
           call momentumExportTransient(mom,ss_MHD,dir)
           call exportTransient(mom,ss_MHD,dir)
         endif

         if (getPrintParams(ss_MHD)) then
           write(*,*) '**************************************************************'
           write(*,*) '************************** MOMENTUM **************************'
           write(*,*) '**************************************************************'
           write(*,*) '(Re,Ha) = ',mom%Re,mom%Ha
           write(*,*) '(Gr,Fr) = ',mom%Gr,mom%Fr
           write(*,*) '(t,dt) = ',mom%t,mom%dTime
           write(*,*) 'Kolmogorov Length = ',mom%L_eta
           write(*,*) 'Kolmogorov Velocity = ',mom%U_eta
           write(*,*) 'Kolmogorov Time = ',mom%t_eta
           ! write(*,'(A29,3E15.6)') ' Kolmogorov scales (L,U,t) = ',mom%L_eta,mom%U_eta,mom%t_eta
           ! write(*,*) '------------------------- GRID INFO --------------------------'
           write(*,*) ''
           write(*,*) 'N_cells = ',(/mom%g%c(1)%N,mom%g%c(2)%N,mom%g%c(3)%N/)
           write(*,*) 'volume = ',mom%g%volume
           write(*,*) 'min/max(h)_x = ',(/mom%g%c(1)%hmin,mom%g%c(1)%hmax/)
           write(*,*) 'min/max(h)_y = ',(/mom%g%c(2)%hmin,mom%g%c(2)%hmax/)
           write(*,*) 'min/max(h)_z = ',(/mom%g%c(3)%hmin,mom%g%c(3)%hmax/)
           write(*,*) 'min/max(dh)_x = ',(/mom%g%c(1)%dhMin,mom%g%c(1)%dhMax/)
           write(*,*) 'min/max(dh)_y = ',(/mom%g%c(2)%dhMin,mom%g%c(2)%dhMax/)
           write(*,*) 'min/max(dh)_z = ',(/mom%g%c(3)%dhMin,mom%g%c(3)%dhMax/)
           write(*,*) 'stretching_x = ',mom%g%c(1)%dhMax-mom%g%c(1)%dhMin
           write(*,*) 'stretching_y = ',mom%g%c(2)%dhMax-mom%g%c(2)%dhMin
           write(*,*) 'stretching_z = ',mom%g%c(3)%dhMax-mom%g%c(3)%dhMin
           ! write(*,*) '------------------------ FIELD INFO --------------------------'
           write(*,*) ''
           call printPhysicalMinMax(mom%U,'u','v','w')
           call printPhysicalMinMax(mom%divU%phi,mom%divU%s,'divU')
           call printPhysicalMinMax(mom%Fo_grid%phi,mom%Fo_grid%s,'Fo_grid')
           call printPhysicalMinMax(mom%Co_grid%phi,mom%Co_grid%s,'Co_grid')
           call printPhysicalMinMax(mom%Re_grid%phi,mom%Re_grid%s,'Re_grid')
           write(*,*) ''
         endif
       end subroutine

       subroutine explicitEuler(mom,F,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(vectorField),intent(in) :: F
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(cp) :: Re,dt
         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation) ! Explicit Euler
         case (1);call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         case (2);call faceAdvect(mom%temp_F,mom%U,mom%U,g)
         case (3);call faceAdvectHybrid(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         end select

         ! if (mom%nstep.gt.0) then ! 2nd order Adams Bashforth
         !   select case (advectiveUFormulation)
         !   case (1);call faceAdvectDonor(mom%Ustar,mom%Unm1,mom%Unm1,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         !   case (2);call faceAdvect(mom%Ustar,mom%Unm1,mom%Unm1,g)
         !   case (3);call faceAdvectHybrid(mom%Ustar,mom%Unm1,mom%Unm1,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         !   end select
         !   call multiply(mom%temp_F,real(3.0/2.0,cp))
         !   call multiply(mom%Ustar,real(1.0/2.0,cp))
         !   call subtract(mom%temp_F,mom%Ustar)
         ! endif

         ! Ustar = -TempVF
         call multiply(mom%temp_F,(-real(1.0,cp)))
         call assign(mom%Ustar,mom%temp_F)

         ! Laplacian Terms -----------------------------------------
         call lap(mom%temp_F,mom%U,g)
         call divide(mom%temp_F,Re)
         call add(mom%Ustar,mom%temp_F)

         ! Pressure Terms ----------------------------------------- ! O(dt^2) pressure treatment
         ! call grad(mom%temp_F,mom%p%phi,g)
         ! call divide(mom%temp_F,real(2.0,cp))
         ! call subtract(mom%Ustar,mom%temp_F)

         ! Source Terms (e.g. N j x B) -----------------------------
         call add(mom%Ustar,F)

         ! Zero wall coincident forcing (may be bad for neumann BCs)
         call zeroWallCoincidentBoundariesVF(mom%Ustar,g)

         ! Solve with explicit Euler --------------------
         ! Ustar = U + dt*Ustar
         call multiply(mom%Ustar,dt)
         call add(mom%Ustar,mom%U)

         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.0) then
           call div(mom%temp_CC%phi,mom%Ustar,g)
           ! Temp = Temp/dt
           call divide(mom%temp_CC,dt) ! O(dt) pressure treatment
           ! call multiply(mom%temp_CC,real(2.0,cp)/dt) ! O(dt^2) pressure treatment
           call zeroGhostPoints(mom%temp_CC%phi)

           ! Solve lap(p) = div(U)/dt
           call poisson(mom%SOR_p,mom%p%phi,mom%temp_CC%phi,mom%p_bcs,g,&
            mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD))

           call grad(mom%temp_F,mom%p%phi,g)
           ! call divide(mom%temp_F,real(2.0,cp)) ! O(dt^2) pressure treatment

           ! Ustar = Ustar - dt*dp/dx
           call multiply(mom%temp_F,dt)
           call subtract(mom%Ustar,mom%temp_F)
         endif

         call assign(mom%Unm1,mom%U) ! store U^n-1

         ! U = Ustar
         call assign(mom%U,mom%Ustar)
         call applyAllBCs(mom%U,mom%U_bcs,g)
       end subroutine

       subroutine semi_implicit_ADI(mom,F,g,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(vectorField),intent(in) :: F
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(cp) :: Re,dt

         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1);call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         case (2);call faceAdvect(mom%temp_F,mom%U,mom%U,g)
         case (3);call faceAdvectHybrid(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         end select

         ! Ustar = -TempVF
         call multiply(mom%temp_F,(-real(1.0,cp)))
         call assign(mom%Ustar,mom%temp_F)

         ! Source Terms (e.g. N j x B) -----------------------------
         ! Ustar = Ustar + F
         call add(mom%Ustar,F)

         ! Solve momentum with ADI --------------------
         ! u_t = 1/Re*(u_xx + u_yy + u_zz) - f
         ! Where f = RHS of NS
         ! mom%Ustar = mom%Ustar + mom%U
         call setDt(mom%ADI_u,dt)
         call setAlpha(mom%ADI_u,real(1.0,cp)/Re)

         call assign(mom%temp_F,mom%Ustar)
         call multiply(mom%temp_F,real(-1.0,cp))

         call apply(mom%ADI_u,mom%U%x,mom%temp_F%x,mom%U_bcs%x,g,&
            mom%ss_ADI,mom%err_ADI,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%U%y,mom%temp_F%y,mom%U_bcs%y,g,&
            mom%ss_ADI,mom%err_ADI,getExportErrors(ss_MHD))
         call apply(mom%ADI_u,mom%U%z,mom%temp_F%z,mom%U_bcs%z,g,&
            mom%ss_ADI,mom%err_ADI,getExportErrors(ss_MHD))

         call assign(mom%Ustar,mom%U)
         
         ! Pressure Correction -------------------------------------
         if (mom%nstep.gt.0) then
           call div(mom%temp_CC%phi,mom%Ustar,g)
           ! Temp = Temp/dt
           call divide(mom%temp_CC,dt)

           ! IMPORTANT: Must include entire pressure since BCs are 
           ! based on last elements (located on boundary)
           call poisson(mom%SOR_p,mom%p%phi,mom%temp_CC%phi,mom%p_bcs,g,&
            mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD))

           call grad(mom%temp_F%x,mom%temp_F%y,mom%temp_F%z,mom%p%phi,g)

           ! Ustar = Ustar - dt*TempVF
           call multiply(mom%temp_F,dt)
           call subtract(mom%Ustar,mom%temp_F)
         endif

         ! U = Ustar
         call assign(mom%U,mom%Ustar)
         call applyAllBCs(mom%U,mom%U_bcs,g)
       end subroutine

       ! ********************* COMPUTE **************************

       subroutine computeKineticEnergy(mom,g,F)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         type(vectorField),intent(in) :: F
         real(cp) :: Re,dt
         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation)
         case (1);call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         case (2);call faceAdvect(mom%temp_F,mom%U,mom%U,g)
         case (3);call faceAdvectHybrid(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         end select

         call multiply(mom%temp_F,mom%U)
         call face2CellCenter(mom%U_CC,mom%temp_F,g)
         call sum(mom%KE_adv,mom%U_CC)

         ! Laplacian Terms -----------------------------------------
         call lap(mom%temp_F,mom%U,g)
         call divide(mom%temp_F,Re)
         call multiply(mom%temp_F,mom%U)
         call face2CellCenter(mom%U_CC,mom%temp_F,g)
         call sum(mom%KE_diff,mom%U_CC)

         ! Source Terms (e.g. N j x B) -----------------------------
         call assign(mom%temp_F,F)
         call multiply(mom%temp_F,mom%U)
         call face2CellCenter(mom%U_CC,mom%temp_F,g)
         call sum(mom%KE_jCrossB,mom%U_CC)

         ! Solve with explicit Euler --------------------
         call assign(mom%temp_F,mom%U)
         call square(mom%temp_F)
         call face2CellCenter(mom%U_CC,mom%temp_F,g)
         call sum(mom%KE_transient,mom%U_CC)

         call assign(mom%temp_F,mom%Unm1)
         call square(mom%temp_F)
         call face2CellCenter(mom%U_CC,mom%temp_F,g)
         call sum(mom%KE_pres,mom%U_CC)
         call subtract(mom%KE_transient,mom%KE_pres)

         call divide(mom%KE_transient,real(2.0,cp)*mom%dTime)

         ! Pressure Correction -------------------------------------
         call grad(mom%temp_F,mom%p%phi,g)
         call multiply(mom%temp_F,mom%U)
         call face2CellCenter(mom%U_CC,mom%temp_F,g)
         call sum(mom%KE_pres,mom%U_CC)
         ! Norms
         call compute(mom%e_KE_terms(1),mom%KE_adv%phi)
         call compute(mom%e_KE_terms(2),mom%KE_diff%phi)
         call compute(mom%e_KE_terms(3),mom%KE_jCrossB%phi)
         call compute(mom%e_KE_terms(4),mom%KE_pres%phi)
         call compute(mom%e_KE_terms(5),mom%KE_transient%phi)
       end subroutine

       subroutine computeTotalKineticEnergy(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
         if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
          call totalEnergy(K_energy,mom%U_CC,mom%g)
          call set(mom%KU_energy,mom%nstep,K_energy)
          call apply(mom%KU_energy)
         endif
       end subroutine

       subroutine computeMomentumStability(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
          if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
           call face2CellCenter(mom%U_CC,mom%U,mom%g)

           call stabilityTerms(mom%Co_grid,mom%U_CC,mom%g,1)
           call multiply(mom%Co_grid,mom%dTime)

           call stabilityTerms(mom%Fo_grid,mom%U_CC,mom%g,2)
           call multiply(mom%Fo_grid,mom%dTime)

           call stabilityTerms(mom%Re_grid,mom%U_CC,mom%g,-1)
           call multiply(mom%Re_grid,mom%Re)
          endif
       end subroutine

       subroutine computeDivergenceMomentum(mom,g)
         implicit none
         type(momentum),intent(inout) :: mom
         type(grid),intent(in) :: g
         call div(mom%divU%phi,mom%U%x,mom%U%y,mom%U%z,g)
         call zeroGhostPoints(mom%divU%phi)
       end subroutine

       ! ********************* AUX *****************************

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
         case (-1)
           if (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = real(0.0,cp); f(:,s(2),:) = real(0.0,cp)
             f(:,2,:) = real(0.0,cp); f(:,s(2)-1,:) = real(0.0,cp)
           endif
           if (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = real(0.0,cp); f(:,:,s(3)) = real(0.0,cp)
             f(:,:,2) = real(0.0,cp); f(:,:,s(3)-1) = real(0.0,cp)
           endif
         case (-2)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = real(0.0,cp); f(s(1),:,:) = real(0.0,cp)
             f(2,:,:) = real(0.0,cp); f(s(1)-1,:,:) = real(0.0,cp)
           endif
           if (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = real(0.0,cp); f(:,:,s(3)) = real(0.0,cp)
             f(:,:,2) = real(0.0,cp); f(:,:,s(3)-1) = real(0.0,cp)
           endif
         case (-3)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = real(0.0,cp); f(s(1),:,:) = real(0.0,cp)
             f(2,:,:) = real(0.0,cp); f(s(1)-1,:,:) = real(0.0,cp)
           endif
           if (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = real(0.0,cp); f(:,s(2),:) = real(0.0,cp)
             f(:,2,:) = real(0.0,cp); f(:,s(2)-1,:) = real(0.0,cp)
           endif
         case default
           stop 'Error: dir must = 0,1,2,3 in zeroWallCoincidentBoundaries'
         end select
       end subroutine

       subroutine zeroWallCoincidentBoundariesVF(f,g)
         implicit none
         type(vectorField),intent(inout) :: f
         type(grid),intent(in) :: g
         call zeroWallCoincidentBoundaries(f%x,f%sx,g,-3)
         call zeroWallCoincidentBoundaries(f%y,f%sy,g,-3)
         call zeroWallCoincidentBoundaries(f%z,f%sz,g,-3)
       end subroutine

       subroutine computeTotalKineticEnergyOld(mom,U_cct,Nici1,Nici2,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(vectorField),intent(in) :: U_cct
         integer,dimension(3),intent(in) :: Nici1,Nici2
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
          if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
           ! call totalEnergy(K_energy,ind%U_cct,ind%g) ! Sergey uses interior...
           call totalEnergy(K_energy,&
             U_cct%x(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             U_cct%y(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             U_cct%z(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)),&
             mom%g)
           call set(mom%KU_energy,mom%nstep,K_energy)
           call apply(mom%KU_energy)
          endif
       end subroutine


       end module