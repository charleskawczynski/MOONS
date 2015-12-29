       module momentumSolver_mod
       use simParams_mod
       
       use BCs_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod

       use init_PBCs_mod
       use init_UBCs_mod
       use init_UField_mod
       use init_PField_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use momentum_solver_mod

       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod

       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod

       use apply_BCs_mod
       use apply_stitches_mod

       use solverSettings_mod
       use myTime_mod

       ! use jacobi_mod
       use FFT_poisson_mod
       use PSE_mod
       use SOR_mod
       use CG_mod
       use GS_Poisson_mod
       ! use ADI_mod
       ! use MG_mod

       use probe_base_mod
       use probe_transient_mod
       use probe_derived_mod
       use export_raw_processed_mod
       
       implicit none
       private
       
       public :: momentum,init,delete,solve
       public :: setDTime,setNmaxPPE
       public :: setPiGroups
       public :: export,exportTransient
       public :: printExportBCs
       public :: computeDivergence
       ! public :: computeKineticEnergy
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
         type(VF) :: U,Ustar
         type(VF) :: Unm1,U_CC
         ! type(VF) :: vorticity
         type(VF) :: temp_F
         type(TF) :: U_E
         type(VF) :: temp_E1,temp_E2
         type(SF) :: p,divU,temp_CC
         type(SF) :: temp_CC2,temp_CC3
         type(SF) :: Fo_grid,Co_grid,Re_grid
         type(SF) :: KE_adv,KE_diff,KE_pres,KE_transient,KE_jCrossB

         type(solverSettings) :: ss_mom,ss_ppe,ss_ADI
         ! type(multiGrid),dimension(3) :: MG
         type(SORSolver) :: SOR_p
         type(GS_Poisson) :: GS_p
         type(CG_Solver_SF) :: CG_P
         type(CG_Solver_VF) :: CG_U
         type(FFTSolver) :: FFT_p
         type(matrix_free_params) :: MFP
         ! type(myADI) :: ADI_p,ADI_u
         type(probe) :: KU_energy

         ! Residuals
         type(norms) :: err_PPE,err_DivU,err_ADI
         type(norms),dimension(5) :: e_KE_terms

         ! Time step, Reynolds number, grid
         integer :: nstep,NmaxPPE
         real(cp) :: dTime,t
         real(cp) :: Re,Ha,Gr,Fr
         type(mesh) :: m
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
       interface export;              module procedure export_momentum;            end interface
       interface exportTransient;     module procedure momentumExportTransient;    end interface
       interface exportTransientFull; module procedure momentumExportTransientFull;end interface
       interface printExportBCs;      module procedure printExportMomentumBCs;     end interface
       interface computeDivergence;   module procedure computeDivergenceMomentum;  end interface

       interface setDTime;            module procedure setDTimeMomentum;           end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initMomentum(mom,m,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing momentum:'

         call init(mom%m,m)

         call init_Edge(mom%U_E,m,0.0_cp)
         call init_Face(mom%U,m,0.0_cp)
         call init_Face(mom%Ustar,m,0.0_cp)
         call init_Face(mom%Unm1,m,0.0_cp)
         call init_Face(mom%temp_F,m,0.0_cp)
         call init_Edge(mom%temp_E1,m,0.0_cp)
         call init_Edge(mom%temp_E2,m,0.0_cp)
         call init_CC(mom%p,m,0.0_cp)
         call init_CC(mom%divU,m,0.0_cp)
         call init_CC(mom%U_CC,m,0.0_cp)
         call init_CC(mom%temp_CC,m,0.0_cp)
         call init_CC(mom%temp_CC2,m,0.0_cp)
         call init_CC(mom%temp_CC3,m,0.0_cp)
         call init_CC(mom%Fo_grid,m,0.0_cp)
         call init_CC(mom%Co_grid,m,0.0_cp)
         call init_CC(mom%Re_grid,m,0.0_cp)
         call init_CC(mom%KE_adv,m,0.0_cp)
         call init_CC(mom%KE_diff,m,0.0_cp)
         call init_CC(mom%KE_pres,m,0.0_cp)
         call init_CC(mom%KE_transient,m,0.0_cp)
         call init_CC(mom%KE_jCrossB,m,0.0_cp)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call init_UBCs(mom%U,m)
         call init_PBCs(mom%p,m)
         write(*,*) '     BCs initialized'

         ! Use mom%m later, for no just m
         call init_Ufield(mom%U,m,dir)
         call init_Pfield(mom%p,m,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         if (solveMomentum) call apply_BCs(mom%U,m)
         write(*,*) '     U BCs applied'
         if (solveMomentum) call apply_BCs(mom%p,m)
         write(*,*) '     P BCs applied'

         call init(mom%err_DivU)
         call init(mom%err_PPE)

         call init(mom%u_center,dir//'Ufield/','transient_u',&
         .not.restartU,mom%U%x%RF(1)%s,(mom%U%x%RF(1)%s+1)/2,m,1)

         call init(mom%v_center,dir//'Ufield/','transient_v',&
         .not.restartU,mom%U%y%RF(1)%s,(mom%U%y%RF(1)%s+1)/2,m,2)

         call init(mom%w_center,dir//'Ufield/','transient_w',&
         .not.restartU,mom%U%z%RF(1)%s,(mom%U%z%RF(1)%s+1)/2,m,3)

         call init(mom%transient_divU,dir//'Ufield/','transient_divU',.not.restartU)
         call init(mom%transient_ppe,dir//'Ufield/','transient_ppe',.not.restartU)
         write(*,*) '     momentum probes initialized'

         call init(mom%u_symmetry,dir//'Ufield/','u_symmetry',&
         .not.restartU,mom%U%z%RF(1)%s,(mom%U%z%RF(1)%s+1)/2,m,3)
         write(*,*) '     momentum probes initialized'

         call export(mom%u_center)
         call export(mom%v_center)
         call export(mom%w_center)
         call export(mom%transient_ppe)
         call export(mom%transient_divU)
         call export(mom%u_symmetry)

         write(*,*) '     momentum probes initialized'


         ! Initialize interior solvers
         call init(mom%SOR_p,mom%p,mom%m)

         call init(mom%GS_p,mom%p,mom%m,dir//'Ufield/','p')

         call init(mom%CG_P,Lap_uniform_props,Lap_uniform_props_explicit,mom%m,&
         mom%MFP,mom%p,mom%temp_F,dir//'Ufield/','p',.false.,.false.)

         mom%MFP%c_mom = -0.5_cp*mom%dTime/mom%Re

         call init(mom%CG_U,mom_diffusion,mom_diffusion_explicit,mom%m,&
         mom%MFP,mom%U,mom%U_CC,dir//'Ufield/','U',.false.,.false.)
         write(*,*) '     momentum SOR initialized'

         ! Initialize solver settings
         call init(mom%ss_ppe)
         call setName(mom%ss_ppe,'pressure poisson    ')
         call setMaxIterations(mom%ss_ppe,mom%NmaxPPE)
         ! call setSubtractMean(mom%ss_ppe)

         ! Init ADI ss
         ! call init(mom%ss_ADI)
         ! call setName(mom%ss_ADI,'momentum ADI        ')

         ! Init Multigrid solver
         ! call init(mom%MG,mom%p%s,mom%p_bcs,mom%m,mom%ss_ppe,.false.)
         ! call setMaxIterations(mom%ss_ADI,1) ! Not needed since apply() is used.

         ! call setMinTolerance(mom%ss_ppe,real(1.0**(-6.0),cp))
         ! call setMixedConditions(mom%ss_ppe)
         if (restartU) then
         call readLastStepFromFile(mom%nstep,dir//'parameters/','n_mom')
         else; mom%nstep = 0
         endif
         call init(mom%KU_energy,dir//'Ufield\','KU',.not.restartU)

         call momentumInfo(mom,newAndOpen(dir//'parameters/','info_mom'))
         mom%t = 0.0_cp
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine deleteMomentum(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U_E)
         call delete(mom%U)
         call delete(mom%Unm1)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%temp_CC2)
         call delete(mom%temp_CC3)

         call delete(mom%divU)
         call delete(mom%U_CC)
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
         call delete(mom%temp_E1)
         call delete(mom%temp_E2)
         call delete(mom%m)

         call delete(mom%SOR_p)
         call delete(mom%CG_P)
         call delete(mom%CG_U)
         call delete(mom%GS_p)
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
         mom%L_eta = Re**(-3.0_cp/4.0_cp)
         mom%U_eta = Re**(-1.0_cp/4.0_cp)
         mom%t_eta = Re**(-1.0_cp/2.0_cp)
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine printExportMomentumBCs(mom,dir)
         implicit none
         type(momentum),intent(in) :: mom
         character(len=*),intent(in) :: dir
         ! if (solveMomentum) call print_BCs(mom%U,'U')
         if (solveMomentum) call export_BCs(mom%U,dir//'parameters/','U')
         ! if (solveMomentum) call print_BCs(mom%p,'p')
         if (solveMomentum) call export_BCs(mom%p,dir//'parameters/','p')
       end subroutine

       subroutine momentumExportTransient(mom,ss_MHD,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir

         if (solveMomentum.and.(getExportTransient(ss_MHD))) then
           call apply(mom%u_center,mom%nstep,mom%U%x%RF(1)%f)
           call apply(mom%v_center,mom%nstep,mom%U%y%RF(1)%f)
           call apply(mom%w_center,mom%nstep,mom%U%z%RF(1)%f)
         endif

         if (solveMomentum.and.getExportErrors(ss_MHD)) then
           call set(mom%transient_ppe,mom%nstep,mom%err_PPE%L2)
           call apply(mom%transient_ppe)
           ! call export(mom%e_KE_terms,dir//'Ufield/energy/','e_KE_terms'//int2str(mom%nstep),1)
           call apply(mom%transient_divU,mom%nstep,mom%divU,mom%m)
           call apply(mom%u_symmetry,mom%nstep,mom%U%z)
         endif
       end subroutine

       subroutine export_momentum(mom,m,F,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: F
         character(len=*),intent(in) :: dir
         if (restartU.and.(.not.solveMomentum)) then
           ! This preserves the initial data
         else
           write(*,*) 'Exporting Solutions for U'
           call export_raw(m,mom%U,dir//'Ufield/','U',0)
           call export_raw(m,mom%p,dir//'Ufield/','p',0)
           call export_raw(m,F,dir//'Ufield/','jCrossB',0)
           call export_raw(m,mom%divU,dir//'Ufield/','divU',0)
           call export_processed(m,mom%U,dir//'Ufield/','U',1)
           call export_processed(m,mom%p,dir//'Ufield/','p',1)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine momentumInfo(mom,un)
         ! Use un = 6 to print to screen
         implicit none
         type(momentum),intent(in) :: mom
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MOMENTUM **************************'
         write(un,*) '**************************************************************'
         write(un,*) '(Re,Ha) = ',mom%Re,mom%Ha
         write(un,*) '(Gr,Fr) = ',mom%Gr,mom%Fr
         write(un,*) '(t,dt) = ',mom%t,mom%dTime
         write(un,*) '(nstep) = ',mom%nstep
         write(un,*) 'Kolmogorov Length = ',mom%L_eta
         write(un,*) 'Kolmogorov Velocity = ',mom%U_eta
         write(un,*) 'Kolmogorov Time = ',mom%t_eta
         write(un,*) ''
         call export(mom%m,un)
         write(un,*) ''
         call printPhysicalMinMax(mom%U,'u')
         call printPhysicalMinMax(mom%p,'p')
         call printPhysicalMinMax(mom%divU,'divU')
         ! call printPhysicalMinMax(mom%Fo_grid,'Fo_grid')
         ! call printPhysicalMinMax(mom%Co_grid,'Co_grid')
         ! call printPhysicalMinMax(mom%Re_grid,'Re_grid')
         write(*,*) ''
       end subroutine

       subroutine momentumExportTransientFull(mom,m,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(VF) :: tempNVF

         call init_Node(tempNVF,m)
         call face2Node(tempNVF,mom%U,m,mom%temp_E1)
         call export_2D_2C(mom%m,tempNVF,dir//'Ufield/transient/','Uni_phys',1,mom%nstep)
         call delete(tempNVF)
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solveMomentumEquation(mom,F,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow
         select case(solveUMethod)
         case (1); call explicitEuler(mom,F,mom%m,ss_MHD)
         case (2); 

         ! call Euler_CG_Donor(mom%CG_P,mom%U,mom%p,F,mom%U_CC,mom%m,mom%Re,mom%dTime,mom%NmaxPPE,&
         ! mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E1,mom%temp_E2,getExportErrors(ss_MHD))

         ! call CN_AB2_PPE_CG_mom_CG(mom_CG,PPE_CG,U,Unm1,p,F,Fnm1,U_CC,m,&
         ! Re,dt,n,Ustar,temp_F,temp_CC,temp_E1,temp_E2,compute_norms)

         ! call CN_AB2_PPE_CG_mom_CG(mom%CG_U,mom%CG_p,mom%U,mom%Unm1,&
         ! mom%p,F,F,mom%U_CC,mom%m,mom%Re,mom%dTime,1000,100,mom%Ustar,&
         ! mom%temp_F,mom%temp_CC,mom%temp_E1,mom%temp_E2,getExportErrors(ss_MHD),mom%nstep)

         call CN_AB2_PPE_GS_mom_CG(mom%CG_U,mom%GS_p,mom%U,mom%Unm1,&
         mom%p,F,F,mom%U_CC,mom%m,mom%Re,mom%dTime,5,100,mom%Ustar,&
         mom%temp_F,mom%temp_CC,mom%temp_E1,mom%temp_E2,getExportErrors(ss_MHD),mom%nstep)

         ! case (2); call semi_implicit_ADI(mom,F,mom%m,ss_MHD)
         case default
         stop 'Error: solveUMethod must = 1,2 in solveMomentumEquation.'
         end select
         mom%t = mom%t + mom%dTime
         mom%nstep = mom%nstep + 1

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge(mom%U_E,mom%U,mom%m,mom%temp_CC,mom%temp_F)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************
         call computeTotalKineticEnergy(mom,ss_MHD)

         ! call computeKineticEnergy(mom,mom%m,F)
         ! call computeMomentumStability(mom,ss_MHD)
         if (getExportErrors(ss_MHD)) call computeDivergence(mom,mom%m)
         ! if (getExportErrors(ss_MHD)) call exportTransientFull(mom,mom%m,dir)
         if (getExportTransient(ss_MHD)) call exportTransient(mom,ss_MHD,dir)

         if (getPrintParams(ss_MHD)) then
           call momentumInfo(mom,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowU')
         else; exportNow = .false.
         endif

         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(mom,mom%m,F,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowU')
         endif
       end subroutine

       subroutine explicitEuler(mom,F,m,ss_MHD)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(mesh),intent(in) :: m
         type(solverSettings),intent(in) :: ss_MHD
         ! ********************** LOCAL VARIABLES ***********************
         real(cp) :: Re,dt
         logical :: TF
         TF = mom%nstep.lt.1
         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation) ! Explicit Euler
         case (1); call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,m)
         case (2); call faceAdvectNew(mom%temp_F,mom%U,mom%U,m)
         end select

         ! Ustar = -TempVF
         ! call multiply(mom%Ustar,mom%temp_F,-1.0_cp)
         call assign(mom%Ustar,mom%temp_F)
         call multiply(mom%Ustar,-1.0_cp)
         ! call printPhysicalMinMax(mom%temp_F,'advect')
         ! if (TF) call export_2D_1C(mom%m,mom%Ustar%x,'out/LDC/Ufield/','advect_x',0,3)
         ! if (TF) call export_2D_1C(mom%m,mom%Ustar%y,'out/LDC/Ufield/','advect_y',0,3)

         ! Laplacian Terms -----------------------------------------
         call lap(mom%temp_F,mom%U,m)
         call multiply(mom%temp_F,1.0_cp/Re)
         ! call printPhysicalMinMax(mom%temp_F,'diff')
         ! if (TF) call export_3D_1C(mom%m,mom%temp_F%x,'out/LDC/Ufield/','diffuse_x',0)
         ! if (TF) call export_2D_1C(mom%m,mom%temp_F%y,'out/LDC/Ufield/','diffuse_y',0,3)
         call add(mom%Ustar,mom%temp_F)

         ! Source Terms (e.m. N j x B) -----------------------------
         call add(mom%Ustar,F)
         ! call printPhysicalMinMax(F,'jcrossB')

         ! Zero wall coincident forcing (may be bad for neumann BCs)
         ! if (TF) call export_3D_1C(mom%m,mom%Ustar%x,'out/LDC/Ufield/','U_before_ZWC',0)
         call zeroWall_conditional(mom%Ustar,m,mom%U)

         ! if (TF) call export_3D_1C(mom%m,mom%Ustar%x,'out/LDC/Ufield/','U_after_ZWC',0)
         ! Solve with explicit Euler --------------------
         ! Ustar = U + dt*Ustar
         call multiply(mom%Ustar,dt)
         call add(mom%Ustar,mom%U)
         ! call printPhysicalMinMax(mom%Ustar,'Unm1')

         ! Apply intermediate BCs and stitching
         call apply_stitches(mom%Ustar,m)
         call apply_BCs(mom%Ustar,m,mom%U)
         ! if (TF) call export_3D_1C(mom%m,mom%Ustar%x,'out/LDC/Ufield/','U_before_div',0)

         ! Pressure Correction -------------------------------------
         call div(mom%temp_CC,mom%Ustar,m)
         ! Temp = Temp/dt
         call multiply(mom%temp_CC,1.0_cp/dt) ! O(dt) pressure treatment
         ! call apply_BCs(mom%p_bcs,mom%temp_CC,m)
         call zeroGhostPoints(mom%temp_CC)
         ! call printPhysicalMinMax(mom%temp_CC,'PPE_input')

         call apply_stitches(mom%temp_CC,m)
         ! if (TF) call export_3D_1C(mom%m,mom%temp_CC,'out/LDC/Ufield/','PPE_input',0)
         ! if (TF) call export_3D_1C(mom%m,mom%temp_CC,'out/LDC/Ufield/','PPE_input_full',0)
         ! if (TF) call export_3D_1C(mom%m,mom%p,'out/LDC/Ufield/','p_before',0)

         ! Solve lap(p) = div(U)/dt
         ! call solve(mom%SOR_p,mom%p,mom%temp_CC,m,&
         !  mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD))

         call solve(mom%CG_p,mom%p,mom%temp_CC,m,&
         mom%NmaxPPE,getExportErrors(ss_MHD))

         ! call solve_PSE(mom%p,mom%temp_CC,m,5,0.00005_cp,&
         !  mom%err_PPE,getExportErrors(ss_MHD),mom%temp_CC2,mom%temp_CC3)
         ! call solve(mom%FFT_p,mom%p,mom%temp_CC,m,&
         !  mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD),3)
         ! if (TF) call export_2D_1C(mom%m,mom%p,'out/LDC/Ufield/','p_solution',0,3)
         ! if (TF) call export_3D_1C(mom%m,mom%p,'out/LDC/Ufield/','p_solution',0)
         ! if (TF) stop 'DONE'

         call grad(mom%temp_F,mom%p,m)
         ! if (TF) call export_2D_1C(mom%m,mom%temp_F%x,'out/LDC/Ufield/','gradP_x',0,3)
         ! call addMeanPressureGrad(mom%temp_F,real(52.0833,cp),1) ! Shercliff Flow
         ! call addMeanPressureGrad(mom%temp_F,real(1.0,cp),1) ! Bandaru
         ! call printPhysicalMinMax(mom%temp_F,'grad(p)')
         
         ! write(*,*) 'nstep = ',mom%nstep
         ! if (mom%nstep.eq.2) stop 'Done'
         ! U = Ustar - dt*dp/dx
         call multiply(mom%temp_F,dt)
         call subtract(mom%U,mom%Ustar,mom%temp_F)

         ! call applyAllGhostBCs(mom%U,m)
         ! call apply_stitches(mom%U,m) ! Needed? or ruining things?
         call apply_BCs(mom%U,m)
         ! if (TF) call export_2D_1C(mom%m,mom%U%x,'out/LDC/Ufield/','unp1_x',0,3)
         ! if (TF) stop 'Done'
       end subroutine

       ! ********************* COMPUTE **************************

!        subroutine computeKineticEnergy(mom,m,F)
!          implicit none
!          type(momentum),intent(inout) :: mom
!          type(mesh),intent(in) :: m
!          type(VF),intent(in) :: F
!          real(cp) :: Re,dt
!          dt = mom%dTime
!          Re = mom%Re

!          ! Advection Terms -----------------------------------------
!          select case (advectiveUFormulation)
!          case (1);call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,m)
!          case (2);call faceAdvect(mom%temp_F,mom%U,mom%U,m)
!          case (3);call faceAdvectHybrid(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,m)
!          end select

!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_adv,mom%U_CC)

!          ! Laplacian Terms -----------------------------------------
!          call lap(mom%temp_F,mom%U,m)
!          call divide(mom%temp_F,Re)
!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_diff,mom%U_CC)

!          ! Source Terms (e.m. N j x B) -----------------------------
!          call assign(mom%temp_F,F)
!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_jCrossB,mom%U_CC)

!          ! Solve with explicit Euler --------------------
!          call assign(mom%temp_F,mom%U)
!          call square(mom%temp_F)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_transient,mom%U_CC)

!          call assign(mom%temp_F,mom%Unm1)
!          call square(mom%temp_F)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_pres,mom%U_CC)
!          call subtract(mom%KE_transient,mom%KE_pres)

!          call divide(mom%KE_transient,2.0_cp*mom%dTime)

!          ! Pressure Correction -------------------------------------
!          call grad(mom%temp_F,mom%p,m)
!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_pres,mom%U_CC)
!          ! Norms
!          call compute(mom%e_KE_terms(1),mom%KE_adv)
!          call compute(mom%e_KE_terms(2),mom%KE_diff)
!          call compute(mom%e_KE_terms(3),mom%KE_jCrossB)
!          call compute(mom%e_KE_terms(4),mom%KE_pres)
!          call compute(mom%e_KE_terms(5),mom%KE_transient)
!        end subroutine

       subroutine computeTotalKineticEnergy(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
         if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
          call totalEnergy(K_energy,mom%U_CC,mom%m)
          call set(mom%KU_energy,mom%nstep,K_energy)
          call apply(mom%KU_energy)
         endif
       end subroutine

       subroutine computeMomentumStability(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
          if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
           call face2CellCenter(mom%U_CC,mom%U,mom%m)

           call stabilityTerms(mom%Co_grid,mom%U_CC,mom%m,1)
           call multiply(mom%Co_grid,mom%dTime)

           call stabilityTerms(mom%Fo_grid,mom%U_CC,mom%m,2)
           call multiply(mom%Fo_grid,mom%dTime)

           call stabilityTerms(mom%Re_grid,mom%U_CC,mom%m,-1)
           call multiply(mom%Re_grid,mom%Re)
          endif
       end subroutine

       subroutine computeDivergenceMomentum(mom,m)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         call div(mom%divU,mom%U,m)
         call zeroGhostPoints(mom%divU)
       end subroutine

       ! ********************* AUX *****************************

       subroutine addMeanPressureGrad(f,mpg,dir)
         implicit none
         type(VF),intent(inout) :: f
         real(cp),intent(in) :: mpg
         integer,intent(in) :: dir
         select case (dir)
         case (1); call subtract(f%x,mpg)
         case (2); call subtract(f%y,mpg)
         case (3); call subtract(f%z,mpg)
         case default
         stop 'Error: dir must = 1,2,3 in addMeanPressureGrad in momentumSolver.f90'
         end select
       end subroutine

       end module