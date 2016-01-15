       module momentum_mod
       use simParams_mod
       
       use BCs_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod

       use momentum_solver_mod
       use momentum_aux_mod
       use init_PBCs_mod
       use init_UBCs_mod
       use init_UField_mod
       use init_PField_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod

       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod
       use export_raw_processed_mod

       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod

       use apply_BCs_mod
       use apply_stitches_mod

       use solverSettings_mod

       ! use jacobi_mod
       use PCG_mod
       use preconditioners_mod
       use GS_Poisson_mod

       use probe_base_mod
       use probe_transient_mod
       use probe_derived_mod
       
       implicit none
       private
       
       public :: momentum,init,delete,solve
       public :: export,exportTransient

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
         ! Tensor fields
         type(TF) :: U_E
         ! Vector fields
         type(VF) :: U,Ustar
         type(VF) :: Unm1,U_CC
         type(VF) :: temp_F
         type(VF) :: temp_E
         ! Scalar fields
         type(SF) :: p,divU,temp_CC
         type(SF) :: Fo_grid,Co_grid,Re_grid
         type(SF) :: KE_adv,KE_diff,KE_pres,KE_transient,KE_jCrossB

         type(GS_Poisson) :: GS_p

         type(matrix_free_params) :: MFP
         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U

         ! Time step, Reynolds number, grid
         type(mesh) :: m
         integer :: N_PPE,N_mom
         real(cp) :: tol_PPE,tol_mom
         integer :: nstep
         real(cp) :: dTime,t
         real(cp) :: Re,Ha,Gr,Fr
         real(cp) :: L_eta,U_eta,t_eta ! Kolmogorov Scales

         ! Transient probes
         real(cp) :: KE
         type(norms) :: norm_divU
         type(probe) :: transient_KE,transient_divU
       end type

       interface init;                module procedure initMomentum;               end interface
       interface delete;              module procedure deleteMomentum;             end interface
       interface solve;               module procedure solve_momentum;             end interface
       interface export;              module procedure export_momentum;            end interface
       interface exportTransient;     module procedure momentumExportTransient;    end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initMomentum(mom,m,N_mom,tol_mom,N_PPE,tol_PPE,dt,Re,Ha,Gr,Fr,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_mom,N_PPE
         real(cp),intent(in) :: tol_mom,tol_PPE
         real(cp),intent(in) :: dt,Re,Ha,Gr,Fr
         character(len=*),intent(in) :: dir
         type(SF) :: prec_PPE
         type(VF) :: prec_mom
         write(*,*) 'Initializing momentum:'

         mom%dTime = dt
         mom%N_mom = N_mom
         mom%N_PPE = N_PPE
         mom%tol_mom = tol_mom
         mom%tol_PPE = tol_PPE
         mom%Re = Re
         mom%Ha = Ha
         mom%Gr = Gr
         mom%Fr = Fr
         mom%L_eta = Re**(-3.0_cp/4.0_cp)
         mom%U_eta = Re**(-1.0_cp/4.0_cp)
         mom%t_eta = Re**(-1.0_cp/2.0_cp)
         mom%KE = 0.0_cp

         call init(mom%m,m)
         call init_Edge(mom%U_E,m,0.0_cp)
         call init_Face(mom%U,m,0.0_cp)
         call init_Face(mom%Ustar,m,0.0_cp)
         call init_Face(mom%Unm1,m,0.0_cp)
         call init_Face(mom%temp_F,m,0.0_cp)
         call init_Edge(mom%temp_E,m,0.0_cp)
         call init_CC(mom%p,m,0.0_cp)
         call init_CC(mom%divU,m,0.0_cp)
         call init_CC(mom%U_CC,m,0.0_cp)
         call init_CC(mom%temp_CC,m,0.0_cp)
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
         ! if (solveMomentum) call print_BCs(mom%U,'U')
         if (solveMomentum) call export_BCs(mom%U,dir//'parameters/','U')
         ! if (solveMomentum) call print_BCs(mom%p,'p')
         if (solveMomentum) call export_BCs(mom%p,dir//'parameters/','p')

         ! Use mom%m later, for no just m
         call init_Ufield(mom%U,m,dir)
         call init_Pfield(mom%p,m,dir)
         write(*,*) '     Field initialized'
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         if (solveMomentum) call apply_BCs(mom%U,m)
         write(*,*) '     U BCs applied'
         if (solveMomentum) call apply_BCs(mom%p,m)
         write(*,*) '     P BCs applied'

         call init(mom%transient_divU,dir//'Ufield/','transient_divU',.not.restartU)
         call export(mom%transient_divU)

         write(*,*) '     momentum probes initialized'

         ! Initialize interior solvers
         call init(mom%GS_p,mom%p,mom%m,dir//'Ufield/','p')
         write(*,*) '     momentum GS_p'

         mom%MFP%c_mom = -0.5_cp*mom%dTime/mom%Re

         call init(prec_mom,mom%U)
         call prec_lap_VF(prec_mom,mom%m,mom%MFP%c_mom)
         ! call prec_identity_VF(prec_mom) ! For ordinary CG
         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom,mom%m,&
         mom%tol_mom,mom%MFP,mom%U,mom%temp_E,dir//'Ufield/','U',.false.,.false.)
         call delete(prec_mom)
         write(*,*) '     momentum PCG_U'

         call init(prec_PPE,mom%p)
         call prec_lap_SF(prec_PPE,mom%m,1.0_cp)
         call init(mom%PCG_P,Lap_uniform_props,Lap_uniform_props_explicit,prec_PPE,mom%m,&
         mom%tol_PPE,mom%MFP,mom%p,mom%temp_F,dir//'Ufield/','p',.false.,.false.)
         call delete(prec_PPE)
         write(*,*) '     momentum PCG_P'

         if (restartU) then
         call readLastStepFromFile(mom%nstep,dir//'parameters/','n_mom')
         else; mom%nstep = 0
         endif
         call init(mom%transient_KE,dir//'Ufield\','KU',.not.restartU)

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

         call delete(mom%transient_divU);
         call delete(mom%temp_E)
         call delete(mom%m)

         call delete(mom%PCG_P)
         call delete(mom%PCG_U)
         call delete(mom%GS_p)
         write(*,*) 'Momentum object deleted'
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine momentumExportTransient(mom,ss_MHD,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         if (solveMomentum.and.getExportErrors(ss_MHD)) then
           call compute_TKE(mom%KE,mom%U_CC,mom%m)
           call set(mom%transient_KE,mom%nstep,mom%KE)
           call apply(mom%transient_KE)
           call compute(mom%norm_divU,mom%divU,mom%m)
           call set(mom%transient_divU,mom%nstep,mom%norm_divU%L2)
           call apply(mom%transient_divU)
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
         write(un,*) '(solveUMethod,N_mom,N_PPE) = ',solveUMethod,mom%N_mom,mom%N_PPE
         write(un,*) 'nstep = ',mom%nstep
         write(un,*) 'Kolmogorov Length = ',mom%L_eta
         write(un,*) 'Kolmogorov Velocity = ',mom%U_eta
         write(un,*) 'Kolmogorov Time = ',mom%t_eta
         write(un,*) ''
         call export(mom%m,un)
         write(un,*) 'KE = ',mom%KE
         ! call printPhysicalMinMax(mom%U,'u')
         ! call printPhysicalMinMax(mom%p,'p')
         call printPhysicalMinMax(mom%divU,'divU')
         write(*,*) ''
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,F,ss_MHD,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow
         select case(solveUMethod)
         case (1)
           call Euler_PCG_Donor(mom%PCG_P,mom%U,mom%U_E,mom%p,F,mom%m,mom%Re,mom%dTime,&
           mom%N_PPE,mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E,getExportErrors(ss_MHD))

         case (2)
           call Euler_GS_Donor(mom%GS_p,mom%U,mom%U_E,mom%p,F,mom%m,mom%Re,mom%dTime,&
           mom%N_PPE,mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E,getExportErrors(ss_MHD))

         case (3)
           call CN_AB2_PPE_PCG_mom_PCG(mom%PCG_U,mom%PCG_p,mom%U,mom%Unm1,&
           mom%U_E,mom%p,F,F,mom%m,mom%Re,mom%dTime,mom%N_PPE,mom%N_mom,mom%Ustar,&
           mom%temp_F,mom%temp_CC,mom%temp_E,getExportErrors(ss_MHD))

         case default; stop 'Error: solveUMethod must = 1,2 in momentum.f90.'
         end select
         mom%t = mom%t + mom%dTime
         mom%nstep = mom%nstep + 1

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)

         ! U at cell edge is needed for advection term at next time step
         ! and in induction solver. Neither case requires the diagonal.
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         ! call computeKineticEnergy(mom,mom%m,F)
         if (getExportErrors(ss_MHD)) call compute_divU(mom%divU,mom%U,mom%m)
         ! if (getExportErrors(ss_MHD)) then
         ! call export_processed_transient(mom%m,mom%U,dir//'Ufield/transient/','U',1,mom%nstep)
         ! endif

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

       end module