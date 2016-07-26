       module momentum_mod
       use current_precision_mod
       use simParams_mod
       
       use BCs_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use domain_mod
       use string_mod
       use path_mod
       use dir_tree_mod

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
       use print_export_mod

       use norms_mod
       use ops_norms_mod
       use ops_discrete_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_embedExtract_mod

       use apply_BCs_mod
       use apply_stitches_mod

       use PCG_mod
       use preconditioners_mod
       use GS_Poisson_mod
       use E_K_Budget_mod

       use probe_base_mod
       use probe_transient_mod
       use probe_derived_mod
       
       implicit none
       private
       
       public :: momentum
       public :: init,delete,display,print,export,import ! Essentials

       public :: solve,export_tec,exportTransient,compute_E_K_Budget

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
         type(SF) :: vol_CC

         type(GS_Poisson) :: GS_p

         type(matrix_free_params) :: MFP
         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U

         ! Time step, Reynolds number, grid
         type(mesh) :: m
         type(mesh) :: m_surface
         integer :: N_PPE,N_mom
         real(cp) :: tol_PPE,tol_mom
         integer :: nstep
         real(cp) :: dTime,t
         real(cp) :: Re,Ha,Gr,Fr
         real(cp) :: L_eta,U_eta,t_eta ! Kolmogorov Scales

         real(cp),dimension(5) :: nrg_budget
         integer :: unit_nrg_budget

         ! Transient probes
         real(cp) :: KE,KE_2C
         type(norms) :: norm_divU
         type(probe) :: transient_KE,transient_KE_2C,transient_divU
       end type

       interface init;                module procedure initMomentum;               end interface
       interface delete;              module procedure deleteMomentum;             end interface
       interface display;             module procedure display_momentum;           end interface
       interface print;               module procedure print_momentum;             end interface
       interface export;              module procedure export_momentum;            end interface
       interface import;              module procedure import_momentum;            end interface

       interface export_tec;          module procedure export_tec_momentum;        end interface
       interface exportTransient;     module procedure momentumExportTransient;    end interface
       interface solve;               module procedure solve_momentum;             end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine initMomentum(mom,m,N_mom,tol_mom,N_PPE,tol_PPE,dTime,Re,Ha,Gr,Fr,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_mom,N_PPE
         real(cp),intent(in) :: tol_mom,tol_PPE
         real(cp),intent(in) :: dTime,Re,Ha,Gr,Fr
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: prec_PPE
         type(VF) :: prec_mom
         write(*,*) 'Initializing momentum:'

         mom%dTime = dTime
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
         mom%KE = 0.0_cp; mom%KE_2C = 0.0_cp
         mom%nrg_budget = 0.0_cp
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

         call init_CC(mom%vol_CC,m)
         call volume(mom%vol_CC,m)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         write(*,*) 'about to define U_BCs'
         call init_UBCs(mom%U,m)
         write(*,*) 'U_BCs defined'
         call init_PBCs(mom%p,m)
         write(*,*) '     BCs initialized'
         if (solveMomentum) call print_BCs(mom%U,'U')
         if (solveMomentum) call export_BCs(mom%U,str(DT%params),'U')
         if (solveMomentum) call print_BCs(mom%p,'p')
         if (solveMomentum) call export_BCs(mom%p,str(DT%params),'p')

         ! Use mom%m later, for no just m
         write(*,*) 'str(DT%U) = ',str(DT%U)
         call init_Ufield(mom%U,m,str(DT%U))
         call init_Pfield(mom%p,m,str(DT%U))
         write(*,*) '     Field initialized'
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         call apply_BCs(mom%U,m)
         call apply_stitches(mom%U,m)
         write(*,*) '     U BCs applied'
         call apply_BCs(mom%p,m)
         call apply_stitches(mom%p,m)
         write(*,*) '     P BCs applied'

         call init(mom%transient_divU,str(DT%U),'transient_divU',.not.restartU)
         call export(mom%transient_divU)
         write(*,*) '     momentum probes initialized'

         mom%unit_nrg_budget = newAndOpen(str(DT%U),'energy_budget')
         ! {error} = {dudt} + {adv} - {diff} + {pres} - {external}
         write(mom%unit_nrg_budget,*) ' TITLE = "momentum energy budget"'
         write(mom%unit_nrg_budget,*) ' VARIABLES = N,unsteady,convection,transport,Lorentz,Viscous_Dissipation'
         write(mom%unit_nrg_budget,*) ' ZONE DATAPACKING = POINT'
         ! Initialize interior solvers
         call init(mom%GS_p,mom%p,mom%m,str(DT%U),'p')
         write(*,*) '     GS solver initialized for p'

         mom%MFP%c_mom = -0.5_cp*mom%dTime/mom%Re

         call init(prec_mom,mom%U)
         call prec_mom_VF(prec_mom,mom%m,mom%MFP%c_mom)
         ! call prec_identity_VF(prec_mom) ! For ordinary CG
         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom,mom%m,&
         mom%tol_mom,mom%MFP,mom%U,mom%temp_E,str(DT%U),'U',.false.,.false.)
         call delete(prec_mom)
         write(*,*) '     PCG solver initialized for U'

         call init(prec_PPE,mom%p)
         call prec_lap_SF(prec_PPE,mom%m)
         ! call prec_identity_SF(prec_PPE) ! For ordinary CG
         call init(mom%PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_PPE,mom%m,&
         mom%tol_PPE,mom%MFP,mom%p,mom%temp_F,str(DT%U),'p',.false.,.false.)
         call delete(prec_PPE)
         write(*,*) '     PCG solver initialized for p'

         if (restartU) then
         call readLastStepFromFile(mom%nstep,str(DT%params),'nstep_mom')
         else; mom%nstep = 0
         endif
         call init(mom%transient_KE,str(DT%U),'KU',.not.restartU)
         call init(mom%transient_KE_2C,str(DT%U),'KE_2C',.not.restartU)

         temp_unit = newAndOpen(str(DT%params),'info_mom')
         call display(mom,temp_unit)
         close(temp_unit)
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

         call delete(mom%transient_divU)
         call delete(mom%transient_KE)
         call delete(mom%transient_KE_2C)
         call delete(mom%temp_E)
         call delete(mom%m)
         call delete(mom%vol_CC)

         call delete(mom%PCG_P)
         call delete(mom%PCG_U)
         call delete(mom%GS_p)
         write(*,*) 'Momentum object deleted'
       end subroutine

       subroutine display_momentum(mom,un)
         implicit none
         type(momentum),intent(in) :: mom
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MOMENTUM **************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Re,Ha = ',mom%Re,mom%Ha
         write(un,*) 'Gr,Fr = ',mom%Gr,mom%Fr
         write(un,*) 't,dt = ',mom%t,mom%dTime
         write(un,*) 'solveUMethod,N_mom,N_PPE = ',solveUMethod,mom%N_mom,mom%N_PPE
         write(un,*) 'tol_mom,tol_PPE = ',mom%tol_mom,mom%tol_PPE
         write(un,*) 'nstep,KE = ',mom%nstep,mom%KE
         ! call displayPhysicalMinMax(mom%U,'U',un)
         call displayPhysicalMinMax(mom%divU,'divU',un)
         ! write(un,*) 'Kolmogorov Length = ',mom%L_eta
         ! write(un,*) 'Kolmogorov Velocity = ',mom%U_eta
         ! write(un,*) 'Kolmogorov Time = ',mom%t_eta
         write(un,*) ''
         call display(mom%m,un)
         write(*,*) ''
       end subroutine

       subroutine print_momentum(mom)
         implicit none
         type(momentum),intent(in) :: mom
         call display(mom,6)
       end subroutine

       subroutine export_momentum(mom,DT)
         implicit none
         type(momentum),intent(in) :: mom
         type(dir_tree),intent(in) :: DT
         type(path) :: restart
         call oldest_modified_file(restart,DT%restart1,DT%restart2,'p.dat')
         call export(mom%U     ,str(restart),'U')
         call export(mom%p     ,str(restart),'p')
         call delete(restart)
       end subroutine

       subroutine import_momentum(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         call import(mom%U     ,str(DT%restart),'U')
         call import(mom%p     ,str(DT%restart),'p')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_momentum(mom,DT,F)
         implicit none
         type(momentum),intent(in) :: mom
         type(VF),intent(in) :: F
         type(dir_tree),intent(in) :: DT
         if (restartU.and.(.not.solveMomentum)) then
           ! This preserves the initial data
         else
           write(*,*) 'export_tec_momentum at mom%nstep = ',mom%nstep
           call export_processed(mom%m,mom%U,str(DT%U),'U',1)
           call export_processed(mom%m,mom%p,str(DT%U),'p',1)
           call export_raw(mom%m,mom%divU,str(DT%U),'divU',1)
           if (solveEnergy.or.solveInduction) call export_raw(mom%m,F,str(DT%U),'jCrossB',0)
           ! call export_processed(mom%m,mom%temp_E,str(DT%U),'vorticity',1)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine momentumExportTransient(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         ! real(cp) :: temp_Ln
         call compute_TKE(mom%KE,mom%U_CC,mom%vol_CC)
         call set(mom%transient_KE,mom%nstep,mom%t,mom%KE)
         call apply(mom%transient_KE)

         call compute_TKE_2C(mom%KE_2C,mom%U_CC%y,mom%U_CC%z,mom%vol_CC,mom%temp_CC)
         call set(mom%transient_KE_2C,mom%nstep,mom%t,mom%KE_2C)
         call apply(mom%transient_KE_2C)
         ! call Ln(temp_Ln,mom%divU,2.0_cp)
         ! call set(mom%transient_divU,mom%nstep,temp_Ln)
         call compute(mom%norm_divU,mom%divU,mom%vol_CC)
         call set(mom%transient_divU,mom%nstep,mom%t,mom%norm_divU%L2)
         call apply(mom%transient_divU)
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,F,PE,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(print_export),intent(in) :: PE
         type(dir_tree),intent(in) :: DT
         logical :: exportNow,exportNowU
         integer :: N_PPE
         if (mom%nstep.lt.1000) then; N_PPE = 2*mom%N_PPE
         else;                        N_PPE = mom%N_PPE
         endif
         N_PPE = mom%N_PPE

         select case(solveUMethod)
         case (1)
           call Euler_PCG_Donor(mom%PCG_P,mom%U,mom%U_E,mom%p,F,mom%m,mom%Re,mom%dTime,&
           N_PPE,mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E,&
           PE%transient_0D)

         case (2)
           call Euler_GS_Donor(mom%GS_p,mom%U,mom%U_E,mom%p,F,mom%m,mom%Re,mom%dTime,&
           mom%N_PPE,mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E,PE%transient_0D)

         case (3)
           call CN_AB2_PPE_PCG_mom_PCG(mom%PCG_U,mom%PCG_p,mom%U,mom%Unm1,&
           mom%U_E,mom%p,F,F,mom%m,mom%Re,mom%dTime,mom%N_PPE,mom%N_mom,mom%Ustar,&
           mom%temp_F,mom%temp_CC,mom%temp_E,PE%transient_0D)

         case (4)
           call Euler_PCG_Donor_no_PPE(mom%U,mom%U_E,F,mom%m,mom%Re,mom%dTime,&
           mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E)

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
         if (PE%transient_0D) then
           call div(mom%divU,mom%U,mom%m)
           call exportTransient(mom)
         endif
         ! if (PE%transient_2D) call export_processed_transient_3C(mom%m,mom%U,str(DT%U_t),'U',1,mom%nstep)
         if (PE%transient_2D) call export_processed_transient_2C(mom%m,mom%U,str(DT%U_t),'U',1,mom%nstep)

         if (PE%info) then
           call print(mom)
           exportNow = readSwitchFromFile(str(DT%params),'exportNow')
           exportNowU = readSwitchFromFile(str(DT%params),'exportNowU')
           write(*,*) ''
         else; exportNow = .false.; exportNowU = .false.
         endif

         if (PE%solution.or.exportNowU.or.exportNow) then
           ! call curl(mom%temp_E,mom%U,m)
           call export(mom,DT)
           call export_tec(mom,DT,F)
           call writeSwitchToFile(.false.,str(DT%params),'exportNowU')
         endif
       end subroutine

       subroutine compute_E_K_Budget(mom,B,B0,J,D_fluid)
         implicit none
         type(momentum),intent(inout) :: mom
         type(domain),intent(in) :: D_fluid
         type(VF),intent(in) :: B,B0,J
         type(TF) :: temp_CC1_TF,temp_CC2_TF
         type(VF) :: temp_F1,temp_F2,temp_B,temp_B0,temp_J
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         call init_CC(temp_CC1_TF,mom%m)
         call init_CC(temp_CC2_TF,mom%m)
         call init_Face(temp_F1,mom%m)
         call init_Face(temp_F2,mom%m)

         call init_Face(temp_B,mom%m)
         call init_Face(temp_B0,mom%m)
         call init_Edge(temp_J,mom%m)
         call assign(temp_B,B)
         call assign(temp_B0,B0)
         call assign(temp_J,J)

         call extractFace(temp_B,B,D_fluid)
         call extractFace(temp_B0,B0,D_fluid)
         call extractEdge(temp_J,J,D_fluid)

         call E_K_Budget(mom%nrg_budget,mom%U,mom%Unm1,mom%U_CC,&
         temp_B,temp_B0,temp_J,mom%p,mom%m,mom%dTime,&
         temp_F1,temp_F2,temp_CC1_TF,temp_CC2_TF)

         write(mom%unit_nrg_budget,*) mom%nstep,mom%nrg_budget
         flush(mom%unit_nrg_budget)

         call delete(temp_B)
         call delete(temp_B0)
         call delete(temp_J)
         call delete(temp_F1)
         call delete(temp_F2)
         call delete(temp_CC1_TF)
         call delete(temp_CC2_TF)
       end subroutine

       end module