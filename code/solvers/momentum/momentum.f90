       module momentum_mod
       use current_precision_mod
       
       use sim_params_mod
       use boundary_conditions_mod
       use block_mod
       use mesh_block_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use mesh_domain_mod
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
       use IO_SF_mod
       use IO_VF_mod
       use export_raw_processed_mod
       use print_export_mod
       use export_now_mod

       use norms_mod
       use ops_norms_mod
       use ops_discrete_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_embedExtract_mod

       use apply_BCs_mod
       use apply_BCs_embed_mod
       use boundary_conditions_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use probe_mod
       use ops_norms_mod

       use PCG_mod
       use preconditioners_mod
       use GS_Poisson_mod
       use E_K_Budget_mod

       
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

         type(GS_Poisson_SF) :: GS_p

         type(matrix_free_params) :: MFP
         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U

         ! Time step, Reynolds number, grid
         type(mesh) :: m
         type(block) :: B
         type(mesh_block) :: MB
         type(mesh) :: m_surface

         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP_U,ISP_P
         type(sim_params) :: SP

         real(cp) :: Re,Ha,Gr,Fr
         real(cp) :: L_eta,U_eta,t_eta ! Kolmogorov Scales

         real(cp),dimension(8) :: e_budget
         integer :: unit_nrg_budget

         ! Probes
         type(probe) :: probe_KE,probe_KE_2C,probe_divU
       end type

       interface init;                module procedure initMomentum;               end interface
       interface delete;              module procedure deleteMomentum;             end interface
       interface display;             module procedure display_momentum;           end interface
       interface print;               module procedure print_momentum;             end interface
       interface export;              module procedure export_momentum;            end interface
       interface import;              module procedure import_momentum;            end interface

       interface export_tec;          module procedure export_tec_momentum;        end interface
       interface export_tec;          module procedure export_tec_momentum_no_ext; end interface
       interface exportTransient;     module procedure momentumExportTransient;    end interface
       interface solve;               module procedure solve_momentum;             end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine initMomentum(mom,m,SP,TMP,ISP_U,ISP_P,Re,Ha,Gr,Fr,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         type(iter_solver_params),intent(in) :: ISP_U,ISP_P
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: Re,Ha,Gr,Fr
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: prec_PPE
         type(VF) :: prec_mom
         write(*,*) 'Initializing momentum:'

         call init(mom%TMP,TMP)
         call init(mom%ISP_U,ISP_U)
         call init(mom%ISP_P,ISP_P)
         mom%Re = Re
         mom%Ha = Ha
         mom%Gr = Gr
         mom%Fr = Fr
         mom%L_eta = Re**(-3.0_cp/4.0_cp)
         mom%U_eta = Re**(-1.0_cp/4.0_cp)
         mom%t_eta = Re**(-1.0_cp/2.0_cp)
         mom%e_budget = 0.0_cp

         call init(mom%SP,SP)
         call init(mom%m,m)

         call init(mom%B,mom%m%B(1)%g)
         call init(mom%MB,mom%B)

         call init_Edge(mom%U_E       ,m,0.0_cp)
         call init_Face(mom%U         ,m,0.0_cp)
         call init_Face(mom%Ustar     ,m,0.0_cp)
         call init_Face(mom%Unm1      ,m,0.0_cp)
         call init_Face(mom%temp_F    ,m,0.0_cp)
         call init_Edge(mom%temp_E    ,m,0.0_cp)
         call init_CC(mom%p           ,m,0.0_cp)
         call init_CC(mom%divU        ,m,0.0_cp)
         call init_CC(mom%U_CC        ,m,0.0_cp)
         call init_CC(mom%temp_CC     ,m,0.0_cp)
         call init_CC(mom%Fo_grid     ,m,0.0_cp)
         call init_CC(mom%Co_grid     ,m,0.0_cp)
         call init_CC(mom%Re_grid     ,m,0.0_cp)
         call init_CC(mom%KE_adv      ,m,0.0_cp)
         call init_CC(mom%KE_diff     ,m,0.0_cp)
         call init_CC(mom%KE_pres     ,m,0.0_cp)
         call init_CC(mom%KE_transient,m,0.0_cp)
         call init_CC(mom%KE_jCrossB  ,m,0.0_cp)

         call init_CC(mom%vol_CC,m)
         call volume(mom%vol_CC,m)
         if (mom%SP%export_cell_volume) then
           call export_raw(mom%m,mom%vol_CC,str(DT%meshes),'mom_cell_volume',0)
         endif

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         write(*,*) 'about to define U_BCs'
         call init_UBCs(mom%U,m)
         write(*,*) 'U_BCs defined'
         call init_PBCs(mom%p,m)
         write(*,*) '     BCs initialized'
         if (mom%SP%solveMomentum) call print_BCs(mom%U,'U')
         if (mom%SP%solveMomentum) call export_BCs(mom%U,str(DT%U_BCs),'U')
         if (mom%SP%solveMomentum) call print_BCs(mom%p,'p')
         if (mom%SP%solveMomentum) call export_BCs(mom%p,str(DT%U_BCs),'p')

         ! Use mom%m later, for no just m
         call init_Ufield(mom%U,m,mom%SP%restartU,str(DT%U_f))
         call init_Pfield(mom%p,m,mom%SP%restartU,str(DT%U_f))
         write(*,*) '     Field initialized'
         
         call export_mesh(mom%MB%m,str(DT%meshes),'mesh_block',0)
         write(*,*) '     about to apply p BCs'
         call random_noise(mom%p); call export_raw(mom%m,mom%p,str(DT%U_f),'p_before',0)
         call apply_BCs(mom%p,m)
         call export_raw(mom%m,mom%p,str(DT%U_f),'p_after',0)
         write(*,*) '     P BCs applied'
         write(*,*) '     about to apply U BCs'
         call random_noise(mom%U); call export_raw(mom%m,mom%U,str(DT%U_f),'U_before',0)
         call apply_BCs(mom%U,m)
         call export_raw(mom%m,mom%U,str(DT%U_f),'U_after',0)
         ! stop 'Done in momentum.f90'
         write(*,*) '     U BCs applied'

         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)
         write(*,*) '     Interpolated fields initialized'

         call init(mom%probe_divU,str(DT%U_r),'probe_divU',mom%SP%restartU)
         call init(mom%probe_KE,str(DT%U_e),'KE',mom%SP%restartU)
         if (m%plane_xyz) call init(mom%probe_KE_2C,str(DT%U_e),'KE_2C',mom%SP%restartU)
         write(*,*) '     momentum probes initialized'

         ! Initialize interior solvers
         call init(mom%GS_p,mom%p,mom%m,mom%ISP_P,str(DT%U_r),'p')
         write(*,*) '     GS solver initialized for p'

         mom%MFP%c_mom = -0.5_cp*mom%TMP%dt/mom%Re

         call init(prec_mom,mom%U)
         call prec_mom_VF(prec_mom,mom%m,mom%MFP%c_mom)
         ! call prec_identity_VF(prec_mom) ! For ordinary CG
         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom,mom%m,&
         mom%ISP_U,mom%MFP,mom%U,mom%temp_E,str(DT%U_r),'U',.false.,.false.)
         call delete(prec_mom)
         write(*,*) '     PCG solver initialized for U'

         call init(prec_PPE,mom%p)
         call prec_lap_SF(prec_PPE,mom%m)
         ! call prec_identity_SF(prec_PPE) ! For ordinary CG
         call init(mom%PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_PPE,mom%m,&
         mom%ISP_P,mom%MFP,mom%p,mom%temp_F,str(DT%U_r),'p',.false.,.false.)
         call delete(prec_PPE)
         write(*,*) '     PCG solver initialized for p'

         temp_unit = new_and_open(str(DT%params),'info_mom')
         call display(mom,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_mom')
         ! if (restart_all) call import(mom,DT)
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

         call delete(mom%probe_divU)
         call delete(mom%probe_KE)
         call delete(mom%probe_KE_2C)
         call delete(mom%temp_E)
         call delete(mom%m)
         call delete(mom%MB)
         call delete(mom%B)
         call delete(mom%vol_CC)

         call delete(mom%PCG_P)
         call delete(mom%PCG_U)
         call delete(mom%GS_p)

         call delete(mom%TMP)
         call delete(mom%ISP_P)
         call delete(mom%ISP_U)
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
         write(un,*) 't,dt = ',mom%TMP%t,mom%TMP%dt
         write(un,*) 'solveUMethod,N_mom,N_PPE = ',mom%SP%solveUMethod,mom%ISP_U%iter_max,mom%ISP_P%iter_max
         write(un,*) 'tol_mom,tol_PPE = ',mom%ISP_U%tol_rel,mom%ISP_P%tol_rel
         write(un,*) 'nstep,KE = ',mom%TMP%n_step,mom%probe_KE%d
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
         integer :: un
         call export(mom%TMP  )
         call export(mom%ISP_U)
         call export(mom%ISP_P)

         un = new_and_open(str(DT%restart),'mom_restart')
         write(un,*) mom%Re
         write(un,*) mom%Ha;       write(un,*) mom%Gr
         write(un,*) mom%Fr;       write(un,*) mom%L_eta
         write(un,*) mom%U_eta;    write(un,*) mom%t_eta
         write(un,*) mom%e_budget
         call close_and_message(un,str(DT%restart),'mom_restart')

         un = new_and_open(str(DT%restart),'mom_MFP')
         call export(mom%MFP,un)
         call close_and_message(un,str(DT%restart),'mom_MFP')

         call export(mom%U     ,str(DT%restart),'U')
         call export(mom%p     ,str(DT%restart),'p')
       end subroutine

       subroutine import_momentum(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         integer :: un
         call import(mom%TMP  )
         call import(mom%ISP_U)
         call import(mom%ISP_P)

         un = open_to_read(str(DT%restart),'mom_restart')
         read(un,*) mom%Re
         read(un,*) mom%Ha;       read(un,*) mom%Gr
         read(un,*) mom%Fr;       read(un,*) mom%L_eta
         read(un,*) mom%U_eta;    read(un,*) mom%t_eta
         read(un,*) mom%e_budget
         call close_and_message(un,str(DT%restart),'mom_restart')

         un = open_to_read(str(DT%restart),'mom_MFP')
         call import(mom%MFP,un)
         call close_and_message(un,str(DT%restart),'mom_MFP')

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
         if (mom%SP%solveInduction.or.mom%SP%solveEnergy) then
           call export_raw(mom%m,F,str(DT%U_f),'F_external',0)
         endif
         call export_tec_momentum_no_ext(mom,DT)
       end subroutine
       subroutine export_tec_momentum_no_ext(mom,DT)
         implicit none
         type(momentum),intent(in) :: mom
         type(dir_tree),intent(in) :: DT
         if (mom%SP%restartU.and.(.not.mom%SP%solveMomentum)) then
           ! This preserves the initial data
         else
           write(*,*) 'export_tec_momentum_no_ext at mom%TMP%n_step = ',mom%TMP%n_step
           call export_raw(mom%m,mom%U,str(DT%U_f),'U',0)
           call export_raw(mom%m,mom%p,str(DT%U_f),'p',0)
           call export_processed(mom%m,mom%U,str(DT%U_f),'U',1)
           call export_processed(mom%m,mom%p,str(DT%U_f),'p',1)
           call export_raw(mom%m,mom%divU,str(DT%U_f),'divU',1)
           ! call export_processed(mom%m,mom%temp_E,str(DT%U_f),'vorticity',1)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine momentumExportTransient(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp) :: temp
         call compute_TKE(temp,mom%U_CC,mom%vol_CC)
         call export(mom%probe_KE,mom%TMP%t,temp)
         if (mom%m%plane_xyz) then
           call compute_TKE_2C(temp,mom%U_CC%y,mom%U_CC%z,mom%vol_CC,mom%temp_CC)
           call export(mom%probe_KE_2C,mom%TMP%t,temp)
         endif
         call Ln(temp,mom%divU,2.0_cp,mom%m); call export(mom%probe_divU,mom%TMP%t,temp)
         ! call Ln(temp,mom%divU,2.0_cp,mom%vol_CC); call export(mom%probe_divU,mom%TMP%t,temp)
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,F,PE,EN,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         ! call assign(mom%Unm1,mom%U) ! If Unm1 is needed

         select case(mom%SP%solveUMethod)
         case (1)
           call Euler_PCG_Donor(mom%PCG_P,mom%U,mom%U_E,mom%p,F,mom%m,mom%Re,mom%TMP%dt,&
           mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E,PE%transient_0D)

         case (2)
           call Euler_GS_Donor(mom%GS_p,mom%U,mom%U_E,mom%p,F,mom%m,mom%Re,mom%TMP%dt,&
           mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E,PE%transient_0D)

         case (3)
           call CN_AB2_PPE_PCG_mom_PCG(mom%PCG_U,mom%PCG_p,mom%U,mom%Unm1,&
           mom%U_E,mom%p,F,F,mom%m,mom%Re,mom%TMP%dt,mom%Ustar,&
           mom%temp_F,mom%temp_CC,mom%temp_E,PE%transient_0D)

         case (4)
           call Euler_Donor_no_PPE(mom%U,mom%U_E,F,mom%m,mom%Re,mom%TMP%dt,&
           mom%Ustar,mom%temp_F,mom%temp_CC,mom%temp_E)

         case default; stop 'Error: solveUMethod must = 1,2 in momentum.f90.'
         end select
         call iterate_step(mom%TMP)

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
         ! if (PE%transient_2D) call export_processed_transient_3C(mom%m,mom%U,str(DT%U_t),'U',1,mom%TMP)
         if (PE%transient_2D) call export_processed_transient_2C(mom%m,mom%U,str(DT%U_t),'U',1,mom%TMP)

         if (PE%info) call print(mom)
         if (PE%solution.or.EN%U%this.or.EN%all%this) then
           ! call curl(mom%temp_E,mom%U,m)
           call export(mom,DT)
           call export_tec(mom,DT,F)
         endif
       end subroutine

       subroutine compute_E_K_Budget(mom,B,B0,J,MD_fluid,Rem,DT)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(momentum),intent(inout) :: mom
         type(mesh_domain),intent(in) :: MD_fluid
         type(VF),intent(in) :: B,B0,J
         real(cp),intent(in) :: Rem
         type(TF) :: TF_CC1,TF_CC2
         type(VF) :: VF_F1,VF_F2,temp_B,temp_B0,temp_J
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         call init_CC(TF_CC1,mom%m)
         call init_CC(TF_CC2,mom%m)
         call init_Face(VF_F1,mom%m)
         call init_Face(VF_F2,mom%m)

         call init_Face(temp_B,mom%m)
         call init_Face(temp_B0,mom%m)
         call init_Edge(temp_J,mom%m)
         call assign(temp_B,B)
         call assign(temp_B0,B0)
         call assign(temp_J,J)

         call extractFace(temp_B,B,MD_fluid)
         call extractFace(temp_B0,B0,MD_fluid)
         call extractEdge(temp_J,J,MD_fluid)

         call assign(mom%Unm1,mom%U)

         call E_K_Budget(DT,mom%e_budget,mom%U,mom%Unm1,mom%U_CC,&
         temp_B,temp_B0,temp_J,mom%p,mom%m,mom%TMP%dt,mom%Re,mom%Ha,Rem,&
         VF_F1,VF_F2,TF_CC1,TF_CC2)

         call export_E_K_budget(mom,DT)

         call delete(temp_B)
         call delete(temp_B0)
         call delete(temp_J)
         call delete(VF_F1)
         call delete(VF_F2)
         call delete(TF_CC1)
         call delete(TF_CC2)
       end subroutine

       subroutine export_E_K_budget(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         type(string),dimension(8) :: vars
         integer :: un,i
         un = new_and_open(str(DT%e_budget),'E_K_budget_terms')
         i=1
         call init(vars(i),'Unsteady = '); i=i+1
         call init(vars(i),'E_K_Convection = '); i=i+1
         call init(vars(i),'E_K_Diffusion = '); i=i+1
         call init(vars(i),'E_K_Pressure = '); i=i+1
         call init(vars(i),'Viscous_Dissipation = '); i=i+1
         call init(vars(i),'E_M_Convection = '); i=i+1
         call init(vars(i),'E_M_Tension = '); i=i+1
         call init(vars(i),'Lorentz = '); i=i+1

         write(un,*) 'kinetic energy budget at nstep=',mom%TMP%n_step
         do i=1,size(vars)
         write(un,*) str(vars(i)),mom%e_budget(i)
         call delete(vars(i))
         enddo
         flush(un)
         call close_and_message(un,str(DT%e_budget),'E_K_budget_terms')
       end subroutine

       end module