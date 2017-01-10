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

       use mesh_stencils_mod
       use momentum_solver_mod
       use momentum_aux_mod
       use init_P_BCs_mod
       use init_U_BCs_mod
       use init_U_Field_mod
       use init_P_Field_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use face_SD_mod
       use datatype_conversion_mod

       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use export_raw_processed_symmetry_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod

       use norms_mod
       use ops_norms_mod
       use ops_discrete_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_embedExtract_mod

       use apply_BCs_mod
       use boundary_conditions_mod
       use clean_divergence_mod

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

       public :: solve,export_tec,compute_E_K_Budget
       public :: prolongate

       type momentum
         logical :: suppress_warning
         ! Tensor fields
         type(TF) :: U_E
         ! Vector fields
         type(VF) :: U,Ustar
         type(VF) :: Unm1,U_CC
         type(VF) :: temp_F
         type(VF) :: temp_E
         ! Scalar fields
         type(SF) :: p,divU,temp_CC

         type(GS_Poisson_SF) :: GS_p

         type(matrix_free_params) :: MFP
         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U

         ! Time step, Reynolds number, grid
         type(mesh) :: m
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

       interface init;                 module procedure init_mom;                   end interface
       interface delete;               module procedure delete_mom;                 end interface
       interface prolongate;           module procedure prolongate_mom;             end interface
       interface display;              module procedure display_momentum;           end interface
       interface print;                module procedure print_momentum;             end interface
       interface export;               module procedure export_momentum;            end interface
       interface import;               module procedure import_momentum;            end interface

       interface export_tec;           module procedure export_tec_momentum;        end interface
       interface export_tec;           module procedure export_tec_momentum_no_ext; end interface
       interface solve;                module procedure solve_momentum;             end interface
       interface init_matrix_based_ops;module procedure init_matrix_based_ops_mom;  end interface
       interface set_MFP;              module procedure set_MFP_mom;                end interface


       interface export_transient1;    module procedure export_transient1_mom;      end interface
       interface export_transient2;    module procedure export_transient2_mom;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_mom(mom,m,SP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: vol_CC
         type(mesh_block) :: MB
         write(*,*) 'Initializing momentum:'

         call init(mom%TMP,SP%VS%U%TMP)
         call init(mom%ISP_U,SP%VS%U%ISP)
         call init(mom%ISP_P,SP%VS%P%ISP)
         mom%Re = SP%DP%Re
         mom%Ha = SP%DP%Ha
         mom%Gr = SP%DP%Gr
         mom%Fr = SP%DP%Fr
         mom%L_eta = mom%Re**(-3.0_cp/4.0_cp)
         mom%U_eta = mom%Re**(-1.0_cp/4.0_cp)
         mom%t_eta = mom%Re**(-1.0_cp/2.0_cp)
         mom%e_budget = 0.0_cp

         call init(mom%SP,SP)
         call init(mom%m,m)

         if (SP%EL%export_mesh_block) then
           call init(MB,mom%m%B(1))
           call export_mesh(MB%m,str(DT%meshes),'mom_mesh_block',0)
           call delete(MB)
         endif
         if (SP%EL%export_cell_volume) then
           call init_CC(vol_CC,m)
           call volume(vol_CC,m)
           call export_raw(mom%m,vol_CC,str(DT%meshes),'mom_cell_volume',0)
           call delete(vol_CC)
         endif

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

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         write(*,*) 'about to define U_BCs'
         call init_U_BCs(mom%U,m,mom%SP)
         write(*,*) 'U_BCs defined'
         call init_P_BCs(mom%p,m,mom%SP)
         write(*,*) '     BCs initialized'
         if (mom%SP%VS%U%SS%solve) call print_BCs(mom%U,'U')
         if (mom%SP%VS%U%SS%solve) call export_BCs(mom%U,str(DT%U%BCs),'U')
         if (mom%SP%VS%U%SS%solve) call print_BCs(mom%p,'p')
         if (mom%SP%VS%U%SS%solve) call export_BCs(mom%p,str(DT%p%BCs),'p')

         call init_U_field(mom%U,m,mom%SP,str(DT%U%field))
         call init_P_field(mom%p,m,mom%SP,str(DT%p%field))
         call assign(mom%Unm1,mom%U)
         write(*,*) '     Field initialized'

         write(*,*) '     about to apply p BCs'
         call apply_BCs(mom%p)
         write(*,*) '     P BCs applied'
         write(*,*) '     about to apply U BCs'
         call apply_BCs(mom%U)
         write(*,*) '     U BCs applied'

         write(*,*) '     about to assemble Laplacian matrices'
         call set_MFP(mom)
         if (mom%SP%matrix_based) call init_matrix_based_ops(mom)

         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)
         write(*,*) '     Interpolated fields initialized'

         call init(mom%probe_divU,str(DT%U%residual),'probe_divU',mom%SP%VS%U%SS%restart,SP,.true.)
         call init(mom%probe_KE,str(DT%U%energy),'KE',mom%SP%VS%U%SS%restart,SP,.false.)
         if (m%MP%plane_any) then
          call init(mom%probe_KE_2C,str(DT%U%energy),'KE_2C',mom%SP%VS%U%SS%restart,SP,.false.)
         endif
         write(*,*) '     momentum probes initialized'

         ! Initialize interior solvers
         call init(mom%GS_p,mom%p,mom%m,mom%ISP_P,str(DT%p%residual),'p')
         write(*,*) '     GS solver initialized for p'

         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom_VF,mom%m,&
         mom%ISP_U,mom%MFP,mom%U,mom%temp_E,str(DT%U%residual),'U',.false.,.false.)
         write(*,*) '     PCG solver initialized for U'

         call init(mom%PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_lap_SF,mom%m,&
         mom%ISP_P,mom%MFP,mom%p,mom%temp_F,str(DT%p%residual),'p',.false.,.false.)
         write(*,*) '     PCG solver initialized for p'

         temp_unit = new_and_open(str(DT%params),'info_mom')
         call display(mom,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_mom')
         ! if (restart_all) call import(mom,DT)
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine delete_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U)
         call delete(mom%U_E)
         call delete(mom%Unm1)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%divU)
         call delete(mom%U_CC)
         call delete(mom%probe_divU)
         call delete(mom%probe_KE)
         call delete(mom%probe_KE_2C)
         call delete(mom%temp_E)
         call delete(mom%m)
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
         write(un,*) 'solveUMethod,N_mom,N_PPE = ',mom%SP%VS%U%SS%solve_method,&
         mom%ISP_U%iter_max,mom%ISP_P%iter_max
         write(un,*) 'tol_mom,tol_PPE = ',mom%ISP_U%tol_rel,mom%ISP_P%tol_rel
         write(un,*) 'nstep,KE = ',mom%TMP%n_step,get_data(mom%probe_KE)
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

         if (.not.mom%SP%EL%export_soln_only) then
           call export(mom%U     ,str(DT%U%restart),'U')
           call export(mom%p     ,str(DT%p%restart),'p')
         endif
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

         if (.not.mom%SP%EL%export_soln_only) then
           call import(mom%U     ,str(DT%U%restart),'U')
           call import(mom%p     ,str(DT%p%restart),'p')
         endif
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_momentum(mom,DT,F)
         implicit none
         type(momentum),intent(in) :: mom
         type(VF),intent(in) :: F
         type(dir_tree),intent(in) :: DT
         if (.not.mom%SP%EL%export_soln_only) then
         if (mom%SP%VS%B%SS%solve.or.mom%SP%VS%T%SS%solve) then
           call export_raw(mom%m,F,str(DT%U%field),'F_external',0)
         endif
         endif
         call export_tec_momentum_no_ext(mom,DT)
       end subroutine
       subroutine export_tec_momentum_no_ext(mom,DT)
         implicit none
         type(momentum),intent(in) :: mom
         type(dir_tree),intent(in) :: DT
         if (mom%SP%VS%U%SS%restart.and.(.not.mom%SP%VS%U%SS%solve)) then
           ! This preserves the initial data
         else
           write(*,*) 'export_tec_momentum_no_ext at mom%TMP%n_step = ',mom%TMP%n_step
           call export_processed(mom%m,mom%U,str(DT%U%field),'U',1)
           if (.not.mom%SP%EL%export_soln_only) then
             call export_processed(mom%m,mom%p,str(DT%p%field),'p',1)
             call export_raw(mom%m,mom%U,str(DT%U%field),'U',0)
             call export_raw(mom%m,mom%p,str(DT%p%field),'p',0)
             if (mom%SP%EL%export_symmetric) then
              call export_processed(mom%m,mom%U,str(DT%U%field),'U',1,6,(/1.0_cp,1.0_cp,1.0_cp/))
              call export_processed(mom%m,mom%p,str(DT%p%field),'p',1,6,1.0_cp)
             endif
             call export_raw(mom%m,mom%divU,str(DT%U%field),'divU',1)
             ! call export_processed(mom%m,mom%temp_E,str(DT%U%field),'vorticity',1)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_transient1_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp) :: temp
         call compute_TKE(temp,mom%U_CC,mom%m)
         call export(mom%probe_KE,mom%TMP,temp)
         if (mom%m%MP%plane_any) then
           call compute_TKE_2C(temp,mom%U_CC%y,mom%U_CC%z,mom%m,mom%temp_CC)
           call export(mom%probe_KE_2C,mom%TMP,temp)
         endif
         call div(mom%divU,mom%U,mom%m)
         call Ln(temp,mom%divU,2.0_cp,mom%m)
         call export(mom%probe_divU,mom%TMP,temp)
       end subroutine

       subroutine export_transient2_mom(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%transient),'U',1,mom%TMP)
         call export_processed(mom%m,mom%p,str(DT%U%transient),'p',1,mom%TMP)
         ! call export_processed(mom%m,mom%U,str(DT%U%transient),'U',1,mom%TMP,3,24)
         ! call export_processed(mom%m,mom%p,str(DT%U%transient),'p',1,mom%TMP,3,24)
       end subroutine

       subroutine set_MFP_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         mom%MFP%coeff = -0.5_cp*mom%TMP%dt/mom%Re
       end subroutine

       subroutine init_matrix_based_ops_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp),dimension(2) :: diffusion_treatment
         call set_MFP(mom)
         call init_Laplacian_SF(mom%m) ! Must come before PPE solver init
         call init_Laplacian_VF(mom%m) ! for lap(U) in momentum
         ! diffusion_treatment = (/-mom%TMP%dt/mom%Re,1.0_cp/) ! diffusion explicit
         ! diffusion_treatment = (/mom%TMP%dt/mom%Re,1.0_cp/)    ! diffusion explicit
         diffusion_treatment = (/mom%TMP%dt/mom%Re,1.0_cp/)    ! diffusion explicit
         ! diffusion_treatment = (/1.0_cp,0.0_cp/)             ! no treatment (requires multiplication by dt/Re)
         ! call modify_Laplacian_VF(mom%m,diffusion_treatment(1),diffusion_treatment(2))
         call multiply_Laplacian_VF(mom%m,diffusion_treatment(1))
         call add_Laplacian_VF(mom%m,diffusion_treatment(2))
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,F,PE,EN,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         select case(mom%SP%VS%U%SS%solve_method)
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
         case default; stop 'Error: solveUMethod must = 1:4 in momentum.f90.'
         end select
         call iterate_step(mom%TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)

         ! U at cell edge is needed for advection term at next time step
         ! and in induction solver. Neither case requires the diagonal.
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         ! call computeKineticEnergy(mom,mom%m,F)
         if (PE%transient_0D) call export_transient1(mom)
         if (PE%transient_2D) call export_transient2(mom,DT)

         ! if (PE%transient_2D) call export_processed_transient_3C(mom%m,mom%U,str(DT%U%transient),'U',1,mom%TMP)
         ! if (PE%transient_2D) call export_processed_transient_2C(mom%m,mom%U,str(DT%U%transient),'U',1,mom%TMP)

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

       subroutine prolongate_mom(mom,F,DT,RM,SS_reached)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(inout) :: F
         type(dir_tree),intent(in) :: DT
         type(refine_mesh),intent(in) :: RM
         logical,intent(in) :: SS_reached
         integer,dimension(3) :: dir
         type(iter_solver_params) :: temp
         logical :: clean_exact
         integer :: i
         write(*,*) '#################### Prolongating momentum solver ####################'
         call export_processed(mom%m,mom%U,str(DT%U%field),'U_SS_'//str(RM%level_last),1)

         dir = get_dir(RM)
         if (SS_reached) dir = (/1,2,3/)
         do i=1,3
           if (dir(i).ne.0) then
             write(*,*) 'Prolongating momentum solver along direction ',i
             call prolongate(mom%m,dir(i))
             call prolongate(mom%U_E,mom%m,dir(i))
             call prolongate(F,mom%m,dir(i))
             call prolongate(mom%U,mom%m,dir(i))
             call prolongate(mom%Ustar,mom%m,dir(i))
             call prolongate(mom%Unm1,mom%m,dir(i))
             call prolongate(mom%temp_F,mom%m,dir(i))
             call prolongate(mom%temp_E,mom%m,dir(i))
             call prolongate(mom%p,mom%m,dir(i))
             call prolongate(mom%divU,mom%m,dir(i))
             call prolongate(mom%U_CC,mom%m,dir(i))
             call prolongate(mom%temp_CC,mom%m,dir(i))
             call set_MFP(mom)
             call prolongate(mom%PCG_P,mom%m,mom%temp_F,mom%MFP,dir(i))
             call prolongate(mom%PCG_U,mom%m,mom%temp_E,mom%MFP,dir(i))
           endif
         enddo
         call init_U_BCs(mom%U,mom%m,mom%SP) ! Needed (better) if U_BCs is a distribution
         write(*,*) 'Finished momentum solver prolongation'
         if (mom%SP%matrix_based) call init_matrix_based_ops(mom)
         call apply_BCs(mom%U)
         call export_processed(mom%m,mom%U,str(DT%U%field),'U_prolongated_'//str(RM%level),1)

         clean_exact = .false.
         if (clean_exact) then ! Doesn't seem to help any for hydro flows
           call init(temp,mom%PCG_P%ISP)
           call init(mom%PCG_P%ISP,solve_exact(str(DT%U%residual)))
           call clean_div(mom%PCG_P,mom%U,mom%p,mom%m,mom%temp_F,mom%temp_CC,.true.)
           call init(mom%PCG_P%ISP,temp)
           call delete(temp)
         else
           call boost(mom%PCG_P%ISP)
           call clean_div(mom%PCG_P,mom%U,mom%p,mom%m,mom%temp_F,mom%temp_CC,.true.)
           call reset(mom%PCG_P%ISP)
         endif

         call export_processed(mom%m,mom%U,str(DT%U%field),'U_cleaned_'//str(RM%level),1)
         write(*,*) '#############################################################'
       end subroutine

       end module