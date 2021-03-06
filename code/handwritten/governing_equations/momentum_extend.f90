       module momentum_extend_mod
       use momentum_mod
       use current_precision_mod

       use sim_params_mod
       use boundary_conditions_extend_mod
       use block_extend_mod
       use BC_funcs_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use mesh_domain_extend_mod
       use string_mod
       use path_extend_mod
       use path_extend_mod
       use dir_tree_mod
       use RK_Params_mod

       use time_marching_methods_mod
       use momentum_aux_mod
       use init_P_BCs_mod
       use init_Ustar_field_mod
       use init_U_BCs_mod
       use init_U_Field_mod
       use init_P_Field_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use datatype_conversion_mod
       use time_statistics_extend_mod

       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use import_raw_mod
       use export_raw_processed_symmetry_mod
       use export_processed_FPL_mod
       use export_frequency_mod
       use export_now_mod

       use norms_extend_mod
       use ops_norms_mod
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
       use probe_extend_mod
       use ops_norms_mod

       use PCG_solver_extend_mod
       use FFT_solver_extend_mod
       use preconditioners_mod
       use E_K_Budget_mod


       implicit none
       private

       public :: momentum
       public :: init,delete,display,print,export,import ! Essentials

       public :: solve,export_tec,compute_export_E_K_Budget
       public :: export_unsteady
       public :: set_necessary_for_restart

       interface init;                      module procedure init_mom;                      end interface
       interface display;                   module procedure display_momentum;              end interface
       interface print;                     module procedure print_momentum;                end interface

       interface export_tec;                module procedure export_tec_momentum;           end interface
       interface solve;                     module procedure solve_momentum;                end interface

       interface export_unsteady;           module procedure export_unsteady_mom;           end interface
       interface export_unsteady_0D;        module procedure export_unsteady_0D_mom;        end interface
       interface export_unsteady_1D;        module procedure export_unsteady_1D_mom;        end interface
       interface export_unsteady_2D;        module procedure export_unsteady_2D_mom;        end interface
       interface export_unsteady_3D;        module procedure export_unsteady_3D_mom;        end interface
       interface set_necessary_for_restart; module procedure set_necessary_for_restart_mom; end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_mom(mom,SP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         type(TF) :: TF_Face
         integer :: temp_unit
         write(*,*) 'Initializing momentum:'

         call init_Edge(mom%U_E       ,mom%m,0.0_cp)
         call init_Face(mom%U         ,mom%m,0.0_cp)
         call init_Face(mom%Unm1      ,mom%m,0.0_cp)
         call init_Face(mom%temp_F1   ,mom%m,0.0_cp)
         call init_Face(mom%temp_F2   ,mom%m,0.0_cp)
         call init_Face(mom%F         ,mom%m,0.0_cp)
         call init_Face(mom%Fnm1      ,mom%m,0.0_cp)
         call init_Face(mom%L         ,mom%m,0.0_cp)
         call init_Face(mom%temp_F3   ,mom%m,0.0_cp)
         call init_Edge(mom%temp_E    ,mom%m,0.0_cp)
         call init_CC(mom%p           ,mom%m,0.0_cp)
         call init_CC(mom%divU        ,mom%m,0.0_cp)
         call init_CC(mom%U_CC        ,mom%m,0.0_cp)
         call init_CC(mom%temp_CC     ,mom%m,0.0_cp)
         call init_CC(mom%temp_CC_VF  ,mom%m,0.0_cp)
         call init_CC(mom%TF_CC       ,mom%m,0.0_cp)
         call init_CC_Edge(mom%TF_CC_edge,mom%m,0.0_cp)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         write(*,*) '     About to define U_BCs'
         call init_U_BCs(mom%U,mom%m,SP)
         call update_BC_vals(mom%U)
         write(*,*) '     U_BCs defined'
         call init_P_BCs(mom%p,mom%m,SP)
         call update_BC_vals(mom%p)
         write(*,*) '     BCs initialized'
         if (SP%VS%U%SS%solve) call export_BCs(mom%p,str(DT%p%BCs),'p')
         if (SP%VS%U%SS%solve) call export_BCs(mom%U,str(DT%U%BCs),'U')
         write(*,*) '     BCs exported'
         ! if (SP%VS%U%SS%solve) call print_BCs(mom%U,'U')
         ! if (SP%VS%U%SS%solve) call print_BCs(mom%p,'p')

         call init_U_field(mom%U,mom%m,SP)
         call init_P_field(mom%p,mom%m,SP)
         call assign(mom%Unm1,mom%U)
         write(*,*) '     Field initialized'

         call apply_BCs(mom%p)
         write(*,*) '     P BCs applied'
         call apply_BCs(mom%U)
         write(*,*) '     U BCs applied'
         call export_processed(mom%m,mom%U,str(DT%U%field),'U_IC',1)
         call export_processed(mom%m,mom%P,str(DT%P%field),'P_IC',1)

         call init(mom%Ustar,mom%U)
         if (SP%VS%U%SS%prescribed_BCs) call set_prescribed_BCs(mom%Ustar)
         call init_Ustar_field(mom%Ustar,mom%U)
         write(*,*) '     Intermediate field initialized'

         call init(mom%TS,mom%m,mom%U,SP%TSP,SP%VS%U%TMP,str(DT%U%stats),'U')
         write(*,*) '     momentum time statistics initialized'

         ! Initialize interior solvers
         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom_VF,mom%m,&
         SP%VS%U%ISP,SP%VS%U%MFP,mom%Ustar,mom%U,mom%TF_CC_edge,str(DT%U%residual),'U',.false.,.false.)
         write(*,*) '     PCG solver initialized for U'

         call delete(TF_Face)
         call init_Face(TF_Face,mom%m,0.0_cp)
         call init(mom%PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_lap_SF,mom%m,&
         SP%VS%P%ISP,SP%VS%P%MFP,mom%p,mom%p,TF_Face,str(DT%p%residual),'p',.false.,.false.)
         call delete(TF_Face)
         write(*,*) '     PCG solver initialized for p'

         call init(mom%FFT_P,mom%p,mom%m,SP%GP%FFT_dir,'FFT_p')
         write(*,*) '     FFT solver initialized for p'

         temp_unit = new_and_open(str(DT%params),'info_mom')
         call display(mom,SP,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_mom')
         call face2CellCenter(mom%U_CC,mom%U,mom%m)  ! Needed after import
         call face2edge_no_diag(mom%U_E,mom%U,mom%m) ! Needed after import
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine display_momentum(mom,SP,un)
         implicit none
         type(momentum),intent(in) :: mom
         type(sim_params),intent(in) :: SP
         integer,intent(in) :: un
         if (SP%FCL%export_heavy) then
           write(un,*) '**********************************************************'
           write(un,*) '************************ MOMENTUM ************************'
           write(un,*) '**********************************************************'
           write(un,*) 'Re,Ha = ',SP%DP%Re,SP%DP%Ha
           write(un,*) 'N,Gr = ',SP%DP%N,SP%DP%Gr
           ! write(un,*) 'flow_rate = ',plane_sum_x(mom%U%x%BF(1)%GF,mom%m%B(1)%g,2,1.0_cp)/SP%DP%Re
           write(un,*) 't,dt = ',SP%VS%U%TMP%t,SP%VS%U%TMP%TS%dt
           write(un,*) 'solveUMethod,N_mom,N_PPE = ',SP%VS%U%SS%solve_method,&
           SP%VS%U%ISP%EC%iter_max,SP%VS%P%ISP%EC%iter_max
           write(un,*) 'tol_mom,tol_PPE = ',SP%VS%U%ISP%EC%tol_rel,SP%VS%P%ISP%EC%tol_rel
           write(un,*) 'nstep,KE = ',SP%VS%U%TMP%n_step,get_data(SP%PS_mom%probe_KE)
           if (mom%TS%TSP%collect) call display(mom%TS%TSP,un)
           ! call displayPhysicalMinMax(mom%U,'U',un)
           call displayPhysicalMinMax(mom%divU,'divU',un)
           write(un,*) 'CFL = ',CFL_number(mom%U_CC,mom%m,SP%VS%U%TMP%TS%dt)
           write(un,*) ''
           ! call display(mom%m,un)
         endif
       end subroutine

       subroutine print_momentum(mom,SP)
         implicit none
         type(momentum),intent(in) :: mom
         type(sim_params),intent(in) :: SP
         call display(mom,SP,6)
       end subroutine

       subroutine set_necessary_for_restart_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call set_necessary_for_restart(mom%U)
         call set_necessary_for_restart(mom%Ustar)
         call set_necessary_for_restart(mom%Unm1)
         call set_necessary_for_restart(mom%p)
         call set_necessary_for_restart(mom%F)
         call set_necessary_for_restart(mom%Fnm1)
         call set_necessary_for_restart(mom%L)
         call set_necessary_for_restart(mom%PCG_U%r)
         call set_necessary_for_restart(mom%PCG_P%r)
         call set_necessary_for_restart(mom%PCG_U%p)
         call set_necessary_for_restart(mom%PCG_P%p)
         call set_necessary_for_restart(mom%PCG_U%x_BC)
         call set_necessary_for_restart(mom%PCG_P%x_BC)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_momentum(mom,SP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         if (SP%FCL%restart_all.and.(.not.SP%VS%U%SS%solve)) then
           ! This preserves the initial data
         else
           write(*,*) 'export_tec_momentum at n_step = ',SP%VS%U%TMP%n_step
           call export_processed(mom%m,mom%U,str(DT%U%field),'U',1)
           if (.not.SP%EL%export_soln_only) then
             call export_processed(mom%m,mom%p,str(DT%p%field),'p',1)
             call export_raw(mom%m,mom%divU,str(DT%U%field),'divU',0)
             call export_raw(mom%m,mom%p,str(DT%P%field),'p',0)
             if (SP%EL%export_symmetric) then
               call export_processed(mom%m,mom%U,str(DT%U%field),'U',1,SP%MP)
               call export_processed(mom%m,mom%p,str(DT%p%field),'p',1,SP%MP)
             endif
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_unsteady_0D_mom(mom,SP,TMP)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(inout) :: SP
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: temp,scale
         scale = SP%DP%KE_scale

         call compute_TKE(temp,mom%U_CC,mom%m,scale)
         call export(SP%PS_mom%probe_KE,TMP,temp)

         if (SP%FCL%compute_surface_power) then
         call surface_power(temp,mom%U,mom%m,mom%temp_F1,mom%temp_F2,mom%temp_CC_VF,mom%TF_CC)
         temp = scale*temp/SP%DP%Re
         call export(SP%PS_mom%probe_Q,TMP,temp)
         endif

         call export(SP%PS_mom%amax_U_x,TMP,amax(mom%U%x))
         call export(SP%PS_mom%amax_U_y,TMP,amax(mom%U%y))
         call export(SP%PS_mom%amax_U_z,TMP,amax(mom%U%z))

         if (mom%m%MP%plane_any) then
         if (mom%m%MP%plane(1)) call compute_TKE_2C(temp,mom%U_CC%y,mom%U_CC%z,mom%m,scale,mom%temp_CC)
         if (mom%m%MP%plane(2)) call compute_TKE_2C(temp,mom%U_CC%x,mom%U_CC%z,mom%m,scale,mom%temp_CC)
         if (mom%m%MP%plane(3)) call compute_TKE_2C(temp,mom%U_CC%x,mom%U_CC%y,mom%m,scale,mom%temp_CC)
         call export(SP%PS_mom%probe_KE_2C,TMP,temp)
         endif
         call div(mom%divU,mom%U,mom%m)
         call compute_Ln(temp,mom%divU,2.0_cp,mom%m)
         call export(SP%PS_mom%probe_divU,TMP,temp)
       end subroutine

       subroutine export_unsteady_1D_mom(mom,SP,TMP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%unsteady),'U',1,TMP,SP%VS%U%unsteady_lines)
         call export_processed(mom%m,mom%p,str(DT%P%unsteady),'p',1,TMP,SP%VS%P%unsteady_lines)
       end subroutine

       subroutine export_unsteady_2D_mom(mom,SP,TMP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U%x,str(DT%U%unsteady),'u',1,TMP,SP%VS%U%unsteady_planes)
         call export_processed(mom%m,mom%p,str(DT%P%unsteady),'p',1,TMP,SP%VS%P%unsteady_planes)
       end subroutine

       subroutine export_unsteady_3D_mom(mom,SP,TMP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%unsteady),'U',1,TMP,SP%VS%U%unsteady_field)
         call export_processed(mom%m,mom%p,str(DT%P%unsteady),'p',1,TMP,SP%VS%P%unsteady_field)
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,SP,F,Fnm1,L,TMP,EF)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(VF),intent(in) :: F,Fnm1,L
         type(export_frequency),intent(in) :: EF
         type(time_marching_params),intent(inout) :: TMP
         integer :: i
         do i=1,TMP%multistep_iter
         select case(SP%VS%U%SS%solve_method)
         case (1)
           call Euler_time_no_diff_Euler_sources_no_correction(mom%U,mom%Ustar,F,TMP)
         case (2)
           call Euler_time_no_diff_AB2_sources_no_correction(mom%U,mom%Ustar,F,Fnm1,TMP)
         case (3)
           call Euler_time_no_diff_Euler_sources(mom%PCG_P,mom%U,mom%Ustar,mom%Unm1,mom%p,&
           F,mom%m,TMP,mom%temp_F1,mom%temp_CC,EF%unsteady_0D%export_now)
         case (4)
           call Euler_time_no_diff_AB2_sources(mom%PCG_P,mom%U,mom%Ustar,mom%Unm1,mom%p,&
           F,Fnm1,mom%m,TMP,mom%temp_F1,mom%temp_CC,EF%unsteady_0D%export_now)
         case (5)
           call Euler_time_Euler_sources(mom%PCG_U,mom%PCG_P,mom%U,mom%Ustar,mom%Unm1,&
           mom%p,F,mom%m,TMP,mom%temp_F1,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case (6)
           call Euler_time_AB2_sources(mom%PCG_U,mom%PCG_P,mom%U,mom%Ustar,mom%Unm1,&
           mom%p,F,Fnm1,mom%m,TMP,mom%temp_F1,mom%L,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case (7)
           call O2_BDF_time_AB2_sources(mom%PCG_U,mom%PCG_P,mom%U,mom%Ustar,&
           mom%Unm1,mom%p,F,Fnm1,mom%m,TMP,mom%temp_F1,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case (8)
           call Euler_time_RK_sources(mom%PCG_U,mom%PCG_P,mom%U,mom%Ustar,mom%Unm1,&
           mom%p,F,Fnm1,L,mom%m,TMP,TMP%RKP,mom%temp_F1,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case (9)
           call Euler_time_AB2_sources_FFT(mom%PCG_U,mom%FFT_P,mom%U,mom%Ustar,mom%Unm1,&
           mom%p,F,Fnm1,mom%m,TMP,mom%temp_F1,mom%L,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case default; stop 'Error: solveUMethod must = 1:4 in momentum.f90.'
         end select
         enddo
         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         ! U at cell edge is needed for advection term at next time step
         ! and in induction solver. Neither case requires the diagonal.
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)
       end subroutine

       subroutine export_unsteady_mom(mom,SP,TMP,EF,EN,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(inout) :: SP
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         type(time_marching_params),intent(inout) :: TMP
         if (EF%unsteady_0D%export_now) call export_unsteady_0D(mom,SP,TMP)
         if (EF%unsteady_1D%export_now) call export_unsteady_1D(mom,SP,TMP,DT)
         if (EF%unsteady_2D%export_now) call export_unsteady_2D(mom,SP,TMP,DT)
         if (EF%unsteady_3D%export_now) call export_unsteady_3D(mom,SP,TMP,DT)
         if (EF%info%export_now) call print(mom,SP)
         if (EF%final_solution%export_now.or.EN%U%this.or.EN%all%this) then
           call export_tec(mom,SP,DT)
         endif
       end subroutine

       subroutine compute_export_E_K_Budget(mom,SP,B,B0,J,MD_fluid,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         type(mesh_domain),intent(in) :: MD_fluid
         type(VF),intent(in) :: B,B0,J
         write(*,*) '       KINETIC ENERGY BUDGET - STARTED'
         call E_K_Budget_wrapper(DT,mom%U,mom%Unm1,&
         B,B0,J,mom%p,mom%m,SP%VS%U%TMP,SP%DP,SP%MP,MD_fluid)
         write(*,*) '       KINETIC ENERGY BUDGET - COMPLETE'
       end subroutine

       end module