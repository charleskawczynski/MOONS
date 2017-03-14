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
       use time_marching_methods_mod
       use momentum_aux_mod
       use init_P_BCs_mod
       use init_Ustar_field_mod
       use init_U_BCs_mod
       use init_U_Field_mod
       use init_P_Field_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use face_SD_mod
       use datatype_conversion_mod
       use time_statistics_mod

       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use export_raw_processed_symmetry_mod
       use export_processed_FPL_mod
       use export_frequency_mod
       use export_now_mod
       use refine_mesh_mod

       use norms_mod
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
       use ops_norms_mod

       use PCG_mod
       use preconditioners_mod
       use GS_Poisson_mod
       use E_K_Budget_mod


       implicit none
       private

       public :: momentum
       public :: init,delete,display,print,export,import ! Essentials

       public :: solve,export_tec,compute_export_E_K_Budget

       type momentum
         logical :: suppress_warning
         ! Tensor fields
         type(TF) :: U_E
         type(TF) :: TF_CC
         type(TF) :: TF_CC_edge
         ! Vector fields
         type(VF) :: U,Ustar,Unm1
         type(VF) :: U_CC
         type(VF) :: temp_F1,temp_F2,temp_F3
         type(VF) :: temp_E,temp_CC_VF
         ! Scalar fields
         type(SF) :: p,divU,temp_CC

         type(GS_Poisson_SF) :: GS_p

         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U

         type(mesh) :: m
         type(sim_params) :: SP
         type(probe) :: probe_KE,probe_KE_2C,probe_divU,probe_Q
         type(time_statistics_VF) :: TS
       end type

       interface init;                 module procedure init_mom;                   end interface
       interface delete;               module procedure delete_mom;                 end interface
       interface display;              module procedure display_momentum;           end interface
       interface print;                module procedure print_momentum;             end interface
       interface export;               module procedure export_momentum;            end interface
       interface import;               module procedure import_momentum;            end interface

       interface export_tec;           module procedure export_tec_momentum;        end interface
       interface export_tec;           module procedure export_tec_momentum_no_ext; end interface
       interface solve;                module procedure solve_momentum;             end interface
       interface init_matrix_based_ops;module procedure init_matrix_based_ops_mom;  end interface

       interface export_unsteady_0D;    module procedure export_unsteady_0D_mom;      end interface
       interface export_unsteady_1D;    module procedure export_unsteady_1D_mom;      end interface
       interface export_unsteady_2D;    module procedure export_unsteady_2D_mom;      end interface
       interface export_unsteady_3D;    module procedure export_unsteady_3D_mom;      end interface

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
         write(*,*) 'Initializing momentum:'

         call init(mom%SP,SP)
         call init(mom%m,m)

         call init_Edge(mom%U_E       ,m,0.0_cp)
         call init_Face(mom%U         ,m,0.0_cp)
         call init_Face(mom%Unm1      ,m,0.0_cp)
         call init_Face(mom%temp_F1   ,m,0.0_cp)
         call init_Face(mom%temp_F2   ,m,0.0_cp)
         call init_Face(mom%temp_F3   ,m,0.0_cp)
         call init_Edge(mom%temp_E    ,m,0.0_cp)
         call init_CC(mom%p           ,m,0.0_cp)
         call init_CC(mom%divU        ,m,0.0_cp)
         call init_CC(mom%U_CC        ,m,0.0_cp)
         call init_CC(mom%temp_CC     ,m,0.0_cp)
         call init_CC(mom%temp_CC_VF  ,m,0.0_cp)
         call init_CC(mom%TF_CC       ,m,0.0_cp)
         call init_CC_Edge(mom%TF_CC_edge,m,0.0_cp)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         write(*,*) 'about to define U_BCs'
         call init_U_BCs(mom%U,m,mom%SP)
         call update_BC_vals(mom%U)
         write(*,*) 'U_BCs defined'
         call init_P_BCs(mom%p,m,mom%SP)
         call update_BC_vals(mom%p)
         write(*,*) '     BCs initialized'
         if (mom%SP%VS%U%SS%solve) call print_BCs(mom%U,'U')
         if (mom%SP%VS%U%SS%solve) call export_BCs(mom%U,str(DT%U%BCs),'U')
         if (mom%SP%VS%U%SS%solve) call print_BCs(mom%p,'p')
         if (mom%SP%VS%U%SS%solve) call export_BCs(mom%p,str(DT%p%BCs),'p')

         call init_U_field(mom%U,m,mom%SP,str(DT%U%field))
         call init_P_field(mom%p,m,mom%SP,str(DT%p%field))
         call assign(mom%Unm1,mom%U)
         write(*,*) '     Field initialized'

         call apply_BCs(mom%p)
         write(*,*) '     P BCs applied'
         call apply_BCs(mom%U)
         write(*,*) '     U BCs applied'

         call init(mom%Ustar,mom%U)
         if (SP%VS%U%SS%prescribed_BCs) call set_prescribed_BCs(mom%Ustar)
         call init_Ustar_field(mom%Ustar,mom%m,mom%U,mom%SP,str(DT%U%field))
         write(*,*) '     Intermediate field initialized'

         write(*,*) '     about to assemble Laplacian matrices'
         if (mom%SP%matrix_based) call init_matrix_based_ops(mom)

         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)
         write(*,*) '     Interpolated fields initialized'

         call init(mom%probe_divU,str(DT%U%residual),'probe_divU',mom%SP%VS%U%SS%restart,.true.)
         call init(mom%probe_KE,str(DT%U%energy),'KE',mom%SP%VS%U%SS%restart,.false.)
         call init(mom%probe_Q,str(DT%U%energy),'probe_Q',mom%SP%VS%U%SS%restart,.true.)
         if (m%MP%plane_any) then
          call init(mom%probe_KE_2C,str(DT%U%energy),'KE_2C',mom%SP%VS%U%SS%restart,.true.)
         endif
         write(*,*) '     momentum probes initialized'
         call init(mom%TS,mom%m,mom%U,mom%SP%TSP,str(DT%U%stats),'U')

         ! Initialize interior solvers
         call init(mom%GS_p,mom%p,mom%m,mom%SP%VS%P%ISP,str(DT%p%residual),'p')
         write(*,*) '     GS solver initialized for p'

         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom_VF,mom%m,&
         mom%SP%VS%U%ISP,mom%SP%VS%U%MFP,mom%Ustar,mom%temp_E,str(DT%U%residual),'U',.false.,.false.)
         write(*,*) '     PCG solver initialized for U'

         call init(mom%PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_lap_SF,mom%m,&
         mom%SP%VS%P%ISP,mom%SP%VS%P%MFP,mom%p,mom%temp_F1,str(DT%p%residual),'p',.false.,.false.)
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
         call delete(mom%temp_F1)
         call delete(mom%temp_F2)
         call delete(mom%temp_F3)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%temp_CC_VF)
         call delete(mom%TF_CC)
         call delete(mom%TF_CC_edge)
         call delete(mom%divU)
         call delete(mom%U_CC)
         call delete(mom%probe_divU)
         call delete(mom%probe_KE)
         call delete(mom%probe_KE_2C)
         call delete(mom%probe_Q)
         call delete(mom%temp_E)
         call delete(mom%m)
         call delete(mom%PCG_P)
         call delete(mom%PCG_U)
         call delete(mom%GS_p)
         call delete(mom%TS)
         call delete(mom%SP)
         write(*,*) 'Momentum object deleted'
       end subroutine

       subroutine display_momentum(mom,un)
         implicit none
         type(momentum),intent(in) :: mom
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MOMENTUM **************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Re,Ha = ',mom%SP%DP%Re,mom%SP%DP%Ha
         write(un,*) 'N,Gr = ',mom%SP%DP%N,mom%SP%DP%Gr
         write(un,*) 't,dt = ',mom%SP%VS%U%TMP%t,mom%SP%VS%U%TMP%dt
         write(un,*) 'solveUMethod,N_mom,N_PPE = ',mom%SP%VS%U%SS%solve_method,&
         mom%SP%VS%U%ISP%iter_max,mom%SP%VS%P%ISP%iter_max
         write(un,*) 'tol_mom,tol_PPE = ',mom%SP%VS%U%ISP%tol_rel,mom%SP%VS%P%ISP%tol_rel
         write(un,*) 'nstep,KE = ',mom%SP%VS%U%TMP%n_step,get_data(mom%probe_KE)
         if (mom%TS%TSP%collect) call display(mom%TS%TSP,un)
         ! call displayPhysicalMinMax(mom%U,'U',un)
         call displayPhysicalMinMax(mom%divU,'divU',un)
         write(un,*) 'CFL = ',CFL_number(mom%U_CC,mom%m,mom%SP%VS%U%TMP%dt)
         write(un,*) ''
         call display(mom%m,un)
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
         if (.not.mom%SP%EL%export_soln_only) then
           call export(mom%U     ,str(DT%U%restart),'U')
           call export(mom%Ustar ,str(DT%U%restart),'Ustar')
           call export(mom%Unm1  ,str(DT%U%restart),'Unm1')
           call export(mom%p     ,str(DT%p%restart),'p')
         endif
       end subroutine

       subroutine import_momentum(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         if (.not.mom%SP%EL%export_soln_only) then
           call import(mom%U     ,str(DT%U%restart),'U')
           call import(mom%Ustar ,str(DT%U%restart),'Ustar')
           call import(mom%Unm1  ,str(DT%U%restart),'Unm1')
           call import(mom%p     ,str(DT%p%restart),'p')
         endif
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_momentum(mom,DT,F,Fnm1)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F,Fnm1
         type(dir_tree),intent(in) :: DT
         if (.not.mom%SP%EL%export_soln_only) then
         if (mom%SP%VS%B%SS%solve.or.mom%SP%VS%T%SS%solve) then
           call export_raw(mom%m,F,str(DT%U%field),'F_external',0)
           call export_raw(mom%m,Fnm1,str(DT%U%field),'Fnm1_external',0)
         endif
         endif
         call export_tec_momentum_no_ext(mom,DT)
       end subroutine
       subroutine export_tec_momentum_no_ext(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         if (mom%SP%VS%U%SS%restart.and.(.not.mom%SP%VS%U%SS%solve)) then
           ! This preserves the initial data
         else
           write(*,*) 'export_tec_momentum_no_ext at n_step = ',mom%SP%VS%U%TMP%n_step
           call export_processed(mom%m,mom%U,str(DT%U%field),'U',1)

           if (.not.mom%SP%EL%export_soln_only) then
             call export_processed(mom%m,mom%p,str(DT%p%field),'p',1)
             call export_raw(mom%m,mom%U,str(DT%U%field),'U',0)
             call export_raw(mom%m,mom%p,str(DT%p%field),'p',0)
             call export_raw(mom%m,mom%Ustar,str(DT%U%field),'Ustar',0)
             call export_raw(mom%m,mom%PCG_P%r,str(DT%P%field),'residual_p',0)
             call export_raw(mom%m,mom%PCG_U%r,str(DT%U%field),'residual_Ustar',0)
             if (mom%SP%EL%export_symmetric) then
              call export_processed(mom%m,mom%U,str(DT%U%field),'U',1,mom%SP%MP)
              call export_processed(mom%m,mom%p,str(DT%p%field),'p',1,mom%SP%MP)
             endif
             call export_raw(mom%m,mom%divU,str(DT%U%field),'divU',0)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_unsteady_0D_mom(mom,TMP)
         implicit none
         type(momentum),intent(inout) :: mom
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: temp,scale
         scale = mom%SP%DP%KE_scale

         call compute_TKE(temp,mom%U_CC,mom%m,scale)
         call export(mom%probe_KE,TMP,temp)

         if (mom%SP%compute_surface_power) then
         temp = surface_power(mom%U,mom%m,mom%temp_F1,mom%temp_F2,mom%temp_CC_VF,mom%TF_CC)
         temp = scale*temp/mom%SP%DP%Re
         call export(mom%probe_Q,TMP,temp)
         endif

         if (mom%m%MP%plane_any) then
         if (mom%m%MP%plane(1)) call compute_TKE_2C(temp,mom%U_CC%y,mom%U_CC%z,mom%m,scale,mom%temp_CC)
         if (mom%m%MP%plane(2)) call compute_TKE_2C(temp,mom%U_CC%x,mom%U_CC%z,mom%m,scale,mom%temp_CC)
         if (mom%m%MP%plane(3)) call compute_TKE_2C(temp,mom%U_CC%x,mom%U_CC%y,mom%m,scale,mom%temp_CC)
         call export(mom%probe_KE_2C,TMP,temp)
         endif
         call div(mom%divU,mom%U,mom%m)
         call Ln(temp,mom%divU,2.0_cp,mom%m)
         call export(mom%probe_divU,TMP,temp)
       end subroutine

       subroutine export_unsteady_1D_mom(mom,TMP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%unsteady),'U',1,TMP,mom%SP%VS%U%unsteady_lines)
         call export_processed(mom%m,mom%p,str(DT%P%unsteady),'p',1,TMP,mom%SP%VS%P%unsteady_lines)
       end subroutine

       subroutine export_unsteady_2D_mom(mom,TMP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%unsteady),'U',1,TMP,mom%SP%VS%U%unsteady_planes)
         call export_processed(mom%m,mom%p,str(DT%P%unsteady),'p',1,TMP,mom%SP%VS%P%unsteady_planes)
       end subroutine

       subroutine export_unsteady_3D_mom(mom,TMP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%unsteady),'U',1,TMP,mom%SP%VS%U%unsteady_field)
         call export_processed(mom%m,mom%p,str(DT%P%unsteady),'p',1,TMP,mom%SP%VS%P%unsteady_field)
       end subroutine

       subroutine init_matrix_based_ops_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp),dimension(2) :: diffusion_treatment
         call init_Laplacian_SF(mom%m) ! Must come before PPE solver init
         call init_Laplacian_VF(mom%m) ! for lap(U) in momentum
         ! diffusion_treatment = (/-mom%SP%VS%U%TMP%dt/mom%SP%DP%Re,1.0_cp/) ! diffusion explicit
         ! diffusion_treatment = (/mom%SP%VS%U%TMP%dt/mom%SP%DP%Re,1.0_cp/)    ! diffusion explicit
         diffusion_treatment = (/mom%SP%VS%U%TMP%dt/mom%SP%DP%Re,1.0_cp/)    ! diffusion explicit
         ! diffusion_treatment = (/1.0_cp,0.0_cp/)             ! no treatment (requires multiplication by dt/Re)
         ! call modify_Laplacian_VF(mom%m,diffusion_treatment(1),diffusion_treatment(2))
         call multiply_Laplacian_VF(mom%m,diffusion_treatment(1))
         call add_Laplacian_VF(mom%m,diffusion_treatment(2))
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,F,Fnm1,TMP,EF,EN,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F,Fnm1
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         type(time_marching_params),intent(inout) :: TMP
         integer :: i

         do i=1,mom%SP%VS%U%TMP%multistep_iter
         select case(mom%SP%VS%U%SS%solve_method)
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
           mom%p,F,mom%m,TMP,mom%temp_F1,mom%temp_E,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case (6)
           call Euler_time_AB2_sources(mom%PCG_U,mom%PCG_P,mom%U,mom%Ustar,mom%Unm1,&
           mom%p,F,Fnm1,mom%m,TMP,mom%temp_F1,mom%temp_E,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case (7)
           call O2_BDF_time_AB2_sources(mom%PCG_U,mom%PCG_P,mom%U,mom%Ustar,&
           mom%Unm1,mom%p,F,Fnm1,mom%m,TMP,mom%temp_F1,mom%temp_E,mom%temp_CC,&
           EF%unsteady_0D%export_now)
         case default; stop 'Error: solveUMethod must = 1:4 in momentum.f90.'
         end select
         call iterate_step(TMP)
         enddo

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)

         ! U at cell edge is needed for advection term at next time step
         ! and in induction solver. Neither case requires the diagonal.
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (EF%unsteady_0D%export_now) call export_unsteady_0D(mom,TMP)
         if (EF%unsteady_1D%export_now) call export_unsteady_1D(mom,TMP,DT)
         if (EF%unsteady_2D%export_now) call export_unsteady_2D(mom,TMP,DT)
         if (EF%unsteady_3D%export_now) call export_unsteady_3D(mom,TMP,DT)
         if (EF%info%export_now) call print(mom)

         if (EF%final_solution%export_now.or.EN%U%this.or.EN%all%this) then
           ! call export(mom,DT)
           call export_tec(mom,DT,F,Fnm1)
         endif
         call update(mom%TS,mom%m,mom%U,TMP,mom%temp_F1,mom%temp_CC_VF,mom%TF_CC)
       end subroutine

       subroutine compute_export_E_K_Budget(mom,B,B0,J,MD_fluid,DT)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(momentum),intent(inout) :: mom
         type(mesh_domain),intent(in) :: MD_fluid
         type(VF),intent(in) :: B,B0,J
         call E_K_Budget_wrapper(DT,mom%U,mom%Unm1,&
         B,B0,J,mom%p,mom%m,mom%SP%VS%U%TMP,mom%SP%DP,mom%SP%MP,MD_fluid)
       end subroutine

       end module