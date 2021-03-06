       module induction_extend_mod
       use induction_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use IO_export_mod
       use IO_import_mod
       use dir_tree_mod
       use string_mod
       use path_extend_mod
       use path_extend_mod
       use export_raw_processed_mod
       use export_raw_processed_symmetry_mod
       use import_raw_mod
       use export_processed_FPL_mod
       use export_frequency_mod
       use export_now_mod
       use induction_sources_mod
       use assign_B0_vs_t_mod
       use datatype_conversion_mod
       use RK_Params_mod

       use init_B_BCs_mod
       use init_Bstar_field_mod
       use init_phi_BCs_mod
       use init_B_field_mod
       use init_phi_field_mod
       use init_B0_field_mod
       use init_B_interior_mod
       use init_J_interior_mod
       use init_Sigma_mod
       use ops_embedExtract_mod
       use clean_divergence_mod
       use BC_funcs_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use probe_mod
       use probe_extend_mod
       use ops_norms_mod
       use var_set_mod
       use mirror_props_mod
       use mirror_props_extend_mod

       use mesh_domain_extend_mod
       use grid_mod
       use mesh_extend_mod
       use norms_extend_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use boundary_conditions_extend_mod
       use apply_BCs_mod
       use ops_advect_mod
       use time_marching_methods_mod
       use induction_solver_mod
       use preconditioners_mod
       use PCG_solver_extend_mod
       use FFT_solver_extend_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use induction_aux_mod
       use E_M_Budget_mod

       implicit none

       private
       public :: induction
       public :: init,delete,display,print,export,import ! Essentials
       public :: solve,export_tec,compute_export_E_M_budget
       public :: export_unsteady
       public :: compute_J_ind
       public :: set_necessary_for_restart

       interface init;                      module procedure init_induction;                end interface
       interface display;                   module procedure display_induction;             end interface
       interface print;                     module procedure print_induction;               end interface
       interface set_necessary_for_restart; module procedure set_necessary_for_restart_ind; end interface

       interface solve;                     module procedure solve_induction;               end interface
       interface export_tec;                module procedure export_tec_induction;          end interface

       interface set_sigma_inv;             module procedure set_sigma_inv_ind;             end interface

       interface export_unsteady;           module procedure export_unsteady_ind;           end interface
       interface export_unsteady_0D;        module procedure export_unsteady_0D_ind;        end interface
       interface export_unsteady_1D;        module procedure export_unsteady_1D_ind;        end interface
       interface export_unsteady_2D;        module procedure export_unsteady_2D_ind;        end interface
       interface export_unsteady_3D;        module procedure export_unsteady_3D_ind;        end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_induction(ind,SP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         type(TF) :: TF_face,sigmaInv_edge_TF
         integer :: temp_unit
         write(*,*) 'Initializing induction:'

         write(*,*) '     Domains copied'
         call init_CC(ind%CC_VF_fluid,ind%m,ind%MD_fluid); call assign(ind%CC_VF_fluid,0.0_cp)
         call init_CC(ind%CC_VF_sigma,ind%m,ind%MD_sigma); call assign(ind%CC_VF_sigma,0.0_cp)
         ! --- tensor,vector and scalar fields ---
         call init_Face(ind%B_interior     ,ind%m,ind%MD_sigma); call assign(ind%B_interior,0.0_cp)
         call init_Edge(ind%J_interior     ,ind%m,ind%MD_sigma); call assign(ind%J_interior,0.0_cp)
         call init_Edge(ind%U_E            ,ind%m,0.0_cp)
         call init_Edge(ind%temp_E_TF      ,ind%m,0.0_cp)
         call init_Face(ind%temp_F1_TF     ,ind%m,0.0_cp)
         call init_Face(ind%temp_F2_TF     ,ind%m,0.0_cp)
         call init_Face(ind%F              ,ind%m,0.0_cp)
         call init_Face(ind%jCrossB        ,ind%m,0.0_cp)
         call init_Face(ind%Fnm1           ,ind%m,0.0_cp)
         call init_Face(ind%L              ,ind%m,0.0_cp)
         call init_Face(ind%B              ,ind%m,0.0_cp)
         call init_Face(ind%Btot           ,ind%m,0.0_cp)
         call init_Face(ind%Bnm1           ,ind%m,0.0_cp)
         call init_Face(ind%B0             ,ind%m,0.0_cp)
         call init_Face(ind%dB0dt          ,ind%m,0.0_cp)
         call init_CC(ind%temp_CC_VF       ,ind%m,0.0_cp)
         call init_Edge(ind%J              ,ind%m,0.0_cp)
         call init_Face(ind%curlUCrossB    ,ind%m,0.0_cp)
         call init_Edge(ind%temp_E         ,ind%m,0.0_cp)
         call init_Edge(ind%sigmaInv_edge  ,ind%m,0.0_cp)
         call init_Face(ind%temp_F1        ,ind%m,0.0_cp)
         call init_Face(ind%temp_F2        ,ind%m,0.0_cp)
         call init_CC(ind%phi              ,ind%m,0.0_cp)
         call init_CC(ind%temp_CC          ,ind%m,0.0_cp)
         call init_CC(ind%divB             ,ind%m,0.0_cp)
         call init_Node(ind%divJ           ,ind%m,0.0_cp)
         call init_CC(ind%sigmaInv_CC      ,ind%m,0.0_cp)
         call init_CC(ind%cell_volume      ,ind%m,0.0_cp)
         call init_CC(ind%cell_inverse_area,ind%m,0.0_cp)
         call init_CC(ind%stresses         ,ind%m,0.0_cp)
         call init_CC(ind%temp_CC_TF       ,ind%m,0.0_cp)

         write(*,*) '     Fields allocated'

         call volume(ind%cell_volume,ind%m)
         call inverse_area(ind%cell_inverse_area,ind%m)
         ! --- Initialize Fields ---
         call init_B_BCs(ind%B,ind%m,SP);     write(*,*) '     B BCs initialized'
         call update_BC_vals(ind%B)
         call init_phi_BCs(ind%phi,ind%m,SP); write(*,*) '     phi BCs initialized'
         call update_BC_vals(ind%phi)

         call init_B0_field(ind%B0,ind%m,SP)
         call init_B_field(ind%B,SP)
         call init_phi_field(ind%phi,SP)
         call assign(ind%Bnm1,ind%B)

         if (SP%IT%unsteady_B0%add) call assign_B0_vs_t(ind%B0,SP%VS%B%TMP)
         call multiply(ind%B0,SP%IT%B_applied%scale)

         write(*,*) '     B-field initialized'
         ! call initB_interior(ind%B_interior,ind%m,ind%MD_sigma,str(DT%B%restart))
         ! call initJ_interior(ind%J_interior,ind%m,ind%MD_sigma,str(DT%J%restart))
         call assign_ghost_XPeriodic(ind%B_interior,0.0_cp)
         call apply_BCs(ind%B);                           write(*,*) '     BCs applied'

         call init(ind%Bstar,ind%B)
         if (SP%VS%B%SS%prescribed_BCs) call set_prescribed_BCs(ind%Bstar)
         call set_BCs_homogeneous(ind%Bstar)
         call init_Bstar_field(ind%Bstar,ind%B)

         write(*,*) '     Intermediate B-field initialized'

         if (SP%VS%B%SS%solve) call export_BCs(ind%B,str(DT%B%BCs),'B')
         if (SP%VS%B%SS%solve) call export_BCs(ind%phi,str(DT%phi%BCs),'phi')
         ! if (SP%VS%B%SS%solve) call print_BCs(ind%B,'B')
         ! if (SP%VS%B%SS%solve) call print_BCs(ind%phi,'phi')

         ! ******************** MATERIAL PROPERTIES ********************
         call set_sigma_inv(ind,SP)
         write(*,*) '     Materials initialized'
         if (SP%EL%export_mat_props) call export_raw(ind%m,ind%sigmaInv_edge,str(DT%mat),'sigmaInv',0)

         ! *************************************************************

         call compute_J_ind(ind,SP)
         call compute_Btot(ind,SP)
         call compute_JCrossB_ind(ind,SP)
         call compute_stresses_ind(ind,SP)

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************

         ! Initialize multigrid
         temp_unit = new_and_open(str(DT%params),'info_ind')
         call display(ind,SP,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_ind')

         write(*,*) '     About to assemble curl-curl matrix'

         call init_Edge(sigmaInv_edge_TF%x,ind%m,0.0_cp) ! x-component used in matrix_free_operators.f90
         call assign(sigmaInv_edge_TF%x,ind%sigmaInv_edge)
         call init(ind%PCG_B,ind_diffusion,ind_diffusion_explicit,prec_ind_VF,ind%m,&
         SP%VS%B%ISP,SP%VS%B%MFP,ind%Bstar,ind%B,sigmaInv_edge_TF,str(DT%B%residual),'B',.false.,.false.)
         call delete(sigmaInv_edge_TF)
         write(*,*) '     PCG Solver initialized for B'

         call init_Face(TF_face%x,ind%m,0.0_cp) ! x-component used in matrix_free_operators.f90
         call init(ind%PCG_cleanB,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_lap_SF,&
         ind%m,SP%VS%phi%ISP,SP%VS%phi%MFP,ind%phi,ind%phi,TF_face,str(DT%phi%residual),'phi',.false.,.false.)
         call delete(TF_face)
         write(*,*) '     PCG Solver initialized for phi'

         call init(ind%FFT_cleanB,ind%phi,ind%m,SP%GP%FFT_dir,'FFT_phi')
         write(*,*) '     FFT Solver initialized for phi'

         write(*,*) '     Finished'
       end subroutine

       subroutine display_induction(ind,SP,un)
         implicit none
         type(induction),intent(in) :: ind
         type(sim_params),intent(in) :: SP
         integer,intent(in) :: un
         if (SP%FCL%export_heavy) then
           write(un,*) '**********************************************************'
           write(un,*) '************************ MAGNETIC ************************'
           write(un,*) '**********************************************************'
           write(un,*) 'Rem,finite_Rem,include_vacuum = ',SP%DP%Rem,SP%SCP%finite_Rem,SP%SCP%include_vacuum
           write(un,*) 't,dt = ',SP%VS%B%TMP%t,SP%VS%B%TMP%TS%dt
           write(un,*) 'solveBMethod,N_ind,N_cleanB = ',SP%VS%B%SS%solve_method,&
           SP%VS%B%ISP%EC%iter_max,SP%VS%phi%ISP%EC%iter_max
           write(un,*) 'tol_ind,tol_cleanB = ',SP%VS%B%ISP%EC%tol_rel,SP%VS%phi%ISP%EC%tol_rel
           write(un,*) 'nstep,ME = ',SP%VS%B%TMP%n_step,get_data(SP%PS_ind%ME(1))
           ! call displayPhysicalMinMax(ind%dB0dt,'dB0dt',un)
           ! call displayPhysicalMinMax(ind%B0,'B0',un)
           call displayPhysicalMinMax(ind%divB,'divB',un)
           call displayPhysicalMinMax(ind%divJ,'divJ',un)
           write(un,*) ''
           ! call display(ind%m,un)
         endif
       end subroutine

       subroutine print_induction(ind,SP)
         implicit none
         type(induction),intent(in) :: ind
         type(sim_params),intent(in) :: SP
         call display(ind,SP,6)
       end subroutine

       subroutine set_necessary_for_restart_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call set_necessary_for_restart(ind%B)
         call set_necessary_for_restart(ind%Bnm1)
         call set_necessary_for_restart(ind%B0)
         call set_necessary_for_restart(ind%dB0dt)
         call set_necessary_for_restart(ind%F)
         call set_necessary_for_restart(ind%Fnm1)
         call set_necessary_for_restart(ind%L)
         call set_necessary_for_restart(ind%Bstar)
         call set_necessary_for_restart(ind%phi)
         call set_necessary_for_restart(ind%J)
         call set_necessary_for_restart(ind%PCG_B%r)
         call set_necessary_for_restart(ind%PCG_cleanB%r)
         call set_necessary_for_restart(ind%PCG_B%p)
         call set_necessary_for_restart(ind%PCG_cleanB%p)
         call set_necessary_for_restart(ind%PCG_B%x_BC)
         call set_necessary_for_restart(ind%PCG_cleanB%x_BC)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_induction(ind,SP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         if (SP%FCL%restart_all.and.(.not.SP%VS%B%SS%solve)) then
           ! This preserves the initial data
         else
           if (SP%VS%B%SS%solve) then
             write(*,*) 'export_tec_induction at n_step = ',SP%VS%B%TMP%n_step
             call export_processed(ind%m,ind%B,str(DT%B%field),'B',1)
             call export_processed(ind%m,ind%B0,str(DT%B%field),'B0',1)
             call export_processed(ind%m,ind%phi ,str(DT%phi%field),'phi',1)
             call export_processed(ind%m,ind%J ,str(DT%J%field),'J',1)
             call export_raw(ind%m,ind%divB ,str(DT%B%field),'divB',0)
             call export_raw(ind%m,ind%phi ,str(DT%phi%field),'phi',0)

             call export_raw(ind%m,ind%U_E%x,str(DT%B%field),'U_E_x',1)
             call export_raw(ind%m,ind%U_E%y,str(DT%B%field),'U_E_y',1)
             call export_raw(ind%m,ind%U_E%z,str(DT%B%field),'U_E_z',1)

             call export_raw(ind%m,ind%jCrossB ,str(DT%jCrossB%field),'jCrossB',0)
             call export_processed(ind%m,ind%jCrossB ,str(DT%jCrossB%field),'jCrossB',1)

             call export_raw(ind%m,ind%stresses%x,str(DT%stresses%field),'stresses_x',0)
             call export_processed(ind%m,ind%stresses%x,str(DT%stresses%field),'stresses_x',1)
             call export_processed(ind%m,ind%stresses%y,str(DT%stresses%field),'stresses_y',1)
             call export_processed(ind%m,ind%stresses%z,str(DT%stresses%field),'stresses_z',1)

             if (SP%EL%export_symmetric) then
             call export_processed(ind%m,ind%B,str(DT%B%field),'B',1,anti_mirror(SP%MP))
             call export_processed(ind%m,ind%J,str(DT%J%field),'J',1,SP%MP)
             endif
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_unsteady_0D_ind(ind,SP,TMP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(inout) :: SP
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: temp,scale
         integer :: i
         call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
         call compute_Ln(temp,ind%divB,2.0_cp,ind%m); call export(SP%PS_ind%probe_divB,TMP,temp)
         call compute_Ln(temp,ind%divJ,2.0_cp,ind%m); call export(SP%PS_ind%probe_divJ,TMP,temp)

         call export(SP%PS_ind%probe_dB0dt(1),TMP,amax(ind%dB0dt%x))
         call export(SP%PS_ind%probe_dB0dt(2),TMP,amax(ind%dB0dt%y))
         call export(SP%PS_ind%probe_dB0dt(3),TMP,amax(ind%dB0dt%z))
         call export(SP%PS_ind%probe_B0(1),TMP,amax(ind%B0%x))
         call export(SP%PS_ind%probe_B0(2),TMP,amax(ind%B0%y))
         call export(SP%PS_ind%probe_B0(3),TMP,amax(ind%B0%z))
         call export(SP%PS_ind%probe_B1(1),TMP,amax(ind%B%x))
         call export(SP%PS_ind%probe_B1(2),TMP,amax(ind%B%y))
         call export(SP%PS_ind%probe_B1(3),TMP,amax(ind%B%z))
         call export(SP%PS_ind%probe_Btot(1),TMP,amax(ind%Btot%x))
         call export(SP%PS_ind%probe_Btot(2),TMP,amax(ind%Btot%y))
         call export(SP%PS_ind%probe_Btot(3),TMP,amax(ind%Btot%z))

         scale = SP%DP%ME_scale

         do i=1,3
           if (i.eq.1) then
             call face2cellCenter(ind%temp_CC_VF,ind%Btot,ind%m)
           elseif (i.eq.2) then
             call face2cellCenter(ind%temp_CC_VF,ind%B0,ind%m)
           elseif (i.eq.3) then
             call face2cellCenter(ind%temp_CC_VF,ind%B,ind%m)
           endif
           call compute_Total_Energy(SP%PS_ind%ME(i),ind%temp_CC_VF,TMP,ind%m,scale)
           call compute_Total_Energy_Domain(SP%PS_ind%ME_fluid(i),ind%temp_CC_VF,ind%CC_VF_fluid,TMP,ind%m,scale,ind%MD_fluid)
           call compute_Total_Energy_Domain(SP%PS_ind%ME_conductor(i),ind%temp_CC_VF,ind%CC_VF_sigma,TMP,ind%m,scale,ind%MD_sigma)
           call compute_Energy_Component(SP%PS_ind%Bx(i),ind%temp_CC_VF%x,TMP,ind%m,scale)
           call compute_Energy_Component(SP%PS_ind%By(i),ind%temp_CC_VF%y,TMP,ind%m,scale)
           call compute_Energy_Component(SP%PS_ind%Bz(i),ind%temp_CC_VF%z,TMP,ind%m,scale)
         enddo

         call face2cellCenter(ind%temp_CC_VF,ind%jCrossB,ind%m)
         call multiply(ind%temp_CC_VF,ind%cell_volume)
         call magnitude(ind%temp_CC,ind%temp_CC_VF)
         call export(SP%PS_ind%amax_JxB  ,TMP,amax(ind%temp_CC))
         call export(SP%PS_ind%amax_JxB_x,TMP,amax(ind%temp_CC_VF%x))
         call export(SP%PS_ind%amax_JxB_y,TMP,amax(ind%temp_CC_VF%y))
         call export(SP%PS_ind%amax_JxB_z,TMP,amax(ind%temp_CC_VF%z))

         call export(SP%PS_ind%amax_stress_xx,TMP,amax(ind%stresses%x%x))
         call export(SP%PS_ind%amax_stress_xy,TMP,amax(ind%stresses%x%y))
         call export(SP%PS_ind%amax_stress_xz,TMP,amax(ind%stresses%x%z))
         call export(SP%PS_ind%amax_stress_yx,TMP,amax(ind%stresses%y%x))
         call export(SP%PS_ind%amax_stress_yy,TMP,amax(ind%stresses%y%y))
         call export(SP%PS_ind%amax_stress_yz,TMP,amax(ind%stresses%y%z))
         call export(SP%PS_ind%amax_stress_zx,TMP,amax(ind%stresses%z%x))
         call export(SP%PS_ind%amax_stress_zy,TMP,amax(ind%stresses%z%y))
         call export(SP%PS_ind%amax_stress_zz,TMP,amax(ind%stresses%z%z))

         call assign(ind%CC_VF_fluid,0.0_cp)
         call assign(ind%temp_CC_TF,ind%stresses)
         call embedCC(ind%temp_CC_TF%x,ind%CC_VF_fluid,ind%MD_fluid)
         call embedCC(ind%temp_CC_TF%y,ind%CC_VF_fluid,ind%MD_fluid)
         call embedCC(ind%temp_CC_TF%z,ind%CC_VF_fluid,ind%MD_fluid)

         call export(SP%PS_ind%amax_stress_walls_xx,TMP,amax(ind%temp_CC_TF%x%x))
         call export(SP%PS_ind%amax_stress_walls_xy,TMP,amax(ind%temp_CC_TF%x%y))
         call export(SP%PS_ind%amax_stress_walls_xz,TMP,amax(ind%temp_CC_TF%x%z))
         call export(SP%PS_ind%amax_stress_walls_yx,TMP,amax(ind%temp_CC_TF%y%x))
         call export(SP%PS_ind%amax_stress_walls_yy,TMP,amax(ind%temp_CC_TF%y%y))
         call export(SP%PS_ind%amax_stress_walls_yz,TMP,amax(ind%temp_CC_TF%y%z))
         call export(SP%PS_ind%amax_stress_walls_zx,TMP,amax(ind%temp_CC_TF%z%x))
         call export(SP%PS_ind%amax_stress_walls_zy,TMP,amax(ind%temp_CC_TF%z%y))
         call export(SP%PS_ind%amax_stress_walls_zz,TMP,amax(ind%temp_CC_TF%z%z))

         scale = SP%DP%JE_scale
         call edge2cellCenter(ind%temp_CC_VF,ind%J,ind%m,ind%temp_F1)
         call compute_Total_Energy(SP%PS_ind%JE,ind%temp_CC_VF,TMP,ind%m,scale)
         call compute_Total_Energy_Domain(SP%PS_ind%JE_fluid,ind%temp_CC_VF,ind%CC_VF_fluid,TMP,ind%m,scale,ind%MD_fluid)
       end subroutine

       subroutine export_unsteady_1D_ind(ind,SP,TMP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(ind%m,ind%B,str(DT%B%unsteady),'B',1,TMP,SP%VS%B%unsteady_lines)
         call export_processed(ind%m,ind%J,str(DT%J%unsteady),'J',1,TMP,SP%VS%B%unsteady_lines)
       end subroutine

       subroutine export_unsteady_2D_ind(ind,SP,TMP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(ind%m,ind%jCrossB,&
          str(DT%jCrossB%unsteady),'jCrossB',1,TMP,SP%VS%jCrossB%unsteady_planes)
         call export_processed(ind%m,ind%B%x,str(DT%B%unsteady),'B_x',1,TMP,SP%VS%B%unsteady_planes)
         call export_processed(ind%m,ind%Btot%x,str(DT%B%unsteady),'Btot_x',1,TMP,SP%VS%B%unsteady_planes)
         ! call export_processed(ind%m,ind%stresses%x,str(DT%stresses%unsteady),'stresses_x',1,TMP,SP%VS%stresses%unsteady_planes)
         ! call export_processed(ind%m,ind%stresses%y,str(DT%stresses%unsteady),'stresses_y',1,TMP,SP%VS%stresses%unsteady_planes)
         call export_processed(ind%m,ind%stresses%z,&
          str(DT%stresses%unsteady),'stresses_z',1,TMP,SP%VS%stresses%unsteady_planes)
         call export_processed(ind%m,ind%temp_CC_TF%z,&
          str(DT%stresses%unsteady),'stresses_walls_z',1,TMP,SP%VS%stresses%unsteady_planes)
         call export_processed(ind%m,ind%J,str(DT%J%unsteady),'J',1,TMP,SP%VS%B%unsteady_planes)

         ! call export_processed(ind%m,ind%B,str(DT%B%unsteady),'B',1,TMP,SP%VS%B%unsteady_planes)
         ! call export_processed(ind%m,ind%J,str(DT%J%unsteady),'J',1,TMP,SP%VS%B%unsteady_planes)
       end subroutine

       subroutine export_unsteady_3D_ind(ind,SP,TMP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(ind%m,ind%B,str(DT%B%unsteady),'B',1,TMP,SP%VS%B%unsteady_field)
         call export_processed(ind%m,ind%J,str(DT%J%unsteady),'J',1,TMP,SP%VS%B%unsteady_field)
       end subroutine

       subroutine compute_J_ind(ind,SP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         ! call assign(ind%J,0.0_cp)
         ! call embedEdge(ind%J,ind%J_interior,ind%MD_sigma)
         call compute_J(ind%J,ind%B,SP%IT%current%scale,ind%m)
       end subroutine

       subroutine compute_JCrossB_ind(ind,SP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         call compute_JCrossB(ind%jCrossB,ind%Btot,ind%J,ind%m,SP%MT%JCrossB%scale,&
         ind%temp_CC,ind%temp_F1_TF,ind%temp_F2_TF)
         ! call compute_B_dot_gradB(ind%jCrossB,ind%B,ind%m,SP%MT%JCrossB%scale,&
         ! ind%temp_CC,ind%temp_F1,ind%temp_F2,ind%temp_E_TF)
       end subroutine

       subroutine compute_stresses_ind(ind,SP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         call compute_Lorentz_stresses(ind%stresses,ind%Btot,ind%m,&
         SP%DP%Al,ind%temp_CC_VF)
       end subroutine

       subroutine set_sigma_inv_ind(ind,SP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         call set_sigma_inv_VF(ind%sigmaInv_edge,ind%m,ind%MD_sigma,SP%DP)
       end subroutine

       subroutine solve_induction(ind,SP,F,Fnm1,L,TMP,EF)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(VF),intent(in) :: F,Fnm1,L
         type(time_marching_params),intent(inout) :: TMP
         type(export_frequency),intent(in) :: EF
         integer :: i
         do i=1,TMP%multistep_iter
         select case (SP%VS%B%SS%solve_method)
         case (1)
           call Euler_time_no_diff_Euler_sources_no_correction(ind%B,ind%Bstar,F,TMP)
         case (2)
           call Euler_time_no_diff_AB2_sources_no_correction(ind%B,ind%Bstar,F,Fnm1,TMP)
         case (3)
           call Euler_time_no_diff_Euler_sources(ind%PCG_cleanB,ind%B,ind%Bstar,&
           ind%Bnm1,ind%phi,F,ind%m,TMP,ind%temp_F2,ind%temp_CC,EF%unsteady_0D%export_now)
         case (4)
          call Euler_time_no_diff_AB2_sources(ind%PCG_cleanB,ind%B,ind%Bstar,&
           ind%Bnm1,ind%phi,F,Fnm1,ind%m,TMP,ind%temp_F1,ind%temp_CC,&
           EF%unsteady_0D%export_now)
         case (5)
           call Euler_time_Euler_sources(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%Bstar,ind%Bnm1,&
           ind%phi,F,ind%m,TMP,ind%temp_F1,ind%temp_CC,&
           EF%unsteady_0D%export_now)
         case (6)
           call Euler_time_AB2_sources(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%Bstar,ind%Bnm1,&
           ind%phi,F,Fnm1,ind%m,TMP,ind%temp_F1,ind%L,ind%temp_CC,&
           EF%unsteady_0D%export_now)
         case (7)
           call O2_BDF_time_AB2_sources(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%Bstar,&
           ind%Bnm1,ind%phi,F,Fnm1,ind%m,TMP,ind%temp_F1,ind%temp_CC,&
           EF%unsteady_0D%export_now)
         case (8)
           call Euler_time_RK_sources(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%Bstar,ind%Bnm1,&
           ind%phi,F,Fnm1,L,ind%m,TMP,TMP%RKP,ind%temp_F1,ind%temp_CC,&
           EF%unsteady_0D%export_now)
         case (9)
           call Euler_time_AB2_sources_FFT(ind%PCG_B,ind%FFT_cleanB,ind%B,ind%Bstar,ind%Bnm1,&
           ind%phi,F,Fnm1,ind%m,TMP,ind%temp_F1,ind%L,ind%temp_CC,&
           EF%unsteady_0D%export_now)
         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select
         if (SP%SCP%embed_B_interior) call embedFace(ind%B,ind%B_interior,ind%MD_sigma)
         enddo
         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call compute_J_ind(ind,SP)
         call compute_Btot(ind,SP)
         call compute_JCrossB_ind(ind,SP)
         call compute_stresses_ind(ind,SP)
       end subroutine

       subroutine compute_Btot(ind,SP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         if (SP%SCP%finite_Rem) then
           call add(ind%Btot,ind%B0,ind%B)
         else
           call assign(ind%Btot,ind%B0)
         endif
       end subroutine

       subroutine export_unsteady_ind(ind,SP,TMP,EF,EN,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(inout) :: SP
         type(time_marching_params),intent(inout) :: TMP
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         if (EF%unsteady_0D%export_now) call export_unsteady_0D(ind,SP,TMP)
         if (EF%unsteady_1D%export_now) call export_unsteady_1D(ind,SP,TMP,DT)
         if (EF%unsteady_2D%export_now) call export_unsteady_2D(ind,SP,TMP,DT)
         if (EF%unsteady_3D%export_now) call export_unsteady_3D(ind,SP,TMP,DT)
         if (EF%info%export_now) call print(ind,SP)
         if (EF%final_solution%export_now.or.EN%B%this.or.EN%all%this) then
           ! call export(ind,str(DT%governing_equations),'ind')
           call export_tec(ind,SP,DT)
         endif
       end subroutine

       subroutine compute_export_E_M_budget(ind,SP,U,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(VF),intent(in) :: U
         type(dir_tree),intent(in) :: DT
         write(*,*) '       MAGNETIC ENERGY BUDGET - STARTED'
         call E_M_Budget_wrapper(DT,U,ind%B,ind%Bnm1,ind%B0,ind%B0,ind%J,ind%m,&
         ind%MD_fluid,ind%MD_sigma,SP%DP,SP%VS%B%TMP,SP%MP)
         write(*,*) '       MAGNETIC ENERGY BUDGET - COMPLETE'
       end subroutine

       end module