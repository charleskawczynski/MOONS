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

       interface init;                 module procedure init_induction;                end interface
       interface display;              module procedure display_induction;             end interface
       interface print;                module procedure print_induction;               end interface
       interface export;               module procedure export_induction;              end interface
       interface import;               module procedure import_induction;              end interface

       interface solve;                module procedure solve_induction;               end interface
       interface export_tec;           module procedure export_tec_induction;          end interface

       interface set_sigma_inv;        module procedure set_sigma_inv_ind;             end interface

       interface export_unsteady;      module procedure export_unsteady_ind;           end interface
       interface export_unsteady_0D;   module procedure export_unsteady_0D_ind;        end interface
       interface export_unsteady_1D;   module procedure export_unsteady_1D_ind;        end interface
       interface export_unsteady_2D;   module procedure export_unsteady_2D_ind;        end interface
       interface export_unsteady_3D;   module procedure export_unsteady_3D_ind;        end interface

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
         call init_Face(ind%Fnm1           ,ind%m,0.0_cp)
         call init_Face(ind%L              ,ind%m,0.0_cp)
         call init_Face(ind%B              ,ind%m,0.0_cp)
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

         write(*,*) '     Fields allocated'

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

         if (SP%IT%unsteady_B0%add) then
           call init(ind%probe_dB0dt(1),str(DT%B%energy),'dB0dt_x',SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
           call init(ind%probe_dB0dt(2),str(DT%B%energy),'dB0dt_y',SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
           call init(ind%probe_dB0dt(3),str(DT%B%energy),'dB0dt_z',SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
           call init(ind%probe_B0(1)   ,str(DT%B%energy),'B0_x',   SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
           call init(ind%probe_B0(2)   ,str(DT%B%energy),'B0_y',   SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
           call init(ind%probe_B0(3)   ,str(DT%B%energy),'B0_z',   SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
         endif
         call init(ind%probe_divB,str(DT%B%residual),'transient_divB',SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
         call init(ind%probe_divJ,str(DT%J%residual),'transient_divJ',SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
         call init(ind%JE,        str(DT%J%energy),'JE',            SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
         call init(ind%JE_fluid,  str(DT%J%energy),'JE_fluid',      SP%VS%B%SS%restart,.true.,SP%VS%B%TMP)
         call init(ind%ME(1)          ,str(DT%B%energy),'ME',           SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME_fluid(1)    ,str(DT%B%energy),'ME_fluid',     SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME_conductor(1),str(DT%B%energy),'ME_conductor', SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME(2)          ,str(DT%B%energy),'ME0',          SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME_fluid(2)    ,str(DT%B%energy),'ME0_fluid',    SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME_conductor(2),str(DT%B%energy),'ME0_conductor',SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME(3)          ,str(DT%B%energy),'ME1',          SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME_fluid(3)    ,str(DT%B%energy),'ME1_fluid',    SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)
         call init(ind%ME_conductor(3),str(DT%B%energy),'ME1_conductor',SP%VS%B%SS%restart,.false.,SP%VS%B%TMP)

         write(*,*) '     B/J probes initialized'

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

         write(*,*) '     Finished'
         if (SP%VS%B%SS%restart) call import(ind,SP,DT)
       end subroutine

       subroutine display_induction(ind,SP,un)
         implicit none
         type(induction),intent(in) :: ind
         type(sim_params),intent(in) :: SP
         integer,intent(in) :: un
         if (SP%FCL%export_heavy) then
           write(un,*) '**************************************************************'
           write(un,*) '************************** MAGNETIC **************************'
           write(un,*) '**************************************************************'
           write(un,*) 'Rem,finite_Rem,include_vacuum = ',SP%DP%Rem,SP%SCP%finite_Rem,SP%SCP%include_vacuum
           write(un,*) 't,dt = ',SP%VS%B%TMP%t,SP%VS%B%TMP%dt
           write(un,*) 'solveBMethod,N_ind,N_cleanB = ',SP%VS%B%SS%solve_method,&
           SP%VS%B%ISP%iter_max,SP%VS%phi%ISP%iter_max
           write(un,*) 'tol_ind,tol_cleanB = ',SP%VS%B%ISP%tol_rel,SP%VS%phi%ISP%tol_rel
           write(un,*) 'nstep,ME = ',SP%VS%B%TMP%n_step,get_data(ind%ME(1))
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

       subroutine export_induction(ind,SP,DT)
         implicit none
         type(induction),intent(in) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'export_induction at n_step = ',SP%VS%B%TMP%n_step
         call export_raw(ind%m,ind%B,str(DT%B%restart),'B',0)
         call export_raw(ind%m,ind%Bnm1,str(DT%B%restart),'Bnm1',0)
         call export_raw(ind%m,ind%B0,str(DT%B%restart),'B0',0)
         call export_raw(ind%m,ind%F,str(DT%B%restart),'F',0)
         call export_raw(ind%m,ind%Fnm1,str(DT%B%restart),'Fnm1',0)
         call export_raw(ind%m,ind%L,str(DT%B%restart),'L',0)
         call export_raw(ind%m,ind%Bstar,str(DT%B%restart),'Bstar',0)
         call export_raw(ind%m,ind%phi,str(DT%phi%restart),'phi',0)
         call export_raw(ind%m,ind%J,str(DT%J%restart),'J',0)
         if (SP%IT%unsteady_B0%add) then
           call export(ind%probe_dB0dt,str(DT%B%restart),'probe_dB0dt')
           call export(ind%probe_B0   ,str(DT%B%restart),'probe_B0')
         endif
         call export(ind%probe_divB,str(DT%B%restart),'probe_divB')
         call export(ind%probe_divJ,str(DT%J%restart),'probe_divJ')
         call export(ind%JE,        str(DT%J%restart),'JE')
         call export(ind%JE_fluid,  str(DT%J%restart),'JE_fluid')
         call export(ind%ME          ,str(DT%B%restart),'ME')
         call export(ind%ME_fluid    ,str(DT%B%restart),'ME_fluid')
         call export(ind%ME_conductor,str(DT%B%restart),'ME_conductor')
       end subroutine

       subroutine import_induction(ind,SP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'import_induction at n_step = ',SP%VS%B%TMP%n_step
         call import_raw(ind%m,ind%B,str(DT%B%restart),'B',0)
         call import_raw(ind%m,ind%Bnm1,str(DT%B%restart),'Bnm1',0)
         call import_raw(ind%m,ind%B0,str(DT%B%restart),'B0',0)
         call import_raw(ind%m,ind%F,str(DT%B%restart),'F',0)
         call import_raw(ind%m,ind%Fnm1,str(DT%B%restart),'Fnm1',0)
         call import_raw(ind%m,ind%L,str(DT%B%restart),'L',0)
         call import_raw(ind%m,ind%Bstar,str(DT%B%restart),'Bstar',0)
         call import_raw(ind%m,ind%phi,str(DT%phi%restart),'phi',0)
         call import_raw(ind%m,ind%J,str(DT%J%restart),'J',0)
         if (SP%IT%unsteady_B0%add) then
           call import(ind%probe_dB0dt,str(DT%B%restart),'probe_dB0dt')
           call import(ind%probe_B0   ,str(DT%B%restart),'probe_B0')
         endif
         call import(ind%probe_divB,str(DT%B%restart),'probe_divB')
         call import(ind%probe_divJ,str(DT%J%restart),'probe_divJ')
         call import(ind%JE,        str(DT%J%restart),'JE')
         call import(ind%JE_fluid,  str(DT%J%restart),'JE_fluid')
         call import(ind%ME          ,str(DT%B%restart),'ME')
         call import(ind%ME_fluid    ,str(DT%B%restart),'ME_fluid')
         call import(ind%ME_conductor,str(DT%B%restart),'ME_conductor')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_induction(ind,SP,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         if (SP%VS%B%SS%restart.and.(.not.SP%VS%B%SS%solve)) then
           ! This preserves the initial data
         else
           if (SP%VS%B%SS%solve) then
             write(*,*) 'export_tec_induction at n_step = ',SP%VS%B%TMP%n_step
             call export_processed(ind%m,ind%B,str(DT%B%field),'B',1)
             call export_processed(ind%m,ind%B0,str(DT%B%field),'B0',1)
             call export_processed(ind%m,ind%phi ,str(DT%phi%field),'phi',1)
             call export_processed(ind%m,ind%J ,str(DT%J%field),'J',1)
             call export_raw(ind%m,ind%divB ,str(DT%B%field),'divB',0)

             if (.not.SP%EL%export_soln_only) then
             if (SP%EL%export_symmetric) then
             call export_processed(ind%m,ind%B,str(DT%B%field),'B',1,anti_mirror(SP%MP))
             call export_processed(ind%m,ind%J,str(DT%J%field),'J',1,SP%MP)
             endif
             call export_raw(ind%m,ind%U_E%x  ,str(DT%B%restart),'U_E_x',0)
             call export_raw(ind%m,ind%U_E%y  ,str(DT%B%restart),'U_E_y',0)
             call export_raw(ind%m,ind%U_E%z  ,str(DT%B%restart),'U_E_z',0)
             endif
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_unsteady_0D_ind(ind,SP,TMP)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: temp,scale
         call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
         call Ln(temp,ind%divB,2.0_cp,ind%m); call export(ind%probe_divB,TMP,temp)
         call Ln(temp,ind%divJ,2.0_cp,ind%m); call export(ind%probe_divJ,TMP,temp)
         if (SP%IT%unsteady_B0%add) then
           call export(ind%probe_dB0dt(1),TMP,ind%dB0dt%x%BF(1)%GF%f(1,1,1))
           call export(ind%probe_dB0dt(2),TMP,ind%dB0dt%y%BF(1)%GF%f(1,1,1))
           call export(ind%probe_dB0dt(3),TMP,ind%dB0dt%z%BF(1)%GF%f(1,1,1))
           call export(ind%probe_B0(1),TMP,ind%B0%x%BF(1)%GF%f(1,1,1))
           call export(ind%probe_B0(2),TMP,ind%B0%y%BF(1)%GF%f(1,1,1))
           call export(ind%probe_B0(3),TMP,ind%B0%z%BF(1)%GF%f(1,1,1))
         endif

         scale = SP%DP%ME_scale
         call add(ind%temp_F1,ind%B,ind%B0)
         call face2cellCenter(ind%temp_CC_VF,ind%temp_F1,ind%m)
         call compute_Total_Energy(ind%ME(1),ind%temp_CC_VF,TMP,ind%m,scale)
         call compute_Total_Energy_Domain(ind%ME_fluid(1),ind%temp_CC_VF,ind%CC_VF_fluid,TMP,ind%m,scale,ind%MD_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(1),ind%temp_CC_VF,ind%CC_VF_sigma,TMP,ind%m,scale,ind%MD_sigma)
         call face2cellCenter(ind%temp_CC_VF,ind%B0,ind%m)
         call compute_Total_Energy(ind%ME(2),ind%temp_CC_VF,TMP,ind%m,scale)
         call compute_Total_Energy_Domain(ind%ME_fluid(2),ind%temp_CC_VF,ind%CC_VF_fluid,TMP,ind%m,scale,ind%MD_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(2),ind%temp_CC_VF,ind%CC_VF_sigma,TMP,ind%m,scale,ind%MD_sigma)
         call face2cellCenter(ind%temp_CC_VF,ind%B,ind%m)
         call compute_Total_Energy(ind%ME(3),ind%temp_CC_VF,TMP,ind%m,scale)
         call compute_Total_Energy_Domain(ind%ME_fluid(3),ind%temp_CC_VF,ind%CC_VF_fluid,TMP,ind%m,scale,ind%MD_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(3),ind%temp_CC_VF,ind%CC_VF_sigma,TMP,ind%m,scale,ind%MD_sigma)

         scale = SP%DP%JE_scale
         call edge2cellCenter(ind%temp_CC_VF,ind%J,ind%m,ind%temp_F1)
         call compute_Total_Energy(ind%JE,ind%temp_CC_VF,TMP,ind%m,scale)
         call compute_Total_Energy_Domain(ind%JE_fluid,ind%temp_CC_VF,ind%CC_VF_fluid,TMP,ind%m,scale,ind%MD_fluid)
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
         call export_processed(ind%m,ind%B,str(DT%B%unsteady),'B',1,TMP,SP%VS%B%unsteady_planes)
         call export_processed(ind%m,ind%J,str(DT%J%unsteady),'J',1,TMP,SP%VS%B%unsteady_planes)
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
         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select
         if (SP%SCP%embed_B_interior) call embedFace(ind%B,ind%B_interior,ind%MD_sigma)
         enddo
         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call compute_J_ind(ind,SP)
       end subroutine

       subroutine export_unsteady_ind(ind,SP,TMP,EF,EN,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(sim_params),intent(in) :: SP
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
           call export(ind,str(DT%governing_equations),'ind')
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