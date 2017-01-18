       module induction_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use IO_export_mod
       use IO_import_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use export_raw_processed_mod
       use export_raw_processed_symmetry_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod
       use assign_B0_vs_t_mod
       use datatype_conversion_mod

       use mesh_stencils_mod
       use init_B_BCs_mod
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
       use ops_norms_mod
       use var_set_mod

       use mesh_domain_mod
       use grid_mod
       use mesh_mod
       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use boundary_conditions_mod
       use apply_BCs_mod
       use ops_advect_mod
       use induction_solver_mod
       use preconditioners_mod
       use PCG_mod
       use Jacobi_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use induction_aux_mod
       use E_M_Budget_mod

       implicit none

       private
       public :: induction
       public :: init,delete,display,print,export,import ! Essentials
       public :: solve,export_tec,compute_E_M_budget

       public :: prolongate

       type induction
         ! --- Tensor fields ---
         type(TF) :: U_E,temp_E_TF                    ! Edge data
         type(TF) :: temp_F1_TF,temp_F2_TF            ! Face data

         ! --- Vector fields ---
         type(VF) :: J,temp_E                         ! Edge data
         type(VF) :: B,B0,B_interior,temp_F1,temp_F2  ! Face data
         type(VF) :: Bstar                            ! Intermediate magnetic field
         type(VF) :: dB0dt
         type(VF) :: temp_CC                          ! CC data
         type(VF) :: sigmaInv_edge
         type(VF) :: J_interior,curlUCrossB,curlE

         ! --- Scalar fields ---
         type(SF) :: sigmaInv_CC
         type(SF) :: divB,divJ,phi,temp_CC_SF         ! CC data

         ! --- Solvers ---
         type(PCG_solver_VF) :: PCG_B
         type(PCG_solver_SF) :: PCG_cleanB

         type(Jacobi) :: JAC_B

         type(matrix_free_params) :: MFP_B
         type(matrix_free_params) :: MFP_cleanB

         ! Subscripts:
         ! Magnetic field:
         type(probe),dimension(3) :: ME,ME_fluid,ME_conductor ! 1 = total, 2 = applied, 3 induced
         type(probe) :: JE,JE_fluid
         type(probe) :: probe_divB,probe_divJ
         type(probe),dimension(3) :: probe_dB0dt,probe_B0

         type(mesh) :: m
         type(mesh_domain) :: MD_fluid,MD_sigma ! Latter for vacuum case

         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP_B,ISP_phi
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: sig_local_over_sig_f
         logical :: finite_Rem,include_vacuum
         type(sim_params) :: SP

         integer :: unit_nrg_budget
         real(cp),dimension(3) :: e_budget
         logical :: suppress_warning
       end type

       interface init;                 module procedure init_induction;                end interface
       interface delete;               module procedure delete_induction;              end interface
       interface display;              module procedure display_induction;             end interface
       interface print;                module procedure print_induction;               end interface
       interface export;               module procedure export_induction;              end interface
       interface import;               module procedure import_induction;              end interface

       interface solve;                module procedure solve_induction;               end interface
       interface export_tec;           module procedure export_tec_induction;          end interface

       interface prolongate;           module procedure prolongate_ind;                end interface
       interface set_MFP;              module procedure set_MFP_ind;                   end interface
       interface set_sigma_inv;        module procedure set_sigma_inv_ind;             end interface
       interface init_matrix_based_ops;module procedure init_matrix_based_ops_ind;     end interface

       interface export_transient1;    module procedure export_transient1_ind;         end interface
       interface export_transient2;    module procedure export_transient2_ind;         end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_induction(ind,m,SP,DT,MD_fluid,MD_sigma)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         type(mesh_domain),intent(in) :: MD_fluid,MD_sigma
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: sigma,vol_CC
         write(*,*) 'Initializing induction:'

         call init(ind%TMP,SP%VS%B%TMP)
         call init(ind%ISP_B,SP%VS%B%ISP)
         call init(ind%ISP_phi,SP%VS%phi%ISP)
         ind%finite_Rem = SP%finite_Rem
         ind%include_vacuum = SP%include_vacuum
         ind%Rem = SP%DP%Rem
         ind%sig_local_over_sig_f = SP%DP%sig_local_over_sig_f
         ind%e_budget = 0.0_cp
         call init(ind%SP,SP)

         call init(ind%m,m)
         call init(ind%MD_fluid,MD_fluid)
         call init(ind%MD_sigma,MD_sigma)
         ! --- tensor,vector and scalar fields ---
         call init_Face(ind%B_interior   ,m,ind%MD_sigma); call assign(ind%B_interior,0.0_cp)
         call init_Edge(ind%J_interior   ,m,ind%MD_sigma); call assign(ind%J_interior,0.0_cp)
         call init_Edge(ind%U_E          ,m,0.0_cp)
         call init_Edge(ind%temp_E_TF    ,m,0.0_cp)
         call init_Face(ind%temp_F1_TF   ,m,0.0_cp)
         call init_Face(ind%temp_F2_TF   ,m,0.0_cp)
         call init_Face(ind%B            ,m,0.0_cp)
         call init_Face(ind%B0           ,m,0.0_cp)
         call init_Face(ind%dB0dt        ,m,0.0_cp)
         call init_CC(ind%temp_CC        ,m,0.0_cp)
         call init_Edge(ind%J            ,m,0.0_cp)
         call init_Face(ind%curlUCrossB  ,m,0.0_cp)
         call init_Face(ind%curlE        ,m,0.0_cp)
         call init_Edge(ind%temp_E       ,m,0.0_cp)
         call init_Edge(ind%sigmaInv_edge,m,0.0_cp)
         call init_Face(ind%temp_F1      ,m,0.0_cp)
         call init_Face(ind%temp_F2      ,m,0.0_cp)
         call init_CC(ind%phi            ,m,0.0_cp)
         call init_CC(ind%temp_CC_SF     ,m,0.0_cp)
         call init_CC(ind%divB           ,m,0.0_cp)
         call init_Node(ind%divJ         ,m,0.0_cp)
         call init_CC(ind%sigmaInv_CC    ,m,0.0_cp)

         call init_CC(vol_CC,m)
         call volume(vol_CC,m)
         if (ind%SP%EL%export_cell_volume) then
           call export_raw(ind%m,vol_CC,str(DT%meshes),'ind_cell_volume',0)
         endif
         call delete(vol_CC)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_B_BCs(ind%B,m,ind%SP);     write(*,*) '     B BCs initialized'
         call init_phi_BCs(ind%phi,m,ind%SP); write(*,*) '     phi BCs initialized'

         call init_B0_field(ind%B0,m,ind%SP,str(DT%B%field))
         call init_B_field(ind%B,m,ind%SP,str(DT%B%field))
         call init_phi_field(ind%phi,m,ind%SP,str(DT%phi%field))

         if (ind%SP%unsteady_B0) call assign_B0_vs_t(ind%B0,ind%TMP)

         write(*,*) '     B-field initialized'
         ! call initB_interior(ind%B_interior,m,ind%MD_sigma,str(DT%B%field))
         ! call initJ_interior(ind%J_interior,m,ind%MD_sigma,str(DT%J%field))
         call assign_ghost_XPeriodic(ind%B_interior,0.0_cp)
         call apply_BCs(ind%B);                           write(*,*) '     BCs applied'

         call init(ind%Bstar,ind%B)
         call assign(ind%Bstar,ind%B)
         write(*,*) '     Intermediate B-field initialized'

         if (ind%SP%VS%B%SS%solve) call print_BCs(ind%B,'B')
         if (ind%SP%VS%B%SS%solve) call export_BCs(ind%B,str(DT%B%BCs),'B')
         if (ind%SP%VS%B%SS%solve) call print_BCs(ind%phi,'phi')
         if (ind%SP%VS%B%SS%solve) call export_BCs(ind%phi,str(DT%phi%BCs),'phi')

         ! ******************** MATERIAL PROPERTIES ********************
         call init_CC(sigma,m,0.0_cp)
         call initSigma(sigma,m,ind%MD_sigma,ind%sig_local_over_sig_f)
         write(*,*) '     Materials initialized'
         if (ind%SP%EL%export_mat_props) call export_raw(m,sigma,str(DT%mat),'sigma',0)
         call divide(ind%sigmaInv_CC,1.0_cp,sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigmaInv_CC,m,ind%temp_F1)

         call set_sigma_inv(ind)

         if (ind%SP%EL%export_mat_props) call export_raw(m,ind%sigmaInv_edge,str(DT%mat),'sigmaInv',0)
         call delete(sigma)
         write(*,*) '     Interface treated'

         ! *************************************************************

         call compute_J_ind(ind)

         call init(ind%probe_dB0dt(1),str(DT%B%energy),'dB0dt_x',ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_dB0dt(2),str(DT%B%energy),'dB0dt_y',ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_dB0dt(3),str(DT%B%energy),'dB0dt_z',ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_B0(1)   ,str(DT%B%energy),'B0_x',   ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_B0(2)   ,str(DT%B%energy),'B0_y',   ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_B0(3)   ,str(DT%B%energy),'B0_z',   ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_divB,str(DT%B%residual),'transient_divB',ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%probe_divJ,str(DT%J%residual),'transient_divJ',ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%JE,        str(DT%J%energy),'JE',            ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%JE_fluid,  str(DT%J%energy),'JE_fluid',      ind%SP%VS%B%SS%restart,SP,.true.)
         call init(ind%ME(1)          ,str(DT%B%energy),'ME',           ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME_fluid(1)    ,str(DT%B%energy),'ME_fluid',     ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME_conductor(1),str(DT%B%energy),'ME_conductor', ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME(2)          ,str(DT%B%energy),'ME0',          ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME_fluid(2)    ,str(DT%B%energy),'ME0_fluid',    ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME_conductor(2),str(DT%B%energy),'ME0_conductor',ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME(3)          ,str(DT%B%energy),'ME1',          ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME_fluid(3)    ,str(DT%B%energy),'ME1_fluid',    ind%SP%VS%B%SS%restart,SP,.false.)
         call init(ind%ME_conductor(3),str(DT%B%energy),'ME1_conductor',ind%SP%VS%B%SS%restart,SP,.false.)

         write(*,*) '     B/J probes initialized'

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************

         ! Initialize multigrid
         temp_unit = new_and_open(str(DT%params),'info_ind')
         call display(ind,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_ind')

         write(*,*) '     About to assemble curl-curl matrix'
         call set_MFP(ind)
         if (ind%SP%matrix_based) call init_matrix_based_ops(ind)

         call init(ind%PCG_B,ind_diffusion,ind_diffusion_explicit,prec_ind_VF,ind%m,&
         ind%ISP_B,ind%MFP_B,ind%Bstar,ind%sigmaInv_edge,str(DT%B%residual),'B',.false.,.false.)
         write(*,*) '     PCG Solver initialized for B'

         call init(ind%PCG_cleanB,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_lap_SF,&
         ind%m,ind%ISP_phi,ind%MFP_B,ind%phi,ind%temp_F1,str(DT%phi%residual),'phi',.false.,.false.)
         write(*,*) '     PCG Solver initialized for phi'

         call init(ind%JAC_B,Lap_uniform_VF_explicit,ind%B,ind%B_interior,&
         ind%sigmaInv_edge,ind%m,ind%MD_sigma,ind%MFP_B,10,ind%ISP_B%tol_rel,&
         str(DT%B%residual),'B',.false.)

         write(*,*) '     Finished'
         ! if (restart_induction) call import(ind,DT)
       end subroutine

       subroutine delete_induction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%temp_E_TF)
         call delete(ind%temp_F1_TF)
         call delete(ind%temp_F2_TF)
         call delete(ind%U_E)

         call delete(ind%B)
         call delete(ind%Bstar)
         call delete(ind%B_interior)
         call delete(ind%B0)
         call delete(ind%dB0dt)
         call delete(ind%J)
         call delete(ind%J_interior)
         call delete(ind%curlUCrossB)
         call delete(ind%curlE)
         call delete(ind%temp_CC)
         call delete(ind%temp_E)
         call delete(ind%temp_F1)
         call delete(ind%temp_F2)
         call delete(ind%sigmaInv_edge)

         call delete(ind%divB)
         call delete(ind%divJ)
         call delete(ind%phi)
         call delete(ind%temp_CC_SF)

         call delete(ind%m)
         call delete(ind%MD_fluid)
         call delete(ind%MD_sigma)

         call delete(ind%probe_divB)
         call delete(ind%probe_divJ)
         call delete(ind%probe_dB0dt)
         call delete(ind%probe_B0)

         call delete(ind%ME)
         call delete(ind%ME_fluid)
         call delete(ind%ME_conductor)
         call delete(ind%JE)
         call delete(ind%JE_fluid)

         call delete(ind%PCG_cleanB)
         call delete(ind%PCG_B)
         call delete(ind%JAC_B)
         call delete(ind%TMP)
         call delete(ind%ISP_B)
         call delete(ind%ISP_phi)

         write(*,*) 'Induction object deleted'
       end subroutine

       subroutine display_induction(ind,un)
         implicit none
         type(induction),intent(in) :: ind
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MAGNETIC **************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Rem,finite_Rem = ',ind%Rem,ind%finite_Rem
         write(un,*) 't,dt = ',ind%TMP%t,ind%TMP%dt
         write(un,*) 'solveBMethod,N_ind,N_cleanB = ',ind%SP%VS%B%SS%solve_method,&
         ind%ISP_B%iter_max,ind%ISP_phi%iter_max
         write(un,*) 'tol_ind,tol_cleanB = ',ind%ISP_B%tol_rel,ind%ISP_phi%tol_rel
         write(un,*) 'nstep,ME = ',ind%TMP%n_step,get_data(ind%ME(1))
         write(un,*) 'include_vacuum = ',ind%include_vacuum
         ! call displayPhysicalMinMax(ind%dB0dt,'dB0dt',un)
         ! call displayPhysicalMinMax(ind%B0,'B0',un)
         call displayPhysicalMinMax(ind%divB,'divB',un)
         call displayPhysicalMinMax(ind%divJ,'divJ',un)
         write(un,*) ''
         call display(ind%m,un)
         write(un,*) ''
       end subroutine

       subroutine print_induction(ind)
         implicit none
         type(induction),intent(in) :: ind
         call display(ind,6)
       end subroutine

       subroutine export_induction(ind,DT)
         implicit none
         type(induction),intent(in) :: ind
         type(dir_tree),intent(in) :: DT
         integer :: un
         call export(ind%TMP)
         call export(ind%ISP_B)
         call export(ind%ISP_phi)

         un = new_and_open(str(DT%restart),'ind_restart')
         write(un,*) ind%Rem
         write(un,*) ind%e_budget
         write(un,*) ind%finite_Rem
         call close_and_message(un,str(DT%restart),'ind_restart')

         un = new_and_open(str(DT%restart),'ind_MFP')
         call export(ind%MFP_B,un)
         call close_and_message(un,str(DT%restart),'ind_MFP')

         if (.not.ind%SP%EL%export_soln_only) then
           call export(ind%B      ,str(DT%B%restart),'B')
           call export(ind%B0     ,str(DT%B%restart),'B0')
           call export(ind%J      ,str(DT%J%restart),'J')
           call export(ind%U_E%x  ,str(DT%B%restart),'U_E_x')
           call export(ind%U_E%y  ,str(DT%B%restart),'U_E_y')
           call export(ind%U_E%z  ,str(DT%B%restart),'U_E_z')
         endif
       end subroutine

       subroutine import_induction(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         integer :: un
         call import(ind%TMP)
         call import(ind%ISP_B)
         call import(ind%ISP_phi)

         un = open_to_read(str(DT%restart),'ind_restart')
         read(un,*) ind%Rem
         read(un,*) ind%e_budget
         read(un,*) ind%finite_Rem
         call close_and_message(un,str(DT%restart),'ind_restart')

         un = open_to_read(str(DT%restart),'ind_MFP')
         call import(ind%MFP_B,un)
         call close_and_message(un,str(DT%restart),'ind_MFP')

         if (.not.ind%SP%EL%export_soln_only) then
           call import(ind%B      ,str(DT%B%restart),'B')
           call import(ind%B0     ,str(DT%B%restart),'B0')
           call import(ind%J      ,str(DT%J%restart),'J')
           call import(ind%U_E%x  ,str(DT%B%restart),'U_E_x')
           call import(ind%U_E%y  ,str(DT%B%restart),'U_E_y')
           call import(ind%U_E%z  ,str(DT%B%restart),'U_E_z')
         endif
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_induction(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         if (ind%SP%VS%B%SS%restart.and.(.not.ind%SP%VS%B%SS%solve)) then
           ! This preserves the initial data
         else
           if (ind%SP%VS%B%SS%solve) then
             write(*,*) 'export_tec_induction at ind%TMP%n_step = ',ind%TMP%n_step
             call export_processed(ind%m,ind%B,str(DT%B%field),'B',1)
             call export_raw(ind%m,ind%phi,str(DT%phi%field),'phi',1)
             call export_processed(ind%m,ind%phi ,str(DT%phi%field),'phi',1)
             if (.not.ind%SP%EL%export_soln_only) then
             if (ind%SP%EL%export_symmetric) then
               call export_processed(ind%m,ind%B,str(DT%B%field),'B',1,6,(/-1.0_cp,-1.0_cp,1.0_cp/))
               call export_processed(ind%m,ind%J,str(DT%J%field),'J',1,6,(/-1.0_cp,-1.0_cp,1.0_cp/))
             endif
             call export_raw(ind%m,ind%B ,str(DT%B%field),'B',0)
             call export_raw(ind%m,ind%Bstar,str(DT%B%field),'Bstar',0)
             call export_raw(ind%m,ind%divB ,str(DT%B%field),'divB',0)
             call export_raw(ind%m,ind%J ,str(DT%J%field),'J',0)
             call export_processed(ind%m,ind%J ,str(DT%J%field),'J',1)
             call export_raw(ind%m,ind%U_E%x  ,str(DT%B%field),'U_E_x',0)
             call export_raw(ind%m,ind%U_E%y  ,str(DT%B%field),'U_E_y',0)
             call export_raw(ind%m,ind%U_E%z  ,str(DT%B%field),'U_E_z',0)
             ! call export_processed(ind%m,ind%B0,str(DT%B%field),'B0',1)
             ! call embedFace(ind%B,ind%B_interior,ind%MD_sigma)
             ! call export_processed(ind%m,ind%B ,str(DT%B%field),'B_final',1)
             ! call export_raw(ind%m,ind%B ,str(DT%B%field),'B_final',0)
             endif
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_transient1_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp) :: temp
         call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
         call Ln(temp,ind%divB,2.0_cp,ind%m); call export(ind%probe_divB,ind%TMP,temp)
         call Ln(temp,ind%divJ,2.0_cp,ind%m); call export(ind%probe_divJ,ind%TMP,temp)
         call export(ind%probe_dB0dt(1),ind%TMP,ind%dB0dt%x%BF(1)%GF%f(1,1,1))
         call export(ind%probe_dB0dt(2),ind%TMP,ind%dB0dt%y%BF(1)%GF%f(1,1,1))
         call export(ind%probe_dB0dt(3),ind%TMP,ind%dB0dt%z%BF(1)%GF%f(1,1,1))
         call export(ind%probe_B0(1),ind%TMP,ind%B0%x%BF(1)%GF%f(1,1,1))
         call export(ind%probe_B0(2),ind%TMP,ind%B0%y%BF(1)%GF%f(1,1,1))
         call export(ind%probe_B0(3),ind%TMP,ind%B0%z%BF(1)%GF%f(1,1,1))
         call add(ind%temp_F1,ind%B,ind%B0)
         call face2cellCenter(ind%temp_CC,ind%temp_F1,ind%m)
         call compute_Total_Energy(ind%ME(1),ind%temp_CC,ind%TMP,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(1),ind%temp_CC,ind%TMP,ind%m,ind%MD_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(1),ind%temp_CC,ind%TMP,ind%m,ind%MD_sigma)
         call face2cellCenter(ind%temp_CC,ind%B0,ind%m)
         call compute_Total_Energy(ind%ME(2),ind%temp_CC,ind%TMP,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(2),ind%temp_CC,ind%TMP,ind%m,ind%MD_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(2),ind%temp_CC,ind%TMP,ind%m,ind%MD_sigma)
         call face2cellCenter(ind%temp_CC,ind%B,ind%m)
         call compute_Total_Energy(ind%ME(3),ind%temp_CC,ind%TMP,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(3),ind%temp_CC,ind%TMP,ind%m,ind%MD_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(3),ind%temp_CC,ind%TMP,ind%m,ind%MD_sigma)
         call edge2cellCenter(ind%temp_CC,ind%J,ind%m,ind%temp_F1)
         call compute_Total_Energy(ind%JE,ind%temp_CC,ind%TMP,ind%m)
         call compute_Total_Energy_Domain(ind%JE_fluid,ind%temp_CC,ind%TMP,ind%m,ind%MD_fluid)
       end subroutine

       subroutine export_transient2_ind(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         call export_processed(ind%m,ind%B,str(DT%B%transient),'B',1,ind%TMP)
         call export_processed(ind%m,ind%J,str(DT%J%transient),'J',1,ind%TMP)
         ! call export_processed(ind%m,ind%B,str(DT%B%transient),'B',1,ind%TMP,3,35)
         ! call export_processed(ind%m,ind%J,str(DT%J%transient),'J',1,ind%TMP,3,35)
       end subroutine

       subroutine compute_J_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         ! call assign(ind%J,0.0_cp)
         ! call embedEdge(ind%J,ind%J_interior,ind%MD_sigma)
         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)
       end subroutine

       subroutine set_MFP_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         if (ind%finite_Rem) then; ind%MFP_B%coeff = ind%TMP%dt/ind%Rem
         else;                     ind%MFP_B%coeff = ind%TMP%dt
         endif
       end subroutine

       subroutine set_sigma_inv_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF) :: sigma_temp_E
         call init_Edge(sigma_temp_E,ind%m,ind%MD_sigma)
         call assign(sigma_temp_E,1.0_cp)
         call assign(ind%sigmaInv_edge,1.0_cp/ind%sig_local_over_sig_f)
         call embedEdge(ind%sigmaInv_edge,sigma_temp_E,ind%MD_sigma)
         call delete(sigma_temp_E)
       end subroutine

       subroutine init_matrix_based_ops_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp),dimension(2) :: diffusion_treatment
         ! diffusion_treatment = (/1.0_cp,0.0_cp/) ! No treatment to curl-curl operator
         diffusion_treatment = (/-ind%MFP_B%coeff,1.0_cp/)    ! diffusion explicit
         ! diffusion_treatment = (/ind%MFP_B%coeff,1.0_cp/)     ! diffusion implicit
         call init_curl_curl(ind%m,ind%sigmaInv_edge)
         call init_Laplacian_SF(ind%m)
         call multiply_curl_curl(ind%m,diffusion_treatment(1))
         call add_curl_curl(ind%m,diffusion_treatment(2))
       end subroutine

       subroutine solve_induction(ind,U,PE,EN,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         if (ind%SP%VS%U%SS%solve) then; call embedVelocity_E(ind%U_E,U,ind%MD_fluid)
         elseif (ind%TMP%n_step.le.1) then;  call embedVelocity_E(ind%U_E,U,ind%MD_fluid)
         endif
         if (ind%SP%unsteady_B0) then
           call assign_B0_vs_t(ind%B0,ind%TMP)
           call assign_dB0_dt_vs_t(ind%dB0dt,ind%TMP)
           call multiply(ind%dB0dt,-1.0_cp) ! added to RHS
           call assign(ind%B0%x,0.0_cp)
           call assign(ind%B0%y,0.0_cp)
           call assign(ind%dB0dt%x,0.0_cp)
           call assign(ind%dB0dt%y,0.0_cp)
         else
           call assign(ind%dB0dt,0.0_cp)
         endif


         select case (ind%SP%VS%B%SS%solve_method)
         case (1)
         call CT_Low_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%TMP%multistep_iter,ind%TMP%dt,ind%temp_F1,ind%temp_F2,ind%temp_E,ind%temp_E_TF)
         case (2)
         call CT_Finite_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%dB0dt,ind%sigmaInv_edge,ind%m,&
         ind%TMP%multistep_iter,ind%TMP%dt,ind%temp_F1,ind%temp_F2,ind%temp_F1_TF%x,&
         ind%temp_E,ind%temp_E_TF)
         case (3)
         call ind_PCG_BE_EE_cleanB_PCG(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%Bstar,&
         ind%phi,ind%B0,ind%U_E,ind%dB0dt,ind%m,ind%TMP%multistep_iter,ind%TMP%dt,&
         PE%transient_0D,ind%temp_F1,ind%temp_F2,ind%temp_E,ind%temp_E_TF,&
         ind%temp_CC_SF,ind%temp_CC)
         case (4)
         if (ind%TMP%n_step.le.1) then
           call compute_J_ind(ind)
           call add(ind%temp_F2,ind%B,ind%B0)
           call advect_B(ind%curlUCrossB,ind%U_E,ind%temp_F2,ind%m,ind%temp_E_TF,ind%temp_E)
           call curl(ind%curlE,ind%J,ind%m)
           call subtract(ind%curlE,ind%curlUCrossB)
         endif
         call CT_Finite_Rem_interior_solved(ind%PCG_cleanB,ind%B,ind%Bstar,&
         ind%B_interior,ind%curlE,ind%phi,ind%m,ind%MD_sigma,&
         ind%TMP%multistep_iter,ind%TMP%dt,PE%transient_0D,&
         ind%temp_CC_SF,ind%temp_F1)
         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select
         call iterate_step(ind%TMP)

         call compute_J_ind(ind)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (PE%transient_0D) call export_transient1(ind)
         if (PE%transient_2D) call export_transient2(ind,DT)

         ! if (PE%transient_2D) call export_processed_transient_3C(ind%m,ind%B,str(DT%B%transient),'B',1,ind%TMP)
         ! if (PE%transient_2D) call export_processed_transient_2C(ind%m,ind%B,str(DT%B%transient),'B',1,ind%TMP)

         if (PE%info) call print(ind)
         if (PE%solution.or.EN%B%this.or.EN%all%this) then
           ! call export(ind,DT)
           call export_tec(ind,DT)
         endif
       end subroutine

       subroutine compute_E_M_budget(ind,U,MD_fluid,Re,Ha,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(mesh_domain),intent(in) :: MD_fluid
         type(dir_tree),intent(in) :: DT
         real(cp),intent(in) :: Re,Ha
         type(TF) :: temp_CC_TF,temp_F1_TF,temp_F2_TF,temp_F3_TF
         type(VF) :: temp_F1,temp_F2,temp_U,temp_CC_VF
         type(VF) :: sigmaInv_F,sigmaInv_F_solid,sigmaInv_F_ideal
         type(SF) :: sigmaInv_CC_solid,sigmaInv_CC_ideal

         call init_CC(temp_CC_TF,ind%m)
         call init_CC(temp_CC_VF,ind%m)
         call init_Face(temp_F1,ind%m)
         call init_Face(temp_F2,ind%m)
         call init_Face(temp_F1_TF,ind%m)
         call init_Face(temp_F2_TF,ind%m)
         call init_Face(temp_F3_TF,ind%m)
         call init_Face(temp_U,ind%m)
         call init_CC(sigmaInv_CC_solid,ind%m,ind%MD_sigma)
         call init_CC(sigmaInv_CC_ideal,ind%m)
         call init_Face(sigmaInv_F_solid,ind%m,ind%MD_sigma)
         call init_Face(sigmaInv_F_ideal,ind%m)
         call init_Face(sigmaInv_F,ind%m)

         call cellCenter2Face(sigmaInv_F,ind%sigmaInv_CC,ind%m)
         call treatInterface(sigmaInv_F,.false.)
         call assign(sigmaInv_F_ideal,0.0_cp)
         call extractFace(sigmaInv_F_solid,sigmaInv_F,ind%MD_sigma)
         call embedFace(sigmaInv_F_ideal,sigmaInv_F_solid,ind%MD_sigma)

         call assign(sigmaInv_CC_ideal,0.0_cp)
         call extractCC(sigmaInv_CC_solid,ind%sigmaInv_CC,ind%MD_sigma)
         call embedCC(sigmaInv_CC_ideal,sigmaInv_CC_solid,ind%MD_sigma)

         call embedFace(temp_U,U,MD_fluid)
         call compute_J_ind(ind)

         call E_M_Budget(DT,ind%e_budget,ind%B,ind%B,ind%B0,ind%B0,ind%J,&
         sigmaInv_F_ideal,sigmaInv_CC_ideal,temp_U,ind%m,ind%TMP%dt,Re,Ha,&
         ind%Rem,temp_CC_TF,temp_CC_VF,temp_F1,temp_F2,&
         temp_F1_TF,temp_F2_TF,temp_F3_TF)

         call export_E_M_budget(ind,DT)

         call delete(temp_CC_TF)
         call delete(temp_CC_VF)
         call delete(temp_F1)
         call delete(temp_F2)
         call delete(temp_F1_TF)
         call delete(temp_F2_TF)
         call delete(temp_F3_TF)
         call delete(temp_U)
         call delete(sigmaInv_CC_ideal)
         call delete(sigmaInv_CC_solid)
         call delete(sigmaInv_F)
         call delete(sigmaInv_F_solid)
         call delete(sigmaInv_F_ideal)
       end subroutine

       subroutine export_E_M_budget(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(string),dimension(3) :: vars
         integer :: un,i
         un = new_and_open(str(DT%e_budget),'E_M_budget_terms')
         call init(vars(1),'Unsteady = ')
         call init(vars(2),'Joule_Heat = ')
         call init(vars(3),'Poynting = ')

         write(un,*) 'magnetic energy budget at nstep=',ind%TMP%n_step
         do i=1,3
         write(un,*) str(vars(i)),ind%e_budget(i)
         call delete(vars(i))
         enddo
         call close_and_message(un,str(DT%e_budget),'E_M_budget_terms')
       end subroutine

       subroutine prolongate_ind(ind,DT,RM,SS_reached)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(refine_mesh),intent(in) :: RM
         logical,intent(in) :: SS_reached
         integer,dimension(3) :: dir
         integer :: i
         type(iter_solver_params) :: temp
         write(*,*) '#################### Prolongating induction solver ####################'
         call export_processed(ind%m,ind%B,str(DT%B%field),'B_SS_'//str(RM%level_last),1)

         dir = get_dir(RM)
         if (SS_reached) dir = (/1,2,3/)
         do i=1,3
           if (dir(i).ne.0) then
             write(*,*) 'Prolongating induction solver along direction ',i
             call prolongate(ind%m,dir(i))
             call prolongate(ind%MD_fluid,dir(i))
             call prolongate(ind%MD_sigma,dir(i))

             call prolongate(ind%U_E,ind%m,dir(i))
             call prolongate(ind%temp_E_TF,ind%m,dir(i))
             call prolongate(ind%temp_F1_TF,ind%m,dir(i))
             call prolongate(ind%temp_F2_TF,ind%m,dir(i))

             call prolongate(ind%J,ind%m,dir(i))
             call prolongate(ind%dB0dt,ind%m,dir(i))
             call prolongate(ind%temp_E,ind%m,dir(i))
             call prolongate(ind%B,ind%m,dir(i))
             call prolongate(ind%Bstar,ind%m,dir(i))
             call prolongate(ind%B0,ind%m,dir(i))
             call prolongate(ind%B_interior,ind%m,dir(i))
             call prolongate(ind%temp_F1,ind%m,dir(i))
             call prolongate(ind%temp_F2,ind%m,dir(i))
             call prolongate(ind%temp_CC,ind%m,dir(i))
             call prolongate(ind%sigmaInv_edge,ind%m,dir(i))
             call prolongate(ind%J_interior,ind%m,dir(i))
             call prolongate(ind%curlUCrossB,ind%m,dir(i))
             call prolongate(ind%curlE,ind%m,dir(i))

             call prolongate(ind%sigmaInv_CC,ind%m,dir(i))
             call prolongate(ind%divB,ind%m,dir(i))
             call prolongate(ind%divJ,ind%m,dir(i))
             call prolongate(ind%phi,ind%m,dir(i))
             call prolongate(ind%temp_CC_SF,ind%m,dir(i))

             call set_sigma_inv(ind)
             call set_MFP(ind)
             call prolongate(ind%PCG_B,ind%m,ind%sigmaInv_edge,ind%MFP_B,dir(i))
             call prolongate(ind%PCG_cleanB,ind%m,ind%temp_F1,ind%MFP_cleanB,dir(i))
           endif
         enddo
         if (ind%SP%matrix_based) call init_matrix_based_ops(ind)

         write(*,*) 'Finished induction solver prolongation'
         call apply_BCs(ind%B)
         call export_processed(ind%m,ind%B,str(DT%B%field),'B_prolongated_'//str(RM%level),1)

         call init(temp,ind%PCG_cleanB%ISP)
         call init(ind%PCG_cleanB%ISP,solve_exact(str(DT%B%residual)))
         call assign(ind%Bstar,ind%B)
         call clean_div(ind%PCG_cleanB,ind%B,ind%Bstar,ind%phi,&
         ind%m,ind%temp_F1,ind%temp_CC_SF,.true.)
         call init(ind%PCG_cleanB%ISP,temp)
         call delete(temp)

         call export_processed(ind%m,ind%B,str(DT%B%field),'B_cleaned_'//str(RM%level),1)
         write(*,*) '#############################################################'
       end subroutine

       end module