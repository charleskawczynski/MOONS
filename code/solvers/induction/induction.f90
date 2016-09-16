       module induction_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use export_SF_mod
       use export_VF_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use IO_SF_mod
       use IO_VF_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use export_raw_processed_mod
       use print_export_mod
       use export_now_mod

       use init_BBCs_mod
       use init_phiBCs_mod
       use init_Bfield_mod
       use init_B_interior_mod
       use init_J_interior_mod
       use init_Sigma_mod
       use ops_embedExtract_mod
       use ops_internal_BC_mod
       use geometric_region_mod
       use BEM_solver_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use probe_mod
       use ops_norms_mod

       use domain_mod
       use grid_mod
       use mesh_mod
       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use BCs_mod
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
       public :: solve,export_tec,exportTransient,compute_E_M_budget
       public :: compute_induction_energies

       type induction
         ! --- Tensor fields ---
         type(TF) :: U_E,temp_E_TF                    ! Edge data
         type(TF) :: temp_F1_TF,temp_F2_TF            ! Face data

         ! --- Vector fields ---
         type(VF) :: J,temp_E                         ! Edge data
         type(VF) :: B,B0,B_interior,temp_F1,temp_F2  ! Face data
         type(VF) :: temp_CC                          ! CC data
         type(VF) :: sigmaInv_edge
         type(VF) :: J_interior,curlUCrossB,curlE

         ! --- Scalar fields ---
         type(SF) :: sigmaInv_CC
         type(SF) :: divB,divJ,phi,temp_CC_SF         ! CC data
         type(SF) :: vol_CC

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

         type(mesh) :: m,m_surface
         type(domain) :: D_fluid,D_sigma ! Latter for vacuum case

         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP_B,ISP_phi
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: sig_local_over_sig_f
         logical :: finite_Rem,include_vacuum
         type(sim_params) :: SP

         integer :: unit_nrg_budget
         real(cp),dimension(3) :: e_budget
       end type

       interface init;                 module procedure init_induction;                end interface
       interface delete;               module procedure delete_induction;              end interface
       interface display;              module procedure display_induction;             end interface
       interface print;                module procedure print_induction;               end interface
       interface export;               module procedure export_induction;              end interface
       interface import;               module procedure import_induction;              end interface

       interface solve;                module procedure solve_induction;               end interface
       interface export_tec;           module procedure export_tec_induction;          end interface
       interface exportTransient;      module procedure inductionExportTransient;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_induction(ind,m,SP,D_fluid,D_sigma,include_vacuum,&
         sig_local_over_sig_f,finite_Rem,Rem,TMP,ISP_B,ISP_phi,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         type(domain),intent(in) :: D_fluid,D_sigma
         logical,intent(in) :: finite_Rem,include_vacuum
         type(iter_solver_params),intent(in) :: ISP_B,ISP_phi
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: Rem,sig_local_over_sig_f
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: sigma,prec_cleanB
         type(VF) :: prec_induction,sigma_temp_E
         type(geometric_region) :: GR_sigma
         write(*,*) 'Initializing induction:'

         call init(ind%TMP,TMP)
         call init(ind%ISP_B,ISP_B)
         call init(ind%ISP_phi,ISP_phi)
         ind%finite_Rem = finite_Rem
         ind%Rem = Rem
         ind%include_vacuum = include_vacuum
         ind%sig_local_over_sig_f = sig_local_over_sig_f
         ind%e_budget = 0.0_cp
         call init(ind%SP,SP)

         call init(ind%m,m)
         call init(ind%D_fluid,D_fluid)
         call init(ind%D_sigma,D_sigma)
         call init(GR_sigma,ind%D_sigma%m_in%hmin,&
                            ind%D_sigma%m_in%hmax,&
                            (/.false.,.false.,.false./),&
                            (/.false.,.false.,.false./))
         call init(GR_sigma,(/ind%D_sigma%m_in%hmin(1),&
                              ind%D_sigma%m_in%hmin(2),&
                              ind%D_sigma%m_in%hmin(3)/),&
                            (/ind%D_sigma%m_in%hmax(1),&
                              ind%D_sigma%m_in%hmax(2),&
                              ind%D_sigma%m_in%hmax(3)/),&
                             (/.true.,.true.,.true./),&
                             (/.true.,.true.,.true./))
         ! call init(GR_sigma,(/ind%D_fluid%m_in%hmin(1),&
         !                      ind%D_fluid%m_in%hmax(2),&
         !                      ind%D_fluid%m_in%hmin(3)/),&
         !                    (/ind%D_fluid%m_in%hmax(1),&
         !                      ind%D_fluid%m_in%hmax(2),&
         !                      ind%D_fluid%m_in%hmax(3)/),&
         !                     (/.false.,.true.,.false./),&
         !                     (/.false.,.true.,.false./))
         ! --- tensor,vector and scalar fields ---
         call init_Face(ind%B_interior   ,ind%D_sigma%m_in,0.0_cp)
         call init_Edge(ind%J_interior   ,ind%D_sigma%m_in,0.0_cp)
         call init_Edge(ind%U_E          ,m,0.0_cp)
         call init_Edge(ind%temp_E_TF    ,m,0.0_cp)
         call init_Face(ind%temp_F1_TF   ,m,0.0_cp)
         call init_Face(ind%temp_F2_TF   ,m,0.0_cp)
         call init_Face(ind%B            ,m,0.0_cp)
         call init_Face(ind%B0           ,m,0.0_cp)
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

         call init_CC(ind%vol_CC,m)
         call volume(ind%vol_CC,m)
         if (ind%SP%export_cell_volume) then
           call export_raw(ind%m,ind%vol_CC,str(DT%meshes),'ind_cell_volume',0)
         endif
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_BBCs(ind%B,m);                         write(*,*) '     B BCs initialized'
         call init_phiBCs(ind%phi,m);                     write(*,*) '     phi BCs initialized'
         call initBfield(ind%B,ind%B0,m,ind%SP%restartB,ind%SP%restartB0,str(DT%B_f))
         write(*,*) '     B-field initialized'
         ! call initB_interior(ind%B_interior,ind%D_sigma%m_in,str(DT%B_f))
         ! call initJ_interior(ind%J_interior,ind%D_sigma%m_in,str(DT%J_f))
         call zeroGhostPoints(ind%B_interior)
         call apply_BCs(ind%B,m);                         write(*,*) '     BCs applied'

         if (ind%SP%solveInduction) call print_BCs(ind%B,'B')
         if (ind%SP%solveInduction) call export_BCs(ind%B,str(DT%B_BCs),'B')
         if (ind%SP%solveInduction) call print_BCs(ind%phi,'phi')
         if (ind%SP%solveInduction) call export_BCs(ind%phi,str(DT%B_BCs),'phi')

         ! ******************** MATERIAL PROPERTIES ********************
         call init_CC(sigma,m,0.0_cp)
         call initSigma(sigma,ind%D_sigma,m,ind%sig_local_over_sig_f)
         write(*,*) '     Materials initialized'
         if (ind%SP%export_mat_props) call export_raw(m,sigma,str(DT%mat),'sigma',0)
         call divide(ind%sigmaInv_CC,1.0_cp,sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigmaInv_CC,m,ind%temp_F1)

         ! call assign(ind%sigmaInv_edge,1.0_cp/sig_local_over_sig_f)
         ! call assign_inside(ind%sigmaInv_edge%x,ind%m,GR_sigma,1.0_cp)
         ! call assign_inside(ind%sigmaInv_edge%y,ind%m,GR_sigma,1.0_cp)
         ! call assign_inside(ind%sigmaInv_edge%z,ind%m,GR_sigma,1.0_cp)

         call init_Edge(sigma_temp_E,D_sigma%m_in,1.0_cp)
         call assign(ind%sigmaInv_edge,1.0_cp/sig_local_over_sig_f)
         call embedEdge(ind%sigmaInv_edge,sigma_temp_E,D_sigma)
         call delete(sigma_temp_E)
         call insulate_lid(ind%sigmaInv_edge,ind%m,1.0_cp/sig_local_over_sig_f)

         ! call cellCenter2Edge(ind%sigmaInv_edge,ind%sigmaInv_CC,m,ind%temp_F1)
         ! call treatInterface(ind%sigmaInv_edge,.false.) ! Logical = take_high_value
         if (ind%SP%export_mat_props) call export_raw(m,ind%sigmaInv_edge,str(DT%mat),'sigmaInv',0)
         call delete(sigma)
         write(*,*) '     Interface treated'
         ! *************************************************************

         call compute_J_ind(ind)

         call init(ind%probe_divB,str(DT%B_r),'transient_divB',ind%SP%restartB)
         call init(ind%probe_divJ,str(DT%J_r),'transient_divJ',ind%SP%restartB)
         call init(ind%JE,str(DT%J_e),'JE',ind%SP%restartB)
         call init(ind%JE_fluid,str(DT%J_e),'JE_fluid',ind%SP%restartB)
         call init(ind%ME(1),str(DT%B_e),'ME',ind%SP%restartB)
         call init(ind%ME_fluid(1),str(DT%B_e),'ME_fluid',ind%SP%restartB)
         call init(ind%ME_conductor(1),str(DT%B_e),'ME_conductor',ind%SP%restartB)
         call init(ind%ME(2),str(DT%B_e),'ME0',ind%SP%restartB)
         call init(ind%ME_fluid(2),str(DT%B_e),'ME0_fluid',ind%SP%restartB)
         call init(ind%ME_conductor(2),str(DT%B_e),'ME0_conductor',ind%SP%restartB)
         call init(ind%ME(3),str(DT%B_e),'ME1',ind%SP%restartB)
         call init(ind%ME_fluid(3),str(DT%B_e),'ME1_fluid',ind%SP%restartB)
         call init(ind%ME_conductor(3),str(DT%B_e),'ME1_conductor',ind%SP%restartB)

         write(*,*) '     B/J probes initialized'

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************

         ! Initialize multigrid
         temp_unit = new_and_open(str(DT%params),'info_ind')
         call display(ind,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_ind')

         if (finite_Rem) then; ind%MFP_B%c_ind = ind%TMP%dt/ind%Rem
         else;                 ind%MFP_B%c_ind = ind%TMP%dt
         endif

         call init(prec_induction,ind%B)
         call prec_ind_VF(prec_induction,ind%m,ind%sigmaInv_edge,ind%MFP_B%c_ind)
         call init(ind%PCG_B,ind_diffusion,ind_diffusion_explicit,prec_induction,ind%m,&
         ind%ISP_B,ind%MFP_B,ind%B,ind%sigmaInv_edge,str(DT%B_r),'B',.false.,.false.)
         call delete(prec_induction)

         write(*,*) '     PCG Solver initialized for B'

         call init(prec_cleanB,ind%phi)
         call prec_lap_SF(prec_cleanB,ind%m)
         call init(ind%PCG_cleanB,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_cleanB,&
         ind%m,ind%ISP_phi,ind%MFP_B,ind%phi,ind%temp_F1,str(DT%B_r),'phi',.false.,.false.)
         call delete(prec_cleanB)
         write(*,*) '     PCG Solver initialized for phi'

         call init(ind%JAC_B,Lap_uniform_VF_explicit,ind%B,ind%B_interior,&
         ind%sigmaInv_edge,ind%m,ind%D_sigma,ind%MFP_B,10,ind%ISP_B%tol_rel,&
         str(DT%B_r),'B',.false.)

         write(*,*) '     Finished'
         ! if (restart_induction) call import(ind,DT)
         call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
       end subroutine

       subroutine delete_induction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%temp_E_TF)
         call delete(ind%temp_F1_TF)
         call delete(ind%temp_F2_TF)
         call delete(ind%U_E)

         call delete(ind%B)
         call delete(ind%B_interior)
         call delete(ind%B0)
         call delete(ind%J)
         call delete(ind%J_interior)
         call delete(ind%curlUCrossB)
         call delete(ind%curlE)
         call delete(ind%temp_CC)
         call delete(ind%temp_E)
         call delete(ind%temp_F1)
         call delete(ind%temp_F2)
         call delete(ind%sigmaInv_edge)
         call delete(ind%vol_CC)

         call delete(ind%divB)
         call delete(ind%divJ)
         call delete(ind%phi)
         call delete(ind%temp_CC_SF)

         call delete(ind%m)
         call delete(ind%D_fluid)
         call delete(ind%D_sigma)

         call delete(ind%probe_divB)
         call delete(ind%probe_divJ)

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
         write(un,*) 'solveBMethod,N_ind,N_cleanB = ',ind%SP%solveBMethod,ind%ISP_B%iter_max,ind%ISP_phi%iter_max
         write(un,*) 'tol_ind,tol_cleanB = ',ind%ISP_B%tol_rel,ind%ISP_phi%tol_rel
         write(un,*) 'nstep,ME = ',ind%TMP%n_step,ind%ME(1)%d
         write(un,*) 'include_vacuum = ',ind%include_vacuum
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

         call export(ind%B      ,str(DT%restart),'B')
         call export(ind%B0     ,str(DT%restart),'B0')
         call export(ind%J      ,str(DT%restart),'J')
         call export(ind%U_E%x  ,str(DT%restart),'U_E_x')
         call export(ind%U_E%y  ,str(DT%restart),'U_E_y')
         call export(ind%U_E%z  ,str(DT%restart),'U_E_z')
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

         call import(ind%B      ,str(DT%restart),'B')
         call import(ind%B0     ,str(DT%restart),'B0')
         call import(ind%J      ,str(DT%restart),'J')
         call import(ind%U_E%x  ,str(DT%restart),'U_E_x')
         call import(ind%U_E%y  ,str(DT%restart),'U_E_y')
         call import(ind%U_E%z  ,str(DT%restart),'U_E_z')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_induction(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         if (ind%SP%restartB.and.(.not.ind%SP%solveInduction)) then
           ! This preserves the initial data
         else
           if (ind%SP%solveInduction) then
             write(*,*) 'export_tec_induction at ind%TMP%n_step = ',ind%TMP%n_step
             call export_processed(ind%m,ind%B ,str(DT%B_f),'B',1)
             call export_raw(ind%m,ind%B ,str(DT%B_f),'B',0)
             call export_raw(ind%m,ind%divB ,str(DT%B_f),'divB',0)
             call export_raw(ind%m,ind%J ,str(DT%J_f),'J',0)
             call export_processed(ind%m,ind%J ,str(DT%J_f),'J',1)
             call export_raw(ind%m,ind%U_E%x  ,str(DT%B_f),'U_E_x',0)
             call export_raw(ind%m,ind%U_E%y  ,str(DT%B_f),'U_E_y',0)
             call export_raw(ind%m,ind%U_E%z  ,str(DT%B_f),'U_E_z',0)
             ! call export_processed(ind%m,ind%B0,str(DT%B_f),'B0',1)
             ! call embedFace(ind%B,ind%B_interior,ind%D_sigma)
             ! call export_processed(ind%m,ind%B ,str(DT%B_f),'B_final',1)
             ! call export_raw(ind%m,ind%B ,str(DT%B_f),'B_final',0)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine compute_J_ind(ind)
         implicit none
         type(induction),intent(inout) :: ind
         ! call assign(ind%J,0.0_cp)
         ! call embedEdge(ind%J,ind%J_interior,ind%D_sigma)
         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)
       end subroutine

       subroutine inductionExportTransient(ind)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp) :: temp
         call Ln(temp,ind%divB,2.0_cp,ind%m); call export(ind%probe_divB,ind%TMP%t,temp)
         call Ln(temp,ind%divJ,2.0_cp,ind%m); call export(ind%probe_divJ,ind%TMP%t,temp)
       end subroutine

       subroutine solve_induction(ind,U,PE,EN,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         if (ind%SP%solveMomentum) then;    call embedVelocity_E(ind%U_E,U,ind%D_fluid)
         elseif (ind%TMP%n_step.le.1) then; call embedVelocity_E(ind%U_E,U,ind%D_fluid)
         endif

         select case (ind%SP%solveBMethod)
         case (1)
         call CT_Low_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%ISP_B%iter_max,ind%TMP%dt,ind%temp_F1,ind%temp_F2,ind%temp_E,ind%temp_E_TF)

         case (2)
         call CT_Finite_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%TMP%dt,ind%temp_F1,ind%temp_F2,ind%temp_F1_TF%x,ind%temp_E,ind%temp_E_TF)

         case (3)
         call ind_PCG_BE_EE_cleanB_PCG(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%B0,ind%U_E,&
         ind%m,ind%TMP%dt,PE%transient_0D,ind%temp_F1,ind%temp_F2,ind%temp_E,&
         ind%temp_E_TF,ind%temp_CC_SF,ind%phi)

         case (4)
         if (ind%TMP%n_step.le.1) then
           call compute_J_ind(ind)
           call add(ind%temp_F2,ind%B,ind%B0)
           call advect_B(ind%curlUCrossB,ind%U_E,ind%temp_F2,ind%m,ind%temp_E_TF,ind%temp_E)
           call curl(ind%curlE,ind%J,ind%m)
           call subtract(ind%curlE,ind%curlUCrossB)
         endif
         call CT_Finite_Rem_interior_solved(ind%PCG_cleanB,ind%B,&
         ind%B_interior,ind%curlE,ind%phi,ind%m,ind%D_sigma,&
         ind%TMP%dt,ind%ISP_B%iter_max,PE%transient_0D,&
         ind%temp_CC_SF,ind%temp_F1)

         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select
         call iterate_step(ind%TMP)

         call compute_J_ind(ind)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if ((PE%transient_0D.or.(ind%TMP%n_step.eq.0))) call compute_induction_energies(ind)

         if (PE%transient_0D) then 
           call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
           call exportTransient(ind)
         endif

         if (PE%transient_2D) call export_processed_transient_3C(ind%m,ind%B,str(DT%B_t),'B',1,ind%TMP)
         ! if (PE%transient_2D) call export_processed_transient_2C(ind%m,ind%B,str(DT%B_t),'B',1,ind%TMP)

         if (PE%info) call print(ind)
         if (PE%solution.or.EN%B%this.or.EN%all%this) then
           ! call export(ind,DT)
           call export_tec(ind,DT)
         endif
       end subroutine

       subroutine compute_induction_energies(ind)
         implicit none
         type(induction),intent(inout) :: ind

         call add(ind%temp_F1,ind%B,ind%B0)
         call face2cellCenter(ind%temp_CC,ind%temp_F1,ind%m)
         call compute_Total_Energy(ind%ME(1),ind%temp_CC,ind%TMP%t,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(1),ind%temp_CC,ind%TMP%t,ind%D_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(1),ind%temp_CC,ind%TMP%t,ind%D_sigma)

         call face2cellCenter(ind%temp_CC,ind%B0,ind%m)
         call compute_Total_Energy(ind%ME(2),ind%temp_CC,ind%TMP%t,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(2),ind%temp_CC,ind%TMP%t,ind%D_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(2),ind%temp_CC,ind%TMP%t,ind%D_sigma)

         call face2cellCenter(ind%temp_CC,ind%B,ind%m)
         call compute_Total_Energy(ind%ME(3),ind%temp_CC,ind%TMP%t,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(3),ind%temp_CC,ind%TMP%t,ind%D_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(3),ind%temp_CC,ind%TMP%t,ind%D_sigma)

         call edge2cellCenter(ind%temp_CC,ind%J,ind%m,ind%temp_F1)
         call compute_Total_Energy(ind%JE,ind%temp_CC,ind%TMP%t,ind%m)
         call compute_Total_Energy_Domain(ind%JE_fluid,ind%temp_CC,ind%TMP%t,ind%D_fluid)
       end subroutine

       subroutine compute_E_M_budget(ind,U,D_fluid,Re,Ha,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(domain),intent(in) :: D_fluid
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
         call init_CC(sigmaInv_CC_solid,ind%D_sigma%m_in)
         call init_CC(sigmaInv_CC_ideal,ind%m)
         call init_Face(sigmaInv_F_solid,ind%D_sigma%m_in)
         call init_Face(sigmaInv_F_ideal,ind%m)
         call init_Face(sigmaInv_F,ind%m)

         call cellCenter2Face(sigmaInv_F,ind%sigmaInv_CC,ind%m)
         call treatInterface(sigmaInv_F,.false.)
         call assign(sigmaInv_F_ideal,0.0_cp)
         call extractFace(sigmaInv_F_solid,sigmaInv_F,ind%D_sigma)
         call embedFace(sigmaInv_F_ideal,sigmaInv_F_solid,ind%D_sigma)

         call assign(sigmaInv_CC_ideal,0.0_cp)
         call extractCC(sigmaInv_CC_solid,ind%sigmaInv_CC,ind%D_sigma)
         call embedCC(sigmaInv_CC_ideal,sigmaInv_CC_solid,ind%D_sigma)

         call embedFace(temp_U,U,D_fluid)
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

       subroutine insulate_lid(S,m,sigma_insulate)
         implicit none
         type(VF),intent(inout) :: S
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: sigma_insulate
         call insulate_lid_SF(S%x,m,sigma_insulate)
         call insulate_lid_SF(S%z,m,sigma_insulate)
       end subroutine

       subroutine insulate_lid_SF(S,m,sigma_insulate)
         implicit none
         type(SF),intent(inout) :: S
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: sigma_insulate
         integer :: t,i,j,k
         real(cp) :: tol
         tol = 10.0_cp**(-10.0_cp)
         do t=1,m%s; do k=1,S%RF(t)%s(3);do j=1,S%RF(t)%s(2);do i=1,S%RF(t)%s(1)
         if (abs(m%g(t)%c(2)%hn(j)-1.0_cp).lt.tol) S%RF(t)%f(i,j,k) = sigma_insulate
         enddo; enddo; enddo; enddo
       end subroutine

       end module