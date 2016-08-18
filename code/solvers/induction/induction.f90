       module induction_mod
       use current_precision_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
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

       use init_BBCs_mod
       use init_phiBCs_mod
       use init_Bfield_mod
       use init_B_interior_mod
       use init_Sigma_mod
       use ops_embedExtract_mod
       use BEM_solver_mod

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
       use induction_solver_mod
       use preconditioners_mod
       use PCG_mod
       use Jacobi_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use induction_aux_mod
       use E_M_Budget_mod

       use probe_base_mod
       use probe_transient_mod

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

         type(errorProbe) :: probe_divB,probe_divJ

         ! Subscripts:
         ! f = fluid, c = conductor, t = total
         ! Magnetic field:
         ! 0 = applied, 1 = induced, total=no subscript
         type(probe),dimension(3) :: ME,ME_fluid,ME_conductor
         type(probe) :: JE,JE_fluid
         type(mesh) :: m,m_surface
         type(domain) :: D_fluid,D_sigma ! Latter for vacuum case
         type(domain) :: D_surface

         integer :: nstep             ! Nth time step
         integer :: N_induction       ! Maximum number iterations in solving B (if iterative)
         integer :: N_cleanB          ! Maximum number iterations to clean B
         real(cp) :: tol_cleanB       ! Tolerance for iteratively solvd cleanB equation
         real(cp) :: tol_induction    ! Tolerance for iteratively solvd induction equation
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         logical :: finite_Rem
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

       subroutine init_induction(ind,m,D_fluid,D_sigma,finite_Rem,Rem,dTime,&
         N_induction,tol_induction,N_cleanB,tol_cleanB,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid,D_sigma
         logical,intent(in) :: finite_Rem
         integer,intent(in) :: N_induction,N_cleanB
         real(cp),intent(in) :: tol_induction,tol_cleanB
         real(cp),intent(in) :: Rem,dTime
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: sigma,prec_cleanB
         type(VF) :: prec_induction
         write(*,*) 'Initializing induction:'

         ind%dTime = dTime
         ind%N_cleanB = N_cleanB
         ind%N_induction = N_induction
         ind%tol_cleanB = tol_cleanB
         ind%tol_induction = tol_induction
         ind%Rem = Rem
         ind%e_budget = 0.0_cp

         if (restartB) then
         call readLastStepFromFile(ind%nstep,str(DT%params),'nstep_ind')
         else; ind%nstep = 0
         endif
         ind%finite_Rem = finite_Rem
         ind%t = 0.0_cp

         call init(ind%m,m)
         call init(ind%D_fluid,D_fluid)
         call init(ind%D_sigma,D_sigma)
         ! --- tensor,vector and scalar fields ---
         call init_Face(ind%B_interior   ,ind%D_sigma%m_in,0.0_cp)
         call init_Edge(ind%U_E          ,m,0.0_cp)
         call init_Edge(ind%temp_E_TF    ,m,0.0_cp)
         call init_Face(ind%temp_F1_TF   ,m,0.0_cp)
         call init_Face(ind%temp_F2_TF   ,m,0.0_cp)
         call init_Face(ind%B            ,m,0.0_cp)
         call init_Face(ind%B0           ,m,0.0_cp)
         call init_CC(ind%temp_CC        ,m,0.0_cp)
         call init_Edge(ind%J            ,m,0.0_cp)
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
         call export_raw(ind%m,ind%vol_CC,str(DT%B),'cell_volume',0)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_BBCs(ind%B,m);                         write(*,*) '     B BCs initialized'
         call init_phiBCs(ind%phi,m);                     write(*,*) '     phi BCs initialized'
         call initBfield(ind%B,ind%B0,m,str(DT%B));       write(*,*) '     B-field initialized'
         call initB_interior(ind%B_interior,ind%D_sigma%m_in,str(DT%B))
         call apply_BCs(ind%B,m);                         write(*,*) '     BCs applied'

         if (solveInduction) call print_BCs(ind%B,'B')
         if (solveInduction) call export_BCs(ind%B,str(DT%params),'B')
         if (solveInduction) call print_BCs(ind%phi,'phi')
         if (solveInduction) call export_BCs(ind%phi,str(DT%params),'phi')

         ! ******************** MATERIAL PROPERTIES ********************
         call init_CC(sigma,m,0.0_cp)
         call initSigma(sigma,ind%D_sigma,m) ! If sigma changes across wall
         write(*,*) '     Materials initialized'
         if (.not.quick_start) call export_raw(m,sigma,str(DT%mat),'sigma',0)
         call divide(ind%sigmaInv_CC,1.0_cp,sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigmaInv_CC,m,ind%temp_F1)
         call treatInterface(ind%sigmaInv_edge,.false.)
         if (.not.quick_start) call export_raw(m,ind%sigmaInv_edge,str(DT%mat),'sigmaInv',0)
         call delete(sigma)
         write(*,*) '     Interface treated'
         ! *************************************************************

         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         call init(ind%probe_divB,str(DT%B),'transient_divB',.not.restartB)
         call init(ind%probe_divJ,str(DT%J),'transient_divJ',.not.restartB)
         call export(ind%probe_divB)
         call export(ind%probe_divJ)

         call init(ind%JE,str(DT%J),'JE',.not.restartB)
         call init(ind%JE_fluid,str(DT%J),'JE_fluid',.not.restartB)

         call init(ind%ME(1),str(DT%B),'ME',.not.restartB)
         call init(ind%ME_fluid(1),str(DT%B),'ME_fluid',.not.restartB)
         call init(ind%ME_conductor(1),str(DT%B),'ME_conductor',.not.restartB)

         call init(ind%ME(2),str(DT%B),'ME0',.not.restartB)
         call init(ind%ME_fluid(2),str(DT%B),'ME0_fluid',.not.restartB)
         call init(ind%ME_conductor(2),str(DT%B),'ME0_conductor',.not.restartB)

         call init(ind%ME(3),str(DT%B),'ME1',.not.restartB)
         call init(ind%ME_fluid(3),str(DT%B),'ME1_fluid',.not.restartB)
         call init(ind%ME_conductor(3),str(DT%B),'ME1_conductor',.not.restartB)

         write(*,*) '     B/J probes initialized'

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************

         ! Initialize multigrid
         temp_unit = newAndOpen(str(DT%params),'info_ind')
         call display(ind,temp_unit)
         close(temp_unit)


         if (finite_Rem) then; ind%MFP_B%c_ind = ind%dTime/ind%Rem
         else;                 ind%MFP_B%c_ind = ind%dTime
         endif

         call init(prec_induction,ind%B)
         call prec_ind_VF(prec_induction,ind%m,ind%sigmaInv_edge,ind%MFP_B%c_ind)
         call init(ind%PCG_B,ind_diffusion,ind_diffusion_explicit,prec_induction,ind%m,&
         ind%tol_induction,ind%MFP_B,ind%B,ind%sigmaInv_edge,str(DT%B),'B',.false.,.false.)
         call delete(prec_induction)

         write(*,*) '     PCG Solver initialized for B'

         call init(prec_cleanB,ind%phi)
         call prec_lap_SF(prec_cleanB,ind%m)
         call init(ind%PCG_cleanB,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_cleanB,&
         ind%m,ind%tol_cleanB,ind%MFP_B,ind%phi,ind%temp_F1,str(DT%B),'phi',.false.,.false.)
         call delete(prec_cleanB)
         write(*,*) '     PCG Solver initialized for phi'

         call init(ind%JAC_B,Lap_uniform_VF_explicit,ind%B,ind%B_interior,&
         ind%sigmaInv_edge,ind%m,ind%D_sigma,ind%MFP_B,10,ind%tol_induction,&
         str(DT%B),'B',.false.)

         write(*,*) '     Finished'
         if (restart_all) call import(ind,DT)
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
         write(un,*) 't,dt = ',ind%t,ind%dTime
         write(un,*) 'solveBMethod,N_ind,N_cleanB = ',solveBMethod,ind%N_induction,ind%N_cleanB
         write(un,*) 'tol_ind,tol_cleanB = ',ind%tol_induction,ind%tol_cleanB
         write(un,*) 'nstep,ME = ',ind%nstep,ind%ME(1)%d
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
         un = newAndOpen(str(DT%restart),'ind_restart')
         write(un,*) ind%dTime;         write(un,*) ind%N_cleanB
         write(un,*) ind%N_induction;   write(un,*) ind%tol_cleanB
         write(un,*) ind%tol_induction; write(un,*) ind%Rem
         write(un,*) ind%e_budget;    write(un,*) ind%nstep
         write(un,*) ind%finite_Rem;    write(un,*) ind%t
         call closeAndMessage(un,str(DT%restart),'ind_restart')
         un = newAndOpen(str(DT%restart,'ind_MFP'))
         call export(ind%MFP_B,un)
         call export(ind%B      ,str(DT%restart),'B')
         call export(ind%B0     ,str(DT%restart),'B0')
         call export(ind%J      ,str(DT%restart),'J')
         call export(ind%U_E%x  ,str(DT%restart),'Uex')
         call export(ind%U_E%y  ,str(DT%restart),'Vey')
         call export(ind%U_E%z  ,str(DT%restart),'Wez')
         call closeAndMessage(un,str(DT%restart),'ind_MFP')
       end subroutine

       subroutine import_induction(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         integer :: un
         un = openToRead(str(DT%restart),'ind_restart')
         read(un,*) ind%dTime;         read(un,*) ind%N_cleanB
         read(un,*) ind%N_induction;   read(un,*) ind%tol_cleanB
         read(un,*) ind%tol_induction; read(un,*) ind%Rem
         read(un,*) ind%e_budget;    read(un,*) ind%nstep
         read(un,*) ind%finite_Rem;    read(un,*) ind%t
         call import(ind%MFP_B,openToRead(str(DT%restart),'ind_MFP'))
         call import(ind%B      ,str(DT%restart),'B')
         call import(ind%B0     ,str(DT%restart),'B0')
         call import(ind%J      ,str(DT%restart),'J')
         call import(ind%U_E%x  ,str(DT%restart),'Uex')
         call import(ind%U_E%y  ,str(DT%restart),'Vey')
         call import(ind%U_E%z  ,str(DT%restart),'Wez')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_induction(ind,DT)
         implicit none
         type(induction),intent(in) :: ind
         type(dir_tree),intent(in) :: DT
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'export_tec_induction at ind%nstep = ',ind%nstep
             call export_processed(ind%m,ind%B ,str(DT%B),'B',1)
             call export_raw(ind%m,ind%B ,str(DT%B),'B',0)
             call export_processed(ind%m,ind%B0,str(DT%B),'B0',1)
             call export_processed(ind%m,ind%J ,str(DT%J),'J',1)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine inductionExportTransient(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call apply(ind%probe_divB,ind%nstep,ind%t,ind%divB,ind%vol_CC)
         call apply(ind%probe_divJ,ind%nstep,ind%t,ind%divJ,ind%vol_CC)
       end subroutine

       subroutine solve_induction(ind,U,PE,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U
         type(print_export),intent(in) :: PE
         type(dir_tree),intent(in) :: DT
         logical :: exportNow,exportNowB,compute_ME

         if (solveMomentum) then; call embedVelocity_E(ind%U_E,U,ind%D_fluid)
         else;if (ind%nstep.le.1) call embedVelocity_E(ind%U_E,U,ind%D_fluid)
         endif

         select case (solveBMethod)
         case (1)
         call CT_Low_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%N_induction,ind%dTime,ind%temp_F1,ind%temp_F2,ind%temp_E,ind%temp_E_TF)

         case (2)
         call CT_Finite_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%dTime,ind%temp_F1,ind%temp_F2,ind%temp_F1_TF%x,ind%temp_E,ind%temp_E_TF)

         case (3)
         call ind_PCG_BE_EE_cleanB_PCG(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%B0,ind%U_E,&
         ind%m,ind%dTime,ind%N_induction,ind%N_cleanB,&
         PE%transient_0D,ind%temp_F1,ind%temp_F2,ind%temp_E,&
         ind%temp_E_TF,ind%temp_CC_SF,ind%phi)

         case (4)
         call CT_Finite_Rem_interior_solved(ind%B,ind%B0,ind%B_interior,ind%U_E,ind%J,&
         ind%sigmaInv_edge,ind%m,ind%D_sigma,ind%dTime,ind%N_induction,ind%temp_F1,ind%temp_F2,&
         ind%temp_F1_TF%x,ind%temp_E,ind%temp_E_TF)

         case (5)
         if (ind%nstep.le.1) call assign(ind%temp_F1,0.0_cp)
         call JAC_interior_solved(ind%JAC_B,ind%B,ind%temp_F1,ind%m,1,ind%N_induction)

         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select

         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem

         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         compute_ME = (computeKB.and.PE%transient_0D.or.(ind%nstep.eq.0))
         if (compute_ME) call compute_induction_energies(ind)

         if (PE%transient_0D) then 
           call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
           call exportTransient(ind)
         endif

         if (PE%transient_2D) call export_processed_transient_3C(ind%m,ind%B,str(DT%B_t),'B',1,ind%nstep)
         ! if (PE%transient_2D) call export_processed_transient_2C(ind%m,ind%B,str(DT%B_t),'B',1,ind%nstep)

         if (PE%info) then
           call print(ind)
           exportNow = readSwitchFromFile(str(DT%params),'exportNow')
           write(*,*) 'got here 1'
           exportNowB = readSwitchFromFile(str(DT%params),'exportNowB')
           write(*,*) 'got here 2'
         else; exportNow = .false.; exportNowB = .false.
         endif

         if (PE%solution.or.exportNowB.or.exportNow) then
           write(*,*) 'got here 3'
           call export(ind,DT)
           write(*,*) 'got here 4'
           call export_tec(ind,DT)
           write(*,*) 'got here 5'
           call writeSwitchToFile(.false.,str(DT%params),'exportNowB')
           write(*,*) 'got here 6'
         endif
       end subroutine

       subroutine compute_induction_energies(ind)
         implicit none
         type(induction),intent(inout) :: ind

         call add(ind%temp_F1,ind%B,ind%B0)
         call face2cellCenter(ind%temp_CC,ind%temp_F1,ind%m)
         call compute_Total_Energy(ind%ME(1),ind%temp_CC,ind%nstep,ind%t,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(1),ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(1),ind%temp_CC,ind%nstep,ind%t,ind%D_sigma)

         call face2cellCenter(ind%temp_CC,ind%B0,ind%m)
         call compute_Total_Energy(ind%ME(2),ind%temp_CC,ind%nstep,ind%t,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(2),ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(2),ind%temp_CC,ind%nstep,ind%t,ind%D_sigma)

         call face2cellCenter(ind%temp_CC,ind%B,ind%m)
         call compute_Total_Energy(ind%ME(3),ind%temp_CC,ind%nstep,ind%t,ind%m)
         call compute_Total_Energy_Domain(ind%ME_fluid(3),ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
         call compute_Total_Energy_Domain(ind%ME_conductor(3),ind%temp_CC,ind%nstep,ind%t,ind%D_sigma)

         call edge2cellCenter(ind%temp_CC,ind%J,ind%m,ind%temp_F1)
         call compute_Total_Energy(ind%JE,ind%temp_CC,ind%nstep,ind%t,ind%m)
         call compute_Total_Energy_Domain(ind%JE_fluid,ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
       end subroutine

       subroutine compute_E_M_budget(ind,U,D_fluid,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(domain),intent(in) :: D_fluid
         type(dir_tree),intent(in) :: DT
         type(TF) :: temp_CC_TF,temp_F1_TF,temp_F2_TF,temp_F3_TF
         type(VF) :: temp_F1,temp_F2,temp_U,sigmaInv_Face,temp_CC_VF

         call init_CC(temp_CC_TF,ind%m)
         call init_CC(temp_CC_VF,ind%m)
         call init_Face(temp_F1,ind%m)
         call init_Face(temp_F2,ind%m)
         call init_Face(temp_F1_TF,ind%m)
         call init_Face(temp_F2_TF,ind%m)
         call init_Face(temp_F3_TF,ind%m)

         call init_Face(temp_U,ind%m)

         call init_Face(sigmaInv_Face,ind%m)
         call cellCenter2Face(sigmaInv_Face,ind%sigmaInv_CC,ind%m)
         call treatInterface(sigmaInv_Face,.false.)

         call embedFace(temp_U,U,D_fluid)
         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         call E_M_Budget(DT,ind%e_budget,ind%B,ind%B,ind%B0,ind%B0,ind%J,&
         sigmaInv_Face,ind%sigmaInv_CC,temp_U,&
         ind%m,ind%dTime,temp_CC_TF,temp_CC_VF,temp_F1,temp_F2,temp_F1_TF,temp_F2_TF,temp_F3_TF)

         call export_E_M_budget(ind,DT)

         call delete(temp_CC_TF)
         call delete(temp_CC_VF)
         call delete(temp_F1)
         call delete(temp_F2)
         call delete(temp_F1_TF)
         call delete(temp_F2_TF)
         call delete(temp_F3_TF)
         call delete(temp_U)
         call delete(sigmaInv_Face)
       end subroutine

       subroutine export_E_M_budget(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(string),dimension(3) :: vars
         integer :: un,i
         un = newAndOpen(str(DT%e_budget),'E_M_budget_terms')
         call init(vars(1),'Unsteady = ')
         call init(vars(2),'Joule_Dissipation = ')
         call init(vars(3),'Poynting = ')

         write(un,*) 'magnetic energy budget at nstep=',ind%nstep
         do i=1,3
         write(un,*) str(vars(i)),ind%e_budget(i)
         call delete(vars(i))
         enddo
         flush(un)
         close(un)
       end subroutine

       subroutine init_E_M_budget_old(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(string) :: vars
         ind%unit_nrg_budget = newAndOpen(str(DT%e_budget),'E_M_budget_terms')
         write(ind%unit_nrg_budget,*) ' TITLE = "magnetic energy budget"'
         call init(vars,' VARIABLES = ')
         call append(vars,'Unsteady,')
         call append(vars,'Joule_Dissipation,')
         call append(vars,'Poynting,')
         write(ind%unit_nrg_budget,*) str(vars)
         write(ind%unit_nrg_budget,*) ' ZONE DATAPACKING = POINT'
         call delete(vars)
       end subroutine

       end module