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
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use induction_aux_mod
       use E_M_Budget_mod

       use probe_base_mod
       use probe_transient_mod

       implicit none

       private
       public :: induction,init,delete,solve
       public :: export,exportTransient
       public :: compute_E_M_budget

       type induction
         ! --- Tensor fields ---
         type(TF) :: U_E,temp_E_TF                    ! Edge data
         type(TF) :: temp_F1_TF,temp_F2_TF            ! Face data

         ! --- Vector fields ---
         type(VF) :: J,temp_E                         ! Edge data
         type(VF) :: B,B0,temp_F1,temp_F2             ! Face data
         type(VF) :: temp_CC                          ! CC data
         type(VF) :: sigmaInv_edge

         ! --- Scalar fields ---
         type(SF) :: sigmaInv_CC
         type(SF) :: divB,divJ,phi,temp_CC_SF         ! CC data
         type(SF) :: vol_CC

         ! --- Solvers ---
         type(PCG_solver_VF) :: PCG_B
         type(PCG_solver_SF) :: PCG_cleanB

         type(matrix_free_params) :: MFP_B
         type(matrix_free_params) :: MFP_cleanB

         type(errorProbe) :: probe_divB,probe_divJ

         type(probe) :: KB_energy,KB0_energy,KBi_energy
         type(probe) :: KB_f_energy,KB0_f_energy,KBi_f_energy
         type(probe) :: KB_c_energy,KB0_c_energy,KBi_c_energy
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
         real(cp),dimension(5) :: nrg_budget
         real(cp),dimension(0:2) :: ME
         real(cp),dimension(0:2) :: ME_fluid
         real(cp),dimension(0:2) :: ME_conductor
       end type

       interface init;                 module procedure init_induction;                end interface
       interface delete;               module procedure delete_induction;              end interface
       interface solve;                module procedure solve_induction;               end interface
       interface export;               module procedure export_induction;              end interface
       interface exportTransient;      module procedure inductionExportTransient;      end interface

       contains

       ! ******************* INIT/DELETE ***********************

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
         ind%nrg_budget = 0.0_cp

         call init(ind%m,m)
         call init(ind%D_fluid,D_fluid)
         call init(ind%D_sigma,D_sigma)
         ! --- tensor,vector and scalar fields ---
         call init_Edge(ind%U_E,m,0.0_cp)
         call init_Edge(ind%temp_E_TF,m,0.0_cp)
         call init_Face(ind%temp_F1_TF,m,0.0_cp)
         call init_Face(ind%temp_F2_TF,m,0.0_cp)
         call init_Face(ind%B,m,0.0_cp)
         call init_Face(ind%B0,m,0.0_cp)
         call init_CC(ind%temp_CC,m,0.0_cp)
         call init_Edge(ind%J,m,0.0_cp)
         call init_Edge(ind%temp_E,m,0.0_cp)
         call init_Edge(ind%sigmaInv_edge,m,0.0_cp)
         call init_Face(ind%temp_F1,m,0.0_cp)
         call init_Face(ind%temp_F2,m,0.0_cp)
         call init_CC(ind%phi,m,0.0_cp)
         call init_CC(ind%temp_CC_SF,m,0.0_cp)
         call init_CC(ind%divB,m,0.0_cp)
         call init_Node(ind%divJ,m,0.0_cp)

         call init_CC(ind%sigmaInv_CC,m,0.0_cp)
         call init_CC(ind%vol_CC,m)
         call volume(ind%vol_CC,m)
         write(*,*) '     Fields allocated'

         ! call BEM_Poisson(m,DT)

         ! --- Initialize Fields ---
         call init_BBCs(ind%B,m)
         write(*,*) '     B BCs initialized'
         call init_phiBCs(ind%phi,m)
         write(*,*) '     phi BCs initialized'

         if (solveInduction) call print_BCs(ind%B,'B')
         if (solveInduction) call export_BCs(ind%B,str(DT%params),'B')
         if (solveInduction) call print_BCs(ind%phi,'phi')
         if (solveInduction) call export_BCs(ind%phi,str(DT%params),'phi')

         call initBfield(ind%B,ind%B0,m,str(DT%B))
         write(*,*) '     B-field initialized'

         call apply_BCs(ind%B,m)
         write(*,*) '     BCs applied'

         ! ******************** MATERIAL PROPERTIES ********************
         call init_CC(sigma,m,0.0_cp)
         call initSigma(sigma,ind%D_sigma,m) ! If sigma changes across wall
         write(*,*) '     Materials initialized'
         if (.not.quick_start) call export_raw(m,sigma,str(DT%mat),'sigma',0)
         call divide(ind%sigmaInv_CC,1.0_cp,sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,ind%sigmaInv_CC,m,ind%temp_F1)
         call treatInterface(ind%sigmaInv_edge)
         if (.not.quick_start) call export_raw(m,ind%sigmaInv_edge,str(DT%mat),'sigmaInv',0)
         call delete(sigma)
         write(*,*) '     Interface treated'
         ! *************************************************************

         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         call init(ind%probe_divB,str(DT%B),'transient_divB',.not.restartB)
         call init(ind%probe_divJ,str(DT%J),'transient_divJ',.not.restartB)
         call export(ind%probe_divB)
         call export(ind%probe_divJ)

         call init(ind%KB_energy,str(DT%B),'KB',.not.restartB)
         call init(ind%KBi_energy,str(DT%B),'KBi',.not.restartB)
         call init(ind%KB0_energy,str(DT%B),'KB0',.not.restartB)
         call init(ind%KB_f_energy,str(DT%B),'KB_f',.not.restartB)
         call init(ind%KBi_f_energy,str(DT%B),'KBi_f',.not.restartB)
         call init(ind%KB0_f_energy,str(DT%B),'KB0_f',.not.restartB)
         call init(ind%KB_c_energy,str(DT%B),'KB_c',.not.restartB)
         call init(ind%KBi_c_energy,str(DT%B),'KBi_c',.not.restartB)
         call init(ind%KB0_c_energy,str(DT%B),'KB0_c',.not.restartB)
         write(*,*) '     B/J probes initialized'

         ind%unit_nrg_budget = newAndOpen(str(DT%B),'energy_budget')
         ! {error} = {dBdt} + {adv} - {diff}
         write(ind%unit_nrg_budget,*) ' TITLE = "induction energy budget"'
         write(ind%unit_nrg_budget,*) ' VARIABLES = N,unsteady,Poynting,Joule_Dissipation,maxwell_stress,E_M_Convection'
         write(ind%unit_nrg_budget,*) ' VARIABLES = N,unsteady,Poynting,Joule_Dissipation,Lorentz'

         write(ind%unit_nrg_budget,*) ' ZONE DATAPACKING = POINT'
         flush(ind%unit_nrg_budget)

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************

         ! Initialize multigrid
         temp_unit = newAndOpen(str(DT%params),'info_ind')
         call inductionInfo(ind,temp_unit)
         close(temp_unit)

         ind%finite_Rem = finite_Rem

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

         if (restartB) then
         call readLastStepFromFile(ind%nstep,str(DT%params),'nstep_ind')
         else; ind%nstep = 0
         endif
         ind%t = 0.0_cp
         ind%ME = 0.0_cp
         ind%ME_fluid = 0.0_cp
         ind%ME_conductor = 0.0_cp
         write(*,*) '     Finished'
       end subroutine

       subroutine delete_induction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%temp_E_TF)
         call delete(ind%temp_F1_TF)
         call delete(ind%temp_F2_TF)
         call delete(ind%U_E)

         call delete(ind%B)
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
         call delete(ind%KB_energy)
         call delete(ind%KBi_energy)
         call delete(ind%KB0_energy)
         call delete(ind%KB_f_energy)
         call delete(ind%KBi_f_energy)
         call delete(ind%KB0_f_energy)
         call delete(ind%KB_c_energy)
         call delete(ind%KBi_c_energy)
         call delete(ind%KB0_c_energy)

         call delete(ind%PCG_cleanB)
         call delete(ind%PCG_B)

         write(*,*) 'Induction object deleted'
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine inductionExportTransient(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call apply(ind%probe_divB,ind%nstep,ind%t,ind%divB,ind%vol_CC)
         call apply(ind%probe_divJ,ind%nstep,ind%t,ind%divJ,ind%vol_CC)
       end subroutine

       subroutine export_induction(ind,m,DT)
         implicit none
         type(induction),intent(in) :: ind
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting Solutions for B at ind%nstep = ',ind%nstep
             call export_processed(m,ind%B ,str(DT%B),'B',1)
             ! call export_raw(m,ind%B0,str(DT%B),'B0',0)
             call export_raw(m,ind%B ,str(DT%B),'B',0)
             call export_raw(m,ind%J ,str(DT%J),'J',0)
             call export_raw(m,ind%U_E%x ,str(DT%B),'Uex',0)
             call export_raw(m,ind%U_E%y ,str(DT%B),'Vey',0)
             call export_raw(m,ind%U_E%z ,str(DT%B),'Wez',0)
             call export_raw(m,ind%divB,str(DT%B),'divB',0)
             call export_raw(m,ind%divJ,str(DT%J),'divJ',0)

             call export_processed(m,ind%B0,str(DT%B),'B0',1)
             call export_processed(m,ind%J ,str(DT%J),'J',1)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine inductionInfo(ind,un)
         ! Use un = 6 to print to screen
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
         write(un,*) 'nstep = ',ind%nstep
         write(un,'(A4,3(ES20.8E3))') ' ME=',ind%ME
         write(un,'(A5,3(ES20.8E3))') ' MEf=',ind%ME_fluid
         write(un,'(A5,3(ES20.8E3))') ' MEc=',ind%ME_conductor
         call printPhysicalMinMax(ind%divB,'divB')
         call printPhysicalMinMax(ind%divJ,'divJ')
         write(un,*) ''
         call display(ind%m,un)
         write(un,*) ''
       end subroutine

       ! ******************* SOLVER ****************************

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

         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select

         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem

         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         compute_ME = (computeKB.and.PE%transient_0D.or.(ind%nstep.eq.0))

         if (compute_ME) then
           call face2cellCenter(ind%temp_CC,ind%B0,ind%m)
           call compute_TME(ind%ME(0),ind%KB0_energy,ind%temp_CC,ind%nstep,ind%t,ind%m)
           call compute_TME_Domain(ind%ME_fluid(0),ind%KB0_f_energy,ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
           call compute_TME_Domain(ind%ME_conductor(0),ind%KB0_c_energy,ind%temp_CC,ind%nstep,ind%t,ind%D_sigma)

           call face2cellCenter(ind%temp_CC,ind%B,ind%m)
           call compute_TME(ind%ME(1),ind%KBi_energy,ind%temp_CC,ind%nstep,ind%t,ind%m)
           call compute_TME_Domain(ind%ME_fluid(1),ind%KBi_f_energy,ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
           call compute_TME_Domain(ind%ME_conductor(1),ind%KBi_c_energy,ind%temp_CC,ind%nstep,ind%t,ind%D_sigma)

           call add(ind%temp_F1,ind%B,ind%B0)
           call face2cellCenter(ind%temp_CC,ind%temp_F1,ind%m)
           call compute_TME(ind%ME(2),ind%KB_energy,ind%temp_CC,ind%nstep,ind%t,ind%m)
           call compute_TME_Domain(ind%ME_fluid(2),ind%KB_f_energy,ind%temp_CC,ind%nstep,ind%t,ind%D_fluid)
           call compute_TME_Domain(ind%ME_conductor(2),ind%KB_c_energy,ind%temp_CC,ind%nstep,ind%t,ind%D_sigma)
         endif

         if (PE%transient_0D) then 
           call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
           call exportTransient(ind)
         endif

         if (PE%transient_2D) call export_processed_transient_3C(ind%m,ind%B,str(DT%B_t),'B',1,ind%nstep)
         ! if (PE%transient_2D) call export_processed_transient_2C(ind%m,ind%B,str(DT%B_t),'B',1,ind%nstep)

         if (PE%info) then
           call inductionInfo(ind,6)
           exportNow = readSwitchFromFile(str(DT%params),'exportNow')
           exportNowB = readSwitchFromFile(str(DT%params),'exportNowB')
         else; exportNow = .false.; exportNowB = .false.
         endif

         if (PE%solution.or.exportNowB.or.exportNow) then
           call export(ind,ind%m,DT)
           call writeSwitchToFile(.false.,str(DT%params),'exportNowB')
         endif
       end subroutine

       subroutine compute_E_M_budget(ind,U,U_CC,D_fluid)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U,U_CC
         type(domain),intent(in) :: D_fluid
         type(TF) :: temp_CC_TF,temp_F1_TF,temp_F2_TF,temp_F3_TF
         type(VF) :: temp_F1,temp_F2,temp_U,temp_U_CC,sigmaInv_Face,temp_CC_VF

         call init_CC(temp_CC_TF,ind%m)
         call init_CC(temp_CC_VF,ind%m)
         call init_Face(temp_F1,ind%m)
         call init_Face(temp_F2,ind%m)
         call init_Face(temp_F1_TF,ind%m)
         call init_Face(temp_F2_TF,ind%m)
         call init_Face(temp_F3_TF,ind%m)

         call init_Face(temp_U,ind%m)
         call init_CC(temp_U_CC,ind%m)

         call init_Face(sigmaInv_Face,ind%m)
         call cellCenter2Face(sigmaInv_Face,ind%sigmaInv_CC,ind%m)
         call treatInterface(sigmaInv_Face)

         call embedCC(temp_U_CC,U_CC,D_fluid)
         call embedFace(temp_U,U,D_fluid)
         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         call E_M_Budget(ind%nrg_budget,ind%B,ind%B,ind%B0,ind%B0,ind%J,&
         sigmaInv_Face,ind%sigmaInv_CC,temp_U,temp_U_CC,&
         ind%m,ind%dTime,temp_CC_TF,temp_CC_VF,temp_F1,temp_F2,temp_F1_TF,temp_F2_TF,temp_F3_TF)

         write(ind%unit_nrg_budget,*) ind%nstep,ind%nrg_budget
         flush(ind%unit_nrg_budget)

         call delete(temp_CC_TF)
         call delete(temp_CC_VF)
         call delete(temp_F1)
         call delete(temp_F2)
         call delete(temp_F1_TF)
         call delete(temp_F2_TF)
         call delete(temp_F3_TF)
         call delete(temp_U)
         call delete(temp_U_CC)
         call delete(sigmaInv_Face)
       end subroutine

       end module