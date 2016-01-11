       module induction_mod
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
       use export_raw_processed_mod

       use init_BBCs_mod
       use init_Bfield_mod
       use init_Sigma_mod
       use ops_embedExtract_mod

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
       use solverSettings_mod
       use induction_solver_mod
       use preconditioners_mod
       use PCG_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use induction_aux_mod

       use probe_base_mod
       use probe_transient_mod

       implicit none

       private
       public :: induction,init,delete,solve
       public :: export,exportTransient

       logical :: lowRem = .false.
       logical :: finiteRem = .false.
       logical :: semi_implicit = .false.


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type induction
         character(len=9) :: name = 'induction'
         ! --- Tensor fields ---
         type(TF) :: U_E,temp_E_TF                    ! Edge data
         type(TF) :: temp_F1_TF,temp_F2_TF            ! Face data

         ! --- Vector fields ---
         type(VF) :: J,temp_E                         ! Edge data
         type(VF) :: B,B0,temp_F1,temp_F2             ! Face data
         type(VF) :: temp_CC                          ! CC data
         type(VF) :: sigmaInv_edge

         ! --- Scalar fields ---
         type(SF) :: divB,divJ,phi,temp_CC_SF         ! CC data

         ! --- Solvers ---
         type(PCG_solver_VF) :: PCG_B
         type(PCG_solver_SF) :: PCG_cleanB

         type(matrix_free_params) :: MFP_B
         type(matrix_free_params) :: MFP_cleanB

         type(errorProbe) :: probe_divB,probe_divJ
         type(probe) :: KB_energy,KB0_energy,KBi_energy
         type(probe) :: KB_f_energy,KB0_f_energy,KBi_f_energy
         type(mesh) :: m
         type(domain) :: D_fluid,D_sigma ! Latter for vacuum case

         integer :: nstep             ! Nth time step
         integer :: N_induction       ! Maximum number iterations in solving B (if iterative)
         integer :: N_cleanB          ! Maximum number iterations to clean B
         real(cp) :: dTime            ! Time step
         real(cp) :: t                ! Time
         real(cp) :: Rem              ! Magnetic Reynolds number
         real(cp) :: theta
         logical :: finite_Rem
       end type

       interface init;                 module procedure init_induction;                end interface
       interface delete;               module procedure delete_induction;              end interface
       interface solve;                module procedure solve_induction;               end interface
       interface export;               module procedure export_induction;              end interface
       interface exportTransient;      module procedure inductionExportTransient;      end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine init_induction(ind,m,D_fluid,D_sigma,Rem,dt,N_induction,N_cleanB,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid,D_sigma
         integer,intent(in) :: N_induction,N_cleanB
         real(cp),intent(in) :: Rem,dt
         character(len=*),intent(in) :: dir
         type(SF) :: sigma,sigmaInv,prec_cleanB
         type(VF) :: prec_induction
         write(*,*) 'Initializing induction:'
         ind%dTime = dt
         ind%N_cleanB = N_cleanB
         ind%N_induction = N_induction
         ind%Rem = Rem

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
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call initBBCs(ind%B,m)
         if (solveInduction) call print_BCs(ind%B,'B')
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,m,dir)
         write(*,*) '     B-field initialized'

         call apply_BCs(ind%B,m)
         write(*,*) '     BCs applied'

         ! ******************** MATERIAL PROPERTIES ********************
         call init_CC(sigma,m,0.0_cp)
         call init_CC(sigmaInv,m,0.0_cp)
         call initSigma(sigma,ind%D_sigma,m) ! If sigma changes across wall
         write(*,*) '     Materials initialized'
         call export_raw(m,sigma,dir//'material/','sigma',0)
         call divide(sigmaInv,1.0_cp,sigma)
         call cellCenter2Edge(ind%sigmaInv_edge,sigmaInv,m,ind%temp_F1)
         call treatInterface(ind%sigmaInv_edge)
         ! call export_raw(m,ind%sigmaInv_edge,dir//'material/','sigmaInv',0)
         call delete(sigma)
         call delete(sigmaInv)
         write(*,*) '     Interface treated'
         ! *************************************************************

         call init(ind%probe_divB,dir//'Bfield/','transient_divB',.not.restartB)
         call init(ind%probe_divJ,dir//'Jfield/','transient_divJ',.not.restartB)
         call export(ind%probe_divB)
         call export(ind%probe_divJ)

         call init(ind%KB_energy,dir//'Bfield\','KB',.not.restartB)
         call init(ind%KBi_energy,dir//'Bfield\','KBi',.not.restartB)
         call init(ind%KB0_energy,dir//'Bfield\','KB0',.not.restartB)
         call init(ind%KB_f_energy,dir//'Bfield\','KB_f',.not.restartB)
         call init(ind%KBi_f_energy,dir//'Bfield\','KBi_f',.not.restartB)
         call init(ind%KB0_f_energy,dir//'Bfield\','KB0_f',.not.restartB)
         write(*,*) '     B/J probes initialized'

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************

         ! Initialize multigrid
         call inductionInfo(ind,newAndOpen(dir//'parameters/','info_ind'))

         ! ind%theta = 1.0_cp - ind%m%dhmin_min*ind%Rem*(1.0_cp/max(sigmaInv))
         ind%theta = 0.9_cp
         if ((ind%theta.le.0.0_cp).or.(ind%theta.ge.1.0_cp)) then
           stop 'Error: 0 < theta < 1 violated in inductionSolver.f90'
         endif

         if (lowRem) ind%MFP_B%c_ind = ind%dTime
         if (finiteRem) ind%MFP_B%c_ind = ind%dTime/ind%Rem
         if (semi_implicit) ind%MFP_B%c_ind = ind%dTime/ind%Rem*ind%theta

         ind%finite_Rem = finiteRem

         call init(prec_induction,ind%B)
         call prec_curl_curl_VF(prec_induction,ind%m,ind%sigmaInv_edge)
         call init(ind%PCG_B,ind_diffusion,ind_diffusion_explicit,prec_induction,ind%m,&
         ind%MFP_B,ind%B,ind%sigmaInv_edge,dir//'Bfield/','B',.false.,.false.)
         call delete(prec_induction)
         write(*,*) '     PCG Solver initialized'

         call init_BC_mesh(ind%phi,ind%m)
         call init_Dirichlet(ind%phi%RF(1)%b)
         call init_BCs(ind%phi,0.0_cp)

         call init(prec_cleanB,ind%phi)
         call prec_lap_SF(prec_cleanB,ind%m)
         call init(ind%PCG_cleanB,Lap_uniform_props,Lap_uniform_props_explicit,prec_cleanB,&
         ind%m,ind%MFP_B,ind%phi,ind%temp_F1,dir//'Bfield/','phi',.false.,.false.)
         call delete(prec_cleanB)
         write(*,*) '     PCG Solver initialized for phi'

         if (restartB) then
         call readLastStepFromFile(ind%nstep,dir//'parameters/','n_ind')
         else; ind%nstep = 0
         endif
         ind%t = 0.0_cp
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

         call delete(ind%PCG_cleanB)
         call delete(ind%PCG_B)

         write(*,*) 'Induction object deleted'
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine inductionExportTransient(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         if (getExportErrors(ss_MHD)) then
           call apply(ind%probe_divB,ind%nstep,ind%divB,ind%m)
           call apply(ind%probe_divJ,ind%nstep,ind%divJ,ind%m)
         endif
       end subroutine

       subroutine export_induction(ind,m,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting Solutions for B'
             ! call export_raw(m,ind%B0,dir//'Bfield/','B0',0)
             ! call export_raw(m,ind%B ,dir//'Bfield/','B',0)
             ! call export_raw(m,ind%J ,dir//'Jfield/','J',0)
             ! call export_raw(m,ind%U_E%x%x ,dir//'Bfield/','U',0)
             ! call export_raw(m,ind%U_E%y%y ,dir//'Bfield/','V',0)
             ! call export_raw(m,ind%U_E%z%z ,dir//'Bfield/','W',0)
             call export_raw(m,ind%divB,dir//'Bfield/','divB',0)
             call export_raw(m,ind%divJ,dir//'Jfield/','divJ',0)

             ! call export_processed(m,ind%B0,dir//'Bfield/','B0',1)
             call export_processed(m,ind%B ,dir//'Bfield/','B',1)
             call export_processed(m,ind%J ,dir//'Jfield/','J',1)
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
         write(un,*) '(Rem) = ',ind%Rem
         write(un,*) '(t,dt) = ',ind%t,ind%dTime
         write(un,*) '(nstep) = ',ind%nstep
         write(un,*) ''
         call export(ind%m,un)
         write(un,*) ''
         call printPhysicalMinMax(ind%B,'B')
         call printPhysicalMinMax(ind%B0,'B0')
         call printPhysicalMinMax(ind%divB,'divB')
         call printPhysicalMinMax(ind%divJ,'divJ')
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_induction(ind,U,ss_MHD,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U
         type(solverSettings),intent(inout) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow,compute_ME

         call embedVelocity_E(ind%U_E,U,ind%D_fluid)

         select case (solveBMethod)
         case (1)
         call CT_Low_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%N_induction,ind%dTime,ind%temp_F1,ind%temp_F2,ind%temp_E,ind%temp_E_TF)

         case (2)
         call CT_Finite_Rem(ind%B,ind%B0,ind%U_E,ind%J,ind%sigmaInv_edge,ind%m,&
         ind%Rem,ind%dTime,ind%temp_F1,ind%temp_F2,ind%temp_E,ind%temp_E_TF)

         case (3)
         ! call ind_PCG_BE_EE_cleanB_PCG(ind%PCG_B,ind%PCG_cleanB,ind%B,ind%B0,ind%U_E,ind%m,&
         ! ind%N_induction,ind%N_cleanB,getExportErrors(ss_MHD),ind%temp_F1,ind%temp_F2,ind%temp_E,&
         ! ind%temp_E_TF,ind%temp_CC,ind%phi)

         case default; stop 'Error: bad solveBMethod input solve_induction in induction.f90'
         end select

         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem

         call compute_J(ind%J,ind%B,ind%Rem,ind%m,ind%finite_Rem)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         compute_ME = (computeKB.and.getExportErrors(ss_MHD).or.(ind%nstep.eq.0))

         call add(ind%temp_F1,ind%B,ind%B0)
         call face2cellCenter(ind%temp_CC,ind%temp_F1,ind%m)
         call compute_TME(ind%KB_energy,ind%temp_CC,ind%nstep,compute_ME,ind%m)
         call compute_TME_fluid(ind%KB_f_energy,ind%temp_CC,ind%nstep,compute_ME,ind%D_fluid)

         call face2cellCenter(ind%temp_CC,ind%B,ind%m)
         call compute_TME(ind%KBi_energy,ind%temp_CC,ind%nstep,compute_ME,ind%m)
         call compute_TME_fluid(ind%KBi_f_energy,ind%temp_CC,ind%nstep,compute_ME,ind%D_fluid)

         call face2cellCenter(ind%temp_CC,ind%B0,ind%m)
         call compute_TME(ind%KB0_energy,ind%temp_CC,ind%nstep,compute_ME,ind%m)
         call compute_TME_fluid(ind%KB0_f_energy,ind%temp_CC,ind%nstep,compute_ME,ind%D_fluid)

         call exportTransient(ind,ss_MHD)

         ! call inductionExportTransientFull(ind,ind%m,dir) ! VERY Expensive

         if (getExportErrors(ss_MHD)) call compute_divBJ(ind%divB,ind%divJ,ind%B,ind%J,ind%m)
         ! if (getExportErrors(ss_MHD)) call exportTransientFull(ind,ind%m,dir)

         if (getPrintParams(ss_MHD)) then
           call inductionInfo(ind,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowB')
         else; exportNow = .false.
         endif

         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(ind,ind%m,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
       end subroutine

       end module