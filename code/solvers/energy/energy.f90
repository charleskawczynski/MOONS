       module energy_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       use mesh_domain_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod

       use PCG_mod
       use PCG_solver_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use preconditioners_mod

       use energy_aux_mod
       use energy_solver_mod
       use init_TBCs_mod
       use init_Tfield_mod
       use init_K_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use ops_embedExtract_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use boundary_conditions_mod
       use apply_BCs_mod

       use probe_mod
       use ops_norms_mod

       implicit none

       private
       public :: energy
       public :: init,delete,display,print,export,import ! Essentials
       public :: solve,export_transient,export_tec

       type energy
         ! --- Vector fields ---
         type(SF) :: T,temp_CC1,temp_CC2   ! CC data
         type(VF) :: temp_F,k              ! Face data
         type(VF) :: U_F                   ! Face data
         type(VF) :: gravity               ! CC data
         type(VF) :: temp_CC1_VF           ! CC data
         type(VF) :: temp_CC2_VF           ! CC data
         ! --- Scalar fields ---
         type(SF) :: divQ                  ! CC data
         type(SF) :: Q_source

         type(probe) :: probe_divQ

         type(mesh) :: m
         type(mesh_domain) :: MD
         type(matrix_free_params) :: MFP

         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP_T

         type(PCG_Solver_SF) :: PCG_T
         type(sim_params) :: SP

         integer :: nstep             ! Nth time step
         integer :: N_nrg             ! Maximum number iterations in solving T (if iterative)
         real(cp) :: dTime            ! Time step
         real(cp) :: time             ! Time
         real(cp) :: tol_nrg             ! Time

         real(cp) :: Re,Pr,Ec,Ha  ! Reynolds, Prandtl, Eckert, Hartmann
         logical :: suppress_warning
       end type

       interface init;               module procedure init_energy;             end interface
       interface delete;             module procedure delete_energy;           end interface
       interface display;            module procedure display_energy;          end interface
       interface print;              module procedure print_energy;            end interface
       interface export;             module procedure export_energy;           end interface
       interface import;             module procedure import_energy;           end interface

       interface solve;              module procedure solve_energy;            end interface
       interface export_transient;   module procedure export_transient_nrg;    end interface
       interface export_tec;         module procedure export_tec_energy;       end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_energy(nrg,m,SP,MD,TMP,ISP_T,Re,Pr,Ec,Ha,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(iter_solver_params),intent(in) :: ISP_T
         real(cp),intent(in) :: Re,Pr,Ec,Ha
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: k_cc,vol_CC
         write(*,*) 'Initializing energy:'
         call init(nrg%TMP,TMP)
         call init(nrg%ISP_T,ISP_T)
         nrg%Re = Re
         nrg%Pr = Pr
         nrg%Ec = Ec
         nrg%Ha = Ha
         call init(nrg%SP,SP)

         call init(nrg%m,m)
         call init(nrg%MD,MD)

         call init_CC(nrg%T,m,0.0_cp)
         call init_CC(nrg%Q_source,m,0.0_cp)
         call init_CC(nrg%temp_CC2,m,0.0_cp)
         call init_Face(nrg%temp_F,m,0.0_cp)

         call init_Face(nrg%k,m,0.0_cp)
         call init_Face(nrg%U_F,m,0.0_cp)
         call init_CC(nrg%temp_CC1,m,0.0_cp)
         call init_CC(nrg%gravity,m,0.0_cp)
         call init_CC(nrg%temp_CC1_VF,m,0.0_cp)
         call init_CC(nrg%temp_CC2_VF,m,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(nrg%divQ,m)
         call init_CC(vol_CC,m)
         call volume(vol_CC,m)
         if (nrg%SP%export_cell_volume) then
           call export_raw(nrg%m,vol_CC,str(DT%meshes),'mom_cell_volume',0)
         endif
         call delete(vol_CC)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_TBCs(nrg%T,nrg%m)
         if (nrg%SP%solveEnergy) call print_BCs(nrg%T,'T')
         if (nrg%SP%solveEnergy) call export_BCs(nrg%T,str(DT%T_BCs),'T')
         write(*,*) '     BCs initialized'

         call initTfield(nrg%T,m,nrg%SP%restartT,str(DT%T))
         write(*,*) '     T-field initialized'

         call apply_BCs(nrg%T,m)
         write(*,*) '     BCs applied'

         call init_CC(k_cc,m,0.0_cp)
         call initK(k_cc,nrg%m,nrg%MD)
         call cellCenter2Face(nrg%k,k_cc,m)
         call delete(k_cc)
         write(*,*) '     Materials initialized'

         call init(nrg%probe_divQ,str(DT%T),'probe_divQ',nrg%SP%restartT,SP)

         nrg%MFP%coeff = -0.5_cp*nrg%TMP%dt/(nrg%Re*nrg%Pr)
         call init(nrg%PCG_T,nrg_diffusion,nrg_diffusion_explicit,prec_lap_SF,nrg%m,&
         nrg%ISP_T,nrg%MFP,nrg%T,nrg%temp_F,str(DT%T),'T',.false.,.false.)

         temp_unit = new_and_open(str(DT%params),'info_nrg')
         call print(nrg)
         call close_and_message(temp_unit,str(DT%params),'info_nrg')

         write(*,*) '     probes initialized'
         write(*,*) '     Finished'
       end subroutine

       subroutine delete_energy(nrg)
         implicit none
         type(energy),intent(inout) :: nrg

         call delete(nrg%T)
         call delete(nrg%Q_source)
         call delete(nrg%temp_F)
         call delete(nrg%k)
         call delete(nrg%temp_CC1)
         call delete(nrg%temp_CC2)

         call delete(nrg%U_F)
         call delete(nrg%gravity)
         call delete(nrg%temp_CC1_VF)
         call delete(nrg%temp_CC2_VF)

         call delete(nrg%divQ)

         call delete(nrg%probe_divQ)
         call delete(nrg%m)
         call delete(nrg%MD)
         call delete(nrg%PCG_T)

         call delete(nrg%TMP)
         call delete(nrg%ISP_T)

         write(*,*) 'energy object deleted'
       end subroutine

       subroutine display_energy(nrg,un)
         implicit none
         type(energy),intent(in) :: nrg
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '*************************** ENERGY ***************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Re,Pr = ',nrg%Re,nrg%Pr
         write(un,*) 'Ec,Ha = ',nrg%Ec,nrg%Ha
         write(un,*) 't,dt = ',nrg%TMP%t,nrg%TMP%dt
         write(un,*) 'solveTMethod,N_nrg = ',nrg%SP%solveTMethod,nrg%ISP_T%iter_max
         write(un,*) 'tol_nrg = ',nrg%ISP_T%tol_rel
         call displayPhysicalMinMax(nrg%T,'T',un)
         call displayPhysicalMinMax(nrg%divQ,'divQ',un)
         write(un,*) ''
         call print(nrg%m)
         write(un,*) ''
       end subroutine

       subroutine print_energy(nrg)
         implicit none
         type(energy),intent(in) :: nrg
         call display(nrg,6)
       end subroutine

       subroutine export_energy(nrg,DT)
         implicit none
         type(energy),intent(in) :: nrg
         type(dir_tree),intent(in) :: DT
         integer :: un
         call export(nrg%TMP)
         call export(nrg%ISP_T)

         un = new_and_open(str(DT%restart),'nrg_restart')
         write(un,*) nrg%Re,nrg%Pr,nrg%Ha,nrg%Ec
         call close_and_message(un,str(DT%restart),'nrg_restart')

         un = new_and_open(str(DT%restart),'nrg_MFP')
         call export(nrg%MFP,un)
         call close_and_message(un,str(DT%restart),'nrg_MFP')

         call export(nrg%T   ,str(DT%restart),'T_nrg')
         call export(nrg%U_F ,str(DT%restart),'U_nrg')
         call export(nrg%k   ,str(DT%restart),'k_nrg')
       end subroutine

       subroutine import_energy(nrg,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(dir_tree),intent(in) :: DT
         integer :: un
         call import(nrg%TMP)
         call import(nrg%ISP_T)

         un = open_to_read(str(DT%restart),'nrg_restart')
         read(un,*) nrg%Re,nrg%Pr,nrg%Ha,nrg%Ec
         call close_and_message(un,str(DT%restart),'nrg_restart')

         un = open_to_read(str(DT%restart),'nrg_MFP')
         call import(nrg%MFP,un)
         call close_and_message(un,str(DT%restart),'nrg_MFP')

         call import(nrg%T   ,str(DT%restart),'T_nrg')
         call import(nrg%U_F ,str(DT%restart),'U_nrg')
         call import(nrg%k   ,str(DT%restart),'k_nrg')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_energy(nrg,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(dir_tree),intent(in) :: DT
         if (nrg%SP%solveEnergy) then
           write(*,*) 'export_tec_energy at nrg%TMP%n_step = ',nrg%TMP%n_step
           call export_processed(nrg%m,nrg%T,str(DT%T),'T',0)
           call export_raw(nrg%m,nrg%T,str(DT%T),'T',0)
           call export_raw(nrg%m,nrg%divQ,str(DT%T),'divQ',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine export_transient_nrg(nrg)
         implicit none
         type(energy),intent(inout) :: nrg
         real(cp) :: temp
         call assign_ghost_XPeriodic(nrg%divQ,0.0_cp)
         call Ln(temp,nrg%divQ,2.0_cp,nrg%m); call export(nrg%probe_divQ,nrg%TMP,temp)
       end subroutine

       subroutine solve_energy(nrg,U,PE,EN,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(VF),intent(in) :: U
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         call assign(nrg%gravity%x,1.0_cp)

         call embed_velocity_F(nrg%U_F,U,nrg%MD)

         select case (nrg%SP%solveTMethod)
         case (1)
         call explicitEuler(nrg%T,nrg%U_F,nrg%TMP%dt,nrg%Re,&
         nrg%Pr,nrg%m,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)

         case (2) ! O2 time marching
         call explicitEuler(nrg%T,nrg%U_F,nrg%TMP%dt,nrg%Re,&
         nrg%Pr,nrg%m,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)

         case (3) ! Diffusion implicit
         call diffusion_implicit(nrg%PCG_T,nrg%T,nrg%U_F,nrg%TMP%dt,nrg%Re,&
         nrg%Pr,nrg%m,PE%transient_0D,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)

         case (4)
         if (nrg%TMP%n_step.le.1) call volumetric_heating_equation(nrg%Q_source,nrg%m,nrg%Re,nrg%Pr)

         call explicitEuler_with_source(nrg%T,nrg%U_F,nrg%TMP%dt,nrg%Re,&
         nrg%Pr,nrg%m,nrg%Q_source,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)

         case (5)
         if (nrg%TMP%n_step.le.1) call volumetric_heating_equation(nrg%Q_source,nrg%m,nrg%Re,nrg%Pr)
         call CN_with_source(nrg%PCG_T,nrg%T,nrg%U_F,nrg%TMP%dt,nrg%Re,&
         nrg%Pr,nrg%m,nrg%Q_source,PE%transient_0D,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)

         case default; stop 'Erorr: bad solveTMethod value in solve_energy in energy.f90'
         end select
         call iterate_step(nrg%TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (PE%transient_0D) then
           call compute_Q(nrg%temp_F,nrg%T,nrg%k,nrg%m)
           call compute_divQ(nrg%divQ,nrg%temp_F,nrg%m)
           call export_transient(nrg)
         endif

         if (PE%info) call print(nrg)

         if (PE%solution.or.EN%T%this.or.EN%all%this) then
           call export(nrg,DT)
           call export_tec(nrg,DT)
         endif
       end subroutine

       end module