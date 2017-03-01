       module energy_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use export_raw_processed_symmetry_mod
       use export_processed_FPL_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use mesh_mod
       use mesh_domain_mod
       use dir_tree_mod
       use string_mod
       use path_mod
       use export_frequency_mod
       use export_now_mod
       use refine_mesh_mod

       use PCG_mod
       use PCG_solver_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use preconditioners_mod

       use energy_aux_mod
       use energy_solver_mod
       use init_T_BCs_mod
       use init_T_field_mod
       use init_gravity_field_mod
       use init_K_mod

       use iter_solver_params_mod
       use time_marching_params_mod
       use time_marching_methods_SF_mod

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
       public :: solve,export_tec

       type energy
         ! --- Vector fields ---
         type(SF) :: T,Tnm1,temp_CC1,temp_CC2   ! CC data
         type(SF) :: F,Fnm1
         type(VF) :: temp_F,k              ! Face data
         type(VF) :: U_F                   ! Face data
         type(VF) :: U_CC                  ! Face data
         type(VF) :: gravity               ! CC data
         type(VF) :: temp_CC1_VF           ! CC data
         type(VF) :: temp_CC2_VF           ! CC data
         type(TF) :: temp_CC_TF            ! CC data
         ! --- Scalar fields ---
         type(SF) :: divQ                  ! CC data
         type(SF) :: Q_source

         type(probe) :: probe_divQ

         type(mesh) :: m
         type(mesh_domain) :: MD

         type(PCG_Solver_SF) :: PCG_T
         type(sim_params) :: SP

         logical :: suppress_warning
       end type

       interface init;               module procedure init_energy;            end interface
       interface delete;             module procedure delete_energy;          end interface
       interface display;            module procedure display_energy;         end interface
       interface print;              module procedure print_energy;           end interface
       interface export;             module procedure export_energy;          end interface
       interface import;             module procedure import_energy;          end interface

       interface solve;              module procedure solve_energy;           end interface
       interface export_unsteady_0D; module procedure export_unsteady_0D_nrg; end interface
       interface export_unsteady_1D; module procedure export_unsteady_1D_nrg; end interface
       interface export_unsteady_2D; module procedure export_unsteady_2D_nrg; end interface
       interface export_unsteady_3D; module procedure export_unsteady_3D_nrg; end interface
       interface export_tec;         module procedure export_tec_energy;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_energy(nrg,m,SP,DT,MD)
         implicit none
         type(energy),intent(inout) :: nrg
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         type(SF) :: k_cc
         write(*,*) 'Initializing energy:'
         call init(nrg%SP,SP)

         call init(nrg%m,m)
         call init(nrg%MD,MD)

         call init_CC(nrg%T,m,0.0_cp)
         call init_CC(nrg%Tnm1,m,0.0_cp)
         call init_CC(nrg%F,m,0.0_cp)
         call init_CC(nrg%Fnm1,m,0.0_cp)
         call init_CC(nrg%Q_source,m,0.0_cp)
         call init_CC(nrg%temp_CC2,m,0.0_cp)
         call init_Face(nrg%temp_F,m,0.0_cp)

         call init_Face(nrg%k,m,0.0_cp)
         call init_Face(nrg%U_F,m,0.0_cp)
         call init_CC(nrg%U_CC,m,0.0_cp)
         call init_CC(nrg%temp_CC1,m,0.0_cp)
         call init_CC(nrg%gravity,m,0.0_cp)
         call init_CC(nrg%temp_CC1_VF,m,0.0_cp)
         call init_CC(nrg%temp_CC2_VF,m,0.0_cp)
         call init_CC(nrg%temp_CC_TF,m,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(nrg%divQ,m)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_T_BCs(nrg%T,nrg%m,nrg%SP)
         if (nrg%SP%VS%T%SS%solve) call print_BCs(nrg%T,'T')
         if (nrg%SP%VS%T%SS%solve) call export_BCs(nrg%T,str(DT%T%BCs),'T')
         write(*,*) '     BCs initialized'

         call init_T_field(nrg%T,m,nrg%SP,str(DT%T%field))
         call init_gravity_field(nrg%gravity,m,nrg%SP,str(DT%T%field))
         write(*,*) '     T-field initialized'

         call apply_BCs(nrg%T)
         call assign(nrg%Tnm1,nrg%T)
         write(*,*) '     BCs applied'

         call init_CC(k_cc,m,0.0_cp)
         call initK(k_cc,nrg%m,nrg%MD)
         call cellCenter2Face(nrg%k,k_cc,m)
         call delete(k_cc)
         write(*,*) '     Materials initialized'

         call init(nrg%Probe_divQ,str(DT%T%residual),'probe_divQ',nrg%SP%VS%T%SS%restart,.true.)

         call init(nrg%PCG_T,nrg_diffusion,nrg_diffusion_explicit,prec_lap_SF,nrg%m,&
         nrg%SP%VS%T%ISP,nrg%SP%VS%T%MFP,nrg%T,nrg%temp_F,str(DT%T%residual),'T',.false.,.false.)

         temp_unit = new_and_open(str(DT%params),'info_nrg')
         call display(nrg,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_nrg')

         write(*,*) '     probes initialized'
         write(*,*) '     Finished'
       end subroutine

       subroutine delete_energy(nrg)
         implicit none
         type(energy),intent(inout) :: nrg

         call delete(nrg%T)
         call delete(nrg%Tnm1)
         call delete(nrg%F)
         call delete(nrg%Fnm1)
         call delete(nrg%Q_source)
         call delete(nrg%temp_F)
         call delete(nrg%k)
         call delete(nrg%temp_CC1)
         call delete(nrg%temp_CC2)

         call delete(nrg%U_F)
         call delete(nrg%U_CC)
         call delete(nrg%gravity)
         call delete(nrg%temp_CC1_VF)
         call delete(nrg%temp_CC2_VF)
         call delete(nrg%temp_CC_TF)

         call delete(nrg%divQ)

         call delete(nrg%Probe_divQ)
         call delete(nrg%m)
         call delete(nrg%MD)
         call delete(nrg%PCG_T)
         call delete(nrg%SP)

         write(*,*) 'energy object deleted'
       end subroutine

       subroutine display_energy(nrg,un)
         implicit none
         type(energy),intent(in) :: nrg
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '*************************** ENERGY ***************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Pe = ',nrg%SP%DP%Pe
         write(un,*) 'Ec,Ha = ',nrg%SP%DP%Ec,nrg%SP%DP%Ha
         write(un,*) 't,dt = ',nrg%SP%VS%T%TMP%t,nrg%SP%VS%T%TMP%dt
         write(un,*) 'solveTMethod,N_nrg = ',nrg%SP%VS%T%SS%solve_method,nrg%SP%VS%T%ISP%iter_max
         write(un,*) 'tol_nrg = ',nrg%SP%VS%T%ISP%tol_rel
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
         call export(nrg%T   ,str(DT%T%restart),'T_nrg')
         call export(nrg%U_F ,str(DT%T%restart),'U_nrg')
         call export(nrg%k   ,str(DT%T%restart),'k_nrg')
       end subroutine

       subroutine import_energy(nrg,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(dir_tree),intent(in) :: DT
         call import(nrg%T   ,str(DT%T%restart),'T_nrg')
         call import(nrg%U_F ,str(DT%T%restart),'U_nrg')
         call import(nrg%k   ,str(DT%T%restart),'k_nrg')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_energy(nrg,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(dir_tree),intent(in) :: DT
         if (nrg%SP%VS%T%SS%solve) then
           write(*,*) 'export_tec_energy at nrg%SP%VS%T%TMP%n_step = ',nrg%SP%VS%T%TMP%n_step
           call export_processed(nrg%m,nrg%T,str(DT%T%field),'T',0)
           call export_raw(nrg%m,nrg%T,str(DT%T%field),'T',0)
           call export_raw(nrg%m,nrg%Tnm1,str(DT%T%field),'Tnm1',0)
           call export_raw(nrg%m,nrg%F,str(DT%T%field),'F',0)
           call export_raw(nrg%m,nrg%Fnm1,str(DT%T%field),'Fnm1',0)
           call export_raw(nrg%m,nrg%divQ,str(DT%T%field),'divQ',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine export_unsteady_0D_nrg(nrg,TMP)
         implicit none
         type(energy),intent(inout) :: nrg
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: temp
         call compute_Q(nrg%temp_F,nrg%T,nrg%k,nrg%m)
         call compute_divQ(nrg%divQ,nrg%temp_F,nrg%m)
         call assign_ghost_XPeriodic(nrg%divQ,0.0_cp)
         call Ln(temp,nrg%divQ,2.0_cp,nrg%m)
         call export(nrg%Probe_divQ,TMP,temp)
       end subroutine

       subroutine export_unsteady_1D_nrg(nrg,TMP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(nrg%m,nrg%T,str(DT%T%unsteady),'T',1,TMP,nrg%SP%VS%T%unsteady_lines)
       end subroutine

       subroutine export_unsteady_2D_nrg(nrg,TMP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(nrg%m,nrg%T,str(DT%T%unsteady),'T',1,TMP,nrg%SP%VS%T%unsteady_planes)
       end subroutine

       subroutine export_unsteady_3D_nrg(nrg,TMP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(nrg%m,nrg%T,str(DT%T%unsteady),'T',1,TMP,nrg%SP%VS%T%unsteady_field)
       end subroutine

       subroutine solve_energy(nrg,F,Fnm1,TMP,EF,EN,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(SF),intent(in) :: F,Fnm1
         type(time_marching_params),intent(inout) :: TMP
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         select case(nrg%SP%VS%T%SS%solve_method)
         case (1)
           call Euler_time_no_diff_Euler_sources_SF(nrg%T,nrg%temp_CC1,F,TMP)
         case (2)
           call Euler_time_no_diff_AB2_sources_SF(nrg%T,nrg%temp_CC1,F,Fnm1,TMP)
         case (3)
           call Euler_time_AB2_sources_SF(nrg%PCG_T,nrg%T,nrg%Tnm1,F,Fnm1,nrg%m,&
           TMP,nrg%temp_CC1,EF%unsteady_0D%export_now)
         case (4)
           call O2_BDF_time_AB2_sources_SF(nrg%PCG_T,nrg%T,nrg%Tnm1,F,Fnm1,nrg%m,&
           TMP,nrg%temp_CC1,EF%unsteady_0D%export_now)
         case default; stop 'Error: solveTMethod must = 1:4 in energy.f90.'
         end select
         call iterate_step(TMP)


         ! select case (nrg%SP%VS%T%SS%solve_method)
         ! case (1)
         ! call explicitEuler(nrg%T,nrg%U_F,TMP%dt,&
         ! nrg%SP%DP%Pe,nrg%m,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)
         ! case (2) ! O2 time marching
         ! call explicitEuler(nrg%T,nrg%U_F,TMP%dt,&
         ! nrg%SP%DP%Pe,nrg%m,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)
         ! case (3) ! Diffusion implicit
         ! call diffusion_implicit(nrg%PCG_T,nrg%T,nrg%U_F,TMP%dt,&
         ! nrg%SP%DP%Pe,nrg%m,EF%unsteady_0D%export_now,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)
         ! case (4)
         ! if (TMP%n_step.le.1) then
         !   call volumetric_heating_equation(nrg%Q_source,nrg%m,nrg%SP%DP%Pe)
         ! endif
         ! call explicitEuler_with_source(nrg%T,nrg%U_F,TMP%dt,&
         ! nrg%SP%DP%Pe,nrg%m,nrg%Q_source,nrg%temp_CC1,nrg%temp_CC2,nrg%temp_F)
         ! case (5)
         ! if (TMP%n_step.le.1) then
           call volumetric_heating_equation(nrg%Q_source,nrg%m,nrg%SP%DP%Pe)
         ! endif
         ! call CN_with_source(nrg%PCG_T,nrg%T,nrg%U_F,TMP%dt,&
         ! nrg%SP%DP%Pe,nrg%m,nrg%Q_source,EF%unsteady_0D%export_now,nrg%temp_CC1,&
         ! nrg%temp_CC2,nrg%temp_F)
         ! case default; stop 'Erorr: bad solveTMethod value in solve_energy in energy.f90'
         ! end select
         ! call iterate_step(TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (EF%unsteady_0D%export_now) call export_unsteady_0D(nrg,TMP)
         if (EF%unsteady_1D%export_now) call export_unsteady_1D(nrg,TMP,DT)
         if (EF%unsteady_2D%export_now) call export_unsteady_2D(nrg,TMP,DT)
         if (EF%unsteady_3D%export_now) call export_unsteady_3D(nrg,TMP,DT)
         if (EF%info%export_now) call print(nrg)

         if (EF%final_solution%export_now.or.EN%T%this.or.EN%all%this) then
           ! call export(nrg,DT)
           call export_tec(nrg,DT)
         endif
       end subroutine

       end module