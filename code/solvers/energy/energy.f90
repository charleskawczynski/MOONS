       module energy_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use import_raw_mod
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
         type(TF) :: temp_F_TF             ! Face data
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
         call init_Face(nrg%temp_F_TF,m,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(nrg%divQ,m)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_T_BCs(nrg%T,nrg%m,SP)
         if (SP%VS%T%SS%solve) call print_BCs(nrg%T,'T')
         if (SP%VS%T%SS%solve) call export_BCs(nrg%T,str(DT%T%BCs),'T')
         write(*,*) '     BCs initialized'

         call init_T_field(nrg%T,m,SP,str(DT%T%field))
         call init_gravity_field(nrg%gravity,m,SP,str(DT%T%field))
         write(*,*) '     T-field initialized'

         call apply_BCs(nrg%T)
         call assign(nrg%Tnm1,nrg%T)
         write(*,*) '     BCs applied'

         call init_CC(k_cc,m,0.0_cp)
         call initK(k_cc,nrg%m,nrg%MD)
         call cellCenter2Face(nrg%k,k_cc,m)
         call delete(k_cc)
         write(*,*) '     Materials initialized'

         call init(nrg%Probe_divQ,str(DT%T%residual),'probe_divQ',SP%VS%T%SS%restart,.true.,SP%VS%T%TMP)

         call init(nrg%PCG_T,nrg_diffusion,nrg_diffusion_explicit,prec_lap_SF,nrg%m,&
         SP%VS%T%ISP,SP%VS%T%MFP,nrg%T,nrg%temp_F_TF,str(DT%T%residual),'T',.false.,.false.)

         temp_unit = new_and_open(str(DT%params),'info_nrg')
         call display(nrg,SP,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_nrg')

         if (SP%VS%T%SS%restart) call import(nrg,SP,DT)
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
         call delete(nrg%temp_F_TF)

         call delete(nrg%divQ)

         call delete(nrg%Probe_divQ)
         call delete(nrg%m)
         call delete(nrg%MD)
         call delete(nrg%PCG_T)

         write(*,*) 'energy object deleted'
       end subroutine

       subroutine display_energy(nrg,SP,un)
         implicit none
         type(energy),intent(in) :: nrg
         type(sim_params),intent(in) :: SP
         integer,intent(in) :: un
         if (SP%export_heavy) then
           write(un,*) '**************************************************************'
           write(un,*) '*************************** ENERGY ***************************'
           write(un,*) '**************************************************************'
           write(un,*) 'Pe = ',SP%DP%Pe
           write(un,*) 'Ec,Ha = ',SP%DP%Ec,SP%DP%Ha
           write(un,*) 't,dt = ',SP%VS%T%TMP%t,SP%VS%T%TMP%dt
           write(un,*) 'solveTMethod,N_nrg = ',SP%VS%T%SS%solve_method,SP%VS%T%ISP%iter_max
           write(un,*) 'tol_nrg = ',SP%VS%T%ISP%tol_rel
           call displayPhysicalMinMax(nrg%T,'T',un)
           call displayPhysicalMinMax(nrg%divQ,'divQ',un)
           write(un,*) ''
           call display(nrg%m,un)
         endif
       end subroutine

       subroutine print_energy(nrg,SP)
         implicit none
         type(energy),intent(in) :: nrg
         type(sim_params),intent(in) :: SP
         call display(nrg,SP,6)
       end subroutine

       subroutine export_energy(nrg,SP,DT)
         implicit none
         type(energy),intent(in) :: nrg
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'export_energy at n_step = ',SP%VS%T%TMP%n_step
         call export_raw(nrg%m,nrg%T,str(DT%T%field),'T',0)
         call export_raw(nrg%m,nrg%Tnm1,str(DT%T%field),'Tnm1',0)
         call export_raw(nrg%m,nrg%F,str(DT%T%field),'F',0)
         call export_raw(nrg%m,nrg%Fnm1,str(DT%T%field),'Fnm1',0)
       end subroutine

       subroutine import_energy(nrg,SP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         write(*,*) 'import_energy at n_step = ',SP%VS%T%TMP%n_step
         call import_raw(nrg%m,nrg%T,str(DT%T%field),'T',0)
         call import_raw(nrg%m,nrg%Tnm1,str(DT%T%field),'Tnm1',0)
         call import_raw(nrg%m,nrg%F,str(DT%T%field),'F',0)
         call import_raw(nrg%m,nrg%Fnm1,str(DT%T%field),'Fnm1',0)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_energy(nrg,SP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         if (SP%VS%T%SS%solve) then
           write(*,*) 'export_tec_energy at SP%VS%T%TMP%n_step = ',SP%VS%T%TMP%n_step
           call export_processed(nrg%m,nrg%T,str(DT%T%field),'T',0)
           call export_raw(nrg%m,nrg%divQ,str(DT%T%field),'divQ',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine export_unsteady_0D_nrg(nrg,SP,TMP)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: temp,scale
         scale = SP%DP%KE_scale
         call compute_Q(nrg%temp_F,nrg%T,nrg%k,nrg%m)
         call compute_divQ(nrg%divQ,nrg%temp_F,nrg%m)
         call assign_ghost_XPeriodic(nrg%divQ,0.0_cp)
         call Ln(temp,nrg%divQ,2.0_cp,nrg%m)
         temp = temp*scale
         call export(nrg%Probe_divQ,TMP,temp)
       end subroutine

       subroutine export_unsteady_1D_nrg(nrg,SP,TMP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(nrg%m,nrg%T,str(DT%T%unsteady),'T',1,TMP,SP%VS%T%unsteady_lines)
       end subroutine

       subroutine export_unsteady_2D_nrg(nrg,SP,TMP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(nrg%m,nrg%T,str(DT%T%unsteady),'T',1,TMP,SP%VS%T%unsteady_planes)
       end subroutine

       subroutine export_unsteady_3D_nrg(nrg,SP,TMP,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         call export_processed(nrg%m,nrg%T,str(DT%T%unsteady),'T',1,TMP,SP%VS%T%unsteady_field)
       end subroutine

       subroutine solve_energy(nrg,SP,F,Fnm1,TMP,EF,EN,DT)
         implicit none
         type(energy),intent(inout) :: nrg
         type(sim_params),intent(in) :: SP
         type(SF),intent(in) :: F,Fnm1
         type(time_marching_params),intent(inout) :: TMP
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         select case(SP%VS%T%SS%solve_method)
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
         case (5)
           call Euler_time_RK_sources_SF(nrg%PCG_T,nrg%T,nrg%Tnm1,F,Fnm1,nrg%m,&
           TMP,TMP%RKP,nrg%temp_CC1,EF%unsteady_0D%export_now)
         case default; stop 'Error: solveTMethod must = 1:4 in energy.f90.'
         end select
         call iterate_step(TMP)

         call volumetric_heating_equation(nrg%Q_source,nrg%m,SP%DP%Pe)

         ! ********************* POST SOLUTION COMPUTATIONS *********************

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (EF%unsteady_0D%export_now) call export_unsteady_0D(nrg,SP,TMP)
         if (EF%unsteady_1D%export_now) call export_unsteady_1D(nrg,SP,TMP,DT)
         if (EF%unsteady_2D%export_now) call export_unsteady_2D(nrg,SP,TMP,DT)
         if (EF%unsteady_3D%export_now) call export_unsteady_3D(nrg,SP,TMP,DT)
         if (EF%info%export_now) call print(nrg,SP)

         if (EF%final_solution%export_now.or.EN%T%this.or.EN%all%this) then
           call export(nrg,SP,DT)
           call export_tec(nrg,SP,DT)
         endif
       end subroutine

       end module