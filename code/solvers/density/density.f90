       module density_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod
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
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use preconditioners_mod

       use density_aux_mod
       use density_solver_mod
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
       public :: density
       public :: init,delete,display,print,export,import ! Essentials
       public :: solve,export_tec

       type density
         ! --- Vector fields ---
         type(SF) :: rho,temp_CC1   ! CC data
         type(VF) :: temp_F,k              ! Face data
         type(VF) :: U_F                   ! Face data
         ! --- Scalar fields ---

         type(probe) :: probe_divQ

         type(mesh) :: m
         type(mesh_domain) :: MD

         type(time_marching_params) :: TMP
         type(iter_solver_params) :: ISP_rho

         type(sim_params) :: SP

         integer :: nstep             ! Nth time step
         integer :: N_nrg             ! Maximum number iterations in solving rho (if iterative)
         real(cp) :: dTime            ! Time step
         real(cp) :: time             ! Time
         real(cp) :: tol_nrg             ! Time

         real(cp) :: Re,Pr,Ec,Ha  ! Reynolds, Prandtl, Eckert, Hartmann
       end type

       interface init;               module procedure init_density;             end interface
       interface delete;             module procedure delete_density;           end interface
       interface display;            module procedure display_density;          end interface
       interface print;              module procedure print_density;            end interface
       interface export;             module procedure export_density;           end interface
       interface import;             module procedure import_density;           end interface

       interface solve;              module procedure solve_density;            end interface
       interface export_tec;         module procedure export_tec_density;       end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_density(dens,m,SP,MD,TMP,ISP_rho,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(sim_params),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         type(iter_solver_params),intent(in) :: ISP_rho
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         write(*,*) 'Initializing density:'
         call init(dens%TMP,TMP)
         call init(dens%ISP_rho,ISP_rho)
         call init(dens%SP,SP)

         call init(dens%m,m)
         call init(dens%MD,MD)

         call init_CC(dens%rho,m,0.0_cp)
         call init_Face(dens%temp_F,m,0.0_cp)

         call init_Face(dens%k,m,0.0_cp)
         call init_Face(dens%U_F,m,0.0_cp)
         call init_CC(dens%temp_CC1,m,0.0_cp)

         ! --- Scalar Fields ---
         call init_CC(vol_CC,m)
         call volume(vol_CC,m)
         if (dens%SP%export_cell_volume) then
           call export_raw(dens%m,vol_CC,str(DT%meshes),'dens_cell_volume',0)
         endif
         call delete(vol_CC)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_TBCs(dens%rho,dens%m)
         if (dens%SP%solvedensity) call print_BCs(dens%rho,'rho')
         if (dens%SP%solvedensity) call export_BCs(dens%rho,str(DT%T_BCs),'rho')
         write(*,*) '     BCs initialized'

         call initTfield(dens%rho,m,dens%SP%restartT,str(DT%rho))
         write(*,*) '     rho-field initialized'

         call apply_BCs(dens%rho,m)
         write(*,*) '     BCs applied'

         call init(dens%probe_divQ,str(DT%rho),'probe_divQ',dens%SP%restartT)

         temp_unit = new_and_open(str(DT%params),'info_nrg')
         call print(dens)
         call close_and_message(temp_unit,str(DT%params),'info_nrg')

         write(*,*) '     probes initialized'
         write(*,*) '     Finished'
       end subroutine

       subroutine delete_density(dens)
         implicit none
         type(density),intent(inout) :: dens

         call delete(dens%rho)
         call delete(dens%temp_F)
         call delete(dens%k)
         call delete(dens%temp_CC1)

         call delete(dens%U_F)

         call delete(dens%probe_divQ)
         call delete(dens%m)
         call delete(dens%MD)

         call delete(dens%TMP)
         call delete(dens%ISP_rho)

         write(*,*) 'density object deleted'
       end subroutine

       subroutine display_density(dens,un)
         implicit none
         type(density),intent(in) :: dens
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '*************************** density ***************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Re,Pr = ',dens%Re,dens%Pr
         write(un,*) 'Ec,Ha = ',dens%Ec,dens%Ha
         write(un,*) 't,dt = ',dens%TMP%t,dens%TMP%dt
         write(un,*) 'solveTMethod,N_nrg = ',dens%SP%solveTMethod,dens%ISP_rho%iter_max
         write(un,*) 'tol_dens = ',dens%ISP_rho%tol_rel
         call displayPhysicalMinMax(dens%rho,'rho',un)
         write(un,*) ''
         call print(dens%m)
         write(un,*) ''
       end subroutine

       subroutine print_density(dens)
         implicit none
         type(density),intent(in) :: dens
         call display(dens,6)
       end subroutine

       subroutine export_density(dens,DT)
         implicit none
         type(density),intent(in) :: dens
         type(dir_tree),intent(in) :: DT
         integer :: un
         call export(dens%TMP)
         call export(dens%ISP_rho)

         un = new_and_open(str(DT%restart),'nrg_restart')
         write(un,*) dens%Re,dens%Pr,dens%Ha,dens%Ec
         call close_and_message(un,str(DT%restart),'nrg_restart')

         call export(dens%rho ,str(DT%restart),'rho_density')
         call export(dens%U_F ,str(DT%restart),'U_density')
         call export(dens%k   ,str(DT%restart),'k_density')
       end subroutine

       subroutine import_density(dens,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(dir_tree),intent(in) :: DT
         integer :: un
         call import(dens%TMP)
         call import(dens%ISP_rho)

         un = open_to_read(str(DT%restart),'nrg_restart')
         read(un,*) dens%Re,dens%Pr,dens%Ha,dens%Ec
         call close_and_message(un,str(DT%restart),'nrg_restart')

         call import(dens%rho ,str(DT%restart),'rho_density')
         call import(dens%U_F ,str(DT%restart),'U_density')
         call import(dens%k   ,str(DT%restart),'k_density')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_density(dens,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(dir_tree),intent(in) :: DT
         if (dens%SP%solvedensity) then
           write(*,*) 'export_tec_density at dens%TMP%n_step = ',dens%TMP%n_step
           call export_processed(dens%m,dens%rho,str(DT%rho),'rho',0)
           call export_raw(dens%m,dens%rho,str(DT%rho),'rho',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine solve_density(dens,U,PE,EN,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(VF),intent(in) :: U
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         call embed_velocity_F(dens%U_F,U,dens%MD)

         select case (dens%SP%solveTMethod)
         case (1); call Euler(dens%rho,dens%U_F,dens%TMP%dt,dens%m,dens%temp_CC1,dens%temp_F)

         case default; stop 'Erorr: bad solveTMethod value in solve_density in density.f90'
         end select
         call iterate_step(dens%TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (PE%info) call print(dens)
         if (PE%solution.or.EN%T%this.or.EN%all%this) then
           call export(dens,DT)
           call export_tec(dens,DT)
         endif
       end subroutine

       end module