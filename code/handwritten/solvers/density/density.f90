       module density_mod
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
       use path_extend_mod
       use path_extend_mod
       use export_frequency_mod
       use export_now_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use preconditioners_mod

       use density_solver_mod
       use init_rho_BCs_mod
       use init_rho_field_mod

       use iter_solver_params_mod
       use time_marching_params_mod
       use time_marching_params_extend_mod

       use ops_embedExtract_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use boundary_conditions_mod
       use apply_BCs_mod

       use probe_mod
       use probe_extend_mod
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

         type(mesh) :: m
         type(mesh_domain) :: MD
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

       subroutine init_density(dens,m,SP,MD,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         write(*,*) 'Initializing density:'

         call init(dens%m,m)
         call init(dens%MD,MD)

         call init_CC(dens%rho,m,0.0_cp)
         call init_Face(dens%temp_F,m,0.0_cp)

         call init_Face(dens%k,m,0.0_cp)
         call init_Face(dens%U_F,m,0.0_cp)
         call init_CC(dens%temp_CC1,m,0.0_cp)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_rho_BCs(dens%rho,m,SP)
         if (SP%VS%rho%SS%solve) call print_BCs(dens%rho,'rho')
         if (SP%VS%rho%SS%solve) call export_BCs(dens%rho,str(DT%rho%BCs),'rho')
         write(*,*) '     BCs initialized'

         call init_rho_field(dens%rho,m,SP,str(DT%rho%field))
         write(*,*) '     rho-field initialized'

         call apply_BCs(dens%rho)
         write(*,*) '     BCs applied'

         temp_unit = new_and_open(str(DT%params),'info_nrg')
         call display(dens,SP,temp_unit)
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

         call delete(dens%m)
         call delete(dens%MD)

         write(*,*) 'density object deleted'
       end subroutine

       subroutine display_density(dens,SP,un)
         implicit none
         type(density),intent(in) :: dens
         type(sim_params),intent(in) :: SP
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '*************************** density ***************************'
         write(un,*) '**************************************************************'
         write(un,*) 't,dt = ',SP%VS%rho%TMP%t,SP%VS%rho%TMP%dt
         write(un,*) 'solveTMethod,N_dens = ',SP%VS%rho%SS%solve_method,SP%VS%rho%ISP%iter_max
         write(un,*) 'tol_dens = ',SP%VS%rho%ISP%tol_rel
         call displayPhysicalMinMax(dens%rho,'rho',un)
         write(un,*) ''
         call print(dens%m)
         write(un,*) ''
       end subroutine

       subroutine print_density(dens,SP)
         implicit none
         type(density),intent(in) :: dens
         type(sim_params),intent(in) :: SP
         call display(dens,SP,6)
       end subroutine

       subroutine export_density(dens,DT)
         implicit none
         type(density),intent(in) :: dens
         type(dir_tree),intent(in) :: DT
         call export(dens%rho ,str(DT%rho%restart),'rho_density')
         call export(dens%U_F ,str(DT%rho%restart),'U_density')
         call export(dens%k   ,str(DT%rho%restart),'k_density')
       end subroutine

       subroutine import_density(dens,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(dir_tree),intent(in) :: DT
         call import(dens%rho ,str(DT%rho%restart),'rho_density')
         call import(dens%U_F ,str(DT%rho%restart),'U_density')
         call import(dens%k   ,str(DT%rho%restart),'k_density')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_density(dens,SP,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(dir_tree),intent(in) :: DT
         type(sim_params),intent(in) :: SP
         if (SP%VS%rho%SS%solve) then
           write(*,*) 'export_tec_density at n_step = ',SP%VS%rho%TMP%n_step
           call export_processed(dens%m,dens%rho,str(DT%rho%field),'rho',0)
           call export_raw(dens%m,dens%rho,str(DT%rho%field),'rho',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine solve_density(dens,SP,U,TMP,EF,EN,DT)
         implicit none
         type(density),intent(inout) :: dens
         type(sim_params),intent(in) :: SP
         type(VF),intent(in) :: U
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         type(time_marching_params),intent(inout) :: TMP

         call embedFace(dens%U_F,U,dens%MD)

         select case (SP%VS%rho%SS%solve_method)
         case (1); call Euler(dens%rho,dens%U_F,TMP%dt,dens%m,dens%temp_CC1,dens%temp_F)

         case default; stop 'Erorr: bad solveTMethod value in solve_density in density.f90'
         end select
         call iterate_step(TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (EF%info%export_now) call print(dens,SP)
         if (EF%final_solution%export_now.or.EN%rho%this.or.EN%all%this) then
           call export_tec(dens,SP,DT)
         endif
       end subroutine

       end module