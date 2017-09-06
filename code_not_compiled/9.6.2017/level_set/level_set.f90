       module level_set_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use SF_mod
       use VF_mod
       use mesh_extend_mod
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

       use level_set_solver_mod
       use init_phi_LS_BCs_mod
       use init_phi_LS_field_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use ops_embedExtract_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use boundary_conditions_mod
       use apply_BCs_mod

       use probe_extend_mod
       use probe_mod
       use ops_norms_mod

       implicit none

       private
       public :: level_set
       public :: init,delete,display,print,export,import ! Essentials
       public :: solve,export_tec

       type level_set
         ! --- Vector fields ---
         type(SF) :: phi_LS,temp_CC1   ! CC data
         type(VF) :: temp_F,k              ! Face data
         type(VF) :: U_F                   ! Face data
         ! --- Scalar fields ---

         type(mesh) :: m
         type(mesh_domain) :: MD

         type(sim_params) :: SP
       end type

       interface init;               module procedure init_level_set;             end interface
       interface delete;             module procedure delete_level_set;           end interface
       interface display;            module procedure display_level_set;          end interface
       interface print;              module procedure print_level_set;            end interface
       interface export;             module procedure export_level_set;           end interface
       interface import;             module procedure import_level_set;           end interface

       interface solve;              module procedure solve_level_set;            end interface
       interface export_tec;         module procedure export_tec_level_set;       end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_level_set(LS,m,SP,MD,DT)
         implicit none
         type(level_set),intent(inout) :: LS
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         write(*,*) 'Initializing level_set:'
         call init(LS%SP,SP)

         call init(LS%m,m)
         call init(LS%MD,MD)

         call init_CC(LS%phi_LS,m,0.0_cp)
         call init_Face(LS%temp_F,m,0.0_cp)

         call init_Face(LS%k,m,0.0_cp)
         call init_Face(LS%U_F,m,0.0_cp)
         call init_CC(LS%temp_CC1,m,0.0_cp)
         write(*,*) '     Fields allocated'

         ! --- Initialize Fields ---
         call init_phi_LS_BCs(LS%phi_LS,m,LS%SP)
         if (LS%SP%VS%phi_LS%SS%solve) call print_BCs(LS%phi_LS,'phi_LS')
         if (LS%SP%VS%phi_LS%SS%solve) call export_BCs(LS%phi_LS,str(DT%phi_LS%BCs),'phi_LS')
         write(*,*) '     BCs initialized'

         call init_phi_LS_field(LS%phi_LS,m,LS%SP,str(DT%phi_LS%field))
         write(*,*) '     phi_LS-field initialized'

         call apply_BCs(LS%phi_LS)
         write(*,*) '     BCs applied'

         temp_unit = new_and_open(str(DT%params),'info_nrg')
         call display(LS,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_nrg')

         write(*,*) '     probes initialized'
         write(*,*) '     Finished'
       end subroutine

       subroutine delete_level_set(LS)
         implicit none
         type(level_set),intent(inout) :: LS

         call delete(LS%phi_LS)
         call delete(LS%temp_F)
         call delete(LS%k)
         call delete(LS%temp_CC1)

         call delete(LS%U_F)

         call delete(LS%m)
         call delete(LS%MD)

         write(*,*) 'level_set object deleted'
       end subroutine

       subroutine display_level_set(LS,un)
         implicit none
         type(level_set),intent(in) :: LS
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '*************************** level_set ***************************'
         write(un,*) '**************************************************************'
         write(un,*) 't,dt = ',LS%SP%VS%phi_LS%TMP%t,LS%SP%VS%phi_LS%TMP%dt
         write(un,*) 'solveTMethod,N_LS = ',LS%SP%VS%phi_LS%SS%solve_method,LS%SP%VS%phi_LS%ISP%iter_max
         write(un,*) 'tol_LS = ',LS%SP%VS%phi_LS%ISP%tol_rel
         call displayPhysicalMinMax(LS%phi_LS,'phi_LS',un)
         write(un,*) ''
         call print(LS%m)
         write(un,*) ''
       end subroutine

       subroutine print_level_set(LS)
         implicit none
         type(level_set),intent(in) :: LS
         call display(LS,6)
       end subroutine

       subroutine export_level_set(LS,DT)
         implicit none
         type(level_set),intent(in) :: LS
         type(dir_tree),intent(in) :: DT
         call export(LS%phi_LS ,str(DT%phi_LS%restart),'phi_LS')
         call export(LS%U_F    ,str(DT%phi_LS%restart),'U_LS')
         call export(LS%k      ,str(DT%phi_LS%restart),'k_LS')
       end subroutine

       subroutine import_level_set(LS,DT)
         implicit none
         type(level_set),intent(inout) :: LS
         type(dir_tree),intent(in) :: DT
         call import(LS%phi_LS ,str(DT%phi_LS%restart),'phi_LS')
         call import(LS%U_F    ,str(DT%phi_LS%restart),'U_LS')
         call import(LS%k      ,str(DT%phi_LS%restart),'k_LS')
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_level_set(LS,DT)
         implicit none
         type(level_set),intent(inout) :: LS
         type(dir_tree),intent(in) :: DT
         if (LS%SP%VS%phi_LS%SS%solve) then
           write(*,*) 'export_tec_level_set at n_step = ',LS%SP%VS%phi_LS%TMP%n_step
           call export_processed(LS%m,LS%phi_LS,str(DT%phi_LS%field),'phi_LS',0)
           call export_raw(LS%m,LS%phi_LS,str(DT%phi_LS%field),'phi_LS',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine solve_level_set(LS,U,TMP,EF,EN,DT)
         implicit none
         type(level_set),intent(inout) :: LS
         type(VF),intent(in) :: U
         type(export_frequency),intent(in) :: EF
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT
         type(time_marching_params),intent(inout) :: TMP

         call embedFace(LS%U_F,U,LS%MD)

         select case (LS%SP%VS%phi_LS%SS%solve_method)
         case (1); call Euler_LS(LS%phi_LS,LS%U_F,TMP%dt,LS%m,LS%temp_CC1,LS%temp_F)

         case default; stop 'Erorr: bad solveTMethod value in solve_level_set in level_set.f90'
         end select
         call iterate_step(TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (EF%info%export_now) call print(LS)
         if (EF%final_solution%export_now.or.EN%phi_LS%this.or.EN%all%this) then
           call export_tec(LS,DT)
         endif
       end subroutine

       end module