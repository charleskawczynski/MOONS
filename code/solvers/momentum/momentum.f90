       module momentum_mod
       use current_precision_mod

       use sim_params_mod
       use boundary_conditions_mod
       use block_mod
       use mesh_block_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use mesh_domain_mod
       use string_mod
       use path_mod
       use dir_tree_mod

       use mesh_stencils_mod
       use momentum_solver_mod
       use momentum_aux_mod
       use init_P_BCs_mod
       use init_Ustar_field_mod
       use init_U_BCs_mod
       use init_U_Field_mod
       use init_P_Field_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use face_SD_mod
       use datatype_conversion_mod

       use IO_tools_mod
       use IO_export_mod
       use IO_import_mod
       use export_raw_processed_mod
       use export_raw_processed_symmetry_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod

       use norms_mod
       use ops_norms_mod
       use ops_discrete_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_embedExtract_mod

       use apply_BCs_mod
       use boundary_conditions_mod
       use clean_divergence_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use probe_mod
       use ops_norms_mod

       use PCG_mod
       use preconditioners_mod
       use GS_Poisson_mod
       use E_K_Budget_mod


       implicit none
       private

       public :: momentum
       public :: init,delete,display,print,export,import ! Essentials

       public :: solve,export_tec,compute_export_E_K_Budget
       public :: prolongate

       type momentum
         logical :: suppress_warning
         ! Tensor fields
         type(TF) :: U_E
         ! Vector fields
         type(VF) :: U,Ustar,Unm1
         type(VF) :: U_CC
         type(VF) :: temp_F1,temp_F2
         type(VF) :: temp_E,temp_CC_VF
         ! Scalar fields
         type(SF) :: p,divU,temp_CC

         type(GS_Poisson_SF) :: GS_p

         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U

         type(mesh) :: m
         type(sim_params) :: SP
         type(probe) :: probe_KE,probe_KE_2C,probe_divU
       end type

       interface init;                 module procedure init_mom;                   end interface
       interface delete;               module procedure delete_mom;                 end interface
       interface prolongate;           module procedure prolongate_mom;             end interface
       interface display;              module procedure display_momentum;           end interface
       interface print;                module procedure print_momentum;             end interface
       interface export;               module procedure export_momentum;            end interface
       interface import;               module procedure import_momentum;            end interface

       interface export_tec;           module procedure export_tec_momentum;        end interface
       interface export_tec;           module procedure export_tec_momentum_no_ext; end interface
       interface solve;                module procedure solve_momentum;             end interface
       interface init_matrix_based_ops;module procedure init_matrix_based_ops_mom;  end interface

       interface export_transient1;    module procedure export_transient1_mom;      end interface
       interface export_transient2;    module procedure export_transient2_mom;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_mom(mom,m,SP,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         write(*,*) 'Initializing momentum:'

         call init(mom%SP,SP)
         call init(mom%m,m)

         call init_Edge(mom%U_E       ,m,0.0_cp)
         call init_Face(mom%U         ,m,0.0_cp)
         call init_Face(mom%Unm1      ,m,0.0_cp)
         call init_Face(mom%temp_F1   ,m,0.0_cp)
         call init_Face(mom%temp_F2   ,m,0.0_cp)
         call init_Edge(mom%temp_E    ,m,0.0_cp)
         call init_CC(mom%p           ,m,0.0_cp)
         call init_CC(mom%divU        ,m,0.0_cp)
         call init_CC(mom%U_CC        ,m,0.0_cp)
         call init_CC(mom%temp_CC     ,m,0.0_cp)
         call init_CC(mom%temp_CC_VF  ,m,0.0_cp)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         write(*,*) 'about to define U_BCs'
         call init_U_BCs(mom%U,m,mom%SP)
         write(*,*) 'U_BCs defined'
         call init_P_BCs(mom%p,m,mom%SP)
         write(*,*) '     BCs initialized'
         if (mom%SP%VS%U%SS%solve) call print_BCs(mom%U,'U')
         if (mom%SP%VS%U%SS%solve) call export_BCs(mom%U,str(DT%U%BCs),'U')
         if (mom%SP%VS%U%SS%solve) call print_BCs(mom%p,'p')
         if (mom%SP%VS%U%SS%solve) call export_BCs(mom%p,str(DT%p%BCs),'p')

         call init_U_field(mom%U,m,mom%SP,str(DT%U%field))
         call init_P_field(mom%p,m,mom%SP,str(DT%p%field))
         call assign(mom%Unm1,mom%U)
         write(*,*) '     Field initialized'

         call apply_BCs(mom%p)
         write(*,*) '     P BCs applied'
         call apply_BCs(mom%U)
         write(*,*) '     U BCs applied'

         call init(mom%Ustar,mom%U)
         call set_prescribed_BCs(mom%Ustar)
         call init_Ustar_field(mom%Ustar,mom%m,mom%U,mom%SP,str(DT%U%field))
         write(*,*) '     Intermediate field initialized'

         write(*,*) '     about to assemble Laplacian matrices'
         if (mom%SP%matrix_based) call init_matrix_based_ops(mom)

         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)
         write(*,*) '     Interpolated fields initialized'

         call init(mom%probe_divU,str(DT%U%residual),'probe_divU',mom%SP%VS%U%SS%restart,SP%DMR,.true.)
         call init(mom%probe_KE,str(DT%U%energy),'KE',mom%SP%VS%U%SS%restart,SP%DMR,.false.)
         if (m%MP%plane_any) then
          call init(mom%probe_KE_2C,str(DT%U%energy),'KE_2C',mom%SP%VS%U%SS%restart,SP%DMR,.false.)
         endif
         write(*,*) '     momentum probes initialized'

         ! Initialize interior solvers
         call init(mom%GS_p,mom%p,mom%m,mom%SP%VS%P%ISP,str(DT%p%residual),'p')
         write(*,*) '     GS solver initialized for p'

         call init(mom%PCG_U,mom_diffusion,mom_diffusion_explicit,prec_mom_VF,mom%m,&
         mom%SP%VS%U%ISP,mom%SP%VS%U%MFP,mom%Ustar,mom%temp_E,str(DT%U%residual),'U',.false.,.false.)
         write(*,*) '     PCG solver initialized for U'

         call init(mom%PCG_P,Lap_uniform_SF,Lap_uniform_SF_explicit,prec_lap_SF,mom%m,&
         mom%SP%VS%P%ISP,mom%SP%VS%P%MFP,mom%p,mom%temp_F1,str(DT%p%residual),'p',.false.,.false.)
         write(*,*) '     PCG solver initialized for p'

         temp_unit = new_and_open(str(DT%params),'info_mom')
         call display(mom,temp_unit)
         call close_and_message(temp_unit,str(DT%params),'info_mom')
         ! if (restart_all) call import(mom,DT)
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine delete_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U)
         call delete(mom%U_E)
         call delete(mom%Unm1)
         call delete(mom%Ustar)
         call delete(mom%temp_F1)
         call delete(mom%temp_F2)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%temp_CC_VF)
         call delete(mom%divU)
         call delete(mom%U_CC)
         call delete(mom%probe_divU)
         call delete(mom%probe_KE)
         call delete(mom%probe_KE_2C)
         call delete(mom%temp_E)
         call delete(mom%m)
         call delete(mom%PCG_P)
         call delete(mom%PCG_U)
         call delete(mom%GS_p)
         call delete(mom%SP)
         write(*,*) 'Momentum object deleted'
       end subroutine

       subroutine display_momentum(mom,un)
         implicit none
         type(momentum),intent(in) :: mom
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MOMENTUM **************************'
         write(un,*) '**************************************************************'
         write(un,*) 'Re,Ha = ',mom%SP%DP%Re,mom%SP%DP%Ha
         write(un,*) 'Gr,Fr = ',mom%SP%DP%Gr,mom%SP%DP%Fr
         write(un,*) 't,dt = ',mom%SP%VS%U%TMP%t,mom%SP%VS%U%TMP%dt
         write(un,*) 'solveUMethod,N_mom,N_PPE = ',mom%SP%VS%U%SS%solve_method,&
         mom%SP%VS%U%ISP%iter_max,mom%SP%VS%P%ISP%iter_max
         write(un,*) 'tol_mom,tol_PPE = ',mom%SP%VS%U%ISP%tol_rel,mom%SP%VS%P%ISP%tol_rel
         write(un,*) 'nstep,KE = ',mom%SP%VS%U%TMP%n_step,get_data(mom%probe_KE)
         ! call displayPhysicalMinMax(mom%U,'U',un)
         call displayPhysicalMinMax(mom%divU,'divU',un)
         write(un,*) 'CFL = ',CFL_number(mom%U_CC,mom%m,mom%SP%VS%U%TMP%dt)
         write(un,*) ''
         call display(mom%m,un)
       end subroutine

       subroutine print_momentum(mom)
         implicit none
         type(momentum),intent(in) :: mom
         call display(mom,6)
       end subroutine

       subroutine export_momentum(mom,DT)
         implicit none
         type(momentum),intent(in) :: mom
         type(dir_tree),intent(in) :: DT
         if (.not.mom%SP%EL%export_soln_only) then
           call export(mom%U     ,str(DT%U%restart),'U')
           call export(mom%Ustar ,str(DT%U%restart),'Ustar')
           call export(mom%Unm1  ,str(DT%U%restart),'Unm1')
           call export(mom%p     ,str(DT%p%restart),'p')
         endif
       end subroutine

       subroutine import_momentum(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         if (.not.mom%SP%EL%export_soln_only) then
           call import(mom%U     ,str(DT%U%restart),'U')
           call import(mom%Ustar ,str(DT%U%restart),'Ustar')
           call import(mom%Unm1  ,str(DT%U%restart),'Unm1')
           call import(mom%p     ,str(DT%p%restart),'p')
         endif
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine export_tec_momentum(mom,DT,F,Fnm1)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F,Fnm1
         type(dir_tree),intent(in) :: DT
         if (.not.mom%SP%EL%export_soln_only) then
         if (mom%SP%VS%B%SS%solve.or.mom%SP%VS%T%SS%solve) then
           call export_raw(mom%m,F,str(DT%U%field),'F_external',0)
           call export_raw(mom%m,Fnm1,str(DT%U%field),'Fnm1_external',0)
         endif
         endif
         call export_tec_momentum_no_ext(mom,DT)
       end subroutine
       subroutine export_tec_momentum_no_ext(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         if (mom%SP%VS%U%SS%restart.and.(.not.mom%SP%VS%U%SS%solve)) then
           ! This preserves the initial data
         else
           write(*,*) 'export_tec_momentum_no_ext at n_step = ',mom%SP%VS%U%TMP%n_step
           call export_processed(mom%m,mom%U,str(DT%U%field),'U',1)

           if (.not.mom%SP%EL%export_soln_only) then
             call export_processed(mom%m,mom%p,str(DT%p%field),'p',1)
             call export_raw(mom%m,mom%U,str(DT%U%field),'U',0)
             call export_raw(mom%m,mom%p,str(DT%p%field),'p',0)
             call export_raw(mom%m,mom%Ustar,str(DT%U%field),'Ustar',1)
             if (mom%SP%EL%export_symmetric) then
              call export_processed(mom%m,mom%U,str(DT%U%field),'U',1,mom%SP%MP)
              call export_processed(mom%m,mom%p,str(DT%p%field),'p',1,mom%SP%MP)
             endif
             call export_raw(mom%m,mom%divU,str(DT%U%field),'divU',1)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine export_transient1_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp) :: temp,scale
         scale = mom%SP%DP%KE_scale

         call compute_TKE(temp,mom%U_CC,mom%m,scale)
         call export(mom%probe_KE,mom%SP%VS%U%TMP,temp)

         if (mom%m%MP%plane_any) then
         if (mom%m%MP%plane(1)) call compute_TKE_2C(temp,mom%U_CC%y,mom%U_CC%z,mom%m,scale,mom%temp_CC)
         if (mom%m%MP%plane(2)) call compute_TKE_2C(temp,mom%U_CC%x,mom%U_CC%z,mom%m,scale,mom%temp_CC)
         if (mom%m%MP%plane(3)) call compute_TKE_2C(temp,mom%U_CC%x,mom%U_CC%y,mom%m,scale,mom%temp_CC)
         call export(mom%probe_KE_2C,mom%SP%VS%U%TMP,temp)
         endif
         call div(mom%divU,mom%U,mom%m)
         call Ln(temp,mom%divU,2.0_cp,mom%m)
         call export(mom%probe_divU,mom%SP%VS%U%TMP,temp)
       end subroutine

       subroutine export_transient2_mom(mom,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(dir_tree),intent(in) :: DT
         call export_processed(mom%m,mom%U,str(DT%U%transient),'U',1,mom%SP%VS%U%TMP)
         call export_processed(mom%m,mom%p,str(DT%P%transient),'p',1,mom%SP%VS%U%TMP)
         ! call export_processed(mom%m,mom%U,str(DT%U%transient),'U',1,mom%SP%VS%U%TMP,3,24)
         ! call export_processed(mom%m,mom%p,str(DT%U%transient),'p',1,mom%SP%VS%U%TMP,3,24)
       end subroutine

       subroutine init_matrix_based_ops_mom(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         real(cp),dimension(2) :: diffusion_treatment
         call init_Laplacian_SF(mom%m) ! Must come before PPE solver init
         call init_Laplacian_VF(mom%m) ! for lap(U) in momentum
         ! diffusion_treatment = (/-mom%SP%VS%U%TMP%dt/mom%SP%DP%Re,1.0_cp/) ! diffusion explicit
         ! diffusion_treatment = (/mom%SP%VS%U%TMP%dt/mom%SP%DP%Re,1.0_cp/)    ! diffusion explicit
         diffusion_treatment = (/mom%SP%VS%U%TMP%dt/mom%SP%DP%Re,1.0_cp/)    ! diffusion explicit
         ! diffusion_treatment = (/1.0_cp,0.0_cp/)             ! no treatment (requires multiplication by dt/Re)
         ! call modify_Laplacian_VF(mom%m,diffusion_treatment(1),diffusion_treatment(2))
         call multiply_Laplacian_VF(mom%m,diffusion_treatment(1))
         call add_Laplacian_VF(mom%m,diffusion_treatment(2))
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_momentum(mom,F,Fnm1,PE,EN,DT)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F,Fnm1
         type(print_export),intent(in) :: PE
         type(export_now),intent(in) :: EN
         type(dir_tree),intent(in) :: DT

         select case(mom%SP%VS%U%SS%solve_method)
         case (1)
           call Euler_PCG_Donor(mom%PCG_P,mom%U,mom%Ustar,mom%U_E,mom%p,F,mom%m,&
           mom%SP%DP%Re,mom%SP%VS%U%TMP%dt,mom%temp_F1,mom%temp_F2,mom%temp_CC,mom%temp_E,&
           PE%transient_0D)
         case (2)
           call Euler_GS_Donor(mom%GS_p,mom%U,mom%Ustar,mom%U_E,mom%p,F,mom%m,&
           mom%SP%DP%Re,mom%SP%VS%U%TMP%dt,mom%temp_F1,mom%temp_F2,mom%temp_CC,mom%temp_E,&
           PE%transient_0D)
         case (3)
           call CN_AB2_PPE_PCG_mom_PCG(mom%PCG_U,mom%PCG_p,mom%U,mom%Ustar,mom%Unm1,&
           mom%U_E,mom%p,F,Fnm1,mom%m,mom%SP%VS%U%MFP,mom%SP%VS%U%TMP%dt,mom%temp_F1,&
           mom%temp_F2,mom%temp_CC,mom%temp_CC_VF,mom%temp_E,PE%transient_0D)
         case (4)
           call Euler_Donor_no_PPE(mom%U,mom%U_E,F,mom%m,mom%SP%DP%Re,mom%SP%VS%U%TMP%dt,&
           mom%temp_F1,mom%temp_F2,mom%temp_CC,mom%temp_E)
         case default; stop 'Error: solveUMethod must = 1:4 in momentum.f90.'
         end select
         call iterate_step(mom%SP%VS%U%TMP)

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)

         ! U at cell edge is needed for advection term at next time step
         ! and in induction solver. Neither case requires the diagonal.
         call face2edge_no_diag(mom%U_E,mom%U,mom%m)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (PE%transient_0D) call export_transient1(mom)
         if (PE%transient_2D) call export_transient2(mom,DT)

         ! if (PE%transient_2D) call export_processed_transient_3C(mom%m,mom%U,str(DT%U%transient),'U',1,mom%SP%VS%U%TMP)
         ! if (PE%transient_2D) call export_processed_transient_2C(mom%m,mom%U,str(DT%U%transient),'U',1,mom%SP%VS%U%TMP)

         if (PE%info) call print(mom)

         if (PE%solution.or.EN%U%this.or.EN%all%this) then
           ! call curl(mom%temp_E,mom%U,m)
           ! call export(mom,DT)
           call export_tec(mom,DT,F,Fnm1)
         endif
       end subroutine

       subroutine compute_export_E_K_Budget(mom,B,B0,J,MD_fluid,DT)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(momentum),intent(inout) :: mom
         type(mesh_domain),intent(in) :: MD_fluid
         type(VF),intent(in) :: B,B0,J
         call E_K_Budget_wrapper(DT,mom%U,mom%Unm1,&
         B,B0,J,mom%p,mom%m,mom%SP%VS%U%TMP,mom%SP%DP,mom%SP%MP,MD_fluid)
       end subroutine

       subroutine prolongate_mom(mom,F,Fnm1,DT,RM,SS_reached)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(inout) :: F,Fnm1
         type(dir_tree),intent(in) :: DT
         type(refine_mesh),intent(in) :: RM
         logical,intent(in) :: SS_reached
         integer,dimension(3) :: dir
         type(iter_solver_params) :: temp
         logical :: clean_exact
         integer :: i
         write(*,*) '#################### Prolongating momentum solver ####################'
         call export_processed(mom%m,mom%U,str(DT%U%field),'U_SS_'//str(RM%level_last),1)

         dir = get_dir(RM)
         if (SS_reached) dir = (/1,2,3/)
         do i=1,3
           if (dir(i).ne.0) then
             write(*,*) 'Prolongating momentum solver along direction ',i
             call prolongate(mom%m,dir(i))
             call prolongate(mom%U_E,mom%m,dir(i))
             call prolongate(F,mom%m,dir(i))
             call prolongate(Fnm1,mom%m,dir(i))
             call prolongate(mom%U,mom%m,dir(i))
             call prolongate(mom%Ustar,mom%m,dir(i))
             call prolongate(mom%Unm1,mom%m,dir(i))
             call prolongate(mom%temp_F1,mom%m,dir(i))
             call prolongate(mom%temp_F2,mom%m,dir(i))
             call prolongate(mom%temp_E,mom%m,dir(i))
             call prolongate(mom%p,mom%m,dir(i))
             call prolongate(mom%divU,mom%m,dir(i))
             call prolongate(mom%U_CC,mom%m,dir(i))
             call prolongate(mom%temp_CC,mom%m,dir(i))
             call prolongate(mom%temp_CC_VF,mom%m,dir(i))
             call prolongate(mom%SP%VS%U%MFP,mom%SP%DMR%dt_reduction_factor)
             call prolongate(mom%PCG_P,mom%m,mom%temp_F1,mom%SP%VS%P%MFP,dir(i))
             call prolongate(mom%PCG_U,mom%m,mom%temp_E,mom%SP%VS%U%MFP,dir(i))
           endif
         enddo
         call init_U_BCs(mom%U,mom%m,mom%SP) ! Needed (better) if U_BCs is a distribution
         write(*,*) 'Finished momentum solver prolongation'
         if (mom%SP%matrix_based) call init_matrix_based_ops(mom)
         call apply_BCs(mom%U)
         call export_processed(mom%m,mom%U,str(DT%U%field),'U_prolongated_'//str(RM%level),1)

         clean_exact = .false.
         call assign(mom%Ustar,mom%U)
         if (clean_exact) then ! Doesn't seem to help any for hydro flows
           call init(temp,mom%PCG_P%ISP)
           call init(mom%PCG_P%ISP,solve_exact(str(DT%U%residual)))
           call clean_div(mom%PCG_P,mom%U,mom%Ustar,mom%p,mom%m,mom%temp_F1,mom%temp_CC,.true.)
           call init(mom%PCG_P%ISP,temp)
           call delete(temp)
         else
           call boost(mom%PCG_P%ISP)
           call clean_div(mom%PCG_P,mom%U,mom%Ustar,mom%p,mom%m,mom%temp_F1,mom%temp_CC,.true.)
           call reset(mom%PCG_P%ISP)
         endif

         call export_processed(mom%m,mom%U,str(DT%U%field),'U_cleaned_'//str(RM%level),1)
         write(*,*) '#############################################################'
       end subroutine

       end module