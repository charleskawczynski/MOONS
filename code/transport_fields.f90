       module induction_mod
       use current_precision_mod
       use sim_params_mod
       use IO_tools_mod
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
       use export_raw_processed_symmetry_mod
       use print_export_mod
       use export_now_mod
       use refine_mesh_mod
       use assign_B0_vs_t_mod

       use mesh_stencils_mod
       use init_BBCs_mod
       use init_phiBCs_mod
       use init_Bfield_mod
       use init_B_interior_mod
       use init_J_interior_mod
       use init_Sigma_mod
       use ops_embedExtract_mod
       use geometric_region_mod
       use divergence_clean_mod

       use iter_solver_params_mod
       use time_marching_params_mod

       use probe_mod
       use ops_norms_mod

       use mesh_domain_mod
       use grid_mod
       use mesh_mod
       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use boundary_conditions_mod
       use apply_BCs_mod
       use ops_advect_mod
       use induction_solver_mod
       use preconditioners_mod
       use PCG_mod
       use Jacobi_mod
       use matrix_free_params_mod
       use matrix_free_operators_mod
       use induction_aux_mod
       use E_M_Budget_mod

       implicit none

       private
       public :: induction
       public :: init,delete,display,print,export,import ! Essentials
       public :: prolongate

       type transport_fields
         type(VF) :: X         !
         type(VF) :: grad_X    !
         type(VF) :: curl_X    !
         type(VF) :: temp_X    !
         type(SF) :: temp_C    ! temp of same size as primary var
         type(SF) :: divX      ! divergence
         type(VF) :: material  ! material properties
       end type

       interface init;       module procedure init_TF;      end interface
       interface delete;     module procedure delete_TF;    end interface
       interface display;    module procedure display_TF;   end interface
       interface print;      module procedure print_TF;     end interface
       interface export;     module procedure export_TF;    end interface
       interface import;     module procedure import_TF;    end interface

       contains

       subroutine init_TF(TF,m)
         implicit none
         type(transport_fields),intent(inout) :: TF
         type(mesh),intent(in) :: m
         call init_Edge(TF%U_E          ,m,0.0_cp)
         call init_Edge(TF%temp_E_TF    ,m,0.0_cp)
         call init_Face(TF%temp_F1_TF   ,m,0.0_cp)
         call init_Face(TF%temp_F2_TF   ,m,0.0_cp)
         call init_Face(TF%B            ,m,0.0_cp)
         call init_Face(TF%B0           ,m,0.0_cp)
         call init_Face(TF%dB0dt        ,m,0.0_cp)
         call init_CC(TF%temp_CC        ,m,0.0_cp)
         call init_Edge(TF%J            ,m,0.0_cp)
         call init_Face(TF%curlUCrossB  ,m,0.0_cp)
         call init_Face(TF%curlE        ,m,0.0_cp)
         call init_Edge(TF%temp_E       ,m,0.0_cp)
         call init_Edge(TF%sigmaInv_edge,m,0.0_cp)
         call init_Face(TF%temp_F1      ,m,0.0_cp)
         call init_Face(TF%temp_F2      ,m,0.0_cp)
         call init_CC(TF%phi            ,m,0.0_cp)
         call init_CC(TF%temp_CC_SF     ,m,0.0_cp)
         call init_CC(TF%divB           ,m,0.0_cp)
         call init_Node(TF%divJ         ,m,0.0_cp)
         call init_CC(TF%sigmaInv_CC    ,m,0.0_cp)
       end subroutine

       subroutine delete_TF(TF)
         implicit none
         type(transport_fields),intent(inout) :: TF
         call delete(TF%U_E)
         call delete(TF%temp_E_TF)
         call delete(TF%temp_F1_TF)
         call delete(TF%temp_F2_TF)
         call delete(TF%B)
         call delete(TF%B0)
         call delete(TF%dB0dt)
         call delete(TF%temp_CC)
         call delete(TF%J)
         call delete(TF%curlUCrossB)
         call delete(TF%curlE)
         call delete(TF%temp_E)
         call delete(TF%sigmaInv_edge)
         call delete(TF%temp_F1)
         call delete(TF%temp_F2)
         call delete(TF%phi)
         call delete(TF%temp_CC_SF)
         call delete(TF%divB)
         call delete(TF%divJ)
         call delete(TF%sigmaInv_CC)
       end subroutine

       subroutine export_induction(ind,DT)
         implicit none
         type(induction),intent(in) :: ind
         type(dir_tree),intent(in) :: DT
         integer :: un
         call export(TF%U_E          ,str(DT%restart),'U_E')
         call export(TF%temp_E_TF    ,str(DT%restart),'temp_E_TF')
         call export(TF%temp_F1_TF   ,str(DT%restart),'temp_F1_TF')
         call export(TF%temp_F2_TF   ,str(DT%restart),'temp_F2_TF')
         call export(TF%B            ,str(DT%restart),'B')
         call export(TF%B0           ,str(DT%restart),'B0')
         call export(TF%dB0dt        ,str(DT%restart),'dB0dt')
         call export(TF%temp_CC      ,str(DT%restart),'temp_CC')
         call export(TF%J            ,str(DT%restart),'J')
         call export(TF%curlUCrossB  ,str(DT%restart),'curlUCrossB')
         call export(TF%curlE        ,str(DT%restart),'curlE')
         call export(TF%temp_E       ,str(DT%restart),'temp_E')
         call export(TF%sigmaInv_edge,str(DT%restart),'sigmaInv_edge')
         call export(TF%temp_F1      ,str(DT%restart),'temp_F1')
         call export(TF%temp_F2      ,str(DT%restart),'temp_F2')
         call export(TF%phi          ,str(DT%restart),'phi')
         call export(TF%temp_CC_SF   ,str(DT%restart),'temp_CC_SF')
         call export(TF%divB         ,str(DT%restart),'divB')
         call export(TF%divJ         ,str(DT%restart),'divJ')
         call export(TF%sigmaInv_CC  ,str(DT%restart),'sigmaInv_CC')
       end subroutine

       subroutine import_induction(ind,DT)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         integer :: un
         call import(TF%U_E          ,str(DT%restart),'U_E')
         call import(TF%temp_E_TF    ,str(DT%restart),'temp_E_TF')
         call import(TF%temp_F1_TF   ,str(DT%restart),'temp_F1_TF')
         call import(TF%temp_F2_TF   ,str(DT%restart),'temp_F2_TF')
         call import(TF%B            ,str(DT%restart),'B')
         call import(TF%B0           ,str(DT%restart),'B0')
         call import(TF%dB0dt        ,str(DT%restart),'dB0dt')
         call import(TF%temp_CC      ,str(DT%restart),'temp_CC')
         call import(TF%J            ,str(DT%restart),'J')
         call import(TF%curlUCrossB  ,str(DT%restart),'curlUCrossB')
         call import(TF%curlE        ,str(DT%restart),'curlE')
         call import(TF%temp_E       ,str(DT%restart),'temp_E')
         call import(TF%sigmaInv_edge,str(DT%restart),'sigmaInv_edge')
         call import(TF%temp_F1      ,str(DT%restart),'temp_F1')
         call import(TF%temp_F2      ,str(DT%restart),'temp_F2')
         call import(TF%phi          ,str(DT%restart),'phi')
         call import(TF%temp_CC_SF   ,str(DT%restart),'temp_CC_SF')
         call import(TF%divB         ,str(DT%restart),'divB')
         call import(TF%divJ         ,str(DT%restart),'divJ')
         call import(TF%sigmaInv_CC  ,str(DT%restart),'sigmaInv_CC')
       end subroutine

       subroutine prolongate_ind(ind,DT,RM,SS_reached)
         implicit none
         type(induction),intent(inout) :: ind
         type(dir_tree),intent(in) :: DT
         type(refine_mesh),intent(in) :: RM
         logical,intent(in) :: SS_reached
         integer,dimension(3) :: dir
         integer :: i
         call export_processed(TF%m,TF%B,str(DT%B_f),'B_SS_'//str(RM%level_last),1)

         dir = get_dir(RM)
         if (SS_reached) dir = (/1,2,3/)
         do i=1,3
           if (dir(i).ne.0) then
             write(*,*) 'Prolongating induction solver along direction ',i
             call prolongate(TF%m,dir(i))
             call prolongate(TF%MD_fluid,dir(i))
             call prolongate(TF%MD_sigma,dir(i))

             call prolongate(TF%U_E,TF%m,dir(i))
             call prolongate(TF%temp_E_TF,TF%m,dir(i))
             call prolongate(TF%temp_F1_TF,TF%m,dir(i))
             call prolongate(TF%temp_F2_TF,TF%m,dir(i))

             call prolongate(TF%J,TF%m,dir(i))
             call prolongate(TF%temp_E,TF%m,dir(i))
             call prolongate(TF%B,TF%m,dir(i))
             call prolongate(TF%B0,TF%m,dir(i))
             call prolongate(TF%B_interior,TF%m,dir(i))
             call prolongate(TF%temp_F1,TF%m,dir(i))
             call prolongate(TF%temp_F2,TF%m,dir(i))
             call prolongate(TF%temp_CC,TF%m,dir(i))
             call prolongate(TF%sigmaInv_edge,TF%m,dir(i))
             call prolongate(TF%J_interior,TF%m,dir(i))
             call prolongate(TF%curlUCrossB,TF%m,dir(i))
             call prolongate(TF%curlE,TF%m,dir(i))

             call prolongate(TF%sigmaInv_CC,TF%m,dir(i))
             call prolongate(TF%divB,TF%m,dir(i))
             call prolongate(TF%divJ,TF%m,dir(i))
             call prolongate(TF%phi,TF%m,dir(i))
             call prolongate(TF%temp_CC_SF,TF%m,dir(i))

             call prolongate(TF%PCG_B,TF%m,dir(i))
             call prolongate(TF%PCG_cleanB,TF%m,dir(i))

           endif
         enddo
         if (TF%SP%matrix_based) call init_matrix_based_ops(ind)

         write(*,*) 'Finished induction solver prolongation'
         call apply_BCs(TF%B,TF%m)
         call export_processed(TF%m,TF%B,str(DT%B_f),'B_prolongated_'//str(RM%level),1)

         call assign(TF%temp_F1,TF%B)
         call boost(TF%PCG_cleanB%ISP)
         call div_clean_PCG(TF%PCG_cleanB,TF%B,TF%phi,&
         TF%temp_F1,TF%m,TF%temp_F2,TF%temp_CC_SF,.true.)
         call reset(TF%PCG_cleanB%ISP)

         call export_processed(TF%m,TF%B,str(DT%B_f),'B_cleaned_'//str(RM%level),1)
       end subroutine

       end module