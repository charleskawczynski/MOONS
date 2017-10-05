       module time_statistics_extend_mod
       use time_statistics_SF_mod
       use time_statistics_VF_mod
       use current_precision_mod
       use sim_params_mod
       use ops_interp_mod
       use time_marching_params_mod
       use string_mod
       use datatype_conversion_mod
       use mesh_extend_mod
       use probe_extend_mod
       use probe_mod
       use ops_norms_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use stats_period_mod
       use stats_period_extend_mod
       use time_statistics_params_mod
       use time_statistics_params_extend_mod
       use export_raw_processed_mod

       implicit none
       private

       public :: time_statistics_SF
       public :: time_statistics_VF
       public :: init,delete,display,print,export,import ! Essentials

       public :: update

       interface init;     module procedure init_TS_SF;      end interface
       interface init;     module procedure init_TS_VF;      end interface
       interface update;   module procedure update_TS_SF;    end interface
       interface update;   module procedure update_TS_VF;    end interface

       contains

       subroutine init_TS_SF(TS,m,U,TSP,TMP,dir,name)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         type(time_statistics_params),intent(in) :: TSP
         type(time_marching_params),intent(in) :: TMP
         character(len=*),intent(in) :: dir,name
         call init(TS%TSP,TSP)
         call init(TS%mean_energy,dir,'mean_energy',.false.,.true.,TMP)
         call init(TS%U_sum,m,get_DL(U))
         call init(TS%U_ave,m,get_DL(U))
         call init(TS%RMS,m,get_DL(U))
         call init(TS%dir,dir)
         call init(TS%name,name)
         call assign(TS%U_sum,0.0_cp)
         call assign(TS%U_ave,0.0_cp)
         call assign(TS%RMS,0.0_cp)
       end subroutine

       subroutine init_TS_VF(TS,m,U,TSP,TMP,dir,name)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         type(time_statistics_params),intent(in) :: TSP
         type(time_marching_params),intent(in) :: TMP
         character(len=*),intent(in) :: dir,name
         call init(TS%TSP,TSP)
         call init(TS%mean_energy,dir,'mean_energy',.false.,.true.,TMP)
         call init(TS%L2_stresses,dir,'L2_stresses',.false.,.true.,TMP)
         call init(TS%U_sum,m,get_DL(U))
         call init(TS%U_ave,m,get_DL(U))
         call init(TS%RMS,m,get_DL(U))
         call init_CC(TS%stresses_sum,m)
         call init_CC(TS%stresses,m)
         call init(TS%dir,dir)
         call init(TS%name,name)
         call assign(TS%U_sum,0.0_cp)
         call assign(TS%U_ave,0.0_cp)
         call assign(TS%RMS,0.0_cp)
         call assign(TS%stresses_sum,0.0_cp)
         call assign(TS%stresses,0.0_cp)
       end subroutine

       subroutine update_TS_SF(TS,m,U,TMP,temp,compute_export_norms)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         type(SF),intent(inout) :: temp
         logical,intent(in) :: compute_export_norms
         real(cp) :: temp_cp
         if (TS%TSP%collect) then
           call update(TS%TSP,TMP)

           if (TS%TSP%O1_stats%compute_stats) then ! Compute 1st order statistics (ubar)
             call add_product(TS%U_sum,U,TMP%TS%dt)

             call assign(TS%U_ave,TS%U_sum)
             call multiply(TS%U_ave,get_coeff(TS%TSP%O1_stats,TMP))

             if (compute_export_norms) then
               call compute_Ln(temp_cp,TS%U_ave,2.0_cp,m)
               temp_cp = 0.5_cp*temp_cp
               call export(TS%mean_energy,TMP,temp_cp)
             endif

             call assign(temp,U)
             call square(temp)
             call add_product(TS%RMS,temp,TMP%TS%dt)
           endif

           if (TS%TSP%O1_stats%export_stats) then
             call square_root(TS%RMS)
             call subtract(temp,U,TS%U_ave)
             call export_processed(m,TS%RMS  ,str(TS%dir),str(TS%name)//'_RMS'         ,1,TMP)
             call export_processed(m,TS%U_ave,str(TS%dir),str(TS%name)//'_time_average',1,TMP)
             call export_processed(m,U       ,str(TS%dir),str(TS%name)//'_instant'     ,1,TMP)
             call export_processed(m,temp    ,str(TS%dir),str(TS%name)//'_fluctuating' ,1,TMP)
             call set_exported_stats(TS%TSP%O1_stats)
           endif
         endif
       end subroutine

       subroutine update_TS_VF(TS,m,U,TMP,temp_VF,temp_CC,TF_CC,compute_export_norms)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         type(VF),intent(inout) :: temp_VF,temp_CC
         type(TF),intent(inout) :: TF_CC
         logical,intent(in) :: compute_export_norms
         real(cp) :: temp_cp
         if (TS%TSP%collect) then

           call update(TS%TSP,TMP)
           if (TS%TSP%O1_stats%compute_stats) then ! Compute 1st order statistics (ubar)
             call add_product(TS%U_sum,U,TMP%TS%dt)

             ! call add(temp_VF,U,Unm1)
             call multiply(TS%U_ave,get_coeff(TS%TSP%O1_stats,TMP))
             call assign(TS%U_ave,TS%U_sum)
             call multiply(TS%U_ave,get_coeff(TS%TSP%O1_stats,TMP))

             if (compute_export_norms) then
               call face2cellcenter(temp_CC,TS%U_ave,m)
               call compute_Ln(temp_cp,temp_CC,2.0_cp,m)
               temp_cp = 0.5_cp*temp_cp
               call export(TS%mean_energy,TMP,temp_cp)
             endif

             call assign(temp_VF,U)
             call square(temp_VF)
             call add_product(TS%RMS,temp_VF,TMP%TS%dt)
             call add_stat(TS%TSP%O1_stats)
           endif

           if (TS%TSP%O2_stats%compute_stats) then ! 2nd order statistics (Reynolds stresses)
             call subtract(temp_VF,U,TS%U_ave) ! fluctuating
             call face2cellcenter(temp_CC,temp_VF,m)
             call multiply(TF_CC%x%x,temp_CC%x,temp_CC%x)
             call multiply(TF_CC%x%y,temp_CC%x,temp_CC%y)
             call multiply(TF_CC%x%z,temp_CC%x,temp_CC%z)
             call multiply(TF_CC%y%x,temp_CC%y,temp_CC%x)
             call multiply(TF_CC%y%y,temp_CC%y,temp_CC%y)
             call multiply(TF_CC%y%z,temp_CC%y,temp_CC%z)
             call multiply(TF_CC%z%x,temp_CC%z,temp_CC%x)
             call multiply(TF_CC%z%y,temp_CC%z,temp_CC%y)
             call multiply(TF_CC%z%z,temp_CC%z,temp_CC%z)
             call add_product(TS%stresses_sum,TF_CC,TMP%TS%dt)

             call assign(TS%stresses,TS%stresses_sum)
             call multiply(TS%stresses,get_coeff(TS%TSP%O2_stats,TMP))

             if (compute_export_norms) then
               call compute_Ln(temp_cp,TS%stresses,2.0_cp,m)
               call export(TS%L2_stresses,TMP,temp_cp)
             endif

             call add_stat(TS%TSP%O2_stats)
           endif

           if (TS%TSP%O1_stats%export_stats) then
             call square_root(TS%RMS)
             call subtract(temp_VF,U,TS%U_ave)
             call export_processed(m,TS%RMS  ,str(TS%dir),str(TS%name)//'_RMS'         ,1,TMP)
             call export_processed(m,TS%U_ave,str(TS%dir),str(TS%name)//'_time_average',1,TMP)
             call export_processed(m,U       ,str(TS%dir),str(TS%name)//'_instant'     ,1,TMP)
             call export_processed(m,temp_VF    ,str(TS%dir),str(TS%name)//'_fluctuating' ,1,TMP)
             call set_exported_stats(TS%TSP%O1_stats)
           endif

           if (TS%TSP%O2_stats%export_stats) then
             call export_processed(m,TS%stresses%x,str(TS%dir),str(TS%name)//'_Rx',1,TMP)
             call export_processed(m,TS%stresses%y,str(TS%dir),str(TS%name)//'_Ry',1,TMP)
             call export_processed(m,TS%stresses%z,str(TS%dir),str(TS%name)//'_Rz',1,TMP)
             call set_exported_stats(TS%TSP%O2_stats)
           endif
         endif
       end subroutine

       function get_coeff(SP,TMP) result(coeff)
         implicit none
         type(stats_period),intent(in) :: SP
         type(time_marching_params),intent(in) :: TMP
         real(cp) :: coeff
         coeff = 1.0_cp/(TMP%t-SP%t_start)
         coeff = 1.0_cp/(TMP%t-SP%t_start+TMP%TS%dt)
       end function

       end module