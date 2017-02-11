       module time_statistics_mod
       use current_precision_mod
       use sim_params_mod
       use ops_interp_mod
       use time_marching_params_mod
       use string_mod
       use datatype_conversion_mod
       use mesh_mod
       use probe_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use stats_period_mod
       use time_statistics_params_mod
       use dynamic_mesh_refinement_mod
       use export_raw_processed_mod

       implicit none
       private

       public :: time_statistics_SF
       public :: time_statistics_VF

       public :: init,delete,display,print,export,import ! Essentials

       public :: update

       type time_statistics_SF
         type(string) :: dir,name
         type(SF) :: U_ave
         type(probe) :: L2_mean
         type(SF) :: RMS
         type(time_statistics_params) :: TSP
       end type

       type time_statistics_VF
         type(string) :: dir,name
         type(VF) :: U_ave
         type(probe) :: L2_mean
         type(VF) :: RMS
         type(TF) :: stresses
         type(time_statistics_params) :: TSP
       end type

       interface init;     module procedure init_TS_SF;      end interface
       interface init;     module procedure init_TS_VF;      end interface
       interface delete;   module procedure delete_TS_SF;    end interface
       interface delete;   module procedure delete_TS_VF;    end interface
       interface update;   module procedure update_TS_SF;    end interface
       interface update;   module procedure update_TS_VF;    end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_TS_SF(TS,m,U,TMP,TSP,DMR,dir,name)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         type(time_marching_params),intent(in) :: TMP
         type(time_statistics_params),intent(in) :: TSP
         type(dynamic_mesh_refinement),intent(in) :: DMR
         character(len=*),intent(in) :: dir,name
         call init(TS%TSP,TSP)
         call set_coefficient(TS%TSP,TMP%dt/TSP%O1_stats%period/m%MP%volume)
         call init(TS%L2_mean,dir,name,.false.,DMR,.true.)
         call init(TS%U_ave,U)
         call init(TS%RMS,U)
         call init(TS%dir,dir)
         call init(TS%name,name)
         call assign(TS%U_ave,U)
         call assign(TS%RMS,U)
       end subroutine

       subroutine init_TS_VF(TS,m,U,TMP,TSP,dir,name)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         type(time_marching_params),intent(in) :: TMP
         type(time_statistics_params),intent(in) :: TSP
         character(len=*),intent(in) :: dir,name
         call init(TS%TSP,TSP)
         call set_coefficient(TS%TSP,TMP%dt/TSP%O1_stats%period/m%MP%volume)
         call init(TS%U_ave,U)
         call init(TS%RMS,U)
         call init_CC(TS%stresses,m)
         call init(TS%dir,dir)
         call init(TS%name,name)
         call assign(TS%U_ave,U)
         call assign(TS%RMS,U)
       end subroutine

       subroutine delete_TS_SF(TS)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         call delete(TS%TSP)
         call delete(TS%U_ave)
         call delete(TS%RMS)
         call delete(TS%dir)
         call delete(TS%name)
       end subroutine

       subroutine delete_TS_VF(TS)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         call delete(TS%TSP)
         call delete(TS%U_ave)
         call delete(TS%RMS)
         call delete(TS%dir)
         call delete(TS%name)
         call delete(TS%stresses)
       end subroutine

       subroutine update_TS_SF(TS,m,U,TMP,temp)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         type(SF),intent(inout) :: temp
         type(string) :: s
         call update(TS%TSP,TMP)

         if (TS%TSP%O1_stats%compute_stats) then ! Compute 1st order statistics (ubar)
           call add_product(TS%U_ave,U,TS%TSP%coefficient)
           call assign(temp,U)
           call square(temp)
           call add_product(TS%RMS,temp,TS%TSP%coefficient)
           call update(TS%TSP,TMP)
         endif

         if (TS%TSP%O1_stats%export_stats) then
           call init(s,'_t=')
           call append(s,cp2str(TMP%t))
           call square_root(TS%RMS)
           call subtract(temp,U,TS%U_ave)
           call export_processed(m,TS%RMS  ,str(TS%dir),str(TS%name)//'_RMS'//str(s)         ,1)
           call export_processed(m,TS%U_ave,str(TS%dir),str(TS%name)//'_time_average'//str(s),1)
           call export_processed(m,U       ,str(TS%dir),str(TS%name)//'_instant'//str(s)     ,1)
           call export_processed(m,temp    ,str(TS%dir),str(TS%name)//'_fluctuating'//str(s) ,1)
           call set_exported_stats(TS%TSP%O1_stats)
           call delete(s)
         endif
       end subroutine

       subroutine update_TS_VF(TS,m,U,TMP,temp,temp_CC,TF_CC)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         type(VF),intent(inout) :: temp,temp_CC
         type(TF),intent(inout) :: TF_CC
         type(string) :: s

         call update(TS%TSP,TMP)
         if (TS%TSP%O1_stats%compute_stats) then ! Compute 1st order statistics (ubar)
           call add_product(TS%U_ave,U,TS%TSP%coefficient)
           call assign(temp,U)
           call square(temp)
           call add_product(TS%RMS,temp,TS%TSP%coefficient)
         endif

         if (TS%TSP%O2_stats%compute_stats) then ! 2nd order statistics (Reynolds stresses)
           call subtract(temp,U,TS%U_ave) ! fluctuating
           call face2cellcenter(temp_CC,temp,m)
           call multiply(TF_CC%x%x,temp_CC%x,temp_CC%x)
           call multiply(TF_CC%x%y,temp_CC%x,temp_CC%y)
           call multiply(TF_CC%x%z,temp_CC%x,temp_CC%z)
           call multiply(TF_CC%y%x,temp_CC%y,temp_CC%x)
           call multiply(TF_CC%y%y,temp_CC%y,temp_CC%y)
           call multiply(TF_CC%y%z,temp_CC%y,temp_CC%z)
           call multiply(TF_CC%z%x,temp_CC%z,temp_CC%x)
           call multiply(TF_CC%z%y,temp_CC%z,temp_CC%y)
           call multiply(TF_CC%z%z,temp_CC%z,temp_CC%z)
           call add_product(TS%stresses,TF_CC,TS%TSP%coefficient)
         endif

         if (TS%TSP%O1_stats%export_stats) then
           call init(s,'_t=')
           call append(s,cp2str(TMP%t))
           call square_root(TS%RMS)
           call subtract(temp,U,TS%U_ave)
           call export_processed(m,TS%RMS  ,str(TS%dir),str(TS%name)//'_RMS'//str(s),1)
           call export_processed(m,TS%U_ave,str(TS%dir),str(TS%name)//'_time_average'//str(s),1)
           call export_processed(m,U       ,str(TS%dir),str(TS%name)//'_instant'//str(s)     ,1)
           call export_processed(m,temp    ,str(TS%dir),str(TS%name)//'_fluctuating'//str(s) ,1)
           call set_exported_stats(TS%TSP%O1_stats)
           call delete(s)
         endif

         if (TS%TSP%O2_stats%export_stats) then
           call init(s,'_t=')
           call append(s,cp2str(TMP%t))
           call export_processed(m,TS%stresses%x,str(TS%dir),str(TS%name)//'_Rx'//str(s),1)
           call export_processed(m,TS%stresses%y,str(TS%dir),str(TS%name)//'_Ry'//str(s),1)
           call export_processed(m,TS%stresses%z,str(TS%dir),str(TS%name)//'_Rz'//str(s),1)
           call set_exported_stats(TS%TSP%O2_stats)
           call delete(s)
         endif
       end subroutine

       end module