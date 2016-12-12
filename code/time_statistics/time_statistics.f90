       module time_statistics_mod
       use current_precision_mod
       use sim_params_mod
       use ops_interp_mod
       use time_marching_params_mod
       use string_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
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
         type(SF) :: RMS
         real(cp) :: period = 0.0_cp
         real(cp) :: t_start = 0.0_cp
         real(cp) :: t_stop = 0.0_cp
         integer :: pad = 0
         logical :: exported_statistics = .false.
       end type

       type time_statistics_VF
         type(string) :: dir,name
         type(VF) :: U_ave
         type(VF) :: RMS
         type(TF) :: stresses
         real(cp) :: period = 0.0_cp
         real(cp) :: t_start = 0.0_cp
         real(cp) :: t_stop = 0.0_cp
         integer :: pad = 0
         logical :: exported_statistics = .false.
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

       subroutine init_TS_SF(TS,U,t_start,t_stop,dir,name,pad)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         type(SF),intent(in) :: U
         real(cp),intent(in) :: t_start,t_stop
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         TS%period = t_stop - t_start
         if (TS%period.le.0.0_cp) then
           stop 'Error: time_statistics period must > 0 in time_statistics.f90'
         endif
         TS%t_start = t_start
         TS%t_stop = t_stop
         TS%pad = pad
         TS%exported_statistics = .false.
         call init(TS%U_ave,U)
         call init(TS%RMS,U)
         call init(TS%dir,dir)
         call init(TS%name,name)
         call assign(TS%U_ave,U)
         call assign(TS%RMS,U)
       end subroutine

       subroutine init_TS_VF(TS,m,U,t_start,t_stop,dir,name,pad)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         real(cp),intent(in) :: t_start,t_stop
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: pad
         TS%period = t_stop - t_start
         if (TS%period.le.0.0_cp) then
           stop 'Error: time_statistics period must > 0 in time_statistics.f90'
         endif
         TS%t_start = t_start
         TS%t_stop = t_stop
         TS%pad = pad
         TS%exported_statistics = .false.
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
         call delete(TS%U_ave)
         call delete(TS%RMS)
         call delete(TS%dir)
         call delete(TS%name)
         TS%t_start = 0.0_cp
         TS%t_stop = 0.0_cp
         TS%pad = 0
         TS%exported_statistics = .false.
       end subroutine

       subroutine delete_TS_VF(TS)
         implicit none
         type(time_statistics_VF),intent(inout) :: TS
         call delete(TS%U_ave)
         call delete(TS%RMS)
         call delete(TS%dir)
         call delete(TS%name)
         call delete(TS%stresses)
         TS%t_start = 0.0_cp
         TS%t_stop = 0.0_cp
         TS%pad = 0
         TS%exported_statistics = .false.
       end subroutine

       subroutine update_TS_SF(TS,m,U,TMP,temp)
         implicit none
         type(time_statistics_SF),intent(inout) :: TS
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         type(SF),intent(inout) :: temp
         call add_product(TS%U_ave,U,TMP%dt/TS%period)
         call assign(temp,U)
         call square(temp)
         call add_product(TS%RMS,temp,TMP%dt/TS%period)

         if ((TMP%t.gt.TS%t_stop).and.(.not.TS%exported_statistics)) then
           call square_root(TS%RMS)
           call subtract(temp,U,TS%U_ave)
           call export_processed(m,TS%RMS  ,str(TS%dir),str(TS%name)//'_RMS',TS%pad)
           call export_processed(m,TS%U_ave,str(TS%dir),str(TS%name)//'_time_average',TS%pad)
           call export_processed(m,U       ,str(TS%dir),str(TS%name)//'_instant'     ,TS%pad)
           call export_processed(m,temp    ,str(TS%dir),str(TS%name)//'_fluctuating' ,TS%pad)
           TS%exported_statistics = .true.
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

         ! 1st order statistics (ubar)
         call add_product(TS%U_ave,U,TMP%dt/TS%period)
         call assign(temp,U)
         call square(temp)
         call add_product(TS%RMS,temp,TMP%dt/TS%period)

         if ((TMP%t.gt.TS%t_start).and.(TMP%t.lt.TS%t_stop)) then
           ! 2nd order statistics (Reynolds stresses)
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
           call add_product(TS%stresses,TF_CC,TMP%dt/TS%period)
         endif
         if ((TMP%t.gt.TS%t_stop).and.(.not.TS%exported_statistics)) then
           call square_root(TS%RMS)
           call subtract(temp,U,TS%U_ave)
           call export_processed(m,TS%RMS  ,str(TS%dir),str(TS%name)//'_RMS',TS%pad)
           call export_processed(m,TS%U_ave,str(TS%dir),str(TS%name)//'_time_average',TS%pad)
           call export_processed(m,U       ,str(TS%dir),str(TS%name)//'_instant'     ,TS%pad)
           call export_processed(m,temp    ,str(TS%dir),str(TS%name)//'_fluctuating' ,TS%pad)
           call export_processed(m,TS%stresses%x,str(TS%dir),str(TS%name)//'_Rx' ,TS%pad)
           call export_processed(m,TS%stresses%y,str(TS%dir),str(TS%name)//'_Ry' ,TS%pad)
           call export_processed(m,TS%stresses%z,str(TS%dir),str(TS%name)//'_Rz' ,TS%pad)
           TS%exported_statistics = .true.
         endif
       end subroutine

       end module