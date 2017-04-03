       module linear_stability_analysis_mod
       use current_precision_mod
       use sim_params_mod
       use ops_interp_mod
       use time_marching_params_mod
       use string_mod
       use datatype_conversion_mod
       use mesh_mod
       use probe_mod
       use ops_norms_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use staLSA_period_mod
       use linear_stability_analysis_params_mod
       use linear_stability_analysis_params_mod
       use export_raw_processed_mod

       implicit none
       private

       public :: linear_stability_analysis_SF
       public :: linear_stability_analysis_VF

       ! public :: init,delete,display,print,export,import ! Essentials
       public :: init,delete

       public :: update

       type linear_stability_analysis_SF
         type(string) :: dir,name
         type(linear_stability_analysis_params) :: LSAP
         type(SF) :: U_base
         type(probe) :: temporal_growth_rate
       end type

       type linear_stability_analysis_VF
         type(string) :: dir,name
         type(linear_stability_analysis_params) :: LSAP
         type(VF) :: U_base
         type(probe) :: temporal_growth_rate
       end type

       interface init;        module procedure init_LSA_SF;        end interface
       interface init;        module procedure init_LSA_VF;        end interface
       interface delete;      module procedure delete_LSA_SF;      end interface
       interface delete;      module procedure delete_LSA_VF;      end interface
       interface update;      module procedure update_LSA_SF;      end interface
       interface update;      module procedure update_LSA_VF;      end interface
       interface assign_base; module procedure assign_base_LSA_SF; end interface
       interface assign_base; module procedure assign_base_LSA_VF; end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_LSA_SF(LSA,m,U,LSAP,dir,name)
         implicit none
         type(linear_stability_analysis_SF),intent(inout) :: LSA
         type(linear_stability_analysis_params),intent(in) :: LSAP
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: U
         character(len=*),intent(in) :: dir,name
         call init(LSA%LSAP,LSAP)
         call init(LSA%U_base,m,get_DL(U))
         call init(LSA%temporal_growth_rate,dir,'temporal_growth_rate',.false.,.true.)
         call init(LSA%dir,dir)
         call init(LSA%name,name)
         call assign(LSA%U_base,0.0_cp)
       end subroutine

       subroutine init_LSA_VF(LSA,m,U,LSAP,dir,name)
         implicit none
         type(linear_stability_analysis_VF),intent(inout) :: LSA
         type(linear_stability_analysis_params),intent(in) :: LSAP
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: dir,name
         call init(LSA%LSAP,LSAP)
         call init(LSA%temporal_growth_rate,dir,'temporal_growth_rate',.false.,.true.)
         call init(LSA%U_base,m,get_DL(U))
         call init(LSA%dir,dir)
         call init(LSA%name,name)
         call assign(LSA%U_base,0.0_cp)
       end subroutine

       subroutine delete_LSA_SF(LSA)
         implicit none
         type(linear_stability_analysis_SF),intent(inout) :: LSA
         call delete(LSA%U_base)
         call delete(LSA%LSAP)
         call delete(LSA%dir)
         call delete(LSA%name)
         call delete(LSA%temporal_growth_rate)
       end subroutine

       subroutine delete_LSA_VF(LSA)
         implicit none
         type(linear_stability_analysis_VF),intent(inout) :: LSA
         call delete(LSA%U_base)
         call delete(LSA%LSAP)
         call delete(LSA%dir)
         call delete(LSA%name)
         call delete(LSA%temporal_growth_rate)
       end subroutine

       subroutine update_LSA_SF(LSA,m,U,TMP,temp)
         implicit none
         type(linear_stability_analysis_SF),intent(inout) :: LSA
         type(time_marching_params),intent(in) :: TMP
         type(SF),intent(in) :: U
         if (LSA%LSAP%perform) then
           call update(LSA%LSAP,TMP)
           if (LSA%LSAP%start_now) call assign(LSA%U_base,U)
         endif
       end subroutine

       subroutine update_LSA_VF(LSA,m,KE,TMP)
         implicit none
         type(linear_stability_analysis_VF),intent(inout) :: LSA
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: KE
         if (LSA%LSAP%perform) then
           call export(LSA%temporal_growth_rate,TMP,log(KE))
         endif
       end subroutine

       subroutine assign_base_LSA_SF(LSA,m,U,TMP,temp)
         implicit none
         type(linear_stability_analysis_SF),intent(inout) :: LSA
         type(time_marching_params),intent(in) :: TMP
         type(SF),intent(in) :: U
         if (LSA%LSAP%perform) then
           call update(LSA%LSAP,TMP)
           if (LSA%LSAP%start_now) call assign(LSA%U_base,U)
         endif
       end subroutine

       subroutine assign_base_LSA_VF(LSA,m,U,TMP)
         implicit none
         type(linear_stability_analysis_VF),intent(inout) :: LSA
         type(time_marching_params),intent(in) :: TMP
         type(VF),intent(in) :: U
         if (LSA%LSAP%perform) then
           call update(LSA%LSAP,TMP)
           if (LSA%LSAP%start_now) call assign(LSA%U_base,U)
         endif
       end subroutine

       end module