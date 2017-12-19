       module mesh_quality_params_extend_mod
       use mesh_quality_params_mod
       use IO_tools_mod
       use current_precision_mod

       implicit none
       private
       public :: mesh_quality_params
       public :: init

       public :: manual

       interface init;     module procedure init_MQP;           end interface
       interface manual;   module procedure manual_MQP;         end interface

       contains

       subroutine init_MQP(MQP,auto_find_N,max_mesh_stretch_ratio,N_max_points_add)
         implicit none
         type(mesh_quality_params),intent(inout) :: MQP
         logical,intent(in) :: auto_find_N
         real(cp),intent(in) :: max_mesh_stretch_ratio
         integer,intent(in) :: N_max_points_add
         MQP%auto_find_N = auto_find_N
         MQP%max_mesh_stretch_ratio = max_mesh_stretch_ratio
         MQP%N_max_points_add = N_max_points_add
         if (MQP%auto_find_N) then
         MQP%N_iter = N_max_points_add+1
         else
         MQP%N_iter = 1
         endif
       end subroutine

       function manual_MQP() result(MQP)
         implicit none
         type(mesh_quality_params) :: MQP
         call init(MQP,.false.,1.0_cp,1)
       end function

       end module