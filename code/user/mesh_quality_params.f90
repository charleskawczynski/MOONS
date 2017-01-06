       module mesh_quality_params_mod
       use IO_tools_mod
       use current_precision_mod

       implicit none
       private
       public :: mesh_quality_params
       public :: init,delete,display,print,export,import ! essentials

       public :: manual

       type mesh_quality_params
         real(cp) :: max_mesh_stretch_ratio = 2.0_cp
         integer :: N_max_points_add = 50
         integer :: N_iter = 1
         logical :: auto_find_N = .false.
       end type

       interface init;     module procedure init_MQP;           end interface
       interface init;     module procedure init_copy_MQP;      end interface
       interface delete;   module procedure delete_MQP;         end interface
       interface display;  module procedure display_MQP;        end interface
       interface print;    module procedure print_MQP;          end interface
       interface export;   module procedure export_MQP;         end interface
       interface import;   module procedure import_MQP;         end interface
       interface export;   module procedure export_MQP_wrapper; end interface
       interface import;   module procedure import_MQP_wrapper; end interface

       interface manual;   module procedure manual_MQP;         end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

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

       subroutine init_copy_MQP(MQP,MQP_in)
         implicit none
         type(mesh_quality_params),intent(inout) :: MQP
         type(mesh_quality_params),intent(in) :: MQP_in
         MQP%auto_find_N = MQP_in%auto_find_N
         MQP%N_iter = MQP_in%N_iter
         MQP%max_mesh_stretch_ratio = MQP_in%max_mesh_stretch_ratio
         MQP%N_max_points_add = MQP_in%N_max_points_add
       end subroutine

       subroutine delete_MQP(MQP)
         implicit none
         type(mesh_quality_params),intent(inout) :: MQP
         MQP%auto_find_N = .false.
         MQP%max_mesh_stretch_ratio = 0.0_cp
         MQP%N_iter = 1
         MQP%N_max_points_add = 0
       end subroutine

       subroutine display_MQP(MQP,un)
         implicit none
         type(mesh_quality_params),intent(in) :: MQP
         integer,intent(in) :: un
         write(un,*) 'auto_find_N            = ',MQP%auto_find_N
         write(un,*) 'N_iter                 = ',MQP%N_iter
         write(un,*) 'max_mesh_stretch_ratio = ',MQP%max_mesh_stretch_ratio
         write(un,*) 'N_max_points_add       = ',MQP%N_max_points_add
       end subroutine

       subroutine print_MQP(MQP)
         implicit none
         type(mesh_quality_params),intent(in) :: MQP
         call display(MQP,6)
       end subroutine

       subroutine export_MQP(MQP,un)
         implicit none
         type(mesh_quality_params),intent(in) :: MQP
         integer,intent(in) :: un
         write(un,*) MQP%auto_find_N
         write(un,*) MQP%max_mesh_stretch_ratio
         write(un,*) MQP%N_iter
         write(un,*) MQP%N_max_points_add
       end subroutine

       subroutine import_MQP(MQP,un)
         implicit none
         type(mesh_quality_params),intent(inout) :: MQP
         integer,intent(in) :: un
         read(un,*) MQP%auto_find_N
         read(un,*) MQP%max_mesh_stretch_ratio
         read(un,*) MQP%N_iter
         read(un,*) MQP%N_max_points_add
       end subroutine

       subroutine export_MQP_wrapper(MQP,dir,name)
         implicit none
         type(mesh_quality_params),intent(in) :: MQP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(MQP,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_MQP_wrapper(MQP,dir,name)
         implicit none
         type(mesh_quality_params),intent(inout) :: MQP
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(MQP,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! ****************************************************************
       ! **************************** OTHER *****************************
       ! ****************************************************************

       function manual_MQP() result(MQP)
         implicit none
         type(mesh_quality_params) :: MQP
         call init(MQP,.false.,1.0_cp,1)
       end function

       end module