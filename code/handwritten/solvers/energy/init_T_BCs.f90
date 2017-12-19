       module init_T_BCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_extend_mod
       use boundary_conditions_extend_mod
       use BC_funcs_mod
       use SF_extend_mod
       use sim_params_mod
       implicit none

       private
       public :: init_T_BCs

       contains

       subroutine init_T_BCs(T,m,SP)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         integer,dimension(3) :: periodic_dir
         integer :: preset_ID
         call init_BC_mesh(T,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Neumann_BCs(T,m)
         T%all_Neumann = .false. ! Needs to be adjusted manually

         preset_ID = SP%VS%T%BC
         periodic_dir = SP%GP%periodic_dir
         ! preset_ID = 1 ! manual override

         select case (preset_ID)
         case (0) ! Default insulating
         case (1); call initFixedBCs(T); call hotFaceBC(T,2)
         case (2); call hotFaceBC(T,1);  call coldFaceBC(T,2)
         case default; stop 'Error: bad preset_ID in init_TBCs.f90.'
         end select
         call make_periodic(T,m,periodic_dir)
       end subroutine

       subroutine hotFaceBC(T,face)
         implicit none
         type(SF),intent(inout) :: T
         integer,intent(in) :: face
         call init_Dirichlet(T%BF(1)%BCs,face)
         call init(T%BF(1)%BCs,1.0_cp,face)
       end subroutine

       subroutine coldFaceBC(T,face)
         implicit none
         type(SF),intent(inout) :: T
         integer,intent(in) :: face
         call init_Dirichlet(T%BF(1)%BCs,face)
         call init(T%BF(1)%BCs,0.0_cp,face)
       end subroutine

       subroutine initFixedBCs(T)
         implicit none
         type(SF),intent(inout) :: T
         call init_Dirichlet(T%BF(1)%BCs)
         call init(T%BF(1)%BCs,0.0_cp)
       end subroutine

       end module