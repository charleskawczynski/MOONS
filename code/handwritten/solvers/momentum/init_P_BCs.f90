       module init_P_BCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_extend_mod
       use SF_mod
       use sim_params_mod
       implicit none

       private
       public :: init_P_BCs

       contains

       subroutine init_P_BCs(p,m,SP)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         integer,dimension(3) :: periodic_dir
         integer :: preset_ID
         call init_BC_mesh(p,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Neumann_BCs(p,m) ! Default
         p%all_Neumann = .true. ! Needs to be adjusted manually

         preset_ID = SP%VS%P%BC
         periodic_dir = SP%GP%periodic_dir
         ! preset_ID = 0 ! manual override

         select case (preset_ID)
         case (0) ! Pure Neumann default
         case (1); call duct_flow_P0_at_xmax(p)
         case (2); call duct_flow_periodic_IO(p)
         case (3); call duct_flow_2domains(p)
         case (4); call flow_past_2D_square(p)
         case default; stop 'Error: bad preset_ID in init_PBCs.f90.'
         end select
         call make_periodic(p,m,periodic_dir)
         call init_BC_props(p,SP%DP%c_w,SP%DP%Robin_coeff)
       end subroutine

       subroutine flow_past_2D_square(p)
         implicit none
         type(SF),intent(inout) :: p
         integer :: i
         p%all_Neumann = .false.
         do i=1,p%s
         call init_Dirichlet(p%BF(i)%BCs,2)
         enddo
         ! call init_Dirichlet(p%BF(5)%BCs%e(8+3)%BCs)
         ! call init_Dirichlet(p%BF(8)%BCs%e(8+3)%BCs)
         ! call init_Dirichlet(p%BF(8)%BCs%e(8+4)%BCs)
         ! call init_Dirichlet(p%BF(7)%BCs%e(8+4)%BCs)
       end subroutine

       subroutine duct_flow_P0_at_xmax(p)
         implicit none
         type(SF),intent(inout) :: p
         integer :: i
         p%all_Neumann = .false.
         do i=1,p%s
         call init_Dirichlet(p%BF(i)%BCs,2)
         enddo
       end subroutine

       subroutine duct_flow_periodic_IO(p)
         implicit none
         type(SF),intent(inout) :: p
         integer :: i
         p%all_Neumann = .true.
         do i=1,p%s
         call init_periodic(p%BF(i)%BCs,1)
         call init_periodic(p%BF(i)%BCs,2)
         enddo
       end subroutine

       subroutine duct_flow_2domains(p)
         implicit none
         type(SF),intent(inout) :: p
         integer :: i
         p%all_Neumann = .false.
         do i=1,p%s
         call init_Dirichlet(p%BF(i)%BCs,2)
         enddo
         ! call init_Dirichlet(p%BF(1)%BCs%e(8+4)%BCs)
         ! call init_Dirichlet(p%BF(2)%BCs%e(8+3)%BCs)
       end subroutine

       end module