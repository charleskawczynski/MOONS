       module init_PBCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use SF_mod
       implicit none

       private
       public :: init_PBCs

       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       ! Default = pure Neumann on all sides
       integer :: preDefinedP_BCs = 0 ! see cases in init_PBCs

       contains

       subroutine init_PBCs(p,m)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         call init_BC_mesh(p,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Neumann_BCs(p,m) ! Default
         p%all_Neumann = .true. ! Needs to be adjusted manually

         select case (preDefinedP_BCs)
         case (0) ! Pure Neumann default
         case (1); call flow_past_2D_square(p)
         case (2); call duct_flow_2D(p)
         case (3); call duct_flow_2D_2domains(p)
         case (4); call periodic_duct_flow(p)
         case default; stop 'Error: preDefinedP_BCs must = 1:5 in init_PBCs in init_PBCs.f90.'
         end select
         call make_periodic(p,m,periodic_dir)
         call init_BC_props(p)
       end subroutine

       subroutine flow_past_2D_square(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%BF(5)%BCs,2)
         call init_Dirichlet(p%BF(7)%BCs,2)
         call init_Dirichlet(p%BF(8)%BCs,2)
         ! call init_Dirichlet(p%BF(5)%BCs%e(8+3)%BCs)
         ! call init_Dirichlet(p%BF(8)%BCs%e(8+3)%BCs)
         ! call init_Dirichlet(p%BF(8)%BCs%e(8+4)%BCs)
         ! call init_Dirichlet(p%BF(7)%BCs%e(8+4)%BCs)
       end subroutine

       subroutine duct_flow_2D(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%BF(1)%BCs,2)
       end subroutine

       subroutine periodic_duct_flow(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .true.
         call init_periodic(p%BF(1)%BCs,1)
         call init_periodic(p%BF(1)%BCs,2)
       end subroutine

       subroutine duct_flow_2D_2domains(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%BF(1)%BCs,2)
         call init_Dirichlet(p%BF(2)%BCs,2)
         ! call init_Dirichlet(p%BF(1)%BCs%e(8+4)%BCs)
         ! call init_Dirichlet(p%BF(2)%BCs%e(8+3)%BCs)
       end subroutine

       end module