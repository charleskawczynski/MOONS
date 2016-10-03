       module init_PBCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
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

         call Neumann_BCs(p) ! Default
         p%all_Neumann = .true. ! Needs to be adjusted manually

         select case (preDefinedP_BCs)
         case (0) ! Pure Neumann default
         case (1); call flow_past_2D_square(p)
         case (2); call duct_flow_2D(p)
         case (3); call duct_flow_2D_2domains(p)
         case (4); call periodic_duct_flow(p)
         case default; stop 'Error: preDefinedP_BCs must = 1:5 in init_PBCs in init_PBCs.f90.'
         end select
         call make_periodic(p,periodic_dir)
       end subroutine

       subroutine flow_past_2D_square(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%GF(5)%b,2)
         call init_Dirichlet(p%GF(7)%b,2)
         call init_Dirichlet(p%GF(8)%b,2)
         call init_Dirichlet(p%GF(5)%b%e(8+3)%b)
         call init_Dirichlet(p%GF(8)%b%e(8+3)%b)
         call init_Dirichlet(p%GF(8)%b%e(8+4)%b)
         call init_Dirichlet(p%GF(7)%b%e(8+4)%b)
       end subroutine

       subroutine duct_flow_2D(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%GF(1)%b,2)
       end subroutine

       subroutine periodic_duct_flow(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .true.
         call init_periodic(p%GF(1)%b,1)
         call init_periodic(p%GF(1)%b,2)
       end subroutine

       subroutine duct_flow_2D_2domains(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%GF(1)%b,2)
         call init_Dirichlet(p%GF(2)%b,2)
         ! call init_Dirichlet(p%GF(1)%b%e(8+4)%b)
         ! call init_Dirichlet(p%GF(2)%b%e(8+3)%b)
       end subroutine

       end module