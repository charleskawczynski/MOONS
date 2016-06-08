       module init_PBCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use SF_mod
       implicit none

       private
       public :: init_PBCs

       integer,dimension(3) :: periodic_dir = (/1,0,0/) ! 1 = true, else false
       ! Default = pure Neumann on all sides
       integer :: preDefinedP_BCs = 1 ! see cases in init_PBCs
       
       contains

       subroutine init_PBCs(p,m)
         implicit none
         type(SF),intent(inout) :: p
         type(mesh),intent(in) :: m
         integer :: i,k,pd
         call init_BC_mesh(p,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         do i=1,m%s
           call init_Neumann(p%RF(i)%b)
           do k=1,3
             pd = periodic_dir(k)
             if ((pd.ne.1).and.(pd.ne.0)) stop 'Error: periodic_dir must = 1,0 in init_PBCs in init_PBCs.f90'
             if (pd.eq.1) call makePeriodic(p%RF(i)%b,k)
           enddo
           call init(p%RF(i)%b,0.0_cp)
         enddo
         p%all_Neumann = .true. ! Needs to be adjusted manually

         select case (preDefinedP_BCs)
         case (1)
         case (2); call flow_past_2D_square(p)
         case (3); call duct_flow_2D(p)
         case (4); call duct_flow_2D_2domains(p)
         case (5); call periodic_duct_flow(p)
         case default; stop 'Error: preDefinedP_BCs must = 1:5 in init_PBCs in init_PBCs.f90.'
         end select
       end subroutine

       subroutine flow_past_2D_square(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%RF(5)%b,2)
         call init_Dirichlet(p%RF(7)%b,2)
         call init_Dirichlet(p%RF(8)%b,2)
         call init_Dirichlet(p%RF(5)%b%e(8+3)%b)
         call init_Dirichlet(p%RF(8)%b%e(8+3)%b)
         call init_Dirichlet(p%RF(8)%b%e(8+4)%b)
         call init_Dirichlet(p%RF(7)%b%e(8+4)%b)
       end subroutine

       subroutine duct_flow_2D(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%RF(1)%b,2)
       end subroutine

       subroutine periodic_duct_flow(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .true.
         call init_periodic(p%RF(1)%b,1)
         call init_periodic(p%RF(1)%b,2)
       end subroutine

       subroutine duct_flow_2D_2domains(p)
         implicit none
         type(SF),intent(inout) :: p
         p%all_Neumann = .false.
         call init_Dirichlet(p%RF(1)%b,2)
         call init_Dirichlet(p%RF(2)%b,2)
         ! call init_Dirichlet(p%RF(1)%b%e(8+4)%b)
         ! call init_Dirichlet(p%RF(2)%b%e(8+3)%b)
       end subroutine

       subroutine makePeriodic(p_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: p_bcs
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init_periodic(p_bcs,1)
                   call init_periodic(p_bcs,2)
         case (2); call init_periodic(p_bcs,3)
                   call init_periodic(p_bcs,4)
         case (3); call init_periodic(p_bcs,5)
                   call init_periodic(p_bcs,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic in init_PBCs.f90'
         end select
       end subroutine

       end module