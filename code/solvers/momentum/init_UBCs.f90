       module init_UBCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
       use VF_mod
       use SF_mod
       use profile_funcs_mod
       implicit none

       private
       public :: init_UBCs
       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       ! Default = no-slip
       integer :: preDefinedU_BCs = 1 ! See init_UBCs for details
       ! integer :: preDefinedU_BCs = 7

       contains

       subroutine init_UBCs(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,pd

         call init_BC_mesh(U%x,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(U%y,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(U%z,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         do i=1,m%s
           call init_Dirichlet(U%x%RF(i)%b)
           call init_Dirichlet(U%y%RF(i)%b)
           call init_Dirichlet(U%z%RF(i)%b)
           call init(U%x%RF(i)%b,0.0_cp)
           call init(U%y%RF(i)%b,0.0_cp)
           call init(U%z%RF(i)%b,0.0_cp)
           do k=1,3
             pd = periodic_dir(k)
             if ((pd.ne.1).and.(pd.ne.0)) stop 'Error: periodic_dir must = 1,0 in init_UBCs in init_UBCs.f90'
             if (pd.eq.1) call makePeriodic(U%x%RF(i)%b,U%y%RF(i)%b,U%z%RF(i)%b,k)
           enddo
         enddo

         select case (preDefinedU_BCs)
         case (0); 
         case (1); call LDC_1_domain(U)
         case (2); call LDC_4_domains(U)
         case (3); call LDC_9_domains(U)
         case (4); call flow_over_2D_square(U)
         case (5); call duct_flow_2D_2domains(U)
         case (6); call Tylers_geometry(U)
         case (7); call duct_flow(U)
         case (8); call channel_flow_1domain(U)
         case (9); call cylinder_driven_cavity(U,m)
         case (10); call fully_developed_duct_flow(U,m)
         case (11); call LDC_crisscross_driven_lid(U)
         case (12); call LDC_double_crisscross_driven_lid(U)
         case default; stop 'Error: preDefinedU_BCs must = 1:5 in init_UBCs in init_UBCs.f90'
         end select
       end subroutine

       subroutine LDC_1_domain(U)
         implicit none
         type(VF),intent(inout) :: U
         call init(U%x%RF(1)%b,1.0_cp,4)
       end subroutine

       subroutine LDC_crisscross_driven_lid(U)
         implicit none
         type(VF),intent(inout) :: U
         integer :: N
         call init(U%x%RF(1)%b,1.0_cp,4)
         N = U%x%RF(1)%b%f(4)%s(1)/2
         U%x%RF(1)%b%f(4)%vals(2:N,:) = -1.0_cp
       end subroutine

       subroutine LDC_double_crisscross_driven_lid(U)
         implicit none
         type(VF),intent(inout) :: U
         integer :: N
         call init(U%x%RF(1)%b,1.0_cp,4)
         N = U%x%RF(1)%b%f(4)%s(1)/2
         U%x%RF(1)%b%f(4)%vals(2:N,:) = -1.0_cp

         call init(U%y%RF(1)%b,-1.0_cp,2)
         N = U%y%RF(1)%b%f(2)%s(1)/2
         U%y%RF(1)%b%f(2)%vals(2:N,:) = 1.0_cp
       end subroutine

       subroutine LDC_4_domains(U)
         implicit none
         type(VF),intent(inout) :: U
         call init(U%x%RF(3)%b,1.0_cp,4) ! periodic in z, driven at ymax
         call init(U%x%RF(4)%b,1.0_cp,4) ! periodic in z, driven at ymax
       end subroutine

       subroutine LDC_9_domains(U)
         implicit none
         type(VF),intent(inout) :: U
         call init(U%x%RF(5)%b,1.0_cp,4) ! periodic in z, driven at ymax
         call init(U%x%RF(8)%b,1.0_cp,4) ! periodic in z, driven at ymax
         call init(U%x%RF(9)%b,1.0_cp,4) ! periodic in z, driven at ymax
         call init(U%x%RF(5)%b%e(8+4),1.0_cp)
         call init(U%x%RF(8)%b%e(8+2),1.0_cp)
         call init(U%x%RF(8)%b%e(8+4),1.0_cp)
         call init(U%x%RF(9)%b%e(8+2),1.0_cp)
       end subroutine

       subroutine duct_flow(U)
         implicit none
         type(VF),intent(inout) :: U
         ! Inlet (uniform)
         call init(U%x%RF(1)%b,1.0_cp,1)
         ! Outlet (fully developed)
         call init_Neumann(U%x%RF(1)%b,2)
         call init_Neumann(U%y%RF(1)%b,2)
         call init_Neumann(U%z%RF(1)%b,2)
       end subroutine

       subroutine duct_flow_2D_2domains(U)
         implicit none
         type(VF),intent(inout) :: U
         ! Inlet (uniform)
         call init(U%x%RF(1)%b,1.0_cp,1)
         call init(U%x%RF(2)%b,1.0_cp,1)
         call init(U%x%RF(1)%b%e(8+2),1.0_cp)
         call init(U%x%RF(2)%b%e(8+1),1.0_cp)
         ! Outlet (fully developed)
         call init_Neumann(U%x%RF(1)%b,2)
         call init_Neumann(U%x%RF(2)%b,2)
       end subroutine

       subroutine channel_flow_1domain(U)
         implicit none
         type(VF),intent(inout) :: U
         ! Inlet (uniform)
         call init(U%x%RF(1)%b,1.0_cp,1)
         ! Outlet (fully developed)
         call init_Neumann(U%x%RF(1)%b,2)
         call init_Neumann(U%y%RF(1)%b,2)
       end subroutine

       subroutine flow_over_2D_square(U)
         implicit none
         type(VF),intent(inout) :: U
         ! 16 edges total must be defined here with velocity of 1
         ! Inlet (uniform)
         call init(U%x%RF(1)%b,1.0_cp,1)
         call init(U%x%RF(2)%b,1.0_cp,1)
         call init(U%x%RF(3)%b,1.0_cp,1)
              ! Edges
              call init(U%x%RF(1)%b%e(8+1),1.0_cp); call init(U%x%RF(1)%b%e(8+2),1.0_cp)
              call init(U%x%RF(2)%b%e(8+1),1.0_cp); call init(U%x%RF(2)%b%e(8+2),1.0_cp)
              call init(U%x%RF(3)%b%e(8+1),1.0_cp); call init(U%x%RF(3)%b%e(8+2),1.0_cp)
         ! Sides (free-stream)
         call init(U%x%RF(2)%b,1.0_cp,4); call init(U%x%RF(3)%b,1.0_cp,3)
         call init(U%x%RF(4)%b,1.0_cp,4); call init(U%x%RF(6)%b,1.0_cp,3)
         call init(U%x%RF(5)%b,1.0_cp,4); call init(U%x%RF(7)%b,1.0_cp,3)
              ! Edges
              call init(U%x%RF(2)%b%e(8+4),1.0_cp); call init(U%x%RF(3)%b%e(8+3),1.0_cp)
              call init(U%x%RF(4)%b%e(8+2),1.0_cp); call init(U%x%RF(6)%b%e(8+1),1.0_cp)
              call init(U%x%RF(4)%b%e(8+4),1.0_cp); call init(U%x%RF(6)%b%e(8+3),1.0_cp)
              call init(U%x%RF(5)%b%e(8+2),1.0_cp); call init(U%x%RF(7)%b%e(8+1),1.0_cp)
              call init(U%x%RF(5)%b%e(8+4),1.0_cp); call init(U%x%RF(7)%b%e(8+3),1.0_cp)
         ! Outlet (fully developed and v=0)
         call init_Neumann(U%x%RF(5)%b,2)
         call init_Neumann(U%x%RF(7)%b,2)
         call init_Neumann(U%x%RF(8)%b,2)
       end subroutine

       subroutine Tylers_geometry(U)
         implicit none
         type(VF),intent(inout) :: U
         integer :: i
         ! THIS NEEDS TO BE FIXED
         do i=4,6
           call init_Neumann(U%x%RF(i)%b,2)
           call init_Neumann(U%y%RF(i)%b,2)
           call init_Neumann(U%z%RF(i)%b,2)
         enddo
         call init_Neumann(U%x%RF(10)%b,2); call init_Neumann(U%x%RF(14)%b,2)
         call init_Neumann(U%y%RF(10)%b,2); call init_Neumann(U%y%RF(14)%b,2)
         call init_Neumann(U%z%RF(10)%b,2); call init_Neumann(U%z%RF(14)%b,2)
         call init(U%x%RF(1)%b,1.0_cp,1)

         call init_Neumann(U%x%RF(1)%b,6)
         call init_Neumann(U%y%RF(1)%b,6)
         call init_Neumann(U%z%RF(1)%b,6)

         call init(U%x%RF(1)%b,1.0_cp,1)
         call init_Neumann(U%x%RF(14)%b,1)
         call init_Neumann(U%y%RF(14)%b,1)
         call init_Neumann(U%z%RF(14)%b,1)
       end subroutine

       subroutine makePeriodic(u_bcs,v_bcs,w_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         integer,intent(in) :: dir
         integer :: face1,face2
         select case (dir)
         case (1); face1 = 1; face2 = 2
         case (2); face1 = 3; face2 = 4
         case (3); face1 = 5; face2 = 6
         case default
         stop 'Error: dir must = 1,2,3 in makePeriodic in initializeUBCs.f90'
         end select
         call init_periodic(u_bcs,face1); call init(u_bcs,0.0_cp,face1)
         call init_periodic(v_bcs,face1); call init(v_bcs,0.0_cp,face1)
         call init_periodic(w_bcs,face1); call init(w_bcs,0.0_cp,face1)

         call init_periodic(u_bcs,face2); call init(u_bcs,0.0_cp,face2)
         call init_periodic(v_bcs,face2); call init(v_bcs,0.0_cp,face2)
         call init_periodic(w_bcs,face2); call init(w_bcs,0.0_cp,face2)
       end subroutine

       subroutine cylinder_driven_cavity(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call cylinderDrivenBCs(U%x%RF(1)%b,U%y%RF(1)%b,U%z%RF(1)%b,m%g(1),3)
       end subroutine

       subroutine fully_developed_duct_flow(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call ductFlow_FD_Profile(U%x%RF(1)%b,U%y%RF(1)%b,U%z%RF(1)%b,m%g(1),1,1)
       end subroutine

       subroutine cylinderDrivenBCs(u_bcs,v_bcs,w_bcs,g,dir)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init(v_bcs,rotatingCylinder(g%c(2),g%c(3),&
          v_bcs%f(1)%s(1),v_bcs%f(1)%s(2),0.1_cp,1.0_cp,1),1)
                   call init(w_bcs,rotatingCylinder(g%c(2),g%c(3),&
          w_bcs%f(1)%s(1),w_bcs%f(1)%s(2),0.1_cp,1.0_cp,2),1)
         case (2); call init(u_bcs,rotatingCylinder(g%c(1),g%c(3),&
          u_bcs%f(2)%s(1),u_bcs%f(2)%s(2),0.1_cp,1.0_cp,1),2)
                   call init(w_bcs,rotatingCylinder(g%c(1),g%c(3),&
          w_bcs%f(2)%s(1),w_bcs%f(2)%s(2),0.1_cp,1.0_cp,2),2)
         case (3); call init(u_bcs,rotatingCylinder(g%c(1),g%c(2),&
          u_bcs%f(3)%s(1),u_bcs%f(3)%s(2),0.1_cp,1.0_cp,1),3)
                   call init(v_bcs,rotatingCylinder(g%c(1),g%c(2),&
          v_bcs%f(3)%s(1),v_bcs%f(3)%s(2),0.1_cp,1.0_cp,2),3)
         end select
       end subroutine

       subroutine ductFlow_FD_Profile(u_bcs,v_bcs,w_bcs,g,dir,posNeg)
         implicit none
         type(BCs),intent(inout) :: u_bcs,v_bcs,w_bcs
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,posNeg
         integer :: face
         face = getFace(dir,posNeg)
         select case (dir)
         case (1); call init(u_bcs,init_FD_DuctFlow(g%c(2),g%c(3),&
                    u_bcs%f(face)%s(1),u_bcs%f(face)%s(2)),1)
         case (2); call init(v_bcs,init_FD_DuctFlow(g%c(1),g%c(3),&
                    v_bcs%f(face)%s(1),v_bcs%f(face)%s(2)),2)
         case (3); call init(w_bcs,init_FD_DuctFlow(g%c(1),g%c(2),&
                    w_bcs%f(face)%s(1),w_bcs%f(face)%s(2)),3)
         end select
       end subroutine

       function getFace(ductDir,IO) result(face)
         implicit none
         integer,intent(in) :: ductDir,IO
         integer :: face
         select case (ductDir)
         case (1); select case (IO)
                   case (-1); face = 1
                   case (1);  face = 2
                   case default; stop 'IO must = 1,-1 in getFace in init_UBCs.f90'
                   end select
         case (2); select case (IO)
                   case (-1); face = 3
                   case (1);  face = 4
                   case default; stop 'IO must = 1,-1 in getFace in init_UBCs.f90'
                   end select
         case (3); select case (IO)
                   case (-1); face = 5
                   case (1);  face = 6
                   case default; stop 'IO must = 1,-1 in getFace in init_UBCs.f90'
                   end select
         case default; stop 'Error: ductDir must = 1,2,3 in getFace in init_UBCs.f90'
         end select
       end function

       end module