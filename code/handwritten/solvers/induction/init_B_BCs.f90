       module init_B_BCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_extend_mod
       use boundary_conditions_extend_mod
       use SF_mod
       use VF_mod
       use sim_params_mod
       implicit none

       private
       public :: init_B_BCs

       contains

       subroutine init_B_BCs(B,m,SP)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         type(sim_params),intent(in) :: SP
         integer,dimension(3) :: periodic_dir
         integer :: preset_ID

         call init_BC_mesh(B%x,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(B%y,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(B%z,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Dirichlet_BCs(B,m)

         preset_ID = SP%VS%B%BC
         periodic_dir = SP%GP%periodic_dir
         ! preset_ID = 0 ! manual override

         select case (preset_ID)
         case (0);
         case (1);  call pseudo_vacuum(B,m)
         case (2);  call init_Bandaru(B)
         case (3);  call periodic_duct_flow(B,m)
         case (4);  call periodic_duct_flow_pseudo_vacuum(B,m)
         case (5);  call periodic_duct_thin_wall(B,m)
         case (6);  call thin_wall(B,m)
         case (7);  call thin_wall_LDC(B,m)
         case (8);  call thin_wall_Hunt(B,m)
         case (9);  call RV_symmetric_zmax(B,m)
         case (10); call PV_symmetric_zmax(B,m)
         case default; stop 'Error: bad preset_ID in init_UBCs.f90'
         end select

         call make_periodic(B,m,periodic_dir)
         call init_BC_props(B,SP%DP%c_w,SP%DP%Robin_coeff)
       end subroutine

       subroutine RV_symmetric_zmax(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,m%s
           ! call init_AntiSymmetric(B%x%BF(i)%BCs,6)
           ! call init_AntiSymmetric(B%y%BF(i)%BCs,6)
           call init_Dirichlet(B%x%BF(i)%BCs,6) ! effectively same as antisymmetric
           call init_Dirichlet(B%y%BF(i)%BCs,6) ! effectively same as antisymmetric
           call init_Neumann(B%z%BF(i)%BCs,6)
         enddo
       end subroutine

       subroutine PV_symmetric_zmax(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i
         call pseudo_vacuum(B,m)
         do i=1,m%s
           ! call init_AntiSymmetric(B%x%BF(i)%BCs,6)
           ! call init_AntiSymmetric(B%y%BF(i)%BCs,6)
           call init_Dirichlet(B%x%BF(i)%BCs,6) ! effectively same as antisymmetric
           call init_Dirichlet(B%y%BF(i)%BCs,6) ! effectively same as antisymmetric
           call init_Neumann(B%z%BF(i)%BCs,6)
         enddo
       end subroutine

       subroutine pseudo_vacuum(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i
         call Dirichlet_BCs(B,m)
         do i=1,m%s
           call init_Neumann(B%x%BF(i)%BCs,1); call init(B%x%BF(i)%BCs,0.0_cp,1)
           call init_Neumann(B%x%BF(i)%BCs,2); call init(B%x%BF(i)%BCs,0.0_cp,2)
           call init_Neumann(B%y%BF(i)%BCs,3); call init(B%y%BF(i)%BCs,0.0_cp,3)
           call init_Neumann(B%y%BF(i)%BCs,4); call init(B%y%BF(i)%BCs,0.0_cp,4)
           call init_Neumann(B%z%BF(i)%BCs,5); call init(B%z%BF(i)%BCs,0.0_cp,5)
           call init_Neumann(B%z%BF(i)%BCs,6); call init(B%z%BF(i)%BCs,0.0_cp,6)
         enddo
       end subroutine

       subroutine periodic_duct_flow(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i,k
         do i=1,m%s
           do k=1,2; call init_periodic(B%x%BF(i)%BCs,k); enddo
           do k=1,2; call init_periodic(B%y%BF(i)%BCs,k); enddo
           do k=1,2; call init_periodic(B%z%BF(i)%BCs,k); enddo
         enddo
       end subroutine

       subroutine periodic_duct_flow_pseudo_vacuum(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i,k
         call pseudo_vacuum(B,m)
         do i=1,m%s
           do k=1,2; call init_periodic(B%x%BF(i)%BCs,k); enddo
           do k=1,2; call init_periodic(B%y%BF(i)%BCs,k); enddo
           do k=1,2; call init_periodic(B%z%BF(i)%BCs,k); enddo
         enddo
       end subroutine

       subroutine periodic_duct_thin_wall(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i,k
         call thin_wall(B,m)
         do i=1,m%s
           do k=1,2; call init_periodic(B%x%BF(i)%BCs,k); enddo
           do k=1,2; call init_periodic(B%y%BF(i)%BCs,k); enddo
           do k=1,2; call init_periodic(B%z%BF(i)%BCs,k); enddo
         enddo
       end subroutine

       subroutine init_Bandaru(B)
         implicit none
         type(VF),intent(inout) :: B
         integer :: i
         do i=1,B%x%s
         call init_periodic(B%x%BF(i)%BCs,1)
         call init_periodic(B%y%BF(i)%BCs,1)
         call init_periodic(B%z%BF(i)%BCs,1)
         call init_periodic(B%x%BF(i)%BCs,2)
         call init_periodic(B%y%BF(i)%BCs,2)
         call init_periodic(B%z%BF(i)%BCs,2)
         call init_Neumann(B%x%BF(i)%BCs,5)
         call init_Neumann(B%x%BF(i)%BCs,6)
         enddo
       end subroutine

       subroutine thin_wall(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i,j
         call pseudo_vacuum(B,m)
         do i=1,m%s
           j=3;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,0.0_cp,j)
           j=4;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,0.0_cp,j)
           j=5;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,0.0_cp,j)
           j=6;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,0.0_cp,j)

           j=1;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,0.0_cp,j)
           j=2;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,0.0_cp,j)
           j=5;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,0.0_cp,j)
           j=6;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,0.0_cp,j)

           j=1;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,0.0_cp,j)
           j=2;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,0.0_cp,j)
           j=3;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,0.0_cp,j)
           j=4;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,0.0_cp,j)
         enddo
       end subroutine

       subroutine thin_wall_face(B,m,face)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer,intent(in) :: face
         integer :: i
         do i=1,m%s
           select case (face)
           case (1); call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,0.0_cp,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,0.0_cp,face)
           case (2); call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,0.0_cp,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,0.0_cp,face)
           case (3); call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,0.0_cp,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,0.0_cp,face)
           case (4); call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,0.0_cp,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,0.0_cp,face)
           case (5); call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,0.0_cp,face)
                     call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,0.0_cp,face)
           case (6); call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,0.0_cp,face)
                     call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,0.0_cp,face)
           case default; stop 'Error: face must = 1:6 in thin_wall_face in init_BBCs.f90'
           end select
         enddo
       end subroutine

       subroutine thin_wall_Hunt(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         call pseudo_vacuum(B,m)
         call thin_wall_face(B,m,5)
         call thin_wall_face(B,m,6)
       end subroutine

       subroutine thin_wall_LDC(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         call pseudo_vacuum(B,m)
         call thin_wall_face(B,m,4)
       end subroutine

       end module