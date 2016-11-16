       module init_BBCs_mod
       use current_precision_mod
       use BC_funcs_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use SF_mod
       use VF_mod
       implicit none

       private

       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false
       integer :: preDefinedB_BCs = 7
       real(cp) :: cw = 0.0_cp
       ! real(cp) :: cw = 0.05_cp
       ! real(cp) :: cw = 0.01_cp
       !                                      0 : B = 0
       !                                      1 : Psuedo-vaccuum BCs (dBn/dn = 0, B_tangential = 0)
       !                                      2 : Bandaru
       !                                      3 : Periodic duct flow
       !                                      4 : Thin wall

       public :: init_BBCs

       contains

       subroutine init_BBCs(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m

         call init_BC_mesh(B%x,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(B%y,m) ! MUST COME BEFORE BVAL ASSIGNMENT
         call init_BC_mesh(B%z,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Dirichlet_BCs(B,m)

         select case (preDefinedB_BCs)
         case (0); 
         case (1); call pseudo_vacuum(B,m)
         case (2); call initBandaru(B)
         case (3); call periodic_duct_flow(B,m)
         case (4); call thin_wall(B,m,cw)
         case (5); call thin_wall_LDC(B,m,cw)
         case (6); call thin_wall_Hunt(B,m,cw)
         case (7); call symmetric_zmax(B,m)
         case default; stop 'Error: preDefinedU_BCs must = 1:5 in init_UBCs in init_UBCs.f90'
         end select

         call make_periodic(B,m,periodic_dir)
         call init_BC_props(B)
       end subroutine

       subroutine symmetric_zmax(B,m)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,m%s
           call init_AntiSymmetric(B%x%BF(i)%BCs,6)
           call init_AntiSymmetric(B%y%BF(i)%BCs,6)
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

       subroutine initBandaru(B)
         implicit none
         type(VF),intent(inout) :: B
         call init_Neumann(B%x%BF(1)%BCs,5)
         call init_Neumann(B%x%BF(1)%BCs,6)
         call init(B%x%BF(1)%BCs,0.0_cp,5)
         call init(B%x%BF(1)%BCs,0.0_cp,6)
       end subroutine

       subroutine thin_wall(B,m,cw)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: cw
         integer :: i,j
         call pseudo_vacuum(B,m)
         do i=1,m%s
           j=3;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,cw,j)
           j=4;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,cw,j)
           j=5;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,cw,j)
           j=6;call init_Robin(B%x%BF(i)%BCs,j); call init(B%x%BF(i)%BCs,cw,j)

           j=1;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,cw,j)
           j=2;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,cw,j)
           j=5;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,cw,j)
           j=6;call init_Robin(B%y%BF(i)%BCs,j); call init(B%y%BF(i)%BCs,cw,j)

           j=1;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,cw,j)
           j=2;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,cw,j)
           j=3;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,cw,j)
           j=4;call init_Robin(B%z%BF(i)%BCs,j); call init(B%z%BF(i)%BCs,cw,j)
         enddo
       end subroutine

       subroutine thin_wall_face(B,m,cw,face)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: cw
         integer,intent(in) :: face
         integer :: i
         do i=1,m%s
           select case (face)
           case (1); call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,cw,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,cw,face)
           case (2); call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,cw,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,cw,face)
           case (3); call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,cw,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,cw,face)
           case (4); call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,cw,face)
                     call init_Robin(B%z%BF(i)%BCs,face); call init(B%z%BF(i)%BCs,cw,face)
           case (5); call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,cw,face)
                     call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,cw,face)
           case (6); call init_Robin(B%x%BF(i)%BCs,face); call init(B%x%BF(i)%BCs,cw,face)
                     call init_Robin(B%y%BF(i)%BCs,face); call init(B%y%BF(i)%BCs,cw,face)
           case default; stop 'Error: face must = 1:6 in thin_wall_face in init_BBCs.f90'
           end select
         enddo
       end subroutine

       subroutine thin_wall_Hunt(B,m,cw)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: cw
         call pseudo_vacuum(B,m)
         call thin_wall_face(B,m,cw,5)
         call thin_wall_face(B,m,cw,6)
       end subroutine

       subroutine thin_wall_LDC(B,m,cw)
         implicit none
         type(VF),intent(inout) :: B
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: cw
         call pseudo_vacuum(B,m)
         call thin_wall_face(B,m,cw,4)
       end subroutine

       end module