       module init_TBCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use boundary_conditions_mod
       use BC_funcs_mod
       use SF_mod
       implicit none
       ! From applyBCs.f90:
       ! bctype = 1 ! Dirichlet - direct - wall coincident
       ! bctype = 2 ! Dirichlet - interpolated - wall incoincident
       ! bctype = 3 ! Neumann - direct - wall coincident ~O(dh^2)
       ! bctype = 4 ! Neumann - direct - wall coincident ~O(dh)
       ! bctype = 5 ! Neumann - interpolated - wall incoincident ~O(dh)

       private

       integer,dimension(3) :: periodic_dir = (/0,0,0/) ! 1 = true, else false

       integer,parameter :: preDefinedT_BCs = 1
       !                                      0 : Insulated (dT/dn = 0)
       !                                      1 : Fixed (T = T_wall)
       !                                      2 : Cold Top, Hot Bottom (y), insulating walls
       !                                      3 : Yi's Duct

       integer,parameter :: hotFace         = 4
       !                                      1 {x_min}
       !                                      2 {x_max}
       !                                      3 {y_min}
       !                                      4 {y_max}
       !                                      5 {z_min}
       !                                      6 {z_max}

       integer,parameter :: coldFace         = 3
       !                                      1 {x_min}
       !                                      2 {x_max}
       !                                      3 {y_min}
       !                                      4 {y_max}
       !                                      5 {z_min}
       !                                      6 {z_max}

       public :: init_TBCs

       contains


       subroutine init_TBCs(T,m)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         call init_BC_mesh(T,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         call Neumann_BCs(T,m)
         T%all_Neumann = .false. ! Needs to be adjusted manually

         select case (preDefinedT_BCs)
         case (0) ! Default insulating
         case (1); call initFixedBCs(T,m)
                   call hotFaceBC(T,m,hotFace)
         case (2); call initInsulatingBCs(T,m)
                   call hotFaceBC(T,m,hotFace)
                   call coldFaceBC(T,m,coldFace)
         case (3); call duct_for_Yi(T,m)
         case default; stop 'Error: preDefinedT_BCs must = 1 in init_TBCs in init_TBCs.f90.'
         end select
         call make_periodic(T,m,periodic_dir)
       end subroutine

       subroutine initInsulatingBCs(T,m)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         call init_Neumann(T%BF(1)%BCs,m%B(1))
         call init(T%BF(1)%BCs,0.0_cp)
       end subroutine

       subroutine duct_for_Yi(T,m)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         call init_Neumann(T%BF(1)%BCs,m%B(1),1); call init(T%BF(1)%BCs,-1.0_cp)
         call init_Neumann(T%BF(1)%BCs,m%B(1),2)
         call init_Neumann(T%BF(1)%BCs,m%B(1),4)
         call init_Dirichlet(T%BF(1)%BCs,m%B(1),3)
       end subroutine

       subroutine hotFaceBC(T,m,face)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         integer,intent(in) :: face
         call init_Dirichlet(T%BF(1)%BCs,m%B(1),face)
         call init(T%BF(1)%BCs,1.0_cp,face)
       end subroutine

       subroutine coldFaceBC(T,m,face)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         integer,intent(in) :: face
         call init_Dirichlet(T%BF(1)%BCs,m%B(1),face)
         call init(T%BF(1)%BCs,0.0_cp,face)
       end subroutine

       subroutine initFixedBCs(T,m)
         implicit none
         type(SF),intent(inout) :: T
         type(mesh),intent(in) :: m
         call init_Dirichlet(T%BF(1)%BCs,m%B(1))
         call init(T%BF(1)%BCs,0.0_cp)
       end subroutine

       end module