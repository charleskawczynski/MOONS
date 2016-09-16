       module init_TBCs_mod
       use current_precision_mod
       use grid_mod
       use mesh_mod
       use BCs_mod
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
         integer :: i,k,pd
         call init_BC_mesh(T,m) ! MUST COME BEFORE BVAL ASSIGNMENT

         do i=1,m%s
           call init_Neumann(T%RF(i)%b); call init(T%RF(i)%b,0.0_cp)
         enddo
         T%all_Neumann = .false. ! Needs to be adjusted manually

         select case (preDefinedT_BCs)
         case (0) ! Default insulating
         case (1); call initFixedBCs(T%RF(1)%b)
                   call hotFaceBC(T%RF(1)%b,hotFace)
         case (2); call initInsulatingBCs(T%RF(1)%b)
                   call hotFaceBC(T%RF(1)%b,hotFace)
                   call coldFaceBC(T%RF(1)%b,coldFace)
         case (3); call duct_for_Yi(T)
         case default; stop 'Error: preDefinedT_BCs must = 1 in init_TBCs in init_TBCs.f90.'
         end select

         do i=1,m%s; do k=1,3
           pd = periodic_dir(k)
           if ((pd.ne.1).and.(pd.ne.0)) stop 'Error: periodic_dir must = 1,0 in init_TBCs in init_TBCs.f90'
           if (pd.eq.1) call makePeriodic(T%RF(i)%b,k)
         enddo; enddo
       end subroutine

       subroutine initInsulatingBCs(T_bcs)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         call init_Neumann(T_bcs)
         call init(T_bcs,0.0_cp)
       end subroutine

       subroutine duct_for_Yi(T)
         implicit none
         type(SF),intent(inout) :: T
         call init_Neumann(T%RF(1)%b,1); call init(T%RF(1)%b,-1.0_cp)
         call init_Neumann(T%RF(1)%b,2)
         call init_Dirichlet(T%RF(1)%b,3)
         call init_Neumann(T%RF(1)%b,4)
       end subroutine

       subroutine hotFaceBC(T_bcs,face)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         integer,intent(in) :: face
         call init_Dirichlet(T_bcs,face)
         call init(T_bcs,1.0_cp,face)
       end subroutine

       subroutine coldFaceBC(T_bcs,face)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         integer,intent(in) :: face
         call init_Dirichlet(T_bcs,face)
         call init(T_bcs,0.0_cp,face)
       end subroutine

       subroutine initFixedBCs(T_bcs)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         call init_Dirichlet(T_bcs)
         call init(T_bcs,0.0_cp)
       end subroutine

       subroutine makePeriodic(T_bcs,dir)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         integer,intent(in) :: dir
         select case (dir)
         case (1); call init_periodic(T_bcs,1)
                   call init_periodic(T_bcs,2)
         case (2); call init_periodic(T_bcs,3)
                   call init_periodic(T_bcs,4)
         case (3); call init_periodic(T_bcs,5)
                   call init_periodic(T_bcs,6)
         case default; stop 'Error: dir must = 1,2,3 in makePeriodic in init_TBCs.f90'
         end select
       end subroutine

       end module