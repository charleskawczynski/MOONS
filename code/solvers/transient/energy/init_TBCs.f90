       module init_TBCs_mod
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

       integer,parameter :: preDefinedT_BCs = 3
       !                                      0 : User-defined case (no override)
       !                                      1 : Insulated (dT/dn = 0)
       !                                      2 : Fixed (T = T_wall)
       !                                      3 : Cold Top, Hot Bottom (y), insulating walls

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


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: initTBCs

       contains

       subroutine initTBCs(T,m)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: T

         call init(T%RF(1)%b,m%g(1),T%RF(1)%s)

         if (preDefinedT_BCs.ne.0) then
           call initPreDefinedBCs(T%RF(1)%b)
         else
           call initUserTBCs(T%RF(1)%b)
         endif
       end subroutine

       subroutine initPreDefinedBCs(T_bcs)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         select case (preDefinedT_BCs)
         case (1); call initInsulatingBCs(T_bcs)
         case (2); call initFixedBCs(T_bcs)
                   call hotFaceBC(T_bcs,hotFace)
         case (3); call initInsulatingBCs(T_bcs)
                   call hotFaceBC(T_bcs,hotFace)
                   call coldFaceBC(T_bcs,coldFace)

         case default
           write(*,*) 'Incorrect preDefinedT_BCs in initPreDefinedTfield';stop
         end select
       end subroutine

       subroutine initInsulatingBCs(T_bcs)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         call init_Neumann(T_bcs)
         call init(T_bcs,0.0_cp)
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

       subroutine initUserTBCs(T_bcs)
         implicit none
         type(BCs),intent(inout) :: T_bcs
         call init_Neumann(T_bcs)
         call init(T_bcs,0.0_cp)
       end subroutine

       end module