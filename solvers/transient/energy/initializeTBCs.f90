       module initializeTBCs_mod
       use grid_mod
       use BCs_mod
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

       integer,parameter :: unstableDir = 1 ! not yet working..
       !                                  1 : x
       !                                  2 : y
       !                                  3 : z
       integer,parameter :: unstableSign = 1 ! : (1,-1)  ! not yet working..


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

       subroutine initTBCs(T_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         if (preDefinedT_BCs.ne.0) then
           call initPreDefinedBCs(T_bcs,g)
         else
           call initUserTBCs(T_bcs,g)
         endif
         call setGrid(T_bcs,g)
         call checkBCs(T_bcs)
       end subroutine

       subroutine initPreDefinedBCs(T_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         integer :: Nx,Ny,Nz,neumann

         ! By default, BCs are insulating
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         neumann = 5
         
         select case (preDefinedT_BCs)
         case (1); call initInsulatingBCs(T_bcs,g)
         case (2); call initFixedBCs(T_bcs,g)
                   call hotFaceBC(T_bcs,g,hotFace)
         case (3); call initInsulatingBCs(T_bcs,g)
                   call hotFaceBC(T_bcs,g,hotFace)
                   call coldFaceBC(T_bcs,g,coldFace)

         case default
           write(*,*) 'Incorrect preDefinedT_BCs in initPreDefinedTfield';stop
         end select
       end subroutine

       subroutine initInsulatingBCs(T_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         integer :: Nx,Ny,Nz,neumann
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         neumann = 5
         call setAllZero(T_bcs,Nx,Ny,Nz,neumann)
       end subroutine

       subroutine hotFaceBC(T_bcs,g,face)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         integer,intent(in) :: face
         real(cp),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz,dirichlet
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         dirichlet = 2

         select case (face)
         case(1); allocate(bvals(Ny,Nz)); call setXMinType(T_bcs,dirichlet)
         case(2); allocate(bvals(Ny,Nz)); call setXMaxType(T_bcs,dirichlet)
         case(3); allocate(bvals(Nx,Nz)); call setYMinType(T_bcs,dirichlet)
         case(4); allocate(bvals(Nx,Nz)); call setYMaxType(T_bcs,dirichlet)
         case(5); allocate(bvals(Nx,Ny)); call setZMinType(T_bcs,dirichlet)
         case(6); allocate(bvals(Nx,Ny)); call setZMaxType(T_bcs,dirichlet)
         case default
         stop 'Error: face must = 1:6 in hotFaceBC in initializeTBCs.f90'
         end select
         bvals = real(1.0,cp)
         select case (face)
         case(1); call setXMinVals(T_bcs,bvals)
         case(2); call setXMaxVals(T_bcs,bvals)
         case(3); call setYMinVals(T_bcs,bvals)
         case(4); call setYMaxVals(T_bcs,bvals)
         case(5); call setZMinVals(T_bcs,bvals)
         case(6); call setZMaxVals(T_bcs,bvals)
         case default
         stop 'Error: face must = 1:6 in hotFaceBC in initializeTBCs.f90'
         end select
         deallocate(bvals)
       end subroutine

       subroutine coldFaceBC(T_bcs,g,face)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         integer,intent(in) :: face
         real(cp),dimension(:,:),allocatable :: bvals
         integer :: Nx,Ny,Nz,dirichlet
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         dirichlet = 2

         select case (face)
         case(1); allocate(bvals(Ny,Nz)); call setXMinType(T_bcs,dirichlet)
         case(2); allocate(bvals(Ny,Nz)); call setXMaxType(T_bcs,dirichlet)
         case(3); allocate(bvals(Nx,Nz)); call setYMinType(T_bcs,dirichlet)
         case(4); allocate(bvals(Nx,Nz)); call setYMaxType(T_bcs,dirichlet)
         case(5); allocate(bvals(Nx,Ny)); call setZMinType(T_bcs,dirichlet)
         case(6); allocate(bvals(Nx,Ny)); call setZMaxType(T_bcs,dirichlet)
         case default
         stop 'Error: face must = 1:6 in coldFaceBC in initializeTBCs.f90'
         end select
         bvals = real(0.0,cp)
         select case (face)
         case(1); call setXMinVals(T_bcs,bvals)
         case(2); call setXMaxVals(T_bcs,bvals)
         case(3); call setYMinVals(T_bcs,bvals)
         case(4); call setYMaxVals(T_bcs,bvals)
         case(5); call setZMinVals(T_bcs,bvals)
         case(6); call setZMaxVals(T_bcs,bvals)
         case default
         stop 'Error: face must = 1:6 in coldFaceBC in initializeTBCs.f90'
         end select
         deallocate(bvals)
       end subroutine

       subroutine initFixedBCs(T_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         integer :: Nx,Ny,Nz,dirichlet
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         dirichlet = 2
         call setAllZero(T_bcs,Nx,Ny,Nz,dirichlet)
       end subroutine

       subroutine initUserTBCs(T_bcs,g)
         implicit none
         type(grid),intent(in) :: g
         type(BCs),intent(inout) :: T_bcs
         integer :: Nx,Ny,Nz,neumann
         Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
         neumann = 5
         call setAllZero(T_bcs,Nx,Ny,Nz,neumann)
       end subroutine

       end module