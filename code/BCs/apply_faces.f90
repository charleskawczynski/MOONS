       module apply_faces_mod
       ! Pre-processor directives: (_DEBUG_APPLYBCS_)
       ! 
       ! Making BCs is a 3 step process:
       ! 
       !       1) Set grid / shape
       !             call init(BCs,g,s)
       !       2) Set type (can use grid information)
       !             call init_Dirichlet(BCs); call init_Dirichlet(BCs,face)
       !             call init_Neumann(BCs);   call init_Neumann(BCs,face)
       !             call init_periodic(BCs);  call init_periodic(BCs,face)
       !       3) Set values
       !             call init(BCs,0.0)       (default)
       !             call init(BCs,0.0,face)
       !             call init(BCs,vals,face)
       ! 
       ! IMPORTANT NOTES:
       ! 
       ! There are two types of Neumann BCs.
       ! 
       ! 1) Explicit Neuamann
       !       - Uses one sided difference stencil to compute 
       !         boundary value, then extrapolates to ghost
       ! 
       ! 2) Implicit Neuamann
       !       - Only computes ghost values
       ! 
       ! Which one to use?
       !     - Use the Explicit Neumann when both
       !              - Data is wall coincident
       !              - Matrix inversion is not used
       !     - Use Implicit Neumann when
       !              - Data is wall incoincident
       ! 

       use SF_mod
       use VF_mod
       use BCs_mod
       use grid_mod
       use mesh_mod
       implicit none

       private
       public :: apply_faces

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       interface apply_faces;       module procedure apply_faces_VF;     end interface
       interface apply_faces;       module procedure apply_faces_SF;     end interface

       contains

       subroutine apply_faces_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_faces(U%x,m)
         call apply_faces(U%y,m)
         call apply_faces(U%z,m)
       end subroutine

       subroutine apply_faces_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: k
         do k=1,3; call apply_face_SF(U,m,k); enddo
       end subroutine

       subroutine apply_face_SF(U,m,dir)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         if (CC_along(dir)) then
           select case (dir)
           case (1); do i=1,m%s
                       if (.not.m%g(i)%st_face%hmin(dir)) call apply_face_CC_RF(U%RF(i),m%g(i),1)
                       if (.not.m%g(i)%st_face%hmax(dir)) call apply_face_CC_RF(U%RF(i),m%g(i),2)
                     enddo
           case (2); do i=1,m%s
                       if (.not.m%g(i)%st_face%hmin(dir)) call apply_face_CC_RF(U%RF(i),m%g(i),3)
                       if (.not.m%g(i)%st_face%hmax(dir)) call apply_face_CC_RF(U%RF(i),m%g(i),4)
                     enddo
           case (3); do i=1,m%s
                       if (.not.m%g(i)%st_face%hmin(dir)) call apply_face_CC_RF(U%RF(i),m%g(i),5)
                       if (.not.m%g(i)%st_face%hmax(dir)) call apply_face_CC_RF(U%RF(i),m%g(i),6)
                     enddo
           end select
         endif
         elseif (Node_along(dir)) then
           select case (dir)
           case (1); do i=1,m%s
                       if (.not.m%g(i)%st_face%hmin(dir)) call apply_face_N_RF(U%RF(i),m%g(i),1)
                       if (.not.m%g(i)%st_face%hmax(dir)) call apply_face_N_RF(U%RF(i),m%g(i),2)
                     enddo
           case (2); do i=1,m%s
                       if (.not.m%g(i)%st_face%hmin(dir)) call apply_face_N_RF(U%RF(i),m%g(i),3)
                       if (.not.m%g(i)%st_face%hmax(dir)) call apply_face_N_RF(U%RF(i),m%g(i),4)
                     enddo
           case (3); do i=1,m%s
                       if (.not.m%g(i)%st_face%hmin(dir)) call apply_face_N_RF(U%RF(i),m%g(i),5)
                       if (.not.m%g(i)%st_face%hmax(dir)) call apply_face_N_RF(U%RF(i),m%g(i),6)
                     enddo
           end select
         else; stop 'Error: datatype not found in applyBCs.f90'
       end subroutine

       subroutine apply_face_N_RF(U,g,face)
         implicit none
         type(realField),intent(inout) :: U
         type(grid),intent(in) :: g
         integer,intent(in) :: face
         ! For readability, the faces are traversed in the order:
         !       {1,2,3,4,5,6} = (x_min,x_max,y_min,y_max,z_min,z_max)
         !
         ! apply_face_Node(ug,ub,ui,ui_opp,bvals,bctype,dh)
         select case (face)
         case (1); call apply_face_Node(RF%f(1,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(2,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(3,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(RF%s(1)-2,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%b%vals,RF%b%bctype,g%c(1)%dhn(1))
         case (2); call apply_face_Node(RF%f(RF%s(1),2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(RF%s(1)-1,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(RF%s(1)-2,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(3,2:RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%b%vals,RF%b%bctype,g%c(1)%dhn(g%c(1)%sn-1))
         case (3); call apply_face_Node(RF%f(2:RF%s(1)-1,1,2:RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,2,2:RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,3,2:RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,RF%s(2)-2,2:RF%s(3)-1),&
                                        RF%b%vals,RF%b%bctype,g%c(2)%dhn(1))
         case (4); call apply_face_Node(RF%f(2:RF%s(1)-1,RF%s(2),2:RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,RF%s(2)-1,2:RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,RF%s(2)-2,2:RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,3,2:RF%s(3)-1),&
                                        RF%b%vals,RF%b%bctype,g%c(2)%dhn(g%c(2)%sn-1))
         case (5); call apply_face_Node(RF%f(2:RF%s(1)-1,2:RF%s(2)-1,1),&
                                        RF%f(2:RF%s(1)-1,2:RF%s(2)-1,2),&
                                        RF%f(2:RF%s(1)-1,2:RF%s(2)-1,3),&
                                        RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)-2),&
                                        RF%b%vals,RF%b%bctype,g%c(3)%dhn(1))
         case (6); call apply_face_Node(RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)),&
                                        RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)-1),&
                                        RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)-2),&
                                        RF%f(2:RF%s(1)-1,2:RF%s(2)-1,3),&
                                        RF%b%vals,RF%b%bctype,g%c(3)%dhn(g%c(3)%sn-1))
         end select
       end subroutine

       subroutine apply_face_CC_RF(U,g,face)
         implicit none
         type(realField),intent(inout) :: U
         type(grid),intent(in) :: g
         integer,intent(in) :: face
         ! For readability, the faces are traversed in the order:
         !       {1,2,3,4,5,6} = (x_min,x_max,y_min,y_max,z_min,z_max)
         !
         ! apply_face_CC(ug,ui,ui_opp,bvals,bctype,dh)
         select case (face)
         case (1); call apply_face_CC(RF%f(1,2:RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%f(2,2:RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%f(RF%s(1)-1,2:RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%b%vals,RF%b%bctype,g%c(1)%dhc(1))
         case (2); call apply_face_CC(RF%f(RF%s(1),2:RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%f(RF%s(1)-1,2:RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%f(2,2:RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%b%vals,RF%b%bctype,g%c(1)%dhc(g%c(1)%sc-1))
         case (3); call apply_face_CC(RF%f(2:RF%s(1)-1,1,2:RF%s(3)-1),&
                                      RF%f(2:RF%s(1)-1,2,2:RF%s(3)-1),&
                                      RF%f(2:RF%s(1)-1,RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%b%vals,RF%b%bctype,g%c(2)%dhc(1))
         case (4); call apply_face_CC(RF%f(2:RF%s(1)-1,RF%s(2),2:RF%s(3)-1),&
                                      RF%f(2:RF%s(1)-1,RF%s(2)-1,2:RF%s(3)-1),&
                                      RF%f(2:RF%s(1)-1,2,2:RF%s(3)-1),&
                                      RF%b%vals,RF%b%bctype,g%c(2)%dhc(g%c(2)%sc-1))
         case (5); call apply_face_CC(RF%f(2:RF%s(1)-1,2:RF%s(2)-1,1),&
                                      RF%f(2:RF%s(1)-1,2:RF%s(2)-1,2),&
                                      RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)-1),&
                                      RF%b%vals,RF%b%bctype,g%c(3)%dhc(1))
         case (6); call apply_face_CC(RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)),&
                                      RF%f(2:RF%s(1)-1,2:RF%s(2)-1,RF%s(3)-1),&
                                      RF%f(2:RF%s(1)-1,2:RF%s(2)-1,2),&
                                      RF%b%vals,RF%b%bctype,g%c(3)%dhc(g%c(3)%sc-1))
         end select
       end subroutine

       subroutine apply_face_Node(ug,ub,ui,ui_opp,bvals,bctype,dh)
         implicit none
         real(cp),intent(inout),dimension(:,:) :: ug,ub,ui,ui_opp
         real(cp),dimension(:,:),intent(in) :: bvals
         real(cp),intent(in) :: dh
         integer,intent(in) :: bctype
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ub = bvals; ug = 2.0_cp*bvals - ui ! Dirichlet - direct - wall coincident
         ! *************************** NEUMANN *****************************
         case (3); ub = ui; ug = ui                   ! Explicit Neumann - direct - wall coincident ~O(dh)?
         case (4); ug = ui - 2.0_cp*bvals*dh          ! Implicit Neumann - direct - wall coincident ~O(dh^2)
         ! *************************** PERIODIC *****************************
         case (6); ub = unb_opp; ug = ui_opp          ! Periodic - direct - wall coincident ~O(dh)
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       subroutine apply_face_CC(ug,ui,ui_opp,bvals,bctype,dh)
         implicit none
         real(cp),intent(inout),dimension(:,:) :: ug,ui,ui_opp
         real(cp),dimension(:,:),intent(in) :: bvals
         real(cp),intent(in) :: dh
         integer,intent(in) :: bctype
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ug = 2.0_cp*bvals - ui ! Dirichlet - interpolated - wall incoincident
         ! *************************** NEUMANN *****************************
         case (2); ug = ui + dh*bvals     ! Implicit Neumann - interpolated - wall incoincident ~O(dh)
         ! *************************** PERIODIC *****************************
         case (3); ug = ui_opp            ! Periodic - interpolated - wall incoincident ~O(dh)
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       end module