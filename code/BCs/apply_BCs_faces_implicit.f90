       module apply_BCs_faces_implicit_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use mesh_mod
       use check_BCs_mod
       use face_edge_corner_indexing_mod
       implicit none

       private
       public :: apply_BCs_faces_implicit

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_BCs_faces_implicit;  module procedure apply_BCs_faces_VF;     end interface
       interface apply_BCs_faces_implicit;  module procedure apply_BCs_faces_SF;     end interface

       contains

       subroutine apply_BCs_faces_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_faces_SF(U%x,m)
         call apply_BCs_faces_SF(U%y,m)
         call apply_BCs_faces_SF(U%z,m)
       end subroutine

       subroutine apply_BCs_faces_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: k
#ifdef _DEBUG_APPLY_BCS_
       call check_defined(U,m)
#endif
         do k = 1,6; call apply_face(U,m,k); enddo
       end subroutine

       subroutine apply_face(U,m,f)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: f
         integer,dimension(4) :: a
         integer :: i,j,k,p

         k = dir_given_face(f)
         a = adjacent_faces_given_dir(k)
         if (CC_along(U,k)) then
           do i=1,m%s
             ! if (any((/(U%RF(i)%b%f(a(j))%b%Periodic,j=1,4)/))) then; p = 0; else; p = 1; endif
             p = 0
             if (.not.m%g(i)%st_faces(f)%TF) call app_CC_SF(U%RF(i),f,p)
           enddo
         elseif (Node_along(U,k)) then
           do i=1,m%s
             ! if (any((/(U%RF(i)%b%f(a(j))%b%Periodic,j=1,4)/))) then; p = 0; else; p = 1; endif
             p = 0
             if (.not.m%g(i)%st_faces(f)%TF) call app_N_SF(U%RF(i),f,p)
           enddo
         else; stop 'Error: datatype not found in apply_BCs_faces.f90'
         endif
       end subroutine

       subroutine app_N_SF(RF,face,p)
         implicit none
         type(RealField),intent(inout) :: RF
         integer,intent(in) :: face,p
         call app_N_RF(RF%f,RF%s,face,RF%b%f(face)%b,1+p,RF%s(1)-p,RF%s(2)-p,RF%s(3)-p)
       end subroutine

       subroutine app_CC_SF(RF,face,p)
         implicit none
         type(RealField),intent(inout) :: RF
         integer,intent(in) :: face,p
         call app_CC_RF(RF%f,RF%s,face,RF%b%f(face)%b,1+p,RF%s(1)-p,RF%s(2)-p,RF%s(3)-p)
       end subroutine

       subroutine app_N_RF(f,s,face,b,p,x,y,z)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(bctype),intent(in) :: b
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face,p,x,y,z
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face) ! face
         case (1); call app_N(f(1,:,:),f(2,:,:),f(3,:,:),f(s(1)-2,:,:),b,p,y,z)
         case (3); call app_N(f(:,1,:),f(:,2,:),f(:,3,:),f(:,s(2)-2,:),b,p,x,z)
         case (5); call app_N(f(:,:,1),f(:,:,2),f(:,:,3),f(:,:,s(3)-2),b,p,x,y)
         case (2); call app_N(f(s(1),:,:),f(s(1)-1,:,:),f(s(1)-2,:,:),f(3,:,:),b,p,y,z)
         case (4); call app_N(f(:,s(2),:),f(:,s(2)-1,:),f(:,s(2)-2,:),f(:,3,:),b,p,x,z)
         case (6); call app_N(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,s(3)-2),f(:,:,3),b,p,x,y)
         end select
       end subroutine

       subroutine app_CC_RF(f,s,face,b,p,x,y,z)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face
         type(bctype),intent(in) :: b
         integer,intent(in) :: p,x,y,z
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face) ! face
         case (1); call app_CC(f(1,:,:),f(2,:,:),f(s(1)-1,:,:),b,p,y,z)
         case (3); call app_CC(f(:,1,:),f(:,2,:),f(:,s(2)-1,:),b,p,x,z)
         case (5); call app_CC(f(:,:,1),f(:,:,2),f(:,:,s(3)-1),b,p,x,y)
         case (2); call app_CC(f(s(1),:,:),f(s(1)-1,:,:),f(2,:,:),b,p,y,z)
         case (4); call app_CC(f(:,s(2),:),f(:,s(2)-1,:),f(:,2,:),b,p,x,z)
         case (6); call app_CC(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,2),b,p,x,y)
         end select
       end subroutine

       subroutine app_CC(ug,ui,ui_opp,b,p,x,y)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug
         real(cp),dimension(:,:),intent(in) :: ui,ui_opp
         type(bctype),intent(in) :: b
         integer,intent(in) :: p,x,y
         call a_CC(ug(p:x,p:y),ui(p:x,p:y),ui_opp(p:x,p:y),b)
       end subroutine

       subroutine app_N(ug,ub,ui,ui_opp,b,p,x,y)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug,ub
         real(cp),dimension(:,:),intent(in) :: ui,ui_opp
         type(bctype),intent(in) :: b
         integer,intent(in) :: p,x,y
         call a_N(ug(p:x,p:y),ub(p:x,p:y),ui(p:x,p:y),ui_opp(p:x,p:y),b)
       end subroutine

       subroutine a_CC(ug,ui,ui_opp,b)
         ! interpolated - (wall incoincident)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug
         real(cp),dimension(:,:),intent(in) :: ui,ui_opp
         type(bctype),intent(in) :: b
         if     (b%Dirichlet) then; ug = - ui
         elseif (b%Neumann) then;   ug = ui
         elseif (b%Periodic) then;  ug = ui_opp
         else; stop 'Error: Bad bctype! Caught in app_CC_imp in apply_BCs_faces_imp.f90'
         endif
       end subroutine

       subroutine a_N(ug,ub,ui,ui_opp,b)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug,ub
         real(cp),dimension(:,:),intent(in) :: ui,ui_opp
         type(bctype),intent(in) :: b
         if     (b%Dirichlet) then; ub = 0.0_cp; ug = - ui
         elseif (b%Neumann) then;   ug = ub ! implied 0 Neumann, needs mod for non-zero Neumann
         elseif (b%Periodic) then;  ug = ui_opp
         else; stop 'Error: Bad bctype! Caught in app_N_imp in apply_BCs_faces_imp.f90'
         endif
       end subroutine

       end module