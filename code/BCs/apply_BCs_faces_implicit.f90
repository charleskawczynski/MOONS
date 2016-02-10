       module apply_BCs_faces_implicit_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use mesh_mod
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
         call apply_face_SF(U,m,1,(/1,2/)) ! SF, mesh, direction, faces along dir
         call apply_face_SF(U,m,2,(/3,4/)) ! SF, mesh, direction, faces along dir
         call apply_face_SF(U,m,3,(/5,6/)) ! SF, mesh, direction, faces along dir
       end subroutine

       subroutine apply_face_SF(U,m,dir,f)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer,dimension(2) :: f
         integer :: i

#ifdef _DEBUG_APPLY_BCS_
       call checkBCs(U,f,m)
#endif

         if (CC_along(U,dir)) then
         do i=1,m%s
         if (.not.m%g(i)%st_face%hmin(dir)) call app_CC_RF(U%RF(i)%f,U%RF(i)%s,f(1),U%RF(i)%b%f(f(1))%b)
         if (.not.m%g(i)%st_face%hmax(dir)) call app_CC_RF(U%RF(i)%f,U%RF(i)%s,f(2),U%RF(i)%b%f(f(2))%b)
         enddo
         elseif (Node_along(U,dir)) then
         do i=1,m%s
         if (.not.m%g(i)%st_face%hmin(dir)) call app_N_RF(U%RF(i)%f,U%RF(i)%s,f(1),U%RF(i)%b%f(f(1))%b)
         if (.not.m%g(i)%st_face%hmax(dir)) call app_N_RF(U%RF(i)%f,U%RF(i)%s,f(2),U%RF(i)%b%f(f(2))%b)
         enddo
         else; stop 'Error: datatype not found in apply_BCs_faces_imp.f90'
         endif
       end subroutine

       subroutine app_N_RF(f,s,face,b)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(bctype),intent(in) :: b
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face) ! face
         case (1); call app_N(f(1,:,:),f(2,:,:),f(3,:,:),f(s(1)-2,:,:),b)
         case (3); call app_N(f(:,1,:),f(:,2,:),f(:,3,:),f(:,s(2)-2,:),b)
         case (5); call app_N(f(:,:,1),f(:,:,2),f(:,:,3),f(:,:,s(3)-2),b)
         case (2); call app_N(f(s(1),:,:),f(s(1)-1,:,:),f(s(1)-2,:,:),f(3,:,:),b)
         case (4); call app_N(f(:,s(2),:),f(:,s(2)-1,:),f(:,s(2)-2,:),f(:,3,:),b)
         case (6); call app_N(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,s(3)-2),f(:,:,3),b)
         end select
       end subroutine

       subroutine app_CC_RF(f,s,face,b)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face
         type(bctype),intent(in) :: b
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face) ! face
         case (1); call app_CC(f(1,:,:),f(2,:,:),f(s(1)-1,:,:),b)
         case (3); call app_CC(f(:,1,:),f(:,2,:),f(:,s(2)-1,:),b)
         case (5); call app_CC(f(:,:,1),f(:,:,2),f(:,:,s(3)-1),b)
         case (2); call app_CC(f(s(1),:,:),f(s(1)-1,:,:),f(2,:,:),b)
         case (4); call app_CC(f(:,s(2),:),f(:,s(2)-1,:),f(:,2,:),b)
         case (6); call app_CC(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,2),b)
         end select
       end subroutine

       subroutine app_CC(ug,ui,ui_opp,b)
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

       subroutine app_N(ug,ub,ui,ui_opp,b)
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

#ifdef _DEBUG_APPLY_BCS_
       subroutine checkBCs(U,f,m)
         implicit none
         type(SF),intent(in) :: U
         integer,dimension(2) :: f
         type(mesh),intent(in) :: m
         integer :: i
         do i=1,m%s
           if (.not.U%RF(i)%b%f(f(1))%b%defined) stop 'Error: bad bctype in checkBCs in apply_BCs_faces_imp.f90'
           if (.not.U%RF(i)%b%f(f(2))%b%defined) stop 'Error: bad bctype in checkBCs in apply_BCs_faces_imp.f90'
         enddo
       end subroutine
#endif

       end module