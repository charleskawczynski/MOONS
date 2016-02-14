       module apply_BCs_faces_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use grid_mod
       use mesh_mod
       implicit none

       private
       public :: apply_BCs_faces

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       interface apply_BCs_faces;       module procedure apply_BCs_faces_VF;     end interface
       interface apply_BCs_faces;       module procedure apply_BCs_faces_SF;     end interface

       contains

       subroutine apply_BCs_faces_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_faces(U%x,m)
         call apply_BCs_faces(U%y,m)
         call apply_BCs_faces(U%z,m)
       end subroutine

       function get_dir_from_face(face) result(dir)
         implicit none
         integer,intent(in) :: face
         integer :: dir
         select case (face)
         case (1,2); dir = 1
         case (3,4); dir = 2
         case (5,6); dir = 3
         case default; stop 'Error: face must = 1:6 in get_dir_from_face in apply_BCs_faces.f90'
         end select
       end function

       subroutine apply_face_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,f

#ifdef _DEBUG_APPLY_BCS_
       call checkBCs(U,f,m)
#endif
         do f=1,6
         k = get_dir_from_face(f)
         if (CC_along(U,k)) then
         do i=1,m%s; if (.not.m%g(i)%st_faces(f)) call a_CC(U%RF(i),f,m%g(i)%c(k)%dhc(1),m%g(i)%c(k)%dhc_e); enddo
         elseif (Node_along(U,k)) then
         do i=1,m%s; if (.not.m%g(i)%st_faces(f)) call a_N(U%RF(i),f,m%g(i)%c(k)%dhn(1),m%g(i)%c(k)%dhn_e); enddo
         else; stop 'Error: datatype not found in apply_BCs_faces.f90'
         endif
         enddo
       end subroutine

       subroutine a_N(RF,face,dh1,dhe)
         implicit none
         type(RealField),intent(inout) :: RF
         integer,intent(in) :: face
         real(cp),intent(in) :: dh1,dhe
         call app_N_RF(RF%f,RF%s,face,RF%b%f(face)%vals,RF%b%f(face)%b,dh1,dhe)
       end subroutine

       subroutine a_CC(RF,face,dh1,dhe)
         implicit none
         type(RealField),intent(inout) :: RF
         integer,intent(in) :: face
         real(cp),intent(in) :: dh1,dhe
         call app_CC_RF(RF%f,RF%s,face,RF%b%f(face)%vals,RF%b%f(face)%b,dh1,dhe)
       end subroutine

       subroutine app_N_RF(f,s,face,v,b,dh1,dhe)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),intent(in) :: dh1,dhe
         real(cp),dimension(:,:),intent(in) :: v
         type(bctype),intent(in) :: b
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face) ! face
         case (1); call app_N(f(1,:,:),f(2,:,:),f(3,:,:),f(s(1)-1,:,:),f(s(1)-2,:,:),v,-dh1,b)
         case (3); call app_N(f(:,1,:),f(:,2,:),f(:,3,:),f(:,s(2)-1,:),f(:,s(2)-2,:),v,-dh1,b)
         case (5); call app_N(f(:,:,1),f(:,:,2),f(:,:,3),f(:,:,s(3)-1),f(:,:,s(3)-2),v,-dh1,b)
         case (2); call app_N(f(s(1),:,:),f(s(1)-1,:,:),f(s(1)-2,:,:),f(2,:,:),f(3,:,:),v,dhe,b)
         case (4); call app_N(f(:,s(2),:),f(:,s(2)-1,:),f(:,s(2)-2,:),f(:,2,:),f(:,3,:),v,dhe,b)
         case (6); call app_N(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,s(3)-2),f(:,:,2),f(:,:,3),v,dhe,b)
         end select
       end subroutine

       subroutine app_CC_RF(f,s,face,v,b,dh1,dhe)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),intent(in) :: dh1,dhe
         real(cp),dimension(:,:),intent(in) :: v
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face
         type(bctype),intent(in) :: b
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face) ! face
         case (1); call app_CC(f(1,:,:),f(2,:,:),f(s(1)-1,:,:),v,-dh1,b)
         case (3); call app_CC(f(:,1,:),f(:,2,:),f(:,s(2)-1,:),v,-dh1,b)
         case (5); call app_CC(f(:,:,1),f(:,:,2),f(:,:,s(3)-1),v,-dh1,b)
         case (2); call app_CC(f(s(1),:,:),f(s(1)-1,:,:),f(2,:,:),v,dhe,b)
         case (4); call app_CC(f(:,s(2),:),f(:,s(2)-1,:),f(:,2,:),v,dhe,b)
         case (6); call app_CC(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,2),v,dhe,b)
         end select
       end subroutine

       subroutine app_CC(ug,ui,ui_opp,bvals,dh,b)
         ! interpolated - (wall incoincident)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug
         real(cp),dimension(:,:),intent(in) :: ui,ui_opp,bvals
         real(cp),intent(in) :: dh
         type(bctype),intent(in) :: b
#ifdef _DEBUG_APPLY_BCS_
         call check_dimensions(ug,bvals)
         call check_dimensions(ui,bvals)
#endif
         if     (b%Dirichlet) then; ug = 2.0_cp*bvals - ui
         elseif (b%Neumann) then;   ug = ui - dh*bvals
         elseif (b%Periodic) then;  ug = ui_opp
         else; stop 'Error: Bad bctype! Caught in app_CC in apply_BCs_faces.f90'
         endif
       end subroutine

       subroutine app_N(ug,ub,ui,ub_opp,ui_opp,bvals,dh,b)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug,ub
         real(cp),dimension(:,:),intent(in) :: ui,ub_opp,ui_opp,bvals
         real(cp),intent(in) :: dh
         type(bctype),intent(in) :: b
#ifdef _DEBUG_APPLY_BCS_
         call check_dimensions(ub,bvals)
         call check_dimensions(ui,bvals)
#endif
         if     (b%Dirichlet) then; ub = bvals; ug = 2.0_cp*ub - ui
         elseif (b%Neumann) then;   ug = ui + 2.0_cp*bvals*dh
         elseif (b%Periodic) then;  ub = ub_opp; ug = ui_opp
         else; stop 'Error: Bad bctype! Caught in app_N in apply_BCs_faces.f90'
         endif
       end subroutine

#ifdef _DEBUG_APPLY_BCS_
       subroutine checkBCs(U,f,m)
         implicit none
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,dimension(2) :: f
         integer :: i
         do i=1,m%s
           if (.not.U%RF(i)%b%f(f(1))%b%defined) stop 'Error: bad bctype in checkBCs in apply_BCs_faces_imp.f90'
           if (.not.U%RF(i)%b%f(f(2))%b%defined) stop 'Error: bad bctype in checkBCs in apply_BCs_faces_imp.f90'
         enddo
       end subroutine

       subroutine check_dimensions(f,g)
         implicit none
         real(cp),dimension(:,:),intent(in) :: f,g
         integer,dimension(2) :: sf,sg
         sf = shape(f); sg = shape(g)
         if ((sf(1).ne.sg(1)).or.(sf(2).ne.sg(2))) then
           write(*,*) 'sf = ',sf
           write(*,*) 'sg = ',sg
           stop 'Error: shapes do not match in check_dimensions in apply_BCs_faces.f90'
         endif
       end subroutine
#endif

       end module