       module apply_BCs_faces_mod
       use current_precision_mod
       use GF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use grid_mod
       use mesh_mod
       use check_BCs_mod
       use face_mod
       use apply_BCs_faces_raw_mod
       use face_edge_corner_indexing_mod
       implicit none

       private
       public :: apply_BCs_faces

       interface apply_BCs_faces;       module procedure apply_BCs_faces_VF;     end interface
       interface apply_BCs_faces;       module procedure apply_BCs_faces_SF;     end interface

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
       call check_defined(U)
#endif
         ! do k = 1,6; call apply_face(U,m,k); enddo

         ! For periodic in x:
         do k = 3,6; call apply_face(U,m,k); enddo
         call apply_face(U,m,1)
         call apply_face(U,m,2)
       end subroutine

       subroutine apply_face(U,m,f)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: f
         integer :: i,k,p
         integer,dimension(4) :: a

         k = dir_given_face(f)
         a = adj_faces_given_dir(k)
         if (U%CC_along(k)) then
           do i=1,m%s
             ! The following if does not satisfy momentum BCs for the 2D LDC...
             ! if (any((/(U%GF(i)%b%f(a(j))%b%Periodic,j=1,4)/))) then; p = 0; else; p = 1; endif
             p = 0
             if (.not.m%g(i)%st_faces(f)%TF) then
               call app_CC_SF(U%GF(i),f,m%g(i)%c(k)%dhc(1),m%g(i)%c(k)%dhc_e,U%GF(i)%s,p)
             endif
           enddo
         elseif (U%N_along(k)) then
           do i=1,m%s
             ! The following if does not satisfy momentum BCs for the 2D LDC...
             ! if (any((/(U%GF(i)%b%f(a(j))%b%Periodic,j=1,4)/))) then; p = 0; else; p = 1; endif
             p = 0
             if (.not.m%g(i)%st_faces(f)%TF) then
               call app_N_SF(U%GF(i),f,m%g(i)%c(k)%dhn(1),m%g(i)%c(k)%dhn_e,U%GF(i)%s,p)
             endif
           enddo
         else; stop 'Error: datatype not found in apply_BCs_faces.f90'
         endif
       end subroutine

       subroutine app_N_SF(GF,face,dh1,dhe,s,p)
         implicit none
         type(grid_field),intent(inout) :: GF
         integer,intent(in) :: face,p
         real(cp),intent(in) :: dh1,dhe
         integer,dimension(3),intent(in) :: s
         call app_N_GF(GF%f,face,GF%b%f(face),GF%b%f(face)%b,dh1,dhe,s(1),s(2),s(3),p)
       end subroutine

       subroutine app_CC_SF(GF,face,dh1,dhe,s,p)
         implicit none
         type(grid_field),intent(inout) :: GF
         integer,intent(in) :: face,p
         real(cp),intent(in) :: dh1,dhe
         integer,dimension(3),intent(in) :: s
         call app_CC_GF(GF%f,face,GF%b%f(face),GF%b%f(face)%b,dh1,dhe,s(1),s(2),s(3),p)
       end subroutine

       subroutine app_N_GF(f,face_ID,v,b,dh1,dhe,x,y,z,p)
         implicit none
         integer,intent(in) :: x,y,z,p
         real(cp),dimension(x,y,z),intent(inout) :: f
         real(cp),intent(in) :: dh1,dhe
         type(face),intent(in) :: v
         type(bctype),intent(in) :: b
         integer,intent(in) :: face_ID
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face_ID) ! face
         case (1); call a_N(f(1,:,:),f(2,:,:),f(3,:,:),f(x-2,:,:),v%vals,dh1,b,-1.0_cp,y,z,p)
         case (3); call a_N(f(:,1,:),f(:,2,:),f(:,3,:),f(:,y-2,:),v%vals,dh1,b,-1.0_cp,x,z,p)
         case (5); call a_N(f(:,:,1),f(:,:,2),f(:,:,3),f(:,:,z-2),v%vals,dh1,b,-1.0_cp,x,y,p)
         case (2); call a_N(f(x,:,:),f(x-1,:,:),f(x-2,:,:),f(3,:,:),v%vals,dhe,b,+1.0_cp,y,z,p)
         case (4); call a_N(f(:,y,:),f(:,y-1,:),f(:,y-2,:),f(:,3,:),v%vals,dhe,b,+1.0_cp,x,z,p)
         case (6); call a_N(f(:,:,z),f(:,:,z-1),f(:,:,z-2),f(:,:,3),v%vals,dhe,b,+1.0_cp,x,y,p)
         end select
       end subroutine

       subroutine app_CC_GF(f,face_ID,v,b,dh1,dhe,x,y,z,p)
         implicit none
         integer,intent(in) :: x,y,z,p
         real(cp),dimension(x,y,z),intent(inout) :: f
         real(cp),intent(in) :: dh1,dhe
         type(face),intent(in) :: v
         integer,intent(in) :: face_ID
         type(bctype),intent(in) :: b
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         select case (face_ID) ! face
         case (1); call a_CC(f(1,:,:),f(2,:,:),f(x-1,:,:),v%vals,dh1,b,1.0_cp,y,z,p)
         case (3); call a_CC(f(:,1,:),f(:,2,:),f(:,y-1,:),v%vals,dh1,b,1.0_cp,x,z,p)
         case (5); call a_CC(f(:,:,1),f(:,:,2),f(:,:,z-1),v%vals,dh1,b,1.0_cp,x,y,p)
         case (2); call a_CC(f(x,:,:),f(x-1,:,:),f(2,:,:),v%vals,dhe,b,1.0_cp,y,z,p)
         case (4); call a_CC(f(:,y,:),f(:,y-1,:),f(:,2,:),v%vals,dhe,b,1.0_cp,x,z,p)
         case (6); call a_CC(f(:,:,z),f(:,:,z-1),f(:,:,2),v%vals,dhe,b,1.0_cp,x,y,p)
         end select
       end subroutine

       subroutine a_CC(ug,ui,ui_opp,bvals,dh,b,nhat,x,y,p)
         ! interpolated - (wall incoincident)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,ui_opp,bvals
         real(cp),intent(in) :: dh,nhat
         type(bctype),intent(in) :: b
#ifdef _DEBUG_APPLY_BCS_
         call check_dimensions(ug,bvals)
         call check_dimensions(ui,bvals)
#endif
         if     (is_Dirichlet(b)) then; call apply_Dirichlet_C(ug,ui,bvals,x,y,p)
         elseif (is_Neumann(b)) then;   call apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)
         elseif (is_Periodic(b)) then;  call apply_Periodic_C(ug,ui_opp,x,y,p)
         elseif (is_Robin(b)) then;     call apply_Robin_C(ug,ui,bvals,dh,nhat,x,y,p)
         else; stop 'Error: Bad bctype! Caught in app_CC in apply_BCs_faces.f90'
         endif
       end subroutine

       subroutine a_N(ug,ub,ui,ui_opp,bvals,dh,b,nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug,ub
         real(cp),dimension(x,y),intent(in) :: ui,ui_opp,bvals
         real(cp),intent(in) :: dh,nhat
         type(bctype),intent(in) :: b
#ifdef _DEBUG_APPLY_BCS_
         call check_dimensions(ub,bvals)
         call check_dimensions(ui,bvals)
#endif
         if     (is_Dirichlet(b)) then; call apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
         elseif (is_Neumann(b)) then;   call apply_Neumann_N(ug,ui,bvals,dh,nhat,x,y,p)
         elseif (is_Periodic(b)) then;  call apply_Periodic_N(ug,ui_opp,x,y,p)
         else; stop 'Error: Bad bctype! Caught in app_N in apply_BCs_faces.f90'
         endif
       end subroutine

       end module