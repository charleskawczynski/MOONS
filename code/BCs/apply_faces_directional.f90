       module apply_faces_mod
       use RF_mod
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

         if (CC_along(U,dir)) then
           do i=1,m%s
             if (.not.m%g(i)%st_face%hmin(dir)) call app_CC_RF(U%RF(i)%f,m%g(i),U%RF(i)%s,f(1),&
                                                               U%RF(i)%b%face(dir)%hmin%vals,&
                                                               U%RF(i)%b%face(dir)%hmin%bctype,&
                                                               m%g(i)%c(dir)%dhc(1),&
                                                               m%g(i)%c(dir)%dhc(m%g(i)%c(dir)%sc-1))
             if (.not.m%g(i)%st_face%hmax(dir)) call app_CC_RF(U%RF(i)%f,m%g(i),U%RF(i)%s,f(2),&
                                                               U%RF(i)%b%face(dir)%hmax%vals,&
                                                               U%RF(i)%b%face(dir)%hmax%bctype,&
                                                               m%g(i)%c(dir)%dhc(m%g(i)%c(dir)%sc-1),&
                                                               m%g(i)%c(dir)%dhc(1))
           enddo
         elseif (Node_along(U,dir)) then
           do i=1,m%s
             if (.not.m%g(i)%st_face%hmin(dir)) call app_N_RF(U%RF(i)%f,m%g(i),U%RF(i)%s,f(1),&
                                                              U%RF(i)%b%face(dir)%hmin%vals,&
                                                              U%RF(i)%b%face(dir)%hmin%bctype,&
                                                              m%g(i)%c(dir)%dhc(1),&
                                                              m%g(i)%c(dir)%dhc(m%g(i)%c(dir)%sc-1))
             if (.not.m%g(i)%st_face%hmax(dir)) call app_N_RF(U%RF(i)%f,m%g(i),U%RF(i)%s,f(2),&
                                                              U%RF(i)%b%face(dir)%hmax%vals,&
                                                              U%RF(i)%b%face(dir)%hmax%bctype,&
                                                              m%g(i)%c(dir)%dhc(m%g(i)%c(dir)%sc-1),&
                                                              m%g(i)%c(dir)%dhc(1))
           enddo
         else; stop 'Error: datatype not found in applyBCs.f90'
         endif
       end subroutine

       subroutine app_N_RF(f,g,s,face,v,t,dh,dho)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),intent(in) :: dh,dho
         real(cp),dimension(:,:),intent(in) :: v
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face,t ! face, bctype
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         !
         select case (face) ! face
         case (1); call app_N(f(1,:,:),f(2,:,:),f(3,:,:),f(s(1),:,:),f(s(1)-1,:,:),f(s(1)-2,:,:),v,t,dh,dho)
         case (3); call app_N(f(:,1,:),f(:,2,:),f(:,3,:),f(:,s(2),:),f(:,s(2)-1,:),f(:,s(2)-2,:),v,t,dh,dho)
         case (5); call app_N(f(:,:,1),f(:,:,2),f(:,:,3),f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,s(3)-2),v,t,dh,dho)
         case (2); call app_N(f(s(1),:,:),f(s(1)-1,:,:),f(s(1)-2,:,:),f(1,:,:),f(2,:,:),f(3,:,:),v,t,dh,dho)
         case (4); call app_N(f(:,s(2),:),f(:,s(2)-1,:),f(:,s(2)-2,:),f(:,1,:),f(:,2,:),f(:,3,:),v,t,dh,dho)
         case (6); call app_N(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,s(3)-2),f(:,:,1),f(:,:,2),f(:,:,3),v,t,dh,dho)
         end select
       end subroutine

       subroutine app_CC_RF(f,g,s,face,v,t,dh,dho)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),intent(in) :: dh,dho
         real(cp),dimension(:,:),intent(in) :: v
         integer,dimension(3),intent(in) :: s ! shape
         integer,intent(in) :: face,t ! face, bctype
         ! For readability, the faces are traversed in the order:
         !       {1,3,5,2,4,6} = (x_min,y_min,z_min,x_max,y_max,z_max)
         !
         select case (face) ! face
         case (1); call app_CC(f(1,:,:),f(2,:,:),f(s(1),:,:),f(s(1)-1,:,:),v,t,dh,dho)
         case (3); call app_CC(f(:,1,:),f(:,2,:),f(:,s(2),:),f(:,s(2)-1,:),v,t,dh,dho)
         case (5); call app_CC(f(:,:,1),f(:,:,2),f(:,:,s(3)),f(:,:,s(3)-1),v,t,dh,dho)
         case (2); call app_CC(f(s(1),:,:),f(s(1)-1,:,:),f(1,:,:),f(2,:,:),v,t,dh,dho)
         case (4); call app_CC(f(:,s(2),:),f(:,s(2)-1,:),f(:,1,:),f(:,2,:),v,t,dh,dho)
         case (6); call app_CC(f(:,:,s(3)),f(:,:,s(3)-1),f(:,:,1),f(:,:,2),v,t,dh,dho)
         end select
       end subroutine

       subroutine app_CC(ug,ui,ug_opp,ui_opp,bvals,bctype,dh,dh_opp)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug
         real(cp),dimension(:,:),intent(in) :: ui,ug_opp,ui_opp,bvals
         real(cp),intent(in) :: dh,dh_opp
         integer,intent(in) :: bctype
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (2); ug = 2.0_cp*bvals - ui ! Dirichlet - interpolated - wall incoincident
         ! *************************** NEUMANN *****************************
         case (5); ug = ui + dh*bvals     ! Implicit Neumann - interpolated - wall incoincident ~O(dh)
         ! *************************** PERIODIC *****************************
         case (7); ug = ui_opp            ! Periodic - interpolated - wall incoincident ~O(dh)
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       subroutine app_N(ug,ub,ui,ug_opp,ub_opp,ui_opp,bvals,bctype,dh,dh_opp)
         implicit none
         real(cp),dimension(:,:),intent(inout) :: ug,ub
         real(cp),dimension(:,:),intent(in) :: ui,ug_opp,ub_opp,ui_opp,bvals
         real(cp),intent(in) :: dh,dh_opp
         integer,intent(in) :: bctype
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ub = bvals; ug = 2.0_cp*bvals - ui ! Dirichlet - direct - wall coincident
         ! *************************** NEUMANN *****************************
         case (3); ub = ui; ug = ui                   ! Explicit Neumann - direct - wall coincident ~O(dh)?
         case (4); ug = ui - 2.0_cp*bvals*dh          ! Implicit Neumann - direct - wall coincident ~O(dh^2)
         ! *************************** PERIODIC *****************************
         case (6); ub = ug_opp; ug = ui_opp          ! Periodic - direct - wall coincident ~O(dh)
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       end module