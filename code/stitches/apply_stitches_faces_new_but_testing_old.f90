       module apply_stitches_faces_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches_faces

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_stitches_faces;    module procedure apply_stitches_faces_VF;     end interface
       interface apply_stitches_faces;    module procedure apply_stitches_faces_SF;     end interface

       contains

       subroutine apply_stitches_faces_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_stitches_faces(U%x,m)
         call apply_stitches_faces(U%y,m)
         call apply_stitches_faces(U%z,m)
       end subroutine

       subroutine apply_stitches_faces_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,x,y,z
         call C0_N1_tensor(U,x,y,z)
         do i=1,m%s; do k=1,3
         if (m%g(i)%st_face%hmin(k)) call app_F(U%RF(i),U%RF(m%g(i)%st_face%hmin_id(k)),k,x,y,z)
         enddo; enddo
       end subroutine

       subroutine app_F(Umin,Umax,dir,px,py,pz)
         ! Along direction dir, we have
         ! 
         !            Umax (attaches at hmax)
         !       |-------------------------------|        Umin (attaches at hmin)
         !                                       |-----------------------------------|
         ! 
         implicit none
         type(realField),intent(inout) :: Umin,Umax
         integer,intent(in) :: dir,px,py,pz
         select case (dir)
         case (1); Umax%f(  Umax%s(1)  ,:,:) = Umin%f(    2+px        ,:,:)
                   Umin%f(    1        ,:,:) = Umax%f(  Umax%s(1)-1-px,:,:)
         case (2); Umax%f(:,  Umax%s(2)  ,:) = Umin%f(:,    2+py        ,:)
                   Umin%f(:,    1        ,:) = Umax%f(:,  Umax%s(2)-1-py,:)
         case (3); Umax%f(:,:,  Umax%s(3)  ) = Umin%f(:,:,    2+pz        )
                   Umin%f(:,:,    1        ) = Umax%f(:,:,  Umax%s(3)-1-pz)
         case default
         stop 'Erorr: dir must = 1,2,3 in applyAllStitches_RF in apply_stitches_faces.f90'
         end select
       end subroutine

       end module