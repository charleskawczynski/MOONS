       module apply_stitches_faces_mod
       use face_edge_corner_indexing_mod
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
         integer,dimension(2) :: f
         if (m%s.gt.1) then
           call C0_N1_tensor(U,x,y,z)
           do i=1,m%s; do k=1,3
           f = normal_faces_given_dir(k)
           if (m%g(i)%st_faces(f(1))%TF) call app_F(U%RF(i),U%RF(m%g(i)%st_faces(f(1))%ID),U%RF(i)%s,k,x,y,z,x,y,z)
           enddo; enddo
         endif
       end subroutine

       subroutine app_F(Umin,Umax,s,dir,x,y,z,a,b,c)
         ! Along direction dir, we have
         ! 
         !            Umax (attaches at hmax)
         !       |-------------------------------|        Umin (attaches at hmin)
         !                                       |-----------------------------------|
         ! 
         implicit none
         type(realField),intent(inout) :: Umin,Umax
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir,x,y,z,a,b,c
         select case (dir)
         case (1); Umax%f(  Umax%s(1)  ,2+b:s(2)-1-b,2+c:s(3)-1-c) = Umin%f(    2+x        ,2+b:s(2)-1-b,2+c:s(3)-1-c)
                   Umin%f(    1        ,2+b:s(2)-1-b,2+c:s(3)-1-c) = Umax%f(  Umax%s(1)-1-x,2+b:s(2)-1-b,2+c:s(3)-1-c)
         case (2); Umax%f(2+a:s(1)-1-a,  Umax%s(2)  ,2+c:s(3)-1-c) = Umin%f(2+a:s(1)-1-a,    2+y        ,2+c:s(3)-1-c)
                   Umin%f(2+a:s(1)-1-a,    1        ,2+c:s(3)-1-c) = Umax%f(2+a:s(1)-1-a,  Umax%s(2)-1-y,2+c:s(3)-1-c)
         case (3); Umax%f(2+a:s(1)-1-a,2+b:s(2)-1-b,  Umax%s(3)  ) = Umin%f(2+a:s(1)-1-a,2+b:s(2)-1-b,    2+z        )
                   Umin%f(2+a:s(1)-1-a,2+b:s(2)-1-b,    1        ) = Umax%f(2+a:s(1)-1-a,2+b:s(2)-1-b,  Umax%s(3)-1-z)
         case default
         stop 'Erorr: dir must = 1,2,3 in applyAllStitches_RF in apply_stitches_faces.f90'
         end select
       end subroutine

       end module