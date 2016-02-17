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

       subroutine get_adjacent_pads(U,m,i,j,k,f,a,b,c)
         implicit none
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,f,a,b,c
         integer,dimension(2),intent(inout) :: a,b,c
         integer,dimension(4) :: fa
         logical,dimension(2) :: TF
         integer :: p
         fa = adj_faces_given_dir(k)
         do p=1,2
           if (k.ne.(dir_given_face(fa(p)))) then
             TF(1) = U%RF(i)%b%f(fa(p))%b%Neumann.and.U%RF(j)%b%f(fa(p))%b%Neumann
             TF(2) = Node_along(U,dir_given_face(fa(p)))
             if (all(TF)) a(p) = 0
           endif
         enddo
         do p=1,2
           if (k.ne.(dir_given_face(fa(p+2)))) then
             TF(1) = U%RF(i)%b%f(fa(p+2))%b%Neumann.and.U%RF(j)%b%f(fa(p+2))%b%Neumann
             TF(2) = Node_along(U,dir_given_face(fa(p+2)))
             if (all(TF)) b(p) = 0
           endif
         enddo

         ! TF(1) = U%RF(i)%b%f(fa(2))%b%Neumann.and.U%RF(j)%b%f(fa(2))%b%Neumann
         ! TF(2) = Node_along(U,dir_given_face(fa(2)))
         ! if (all(TF)) a(2) = 0
       end subroutine

       subroutine apply_stitches_faces_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,x,y,z,a,b,c
         integer,dimension(2) :: f
         logical :: TF
         if (m%s.gt.1) then
           call C0_N1_tensor(U,x,y,z); a=x;b=y;z=c
           do i=1,m%s; do k=1,3
           f = normal_faces_given_dir(k)
           if (m%g(i)%st_faces(f(1))%TF) then
             call get_adjacent_pads(U,m,i,m%g(i)%st_faces(f(1))%ID,k,f(1),a,b,c)
             call app_F(U%RF(i),U%RF(m%g(i)%st_faces(f(1))%ID),U%RF(i)%s,k,x,y,z,a,b,c)
           endif
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