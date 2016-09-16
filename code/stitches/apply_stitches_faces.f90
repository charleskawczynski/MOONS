       module apply_stitches_faces_mod
       use current_precision_mod
       use face_edge_corner_indexing_mod
       ! use export_raw_processed_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches_faces



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

       subroutine adjust_bounds(U,m,i,j,k,a,b,x,y,z)
         ! This routine may need more tweaking
         implicit none
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,x,y,z
         integer,dimension(3),intent(inout) :: a,b
         integer,dimension(4) :: fa
         logical,dimension(3) :: L
         integer :: d
         ! Consider grids i and j with a stitched face f_st between them. 
         ! Let f(1:4) denote the faces between grids i and j which are 
         ! co-planar and share an edge. Let us consider co-planar faces 
         ! p here (faces 4 and 4 are co-planar and share an edge) whose 
         ! shared edge is e.
         ! Stitching on cell faces should follow:
         !    - Cell center data should be stitched on all interior cells
         !    - Cell corner data (or face data) should be stitched on all 
         !      interior nodes. Boundary nodes on the face along edge e
         !      should be stitched ONLY if
         !              - U%RF(i)%st_faces(f(p))%TF and U%RF(j)%st_faces(f(p))%TF are true
         !           or - U%RF(i)%b%f(p)%b%Neumann and U%RF(j)%b%f(p)%b%Neumann
         ! 
         ! Alternatively, stitching should NOT occur if
         !              - .not.U%RF(i)%st_faces(f(p))%TF and U%RF(i)%b%f(p)%b%Neumann
         !           or - .not.U%RF(j)%st_faces(f(p))%TF and U%RF(j)%b%f(p)%b%Neumann
         ! 
         ! a = 2; b = U%RF(i)%s-1 ! Works for 9 domains but fails to satisfy BCs for 2D flow over square
         ! a = 2; b = U%RF(i)%s-1 Need to get back here IF BCs of i and j are Neumann...

         if (U%RF(i)%b%defined) then ! Satisfies BCs for 2D flow over a square but fails for 9 domains
         a = (/2+x,2+y,2+z/); b = (/U%RF(i)%s(1)-1-x,U%RF(i)%s(2)-1-y,U%RF(i)%s(3)-1-z/)
         fa = adj_faces_given_dir(k)

         d = dir_given_face(fa(1))
         L(1) = m%g(i)%st_faces(fa(1))%TF.and.m%g(j)%st_faces(fa(1))%TF
         L(2) = .not.m%g(i)%st_faces(fa(1))%TF.and.U%RF(i)%b%f(fa(1))%b%Neumann
         L(3) = .not.m%g(j)%st_faces(fa(1))%TF.and.U%RF(j)%b%f(fa(1))%b%Neumann
         if ((k.ne.d).and.U%N_along(d).and.(L(1).or.(L(2).and.L(3)))) a(d) = a(d)-1
         ! if (L(2).and.L(3)) a(d) = 1

         d = dir_given_face(fa(2))
         L(1) = m%g(i)%st_faces(fa(2))%TF.and.m%g(j)%st_faces(fa(2))%TF
         L(2) = .not.m%g(i)%st_faces(fa(2))%TF.and.U%RF(i)%b%f(fa(2))%b%Neumann
         L(3) = .not.m%g(j)%st_faces(fa(2))%TF.and.U%RF(j)%b%f(fa(2))%b%Neumann
         if ((k.ne.d).and.U%N_along(d).and.(L(1).or.(L(2).and.L(3)))) b(d) = b(d)+1
         ! if (L(2).and.L(3)) b(d) = U%RF(i)%s(d)

         d = dir_given_face(fa(3))
         L(1) = m%g(i)%st_faces(fa(3))%TF.and.m%g(j)%st_faces(fa(3))%TF
         L(2) = .not.m%g(i)%st_faces(fa(3))%TF.and.U%RF(i)%b%f(fa(3))%b%Neumann
         L(3) = .not.m%g(j)%st_faces(fa(3))%TF.and.U%RF(j)%b%f(fa(3))%b%Neumann
         if ((k.ne.d).and.U%N_along(d).and.(L(1).or.(L(2).and.L(3)))) a(d) = a(d)-1
         ! if (L(2).and.L(3)) a(d) = 1

         d = dir_given_face(fa(4))
         L(1) = m%g(i)%st_faces(fa(4))%TF.and.m%g(j)%st_faces(fa(4))%TF
         L(2) = .not.m%g(i)%st_faces(fa(4))%TF.and.U%RF(i)%b%f(fa(4))%b%Neumann
         L(3) = .not.m%g(j)%st_faces(fa(4))%TF.and.U%RF(j)%b%f(fa(4))%b%Neumann
         if ((k.ne.d).and.U%N_along(d).and.(L(1).or.(L(2).and.L(3)))) b(d) = b(d)+1
         ! if (L(2).and.L(3)) b(d) = U%RF(i)%s(d)

         else
         a = (/2+x,2+y,2+z/); b = (/U%RF(i)%s(1)-1-x,U%RF(i)%s(2)-1-y,U%RF(i)%s(3)-1-z/)
         fa = adj_faces_given_dir(k)
         d = dir_given_face(fa(1))
         if (U%N_along(d).and.(m%g(i)%st_faces(fa(1))%TF.and.m%g(j)%st_faces(fa(1))%TF)) a(d) = a(d)-1
         d = dir_given_face(fa(2))
         if (U%N_along(d).and.(m%g(i)%st_faces(fa(2))%TF.and.m%g(j)%st_faces(fa(2))%TF)) b(d) = b(d)+1
         d = dir_given_face(fa(3))
         if (U%N_along(d).and.(m%g(i)%st_faces(fa(3))%TF.and.m%g(j)%st_faces(fa(3))%TF)) a(d) = a(d)-1
         d = dir_given_face(fa(4))
         if (U%N_along(d).and.(m%g(i)%st_faces(fa(4))%TF.and.m%g(j)%st_faces(fa(4))%TF)) b(d) = b(d)+1
         endif
         if (m%g(i)%c(1)%N.eq.1) then; a(1) = 1; b(1) = U%RF(i)%s(1); endif
         if (m%g(i)%c(2)%N.eq.1) then; a(2) = 1; b(2) = U%RF(i)%s(2); endif
         if (m%g(i)%c(3)%N.eq.1) then; a(3) = 1; b(3) = U%RF(i)%s(3); endif
       end subroutine

       subroutine apply_stitches_faces_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,x,y,z
         integer,dimension(2) :: f
         integer,dimension(3) :: a,b
         if (m%s.gt.1) then
           call C0_N1_tensor(U,x,y,z)
           do i=1,m%s; do k=1,3
             f = normal_faces_given_dir(k)
             if (m%g(i)%st_faces(f(1))%TF) then
               call adjust_bounds(U,m,i,m%g(i)%st_faces(f(1))%ID,k,a,b,x,y,z)
               call app_F(U%RF(i),U%RF(m%g(i)%st_faces(f(1))%ID),k,x,y,z,a,b)
             endif
           enddo; enddo
         endif
       end subroutine

       subroutine app_F(Umin,Umax,dir,x,y,z,a,b)
         ! Along direction dir, we have
         ! 
         !            Umax (attaches at hmax)
         !       |-------------------------------|        Umin (attaches at hmin)
         !                                       |-----------------------------------|
         ! 
         ! x,y,z are pads along dir and a,b,c are 
         ! pads along directions adjacent to dir
         implicit none
         type(realField),intent(inout) :: Umin,Umax
         integer,dimension(3),intent(in) :: a,b
         integer,intent(in) :: dir,x,y,z
         select case (dir)
         case (1); Umax%f(Umax%s(1),a(2):b(2),a(3):b(3)) = Umin%f(  2+x        ,a(2):b(2),a(3):b(3))
                   Umin%f(  1      ,a(2):b(2),a(3):b(3)) = Umax%f(Umax%s(1)-1-x,a(2):b(2),a(3):b(3))
         case (2); Umax%f(a(1):b(1),Umax%s(2),a(3):b(3)) = Umin%f(a(1):b(1),  2+y        ,a(3):b(3))
                   Umin%f(a(1):b(1),  1      ,a(3):b(3)) = Umax%f(a(1):b(1),Umax%s(2)-1-y,a(3):b(3))
         case (3); Umax%f(a(1):b(1),a(2):b(2),Umax%s(3)) = Umin%f(a(1):b(1),a(2):b(2),  2+z        )
                   Umin%f(a(1):b(1),a(2):b(2),  1      ) = Umax%f(a(1):b(1),a(2):b(2),Umax%s(3)-1-z)
         case default
         stop 'Erorr: dir must = 1,2,3 in applyAllStitches_RF in apply_stitches_faces.f90'
         end select
       end subroutine

       end module