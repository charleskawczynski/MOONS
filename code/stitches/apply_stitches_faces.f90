       module apply_stitches_faces_mod
       use face_edge_corner_indexing_mod
       ! use export_raw_processed_mod
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

       subroutine adjust_bounds_new(U,m,i,j,k,a,b)
         implicit none
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k
         integer,dimension(3),intent(inout) :: a,b
         integer,dimension(4) :: fa
         integer :: d
         a = 2; b = U%RF(i)%s-1 ! Works for 9 domains but fails to satisfy BCs for 2D flow over square
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
         if (U%RF(i)%b%defined.and.k.ne.d) then ! Satisfies BCs for 2D flow over a square but fails for 9 domains
         fa = adj_faces_given_dir(k)

         d = dir_given_face(fa(1))
         if (Node_along(U,d).and..not.m%g(i)%st_faces(fa(1))%TF.and.U%RF(i)%b%f(fa(1))%b%Dirichlet) a(d) = a(d)+1

         d = dir_given_face(fa(2))
         if (Node_along(U,d).and..not.m%g(i)%st_faces(fa(2))%TF.and.U%RF(i)%b%f(fa(2))%b%Dirichlet) b(d) = b(d)-1
         ! write(*,*) 'U%RF(i)%s-1 = ',U%RF(i)%s-1
         ! write(*,*) 'fa = ',fa
         ! write(*,*) 'a,b = ',a,b
         ! write(*,*) 'i,j,k,d = ',i,j,k,d
         ! write(*,*) 'working in apply_stitches_faces.f90'
         ! endif

         d = dir_given_face(fa(3))
         if (Node_along(U,d).and..not.m%g(i)%st_faces(fa(3))%TF.and.U%RF(i)%b%f(fa(3))%b%Dirichlet) a(d) = a(d)+1

         d = dir_given_face(fa(4))
         if (Node_along(U,d).and..not.m%g(i)%st_faces(fa(4))%TF.and.U%RF(i)%b%f(fa(4))%b%Dirichlet) b(d) = b(d)-1
         endif
       end subroutine

       subroutine adjust_bounds(U,i,j,k,a,b)
         implicit none
         type(SF),intent(in) :: U
         integer,intent(in) :: i,j,k
         integer,dimension(3),intent(inout) :: a,b
         integer,dimension(4) :: f
         integer :: p,d
         f = adj_faces_given_dir(k)
         
         ! a = 2; b = U%RF(i)%s-1 ! Works for 9 domains but fails to satisfy BCs for 2D flow over square

         if ((k.ne.d).and.U%RF(i)%b%defined) then ! Satisfies BCs for 2D flow over a square but fails for 9 domains
         p = 1; d = dir_given_face(f(p))
         if (U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) a(d) = a(d)-1

         p = 2; d = dir_given_face(f(p))
         if (U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) b(d) = b(d)+1

         p = 3; d = dir_given_face(f(p))
         if (U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) a(d) = a(d)-1

         p = 4; d = dir_given_face(f(p))
         if (U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) b(d) = b(d)+1
         endif
       end subroutine

       subroutine adjust_bounds_new2(U,m,i,j,k,a,b,x,y,z)
         implicit none
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,x,y,z
         integer,dimension(3),intent(inout) :: a,b
         integer,dimension(4) :: fa
         logical,dimension(3) :: TF
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

         a = 2; b = U%RF(i)%s

         if (U%RF(i)%b%defined.and.(k.ne.d)) then ! Satisfies BCs for 2D flow over a square but fails for 9 domains
         a = (/2+x,2+y,2+z/); b = (/U%RF(i)%s(1)-1-x,U%RF(i)%s(2)-1-y,U%RF(i)%s(3)-1-z/)
         fa = adj_faces_given_dir(k)

         d = dir_given_face(fa(1))
         TF(1) = m%g(i)%st_faces(fa(1))%TF.and.m%g(j)%st_faces(fa(1))%TF
         TF(2) = .not.m%g(i)%st_faces(fa(1))%TF.and.U%RF(i)%b%f(fa(1))%b%Neumann
         TF(3) = .not.m%g(j)%st_faces(fa(1))%TF.and.U%RF(j)%b%f(fa(1))%b%Neumann
         if (Node_along(U,d).and.(TF(1).or.(TF(2).and.TF(3)))) a(d) = a(d)-1

         d = dir_given_face(fa(2))
         TF(1) = m%g(i)%st_faces(fa(2))%TF.and.m%g(j)%st_faces(fa(2))%TF
         TF(2) = .not.m%g(i)%st_faces(fa(2))%TF.and.U%RF(i)%b%f(fa(2))%b%Neumann
         TF(3) = .not.m%g(j)%st_faces(fa(2))%TF.and.U%RF(j)%b%f(fa(2))%b%Neumann
         if (Node_along(U,d).and.(TF(1).or.(TF(2).and.TF(3)))) b(d) = b(d)+1

         d = dir_given_face(fa(3))
         TF(1) = m%g(i)%st_faces(fa(3))%TF.and.m%g(j)%st_faces(fa(3))%TF
         TF(2) = .not.m%g(i)%st_faces(fa(3))%TF.and.U%RF(i)%b%f(fa(3))%b%Neumann
         TF(3) = .not.m%g(j)%st_faces(fa(3))%TF.and.U%RF(j)%b%f(fa(3))%b%Neumann
         if (Node_along(U,d).and.(TF(1).or.(TF(2).and.TF(3)))) a(d) = a(d)-1

         d = dir_given_face(fa(4))
         TF(1) = m%g(i)%st_faces(fa(4))%TF.and.m%g(j)%st_faces(fa(4))%TF
         TF(2) = .not.m%g(i)%st_faces(fa(4))%TF.and.U%RF(i)%b%f(fa(4))%b%Neumann
         TF(3) = .not.m%g(j)%st_faces(fa(4))%TF.and.U%RF(j)%b%f(fa(4))%b%Neumann
         if (Node_along(U,d).and.(TF(1).or.(TF(2).and.TF(3)))) b(d) = b(d)+1

         else
         a = (/2+x,2+y,2+z/); b = (/U%RF(i)%s(1)-1-x,U%RF(i)%s(2)-1-y,U%RF(i)%s(3)-1-z/)
         fa = adj_faces_given_dir(k)
         d = dir_given_face(fa(1))
         if (Node_along(U,d).and.(m%g(i)%st_faces(fa(1))%TF.and.m%g(j)%st_faces(fa(1))%TF)) a(d) = a(d)-1
         d = dir_given_face(fa(2))
         if (Node_along(U,d).and.(m%g(i)%st_faces(fa(2))%TF.and.m%g(j)%st_faces(fa(2))%TF)) b(d) = b(d)+1
         d = dir_given_face(fa(3))
         if (Node_along(U,d).and.(m%g(i)%st_faces(fa(3))%TF.and.m%g(j)%st_faces(fa(3))%TF)) a(d) = a(d)-1
         d = dir_given_face(fa(4))
         if (Node_along(U,d).and.(m%g(i)%st_faces(fa(4))%TF.and.m%g(j)%st_faces(fa(4))%TF)) b(d) = b(d)+1
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
               ! a = (/2+x,2+y,2+z/); b = (/U%RF(i)%s(1)-1-x,U%RF(i)%s(2)-1-y,U%RF(i)%s(3)-1-z/)
               ! call adjust_bounds(U,i,m%g(i)%st_faces(f(1))%ID,k,a,b)
               call adjust_bounds_new2(U,m,i,m%g(i)%st_faces(f(1))%ID,k,a,b,x,y,z)
               ! call adjust_bounds_new(U,m,i,m%g(i)%st_faces(f(1))%ID,k,a,b)
               ! call adjust_bounds(U,i,m%g(i)%st_faces(f(1))%ID,f(1),k,a,b)
               call app_F(U%RF(i),U%RF(m%g(i)%st_faces(f(1))%ID),k,x,y,z,a,b)
               ! call app_F(U,m,i,m%g(i)%st_faces(f(1))%ID,k,x,y,z,a,b)
             endif
           enddo; enddo
         endif
       end subroutine

       subroutine app_F_debug(U,m,i,j,dir,x,y,z,a,b)
         ! Along direction dir, we have
         ! 
         !            Umax (attaches at hmax)
         !       |-------------------------------|        Umin (attaches at hmin)
         !                                       |-----------------------------------|
         ! 
         ! x,y,z are pads along dir and a,b,c are 
         ! pads along directions adjacent to dir
         implicit none
         type(SF),intent(inout) :: U
         integer,dimension(3),intent(in) :: a,b
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,x,y,z,i,j
#ifdef _DEBUG_APPLY_STITCHES
         real(cp) :: temp,tol
         tol = 10.0_cp**(-6.0_cp)
#endif
         select case (dir)
         case (1); U%RF(j)%f(U%RF(j)%s(1),a(2):b(2),a(3):b(3)) = U%RF(i)%f(  2+x        ,a(2):b(2),a(3):b(3))
                   U%RF(i)%f(  1      ,a(2):b(2),a(3):b(3))    = U%RF(j)%f(U%RF(j)%s(1)-1-x,a(2):b(2),a(3):b(3))
         case (2); U%RF(j)%f(a(1):b(1),U%RF(j)%s(2),a(3):b(3)) = U%RF(i)%f(a(1):b(1),  2+y        ,a(3):b(3))
                   U%RF(i)%f(a(1):b(1),  1      ,a(3):b(3))    = U%RF(j)%f(a(1):b(1),U%RF(j)%s(2)-1-y,a(3):b(3))
         case (3); U%RF(j)%f(a(1):b(1),a(2):b(2),U%RF(j)%s(3)) = U%RF(i)%f(a(1):b(1),a(2):b(2),  2+z        )
                   U%RF(i)%f(a(1):b(1),a(2):b(2),  1      )    = U%RF(j)%f(a(1):b(1),a(2):b(2),U%RF(j)%s(3)-1-z)
         case default
         stop 'Erorr: dir must = 1,2,3 in applyAllStitches_RF in apply_stitches_faces.f90'
         end select

#ifdef _DEBUG_APPLY_STITCHES
         select case (dir)
         case (1)
         if (x.eq.1) then
         temp = maxval(abs(U%RF(j)%f(U%RF(j)%s(1)-1,a(2):b(2),a(3):b(3))-U%RF(i)%f(2,a(2):b(2),a(3):b(3))))
         if (temp.gt.tol) then
         call export_raw(m,U,'out/LDC/','U',0); write(*,*) 'dir,i,j = ',dir,i,j
         write(*,*) 'temp = ',temp
         write(*,*) 'loc = ',maxloc(abs(U%RF(i)%f(U%RF(i)%s(1)-1,a(2):b(2),a(3):b(3))-U%RF(j)%f(2,a(2):b(2),a(3):b(3))))
         stop 'Error: wall normal stitch-face values are not equal.'
         endif; endif
         case (2)
         if (y.eq.1) then
         temp = maxval(abs(U%RF(j)%f(a(1):b(1),U%RF(j)%s(2)-1,a(3):b(3))-U%RF(i)%f(a(1):b(1),2,a(3):b(3))))
         if (temp.gt.tol) then
         call export_raw(m,U,'out/LDC/','U',0); write(*,*) 'dir,i,j = ',dir,i,j
         write(*,*) 'temp = ',temp
         write(*,*) 'loc = ',maxloc(abs(U%RF(i)%f(a(1):b(1),U%RF(i)%s(2)-1,a(3):b(3))-U%RF(j)%f(a(1):b(1),2,a(3):b(3))))
         stop 'Error: wall normal stitch-face values are not equal.'
         endif; endif
         case (3)
         if (z.eq.1) then
         temp = maxval(abs(U%RF(j)%f(a(1):b(1),a(2):b(2),U%RF(j)%s(3)-1)-U%RF(i)%f(a(1):b(1),a(2):b(2),2)))
         if (temp.gt.tol) then
         call export_raw(m,U,'out/LDC/','U',0); write(*,*) 'dir,i,j = ',dir,i,j
         write(*,*) 'temp = ',temp
         write(*,*) 'loc = ',maxloc(abs(U%RF(i)%f(a(1):b(1),a(2):b(2),U%RF(i)%s(3)-1)-U%RF(j)%f(a(1):b(1),a(2):b(2),2)))
         stop 'Error: wall normal stitch-face values are not equal.'
         endif; endif
         case default; stop 'Erorr: dir must = 1,2,3 in applyAllStitches_RF in apply_stitches_faces.f90'
         end select
#endif
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