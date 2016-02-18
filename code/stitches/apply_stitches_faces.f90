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

       subroutine adjust_bounds(U,i,j,k,a,b)
         implicit none
         type(SF),intent(in) :: U
         integer,intent(in) :: i,j,k
         integer,dimension(3),intent(inout) :: a,b
         integer,dimension(4) :: f
         integer :: p,d
         f = adj_faces_given_dir(k)
         if (U%RF(i)%b%defined) then
         p = 1; d = dir_given_face(f(p))
         if (k.ne.d.and.U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) a(d) = a(d)-1

         p = 2; d = dir_given_face(f(p))
         if (k.ne.d.and.U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) b(d) = b(d)+1

         p = 3; d = dir_given_face(f(p))
         if (k.ne.d.and.U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) a(d) = a(d)-1

         p = 4; d = dir_given_face(f(p))
         if (k.ne.d.and.U%RF(i)%b%f(f(p))%b%Neumann.and.U%RF(j)%b%f(f(p))%b%Neumann.and.Node_along(U,d)) b(d) = b(d)+1
         endif
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
               a = (/2+x,2+y,2+z/); b = (/U%RF(i)%s(1)-1-x,U%RF(i)%s(2)-1-y,U%RF(i)%s(3)-1-z/)
               call adjust_bounds(U,i,m%g(i)%st_faces(f(1))%ID,k,a,b)
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