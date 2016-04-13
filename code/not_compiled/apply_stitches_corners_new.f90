       module apply_stitches_corners_mod
       use face_edge_corner_indexing_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches_corners

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_stitches_corners;    module procedure apply_stitches_corners_VF;     end interface
       interface apply_stitches_corners;    module procedure apply_stitches_corners_SF;     end interface

       contains

       subroutine apply_stitches_corners_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_stitches_corners(U%x,m)
         call apply_stitches_corners(U%y,m)
         call apply_stitches_corners(U%z,m)
       end subroutine

       subroutine apply_stitches_corners_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,x,y,z
         ! The second if statement is commented because in app_E, 
         ! both minmin and maxmax, e.g., are assigned, and so calling
         ! maxmax would be redundant and is uneccesary.
         ! These if statements were left here for readability purposes.
         if (m%s.gt.1) then
           call C0_N1_tensor(U,x,y,z)
           do i=1,m%s
           if (m%g(i)%st_corners(1)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(1)%ID),1,k,x,y,z)
           if (m%g(i)%st_corners(2)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(2)%ID),2,k,x,y,z)
           if (m%g(i)%st_corners(3)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(3)%ID),3,k,x,y,z)
           if (m%g(i)%st_corners(4)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(4)%ID),4,k,x,y,z)
           if (m%g(i)%st_corners(5)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(5)%ID),5,k,x,y,z)
           if (m%g(i)%st_corners(6)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(6)%ID),6,k,x,y,z)
           if (m%g(i)%st_corners(7)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(7)%ID),7,k,x,y,z)
           if (m%g(i)%st_corners(8)%TF) call app_C(U%RF(i),U%RF(m%g(i)%st_corners(8)%ID),8,k,x,y,z)
           enddo; enddo
         endif
       end subroutine

       subroutine app_C(U,V,corner,dir,px,py,pz)
         ! 
         !                |       *       |        
         !                |       *       |   g2   
         !                |       *       |        
         !        -------- ------- ---F---N--------
         !                |       *       |        
         !                |       *   C   F        
         !                |       *       |        
         !        ******** ******* ******* ********  * = physical boundary
         !                |       *       |        
         !                F   C   *       |        
         !                |       *       |        
         !        --------N---F--- ------- --------
         !                |       *       |        
         !           g1   |       *       |        
         !                |       *       |        
         ! 
         implicit none
         type(realField),intent(inout) :: U,V
         integer,intent(in) :: corner,dir,px,py,pz
         select case (corner) ! LHS are ghost points, RHS are physical points
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (1)
         U%f(1,1,1) = V%f(V%s(2)-1-px,V%s(2)-1-py,V%s(3)-1-pz)
         V%f(V%s(1),V%s(2),V%s(3)) = U%f(2+px,2+py,2+pz)
         case (2); U%f(:,  1   ,U%s(3)) = V%f(:,V%s(2)-1-py,   2+pz    )
                   V%f(:,V%s(2),  1   ) = U%f(:,   2+py    ,U%s(3)-1-pz)
         case (3); U%f(:,U%s(2),  1   ) = V%f(:,   2+py    ,V%s(3)-1-pz)
                   V%f(:,  1   ,V%s(3)) = U%f(:,U%s(2)-1-py,   2+pz    )
         case (4); U%f(:,U%s(2),U%s(3)) = V%f(:,   2+py    ,   2+pz    )
                   V%f(:,  1   ,  1   ) = U%f(:,U%s(2)-1-py,U%s(3)-1-pz)
         case (4); U%f(:,U%s(2),U%s(3)) = V%f(:,   2+py    ,   2+pz    )
                   V%f(:,  1   ,  1   ) = U%f(:,U%s(2)-1-py,U%s(3)-1-pz)
         case (4); U%f(:,U%s(2),U%s(3)) = V%f(:,   2+py    ,   2+pz    )
                   V%f(:,  1   ,  1   ) = U%f(:,U%s(2)-1-py,U%s(3)-1-pz)
         case (4); U%f(:,U%s(2),U%s(3)) = V%f(:,   2+py    ,   2+pz    )
                   V%f(:,  1   ,  1   ) = U%f(:,U%s(2)-1-py,U%s(3)-1-pz)
         case (4); U%f(:,U%s(2),U%s(3)) = V%f(:,   2+py    ,   2+pz    )
                   V%f(:,  1   ,  1   ) = U%f(:,U%s(2)-1-py,U%s(3)-1-pz)
         case default; stop 'Error: corner must = 1:8 in app_C in apply_stitches_corners.f90'
         end select
       end subroutine

       end module