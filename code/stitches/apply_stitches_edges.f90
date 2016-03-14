       module apply_stitches_edges_mod
       use face_edge_corner_indexing_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: apply_stitches_edges

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface apply_stitches_edges;    module procedure apply_stitches_edges_VF;     end interface
       interface apply_stitches_edges;    module procedure apply_stitches_edges_SF;     end interface

       contains

       subroutine apply_stitches_edges_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_stitches_edges(U%x,m)
         call apply_stitches_edges(U%y,m)
         call apply_stitches_edges(U%z,m)
       end subroutine

       subroutine apply_stitches_edges_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k,x,y,z
         integer,dimension(4) :: e
         ! The second if statement is commented because in app_E, 
         ! both minmin and maxmax, e.g., are assigned, and so calling
         ! maxmax would be redundant and is uneccesary.
         ! These if statements were left here for readability purposes.
         if (m%s.gt.1) then
           call C0_N1_tensor(U,x,y,z)
           do i=1,m%s; do k=1,3
           e = edges_given_dir(k)
           if (m%g(i)%st_edges(e(1))%TF) call app_E(U%RF(i),U%RF(m%g(i)%st_edges(e(1))%ID),1,k,x,y,z)
           if (m%g(i)%st_edges(e(2))%TF) call app_E(U%RF(i),U%RF(m%g(i)%st_edges(e(2))%ID),2,k,x,y,z)
           if (m%g(i)%st_edges(e(3))%TF) call app_E(U%RF(i),U%RF(m%g(i)%st_edges(e(3))%ID),3,k,x,y,z)
           if (m%g(i)%st_edges(e(4))%TF) call app_E(U%RF(i),U%RF(m%g(i)%st_edges(e(4))%ID),4,k,x,y,z)
           enddo; enddo
         endif
       end subroutine

       subroutine app_E(U,V,edge,dir,px,py,pz)
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
         integer,intent(in) :: edge,dir,px,py,pz
         select case (dir)
         case (1)
           select case (edge) ! LHS are ghost points, RHS are physical points
           case (1); U%f(:,  1   ,  1   ) = V%f(:,V%s(2)-1-py,V%s(3)-1-pz)
                     V%f(:,V%s(2),V%s(3)) = U%f(:,   2+py    ,   2+pz    )
           case (2); U%f(:,  1   ,U%s(3)) = V%f(:,V%s(2)-1-py,   2+pz    )
                     V%f(:,V%s(2),  1   ) = U%f(:,   2+py    ,U%s(3)-1-pz)
           case (3); U%f(:,U%s(2),  1   ) = V%f(:,   2+py    ,V%s(3)-1-pz)
                     V%f(:,  1   ,V%s(3)) = U%f(:,U%s(2)-1-py,   2+pz    )
           case (4); U%f(:,U%s(2),U%s(3)) = V%f(:,   2+py    ,   2+pz    )
                     V%f(:,  1   ,  1   ) = U%f(:,U%s(2)-1-py,U%s(3)-1-pz)
           case default; stop 'Error: edge must = 1:4 in app_CC in apply_stitches_edges.f90'
           end select
         case (2)
           select case (edge) ! LHS are ghost points, RHS are physical points
           case (1); U%f(  1   ,:,  1   ) = V%f(V%s(1)-1-px,:,V%s(3)-1-pz)
                     V%f(V%s(1),:,V%s(3)) = U%f(   2+px    ,:,   2+pz    )
           case (2); U%f(  1   ,:,U%s(3)) = V%f(V%s(1)-1-px,:,   2+pz    )
                     V%f(V%s(1),:,  1   ) = U%f(   2+px    ,:,U%s(3)-1-pz)
           case (3); U%f(U%s(1),:,  1   ) = V%f(   2+px    ,:,V%s(3)-1-pz)
                     V%f(  1   ,:,V%s(3)) = U%f(U%s(1)-1-px,:,   2+pz    )
           case (4); U%f(U%s(1),:,U%s(3)) = V%f(   2+px    ,:,   2+pz    )
                     V%f(  1   ,:,  1   ) = U%f(U%s(1)-1-px,:,U%s(3)-1-pz)
           case default; stop 'Error: edge must = 1:4 in app_CC in apply_stitches_edges.f90'
           end select
         case (3)
           select case (edge) ! LHS are ghost points, RHS are physical points
           case (1); U%f(  1   ,  1   ,:) = V%f(V%s(1)-1-px,V%s(2)-1-py,:)
                     V%f(V%s(1),V%s(2),:) = U%f(   2+px    ,   2+py    ,:)
           case (2); U%f(  1   ,U%s(2),:) = V%f(V%s(1)-1-px,   2+py    ,:)
                     V%f(V%s(1),  1   ,:) = U%f(   2+px    ,U%s(2)-1-py,:)
           case (3); U%f(U%s(1),  1   ,:) = V%f(   2+px    ,V%s(2)-1-py,:)
                     V%f(  1   ,V%s(2),:) = U%f(U%s(1)-1-px,   2+py    ,:)
           case (4); U%f(U%s(1),U%s(2),:) = V%f(   2+px    ,   2+py    ,:)
                     V%f(  1   ,  1   ,:) = U%f(U%s(1)-1-px,U%s(2)-1-py,:)
           case default; stop 'Error: edge must = 1:4 in app_CC in apply_stitches_edges.f90'
           end select
         case default; stop 'Error: dir must = 1,2,3 in app_CC in apply_stitches_edges.f90'
         end select
       end subroutine

       end module