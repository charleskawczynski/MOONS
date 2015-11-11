       ! *******************************************************************************
       ! ********************************* INIT EDGES **********************************
       ! *******************************************************************************

       subroutine init_Edges(BC)
         implicit none
         type(BCs),intent(inout) :: BC
         integer,dimension(2) :: a ! index of adjacent faces
         integer :: i
         do i=1,12
           a = adjacent_faces(i)
               if ((BC%f(a(1))%b%Dirichlet).and.(BC%f(a(2))%b%Dirichlet)) then
               call init_Dirichlet(BC%e(i)%b); call setEdgeBy2Faces(BC%e(i),BC%f(a(1)),BC%f(a(2)),i)
           elseif (BC%f(a(1))%b%Dirichlet) then
               call init_Dirichlet(BC%e(i)%b); call setEdgeBy1Face(BC%e(i),BC%f(a(1)),i)
           elseif (BC%f(a(2))%b%Dirichlet) then
               call init_Dirichlet(BC%e(i)%b); call setEdgeByFaces(BC%e(i),BC%f(a(2)),i)
           elseif ((BC%f(a(1))%b%Neumann).and.(BC%f(a(2))%b%Neumann)) then
               call init_Neumann(BC%e(i)%b); call setEdgeBy2Faces(BC%e(i),BC%f(a(1)),BC%f(a(2)),i)
           else; stop 'Error: edge definition case not defined in init_Edges in BCs.f90'
           endif
         enddo
       end subroutine

       function adjacent_faces(i_edge) result (i_faces)
         implicit none
         integer,intent(in) :: i_edge
         integer,dimension(2) :: i_faces
         select case (i_edge)
         case (1);  i_faces = (/3,5/) ! x (ymin,zmin)
         case (2);  i_faces = (/3,6/) ! x (ymin,zmax)
         case (3);  i_faces = (/4,5/) ! x (ymax,zmin)
         case (4);  i_faces = (/4,6/) ! x (ymax,zmax)
         case (5);  i_faces = (/1,5/) ! y (xmin,zmin)
         case (6);  i_faces = (/1,6/) ! y (xmin,zmax)
         case (7);  i_faces = (/2,5/) ! y (xmax,zmin)
         case (8);  i_faces = (/2,6/) ! y (xmax,zmax)
         case (9);  i_faces = (/1,3/) ! z (xmin,ymin)
         case (10); i_faces = (/1,4/) ! z (xmin,ymax)
         case (11); i_faces = (/2,3/) ! z (xmax,ymin)
         case (12); i_faces = (/2,4/) ! z (xmax,ymax)
         end select
       end function

       subroutine setEdgeBy1Face(e,f,i_e)
         implicit none
         type(edge),intent(inout) :: e
         type(face),intent(in) :: f
         integer,intent(in) :: i_e
         integer :: a,b
         i_f = adjacent_faces(i_e)
         select case (i_f)
         case(1)
         select case (i_e)
         case (1);  call init(e,f%vals(:,:)) ! x (ymin,zmin)
         case (2);  call init(e,f%vals(:,:)) ! x (ymin,zmax)
         case (3);  call init(e,f%vals(:,:)) ! x (ymax,zmin)
         case (4);  call init(e,f%vals(:,:)) ! x (ymax,zmax)
         case (5);  call init(e,f%vals(:,:)) ! y (xmin,zmin)
         case (6);  call init(e,f%vals(:,:)) ! y (xmin,zmax)
         case (7);  call init(e,f%vals(:,:)) ! y (xmax,zmin)
         case (8);  call init(e,f%vals(:,:)) ! y (xmax,zmax)
         case (9);  call init(e,f%vals(:,:)) ! z (xmin,ymin)
         case (10); call init(e,f%vals(:,:)) ! z (xmin,ymax)
         case (11); call init(e,f%vals(:,:)) ! z (xmax,ymin)
         case (12); call init(e,f%vals(:,:)) ! z (xmax,ymax)
         end select
       end subroutine

       subroutine setEdgeBy2Faces(e,f1,f2,i_e)
         implicit none
         type(edge),intent(inout) :: e
         type(face),intent(in) :: f1,f2
         integer,intent(in) :: i_e
         integer :: a1,b1,a2,b2
         i_f = adjacent_faces(i_e)
         select case (i_e)
         case (1);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! x (ymin,zmin)
         case (2);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! x (ymin,zmax)
         case (3);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! x (ymax,zmin)
         case (4);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! x (ymax,zmax)
         case (5);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! y (xmin,zmin)
         case (6);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! y (xmin,zmax)
         case (7);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! y (xmax,zmin)
         case (8);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! y (xmax,zmax)
         case (9);  a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! z (xmin,ymin)
         case (10); a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! z (xmin,ymax)
         case (11); a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! z (xmax,ymin)
         case (12); a1=f1%s(1);b1=f1%s(2);a2=f2%s(1);b2=f2%s(2) ! z (xmax,ymax)
         end select
         select case (i_e)
         case (1);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! x (ymin,zmin)
         case (2);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! x (ymin,zmax)
         case (3);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! x (ymax,zmin)
         case (4);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! x (ymax,zmax)
         case (5);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! y (xmin,zmin)
         case (6);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! y (xmin,zmax)
         case (7);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! y (xmax,zmin)
         case (8);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! y (xmax,zmax)
         case (9);  call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! z (xmin,ymin)
         case (10); call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! z (xmin,ymax)
         case (11); call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! z (xmax,ymin)
         case (12); call init(e,0.5_cp*(f1%vals(:,:)+f2%vals(:,:))) ! z (xmax,ymax)
         end select
       end subroutine

       subroutine setEdgeBy2Faces(BC,i_e)
         implicit none
         type(BCs),intent(inout) :: BC
         integer,intent(in) :: i_e
         select case (i_e)
         case (1);  a1=BC%f(3)%s(1);b1=BC%f(3)%s(2);a2=BC%f(5)%s(1);b2=BC%f(5)%s(2) ! x (ymin,zmin)
         case (2);  a1=BC%f(3)%s(1);b1=BC%f(3)%s(2);a2=BC%f(6)%s(1);b2=BC%f(6)%s(2) ! x (ymin,zmax)
         case (3);  a1=BC%f(4)%s(1);b1=BC%f(4)%s(2);a2=BC%f(5)%s(1);b2=BC%f(5)%s(2) ! x (ymax,zmin)
         case (4);  a1=BC%f(4)%s(1);b1=BC%f(4)%s(2);a2=BC%f(6)%s(1);b2=BC%f(6)%s(2) ! x (ymax,zmax)
         case (5);  a1=BC%f(1)%s(1);b1=BC%f(1)%s(2);a2=BC%f(5)%s(1);b2=BC%f(5)%s(2) ! y (xmin,zmin)
         case (6);  a1=BC%f(1)%s(1);b1=BC%f(1)%s(2);a2=BC%f(6)%s(1);b2=BC%f(6)%s(2) ! y (xmin,zmax)
         case (7);  a1=BC%f(2)%s(1);b1=BC%f(2)%s(2);a2=BC%f(5)%s(1);b2=BC%f(5)%s(2) ! y (xmax,zmin)
         case (8);  a1=BC%f(2)%s(1);b1=BC%f(2)%s(2);a2=BC%f(6)%s(1);b2=BC%f(6)%s(2) ! y (xmax,zmax)
         case (9);  a1=BC%f(1)%s(1);b1=BC%f(1)%s(2);a2=BC%f(3)%s(1);b2=BC%f(3)%s(2) ! z (xmin,ymin)
         case (10); a1=BC%f(1)%s(1);b1=BC%f(1)%s(2);a2=BC%f(4)%s(1);b2=BC%f(4)%s(2) ! z (xmin,ymax)
         case (11); a1=BC%f(2)%s(1);b1=BC%f(2)%s(2);a2=BC%f(3)%s(1);b2=BC%f(3)%s(2) ! z (xmax,ymin)
         case (12); a1=BC%f(2)%s(1);b1=BC%f(2)%s(2);a2=BC%f(4)%s(1);b2=BC%f(4)%s(2) ! z (xmax,ymax)
         end select
         select case (i_e)
         case (1);  call init(BC%e(1) ,0.5_cp*(BC%f(3)%vals(a1,b1)+BC%f(5)%vals(a2,b2))) ! x (ymin,zmin)
         case (2);  call init(BC%e(2) ,0.5_cp*(BC%f(3)%vals(a1,b1)+BC%f(6)%vals(a2,b2))) ! x (ymin,zmax)
         case (3);  call init(BC%e(3) ,0.5_cp*(BC%f(4)%vals(a1,b1)+BC%f(5)%vals(a2,b2))) ! x (ymax,zmin)
         case (4);  call init(BC%e(4) ,0.5_cp*(BC%f(4)%vals(a1,b1)+BC%f(6)%vals(a2,b2))) ! x (ymax,zmax)
         case (5);  call init(BC%e(5) ,0.5_cp*(BC%f(1)%vals(a1,b1)+BC%f(5)%vals(a2,b2))) ! y (xmin,zmin)
         case (6);  call init(BC%e(6) ,0.5_cp*(BC%f(1)%vals(a1,b1)+BC%f(6)%vals(a2,b2))) ! y (xmin,zmax)
         case (7);  call init(BC%e(7) ,0.5_cp*(BC%f(2)%vals(a1,b1)+BC%f(5)%vals(a2,b2))) ! y (xmax,zmin)
         case (8);  call init(BC%e(8) ,0.5_cp*(BC%f(2)%vals(a1,b1)+BC%f(6)%vals(a2,b2))) ! y (xmax,zmax)
         case (9);  call init(BC%e(9) ,0.5_cp*(BC%f(1)%vals(a1,b1)+BC%f(3)%vals(a2,b2))) ! z (xmin,ymin)
         case (10); call init(BC%e(10),0.5_cp*(BC%f(1)%vals(a1,b1)+BC%f(4)%vals(a2,b2))) ! z (xmin,ymax)
         case (11); call init(BC%e(11),0.5_cp*(BC%f(2)%vals(a1,b1)+BC%f(3)%vals(a2,b2))) ! z (xmax,ymin)
         case (12); call init(BC%e(12),0.5_cp*(BC%f(2)%vals(a1,b1)+BC%f(4)%vals(a2,b2))) ! z (xmax,ymax)
         end select
       end subroutine

