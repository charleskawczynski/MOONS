       module ops_discrete_complex_mod
       use del_mod
       use delVC_mod
       use grid_mod
       use vectorField_mod
       use scalarField_mod
       use interpOps_mod
       use ops_aux_mod
       use ops_discrete_mod

       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: faceCrossFace_E
       public :: faceCrossCC_E
       public :: edgeCrossCC_E
       public :: faceCurlCross_F

       contains

       function orthogonalDirection(dir1,dir2) result(orthDir)
         implicit none
         integer,intent(in) :: dir1,dir2
         integer :: orthDir
         select case (dir1)
           case (1);
             select case (dir2)
             case (2); orthDir = 3
             case (3); orthDir = 2
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case (2);
             select case (dir2)
             case (1); orthDir = 3
             case (3); orthDir = 1
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case (3);
             select case (dir2)
             case (1); orthDir = 2
             case (2); orthDir = 1
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case default
             stop 'Error: bad input to orthogonalDirection'
         end select
       end function

       subroutine mult(A,B,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: A
         real(cp),dimension(:,:,:),intent(inout) :: B
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           A(i,j,k) = A(i,j,k)*B(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine square(A,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: A
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           A(i,j,k) = A(i,j,k)*A(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine squareVF(A)
         implicit none
         type(vectorField),intent(inout) :: A
         call square(A%x,A%sx)
         call square(A%y,A%sy)
         call square(A%z,A%sz)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine faceCrossFace_E(AcrossB,A,B,g)
         ! Computes
         ! 
         !      ( u_face x B_face )_edge
         ! 
         ! While minimizing interpolations.
         implicit none
         type(vectorField),intent(inout) :: AcrossB
         type(vectorField),intent(in) :: A,B
         type(grid),intent(in) :: g
         type(vectorField) :: tempA,tempB
         call allocateVectorField(tempA,AcrossB)
         call allocateVectorField(tempB,AcrossB)
         call myFace2Edge(tempA%y,A%y,g,2,1)
         call myFace2Edge(tempA%z,A%z,g,3,1)
         call myFace2Edge(tempB%y,B%y,g,2,1)
         call myFace2Edge(tempB%z,B%z,g,3,1)
         call cross(AcrossB%x,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,1)
         call myFace2Edge(tempA%x,A%x,g,1,2)
         call myFace2Edge(tempA%z,A%z,g,3,2)
         call myFace2Edge(tempB%x,B%x,g,1,2)
         call myFace2Edge(tempB%z,B%z,g,3,2)
         call cross(AcrossB%y,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,2)
         call myFace2Edge(tempA%x,A%x,g,1,3)
         call myFace2Edge(tempA%y,A%y,g,2,3)
         call myFace2Edge(tempB%x,B%x,g,1,3)
         call myFace2Edge(tempB%y,B%y,g,2,3)
         call cross(AcrossB%z,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempA)
         call delete(tempB)
       end subroutine

       subroutine faceCrossCC_E(AcrossB,A,B,g)
         ! Computes
         ! 
         !      ( u_face x B_CC )_edge
         ! 
         ! While minimizing interpolations.
         implicit none
         type(vectorField),intent(inout) :: AcrossB
         type(vectorField),intent(in) :: A,B
         type(grid),intent(in) :: g
         type(vectorField) :: tempA,tempB
         call allocateVectorField(tempA,AcrossB%sx(1),AcrossB%sx(2),AcrossB%sx(3))
         call allocateVectorField(tempB,tempA)
         call myFace2Edge(tempA%y,A%y,g,2,1)
         call myFace2Edge(tempA%z,A%z,g,3,1)
         call myCellCenter2Edge(tempB%y,B%y,g,1)
         call myCellCenter2Edge(tempB%z,B%z,g,1)
         call cross(AcrossB%x,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,1)
         call delete(tempA)
         call delete(tempB)
         call allocateVectorField(tempA,AcrossB%sy(1),AcrossB%sy(2),AcrossB%sy(3))
         call allocateVectorField(tempB,tempA)
         call myFace2Edge(tempA%x,A%x,g,1,2)
         call myFace2Edge(tempA%z,A%z,g,3,2)
         call myCellCenter2Edge(tempB%x,B%x,g,2)
         call myCellCenter2Edge(tempB%z,B%z,g,2)
         call cross(AcrossB%y,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,2)
         call delete(tempA)
         call delete(tempB)
         call allocateVectorField(tempA,AcrossB%sz(1),AcrossB%sz(2),AcrossB%sz(3))
         call allocateVectorField(tempB,tempA)
         call myFace2Edge(tempA%x,A%x,g,1,3)
         call myFace2Edge(tempA%y,A%y,g,2,3)
         call myCellCenter2Edge(tempB%x,B%x,g,3)
         call myCellCenter2Edge(tempB%y,B%y,g,3)
         call cross(AcrossB%z,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempA)
         call delete(tempB)
       end subroutine

       subroutine edgeCrossCC_E(UcrossB,U,V,W,B,g)
         ! Computes
         ! 
         !      ( u_edge x B_CC )_edge
         ! 
         ! While minimizing interpolations.
         implicit none
         type(vectorField),intent(inout) :: UcrossB
         type(vectorField),intent(in) :: U,V,W,B
         type(grid),intent(in) :: g
         type(vectorField) :: tempB
         call allocateVectorField(tempB,UcrossB%sx(1),UcrossB%sx(2),UcrossB%sx(3))
         call myCellCenter2Edge(tempB%y,B%y,g,1)
         call myCellCenter2Edge(tempB%z,B%z,g,1)
         call cross(UcrossB%x,U%x,V%x,W%x,tempB%x,tempB%y,tempB%z,1)
         call delete(tempB)
         call allocateVectorField(tempB,UcrossB%sy(1),UcrossB%sy(2),UcrossB%sy(3))
         call myCellCenter2Edge(tempB%x,B%x,g,2)
         call myCellCenter2Edge(tempB%z,B%z,g,2)
         call cross(UcrossB%y,U%y,V%y,W%y,tempB%x,tempB%y,tempB%z,2)
         call delete(tempB)
         call allocateVectorField(tempB,UcrossB%sz(1),UcrossB%sz(2),UcrossB%sz(3))
         call myCellCenter2Edge(tempB%x,B%x,g,3)
         call myCellCenter2Edge(tempB%y,B%y,g,3)
         call cross(UcrossB%z,U%z,V%z,W%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempB)
       end subroutine

       subroutine faceCurlCross_F(div,U,B,temp_E1,temp_E2,temp_CC,g)
         ! Computes
         ! 
         !                          d
         !  div_i = ∇ x (u x B) = --- (uj Bi - ui Bj)
         !                         dxj
         ! 
         ! While minimizing interpolations.
         !           div_i, U, B           --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         !           temp_CC               --> cell center.
         ! 
         implicit none
         type(vectorField),intent(inout) :: div
         type(vectorField),intent(in) :: U,B
         type(vectorField),intent(inout) :: temp_E1,temp_E2
         type(scalarField),intent(inout) :: temp_CC
         type(vectorField) :: temp
         type(grid),intent(in) :: g
         call allocateVectorField(temp,div)
         call faceAdvectDonorNoDiag(div,U,B,temp_E1,temp_E2,temp_CC,g)
         call faceAdvectDonorNoDiag(temp,B,U,temp_E1,temp_E2,temp_CC,g)
         call subtract(div,temp)
         call delete(temp)
       end subroutine

       subroutine faceAdvectDonorNoDiag(div,U,Ui,temp_E1,temp_E2,temp_CC,g)
         ! Computes
         ! 
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         !           temp_CC               --> cell center.
         ! 
         implicit none
         type(vectorField),intent(inout) :: div
         type(vectorField),intent(in) :: U,ui
         type(vectorField),intent(inout) :: temp_E1,temp_E2
         type(scalarField),intent(inout) :: temp_CC
         type(grid),intent(in) :: g
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next

         call zeroGhostPoints(div)

         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         ! x (y,z edges)
         call myFace2Edge(temp_E1%y,Ui%x,g,1,2)
         call myFace2Edge(temp_E2%y, U%z,g,3,2)
         call myFace2Edge(temp_E1%z,Ui%x,g,1,3)
         call myFace2Edge(temp_E2%z, U%y,g,2,3)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%assign(div%x,temp_E1%y,g,1,3,pad)
         call d%add(div%x,temp_E1%z,g,1,2,pad)

         ! y (x,z edges)
         call myFace2Edge(temp_E1%z,Ui%y,g,2,3)
         call myFace2Edge(temp_E2%z, U%x,g,1,3)
         call myFace2Edge(temp_E1%x,Ui%y,g,2,1)
         call myFace2Edge(temp_E2%x, U%z,g,3,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%assign(div%y,temp_E1%x,g,1,3,pad)
         call d%add(div%y,temp_E1%z,g,1,1,pad)

         ! z (x,y edges)
         call myFace2Edge(temp_E1%y,Ui%z,g,3,2)
         call myFace2Edge(temp_E2%y, U%x,g,1,2)
         call myFace2Edge(temp_E1%x,Ui%z,g,3,1)
         call myFace2Edge(temp_E2%x, U%y,g,2,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call d%assign(div%z,temp_E1%x,g,1,2,pad)
         call d%add(div%z,temp_E1%y,g,1,1,pad)
       end subroutine


       end module