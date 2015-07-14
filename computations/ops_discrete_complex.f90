       module ops_discrete_complex_mod
       use del_mod
       use delVC_mod
       use grid_mod
       use VF_mod
       use ops_interp_mod
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
         type(VF),intent(inout) :: AcrossB
         type(VF),intent(in) :: A,B
         type(grid),intent(in) :: g
         type(VF) :: tempA,tempB
         call init(tempA,AcrossB)
         call init(tempB,AcrossB)
         call face2Edge(tempA%y,A%y,g,2,1)
         call face2Edge(tempA%z,A%z,g,3,1)
         call face2Edge(tempB%y,B%y,g,2,1)
         call face2Edge(tempB%z,B%z,g,3,1)
         call cross(AcrossB%x,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,1)
         call face2Edge(tempA%x,A%x,g,1,2)
         call face2Edge(tempA%z,A%z,g,3,2)
         call face2Edge(tempB%x,B%x,g,1,2)
         call face2Edge(tempB%z,B%z,g,3,2)
         call cross(AcrossB%y,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,2)
         call face2Edge(tempA%x,A%x,g,1,3)
         call face2Edge(tempA%y,A%y,g,2,3)
         call face2Edge(tempB%x,B%x,g,1,3)
         call face2Edge(tempB%y,B%y,g,2,3)
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
         type(VF),intent(inout) :: AcrossB
         type(VF),intent(in) :: A,B
         type(grid),intent(in) :: g
         type(VF) :: tempA,tempB
         call init(tempA,AcrossB%sx(1),AcrossB%sx(2),AcrossB%sx(3))
         call init(tempB,tempA)
         call face2Edge(tempA%y,A%y,g,2,1)
         call face2Edge(tempA%z,A%z,g,3,1)
         call cellCenter2Edge(tempB%y,B%y,g,1)
         call cellCenter2Edge(tempB%z,B%z,g,1)
         call cross(AcrossB%x,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,1)
         call delete(tempA)
         call delete(tempB)
         call init(tempA,AcrossB%sy(1),AcrossB%sy(2),AcrossB%sy(3))
         call init(tempB,tempA)
         call face2Edge(tempA%x,A%x,g,1,2)
         call face2Edge(tempA%z,A%z,g,3,2)
         call cellCenter2Edge(tempB%x,B%x,g,2)
         call cellCenter2Edge(tempB%z,B%z,g,2)
         call cross(AcrossB%y,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,2)
         call delete(tempA)
         call delete(tempB)
         call init(tempA,AcrossB%sz(1),AcrossB%sz(2),AcrossB%sz(3))
         call init(tempB,tempA)
         call face2Edge(tempA%x,A%x,g,1,3)
         call face2Edge(tempA%y,A%y,g,2,3)
         call cellCenter2Edge(tempB%x,B%x,g,3)
         call cellCenter2Edge(tempB%y,B%y,g,3)
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
         type(VF),intent(inout) :: UcrossB
         type(VF),intent(in) :: U,V,W,B
         type(grid),intent(in) :: g
         type(VF) :: tempB
         call init(tempB,UcrossB%sx(1),UcrossB%sx(2),UcrossB%sx(3))
         call cellCenter2Edge(tempB%y,B%y,g,1)
         call cellCenter2Edge(tempB%z,B%z,g,1)
         call cross(UcrossB%x,U%x,V%x,W%x,tempB%x,tempB%y,tempB%z,1)
         call delete(tempB)
         call init(tempB,UcrossB%sy(1),UcrossB%sy(2),UcrossB%sy(3))
         call cellCenter2Edge(tempB%x,B%x,g,2)
         call cellCenter2Edge(tempB%z,B%z,g,2)
         call cross(UcrossB%y,U%y,V%y,W%y,tempB%x,tempB%y,tempB%z,2)
         call delete(tempB)
         call init(tempB,UcrossB%sz(1),UcrossB%sz(2),UcrossB%sz(3))
         call cellCenter2Edge(tempB%x,B%x,g,3)
         call cellCenter2Edge(tempB%y,B%y,g,3)
         call cross(UcrossB%z,U%z,V%z,W%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempB)
       end subroutine

       subroutine faceCurlCross_F(div,U,B,temp_E1,temp_E2,g)
         ! Computes
         ! 
         !                                           d
         !  div_i = {∇ x (u_face x B_face)}_face = --- (uj Bi - ui Bj)
         !                                          dxj
         ! 
         ! While minimizing interpolations.
         !           div_i, U, B           --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         ! 
         implicit none
         type(VF),intent(inout) :: div
         type(VF),intent(in) :: U,B
         type(VF),intent(inout) :: temp_E1,temp_E2
         type(VF) :: temp
         type(grid),intent(in) :: g
         call init(temp,div)
         call faceAdvectDonorNoDiag(div,U,B,temp_E1,temp_E2,g)
         call faceAdvectDonorNoDiag(temp,B,U,temp_E1,temp_E2,g)
         call subtract(div,temp)
         call delete(temp)
       end subroutine

       subroutine faceAdvectDonorNoDiag(div,U,Ui,temp_E1,temp_E2,g)
         ! Computes
         ! 
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         ! 
         implicit none
         type(VF),intent(inout) :: div
         type(VF),intent(in) :: U,ui
         type(VF),intent(inout) :: temp_E1,temp_E2
         type(grid),intent(in) :: g
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next

         call zeroGhostPoints(div)

         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         ! x (y,z edges)
         call face2Edge(temp_E1%y,Ui%x,g,1,2)
         call face2Edge(temp_E2%y, U%z,g,3,2)
         call face2Edge(temp_E1%z,Ui%x,g,1,3)
         call face2Edge(temp_E2%z, U%y,g,2,3)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%assign(div%x,temp_E1%y,g,1,3,pad)
         call d%add(div%x,temp_E1%z,g,1,2,pad)

         ! y (x,z edges)
         call face2Edge(temp_E1%z,Ui%y,g,2,3)
         call face2Edge(temp_E2%z, U%x,g,1,3)
         call face2Edge(temp_E1%x,Ui%y,g,2,1)
         call face2Edge(temp_E2%x, U%z,g,3,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%assign(div%y,temp_E1%x,g,1,3,pad)
         call d%add(div%y,temp_E1%z,g,1,1,pad)

         ! z (x,y edges)
         call face2Edge(temp_E1%y,Ui%z,g,3,2)
         call face2Edge(temp_E2%y, U%x,g,1,2)
         call face2Edge(temp_E1%x,Ui%z,g,3,1)
         call face2Edge(temp_E2%x, U%y,g,2,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call d%assign(div%z,temp_E1%x,g,1,2,pad)
         call d%add(div%z,temp_E1%y,g,1,1,pad)
       end subroutine


       end module