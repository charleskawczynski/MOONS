       module ops_discrete_complex_mod
       use ops_del_mod
       use mesh_mod
       use VF_mod
       use TF_mod
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

       public :: advect_B        ! ∇x(uxB)

       public :: faceCrossFace_E ! Assumes B lives on cell face
       public :: faceCurlCross_F ! Assumes B lives on cell face

       ! Not recommended since B is assumed to live on cell center..
       public :: faceCrossCC_E   ! Assumes B lives on cell center
       public :: edgeCrossCC_E   ! Assumes B lives on cell center

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine advect_B(adv,U_E,B_F,m,temp_E_TF,temp_E)
         ! Computes
         ! 
         !      (∇ x ( u_edge x B_face )_edge)_face
         ! 
         ! While minimizing interpolations.
         implicit none
         type(VF),intent(inout) :: adv,temp_E
         type(TF),intent(inout) :: temp_E_TF
         type(TF),intent(in) :: U_E
         type(VF),intent(in) :: B_F
         type(mesh),intent(in) :: m
         call edgeCrossFace_E(temp_E,U_E,B_F,m,temp_E_TF)
         call curl(adv,temp_E,m)
       end subroutine

       subroutine edgeCrossFace_E(AcrossB_E,A_E,B_F,m,temp_B_E)
         ! Computes
         ! 
         !      ( u_edge x B_face )_edge
         ! 
         ! While minimizing interpolations.
         ! There is some memory abuse here however, 
         ! since the diagonals are not used..
         implicit none
         type(VF),intent(inout) :: AcrossB_E
         type(TF),intent(in) :: A_E
         type(VF),intent(in) :: B_F
         type(mesh),intent(in) :: m
         type(TF),intent(inout) :: temp_B_E
         call face2Edge_no_diag(temp_B_E,B_F,m)
         call cross(AcrossB_E%x,A_E%x%x,A_E%y%x,A_E%z%x,temp_B_E%x%x,temp_B_E%y%x,temp_B_E%z%x,1)
         call cross(AcrossB_E%y,A_E%x%y,A_E%y%y,A_E%z%y,temp_B_E%x%y,temp_B_E%y%y,temp_B_E%z%y,2)
         call cross(AcrossB_E%z,A_E%x%z,A_E%y%z,A_E%z%z,temp_B_E%x%z,temp_B_E%y%z,temp_B_E%z%z,3)
       end subroutine

       subroutine faceCrossFace_E(AcrossB,A,B,m)
         ! Computes
         ! 
         !      ( u_face x B_face )_edge
         ! 
         ! While minimizing interpolations.
         implicit none
         type(VF),intent(inout) :: AcrossB
         type(VF),intent(in) :: A,B
         type(mesh),intent(in) :: m
         type(VF) :: tempA,tempB
         call init(tempA,AcrossB)
         call init(tempB,AcrossB)
         call face2Edge(tempA%y,A%y,m,2,1)
         call face2Edge(tempA%z,A%z,m,3,1)
         call face2Edge(tempB%y,B%y,m,2,1)
         call face2Edge(tempB%z,B%z,m,3,1)
         call cross(AcrossB%x,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,1)
         call face2Edge(tempA%x,A%x,m,1,2)
         call face2Edge(tempA%z,A%z,m,3,2)
         call face2Edge(tempB%x,B%x,m,1,2)
         call face2Edge(tempB%z,B%z,m,3,2)
         call cross(AcrossB%y,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,2)
         call face2Edge(tempA%x,A%x,m,1,3)
         call face2Edge(tempA%y,A%y,m,2,3)
         call face2Edge(tempB%x,B%x,m,1,3)
         call face2Edge(tempB%y,B%y,m,2,3)
         call cross(AcrossB%z,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempA)
         call delete(tempB)
       end subroutine

       subroutine faceCurlCross_F(div,U,B,m,temp_E1,temp_E2,temp_F)
         ! The name of this routine should be fixed since this is 
         ! technically not correct. The divergence of uj Bi - ui Bj
         ! is computed, which is minus curl(uxB).
         ! 
         ! Computes
         ! 
         !                                                d
         !  div_i = {∇ x (u_face x B_face)}_face =   -   --- (uj Bi - ui Bj)
         !                                           ^   dxj
         !                                           |
         !                                           |
         !                                          minus
         ! 
         ! While minimizing interpolations.
         !           div_i, U, B           --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         ! 
         implicit none
         type(VF),intent(inout) :: div
         type(VF),intent(in) :: U,B
         type(VF),intent(inout) :: temp_E1,temp_E2,temp_F
         type(mesh),intent(in) :: m
         call faceAdvectDonorNoDiag(div,U,B,temp_E1,temp_E2,m)
         call faceAdvectDonorNoDiag(temp_F,B,U,temp_E1,temp_E2,m)
         call subtract(div,temp_F)
       end subroutine

       subroutine faceAdvectDonorNoDiag(div,U,Ui,temp_E1,temp_E2,m)
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
         type(mesh),intent(in) :: m
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next

         call zeroGhostPoints(div)

         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         ! x (y,z edges)
         call face2Edge(temp_E1%y,Ui%x,m,1,2)
         call face2Edge(temp_E2%y, U%z,m,3,2)
         call face2Edge(temp_E1%z,Ui%x,m,1,3)
         call face2Edge(temp_E2%z, U%y,m,2,3)
         call multiply(temp_E1%y,temp_E2%y)
         call multiply(temp_E1%z,temp_E2%z)
         call d%assign(div%x,temp_E1%y,m,1,3,pad)
         call d%add(div%x,temp_E1%z,m,1,2,pad)

         ! y (x,z edges)
         call face2Edge(temp_E1%z,Ui%y,m,2,3)
         call face2Edge(temp_E2%z, U%x,m,1,3)
         call face2Edge(temp_E1%x,Ui%y,m,2,1)
         call face2Edge(temp_E2%x, U%z,m,3,1)
         call multiply(temp_E1%x,temp_E2%x)
         call multiply(temp_E1%z,temp_E2%z)
         call d%assign(div%y,temp_E1%x,m,1,3,pad)
         call d%add(div%y,temp_E1%z,m,1,1,pad)

         ! z (x,y edges)
         call face2Edge(temp_E1%y,Ui%z,m,3,2)
         call face2Edge(temp_E2%y, U%x,m,1,2)
         call face2Edge(temp_E1%x,Ui%z,m,3,1)
         call face2Edge(temp_E2%x, U%y,m,2,1)
         call multiply(temp_E1%x,temp_E2%x)
         call multiply(temp_E1%y,temp_E2%y)
         call d%assign(div%z,temp_E1%x,m,1,2,pad)
         call d%add(div%z,temp_E1%y,m,1,1,pad)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! ************************ OUTDATED ************************
       ! **********************************************************
       ! **********************************************************

       subroutine faceCrossCC_E(AcrossB,A,B,m,tempF)
         ! Computes
         ! 
         !      ( u_face x B_CC )_edge
         ! 
         ! While minimizing interpolations.
         implicit none
         type(VF),intent(inout) :: AcrossB,tempF
         type(VF),intent(in) :: A,B
         type(mesh),intent(in) :: m
         type(VF) :: tempA,tempB
         call init(tempA,AcrossB%x)
         call init(tempB,tempA)
         call face2Edge(tempA%y,A%y,m,2,1)
         call face2Edge(tempA%z,A%z,m,3,1)
         call cellCenter2Edge(tempB%y,B%y,m,tempF%y,1)
         call cellCenter2Edge(tempB%z,B%z,m,tempF%y,1)
         call cross(AcrossB%x,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,1)
         call delete(tempA)
         call delete(tempB)
         call init(tempA,AcrossB%y)
         call init(tempB,tempA)
         call face2Edge(tempA%x,A%x,m,1,2)
         call face2Edge(tempA%z,A%z,m,3,2)
         call cellCenter2Edge(tempB%x,B%x,m,tempF%x,2)
         call cellCenter2Edge(tempB%z,B%z,m,tempF%x,2)
         call cross(AcrossB%y,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,2)
         call delete(tempA)
         call delete(tempB)
         call init(tempA,AcrossB%z)
         call init(tempB,tempA)
         call face2Edge(tempA%x,A%x,m,1,3)
         call face2Edge(tempA%y,A%y,m,2,3)
         call cellCenter2Edge(tempB%x,B%x,m,tempF%x,3)
         call cellCenter2Edge(tempB%y,B%y,m,tempF%x,3)
         call cross(AcrossB%z,tempA%x,tempA%y,tempA%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempA)
         call delete(tempB)
       end subroutine

       subroutine edgeCrossCC_E(UcrossB,U,V,W,B,m,tempF)
         ! This routine is essentially outdated 
         ! since B should live on the cell face...
         ! 
         ! Computes
         ! 
         !      ( u_edge x B_CC )_edge
         ! 
         ! While minimizing interpolations.
         implicit none
         type(VF),intent(inout) :: UcrossB,tempF
         type(VF),intent(in) :: U,V,W,B
         type(mesh),intent(in) :: m
         type(VF) :: tempB
         call init(tempB,UcrossB%x)
         call cellCenter2Edge(tempB%y,B%y,m,tempF%y,1)
         call cellCenter2Edge(tempB%z,B%z,m,tempF%y,1)
         call cross(UcrossB%x,U%x,V%x,W%x,tempB%x,tempB%y,tempB%z,1)
         call delete(tempB)
         call init(tempB,UcrossB%y)
         call cellCenter2Edge(tempB%x,B%x,m,tempF%x,2)
         call cellCenter2Edge(tempB%z,B%z,m,tempF%x,2)
         call cross(UcrossB%y,U%y,V%y,W%y,tempB%x,tempB%y,tempB%z,2)
         call delete(tempB)
         call init(tempB,UcrossB%z)
         call cellCenter2Edge(tempB%x,B%x,m,tempF%x,3)
         call cellCenter2Edge(tempB%y,B%y,m,tempF%x,3)
         call cross(UcrossB%z,U%z,V%z,W%z,tempB%x,tempB%y,tempB%z,3)
         call delete(tempB)
       end subroutine

       end module