       module ops_advect_mod
       use current_precision_mod
       use ops_del_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use apply_stitches_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_aux_mod

       implicit none

       private
       public :: advect_U
       public :: advect_B

       contains

       subroutine advect_U(div,U,U_E,m,compute_U_E,temp_E,temp_CC)
         ! Computes
         ! 
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE and U_E         --> cell edge.
         !           temp_CC               --> cell center.
         ! 
         implicit none
         type(VF),intent(inout) :: div
         type(VF),intent(in) :: U
         type(TF),intent(inout) :: U_E
         type(VF),intent(inout) :: temp_E
         type(SF),intent(inout) :: temp_CC
         type(mesh),intent(in) :: m
         logical,intent(in) :: compute_U_E
         type(del) :: d

         ! d/dxj (uj ui) for i=j
         call face2CellCenter(temp_CC,U%x,m,1)
         call square(temp_CC)
         call d%assign(div%x,temp_CC,m,1,1,1)
         call face2CellCenter(temp_CC,U%y,m,2)
         call square(temp_CC)
         call d%assign(div%y,temp_CC,m,1,2,1)
         call face2CellCenter(temp_CC,U%z,m,3)
         call square(temp_CC)
         call d%assign(div%z,temp_CC,m,1,3,1)

         if (compute_U_E) call face2Edge_no_diag(U_E,U,m)
         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         call multiply(temp_E%y,U_E%x%y,U_E%z%y) ! x (y edge)
         call multiply(temp_E%z,U_E%x%z,U_E%y%z) ! x (z edge)
         call d%add(div%x,temp_E%y,m,1,3,1)
         call d%add(div%x,temp_E%z,m,1,2,1)

         call multiply(temp_E%x,U_E%y%x,U_E%z%x) ! y (x edge)
         call multiply(temp_E%z,U_E%y%z,U_E%x%z) ! y (z edge)
         call d%add(div%y,temp_E%x,m,1,3,1)
         call d%add(div%y,temp_E%z,m,1,1,1)

         call multiply(temp_E%x,U_E%z%x,U_E%y%x) ! z (x edge)
         call multiply(temp_E%y,U_E%z%y,U_E%x%y) ! z (y edge)
         call d%add(div%z,temp_E%x,m,1,2,1)
         call d%add(div%z,temp_E%y,m,1,1,1)
         call zeroGhostPoints(div)
       end subroutine

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

       end module