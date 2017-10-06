       module ops_advect_mod
       use current_precision_mod
       use ops_del_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_aux_mod

       implicit none

       private
       public :: advect_U_divergence
       public :: advect_U_convection
       public :: advect_B

       interface advect_U_convection; module procedure advect_U_convection1; end interface
       interface advect_U_convection; module procedure advect_U_convection2; end interface

       contains

       subroutine advect_U_divergence(div,U,U_E,m,compute_U_E,temp_E,temp_CC)
         ! Computes
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE and U_E         --> cell edge.
         !           temp_CC               --> cell center.
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
         call d%assign(div%x,temp_CC,m,1,1,0) ! zero pad needed for Neumann BCs
         call face2CellCenter(temp_CC,U%y,m,2)
         call square(temp_CC)
         call d%assign(div%y,temp_CC,m,1,2,0) ! zero pad needed for Neumann BCs
         call face2CellCenter(temp_CC,U%z,m,3)
         call square(temp_CC)
         call d%assign(div%z,temp_CC,m,1,3,0) ! zero pad needed for Neumann BCs
         if (compute_U_E) call face2Edge_no_diag(U_E,U,m)
         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         call multiply(temp_E%y,U_E%x%y,U_E%z%y) ! x (y edge)
         call multiply(temp_E%z,U_E%x%z,U_E%y%z) ! x (z edge)
         call d%add(div%x,temp_E%y,m,1,3,0) ! zero pad needed for Neumann BCs
         call d%add(div%x,temp_E%z,m,1,2,0) ! zero pad needed for Neumann BCs

         call multiply(temp_E%x,U_E%y%x,U_E%z%x) ! y (x edge)
         call multiply(temp_E%z,U_E%y%z,U_E%x%z) ! y (z edge)
         call d%add(div%y,temp_E%x,m,1,3,0) ! zero pad needed for Neumann BCs
         call d%add(div%y,temp_E%z,m,1,1,0) ! zero pad needed for Neumann BCs

         call multiply(temp_E%x,U_E%z%x,U_E%y%x) ! z (x edge)
         call multiply(temp_E%y,U_E%z%y,U_E%x%y) ! z (y edge)
         call d%add(div%z,temp_E%x,m,1,2,0) ! zero pad needed for Neumann BCs
         call d%add(div%z,temp_E%y,m,1,1,0) ! zero pad needed for Neumann BCs
         ! call assign_ghost_XPeriodic(div,0.0_cp)
       end subroutine

       subroutine advect_U_convection1(div,U,U_E,m,compute_U_E,temp_F1,temp_F2,temp_CC)
         ! Computes
         !               d
         !  div_i = u_j --- u_i
         !              dx_j
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE and U_E         --> cell edge.
         !           temp_CC               --> cell center.
         implicit none
         type(VF),intent(inout) :: div,temp_F1,temp_F2
         type(VF),intent(in) :: U
         type(TF),intent(inout) :: U_E
         type(SF),intent(inout) :: temp_CC
         type(mesh),intent(in) :: m
         logical,intent(in) :: compute_U_E
         type(del) :: d
         ! d/dxj (uj ui) for i=j
         call face2CellCenter(temp_CC,U%x,m,1)
         call d%assign(div%x,temp_CC,m,1,1,0)
         call multiply(div%x,U%x)
         call face2CellCenter(temp_CC,U%y,m,2)
         call d%assign(div%y,temp_CC,m,1,2,0)
         call multiply(div%y,U%y)
         call face2CellCenter(temp_CC,U%z,m,3)
         call d%assign(div%z,temp_CC,m,1,3,0)
         call multiply(div%z,U%z)
         if (compute_U_E) call face2Edge_no_diag(U_E,U,m)
         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         call d%assign( temp_F1%x,    U_E%x%z,m,1      ,2,0)
         call face2Face(temp_F2%x,        U%y,m,temp_CC,2,1)
         call multiply( temp_F1%x,temp_F2%x)
         call add(div%x,temp_F1%x)
         call d%assign( temp_F1%x,    U_E%x%y,m,1      ,3,0)
         call face2Face(temp_F2%x,        U%z,m,temp_CC,3,1)
         call multiply( temp_F1%x,temp_F2%x)
         call add(div%x,temp_F1%x)

         call d%assign( temp_F1%y,    U_E%y%z,m,1      ,1,0)
         call face2Face(temp_F2%y,        U%x,m,temp_CC,1,2)
         call multiply( temp_F1%y,temp_F2%y)
         call add(div%y,temp_F1%y)
         call d%assign( temp_F1%y,    U_E%y%x,m,1      ,3,0)
         call face2Face(temp_F2%y,        U%z,m,temp_CC,3,2)
         call multiply( temp_F1%y,temp_F2%y)
         call add(div%y,temp_F1%y)

         call d%assign( temp_F1%z,    U_E%z%y,m,1      ,1,0)
         call face2Face(temp_F2%z,        U%x,m,temp_CC,1,3)
         call multiply( temp_F1%z,temp_F2%z)
         call add(div%z,temp_F1%z)
         call d%assign( temp_F1%z,    U_E%z%x,m,1      ,2,0)
         call face2Face(temp_F2%z,        U%y,m,temp_CC,2,3)
         call multiply( temp_F1%z,temp_F2%z)
         call add(div%z,temp_F1%z)
         ! call assign_ghost_XPeriodic(div,0.0_cp)
       end subroutine

       subroutine advect_U_convection2(div,U_adv,U,U_E,m,temp_F1,temp_F2,temp_CC)
         ! Computes
         !                   d
         !  div_i = u_adv_j --- u_i
         !                  dx_j
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE and U_E         --> cell edge.
         !           temp_CC               --> cell center.
         implicit none
         type(VF),intent(inout) :: div,temp_F1,temp_F2
         type(VF),intent(in) :: U,U_adv
         type(TF),intent(inout) :: U_E
         type(SF),intent(inout) :: temp_CC
         type(mesh),intent(in) :: m
         type(del) :: d
         ! d/dxj (uj ui) for i=j
         call face2CellCenter(temp_CC,U%x,m,1)
         call d%assign(div%x,temp_CC,m,1,1,0)
         call multiply(div%x,U_adv%x)
         call face2CellCenter(temp_CC,U%y,m,2)
         call d%assign(div%y,temp_CC,m,1,2,0)
         call multiply(div%y,U_adv%y)
         call face2CellCenter(temp_CC,U%z,m,3)
         call d%assign(div%z,temp_CC,m,1,3,0)
         call multiply(div%z,U_adv%z)
         call face2Edge_no_diag(U_E,U,m)
         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         call d%assign( temp_F1%x,    U_E%x%z,m,1      ,2,0)
         call face2Face(temp_F2%x,    U_adv%y,m,temp_CC,2,1)
         call multiply( temp_F1%x,temp_F2%x)
         call add(div%x,temp_F1%x)
         call d%assign( temp_F1%x,    U_E%x%y,m,1      ,3,0)
         call face2Face(temp_F2%x,    U_adv%z,m,temp_CC,3,1)
         call multiply( temp_F1%x,temp_F2%x)
         call add(div%x,temp_F1%x)

         call d%assign( temp_F1%y,    U_E%y%z,m,1      ,1,0)
         call face2Face(temp_F2%y,    U_adv%x,m,temp_CC,1,2)
         call multiply( temp_F1%y,temp_F2%y)
         call add(div%y,temp_F1%y)
         call d%assign( temp_F1%y,    U_E%y%x,m,1      ,3,0)
         call face2Face(temp_F2%y,    U_adv%z,m,temp_CC,3,2)
         call multiply( temp_F1%y,temp_F2%y)
         call add(div%y,temp_F1%y)

         call d%assign( temp_F1%z,    U_E%z%y,m,1      ,1,0)
         call face2Face(temp_F2%z,    U_adv%x,m,temp_CC,1,3)
         call multiply( temp_F1%z,temp_F2%z)
         call add(div%z,temp_F1%z)
         call d%assign( temp_F1%z,    U_E%z%x,m,1      ,2,0)
         call face2Face(temp_F2%z,    U_adv%y,m,temp_CC,2,3)
         call multiply( temp_F1%z,temp_F2%z)
         call add(div%z,temp_F1%z)
         call assign_ghost_XPeriodic(div,0.0_cp)
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
         call cross_product(AcrossB_E,A_E,temp_B_E)
       end subroutine

       end module