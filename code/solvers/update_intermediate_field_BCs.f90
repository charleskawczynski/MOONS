       module update_intermediate_field_BCs_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none
       private

       public :: update_intermediate_field_BCs
       public :: update_intermediate_field_BCs_new

       contains

       subroutine update_intermediate_field_BCs(Xstar,X,m,temp_F)
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         call grad_component(temp_F,X,m)
         call assign_Neumann_BCs(Xstar,temp_F)
       end subroutine

       subroutine update_intermediate_field_BCs_new(Xstar,X,phi,m,temp_F1,temp_F2,temp_CC)
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F1,temp_F2,temp_CC
         type(SF),intent(in) :: phi
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         ! Dirichlet
         call grad(temp_F1,phi,m)         ! phi-component
         call add(temp_F1,X)              ! U  -component
         call assign_Dirichlet_BCs(Xstar,temp_F1)
         ! Neumann
         call lap_component(temp_CC,phi,m)
         call extrap(temp_CC,m)
         call cellCenter2Face(temp_F2,temp_CC,m)
         !
         call grad_component(temp_F1,X,m) ! U  -component
         call add(temp_F1,temp_F2)        ! phi-component
         call assign_Neumann_BCs(Xstar,temp_F1)
       end subroutine

       end module