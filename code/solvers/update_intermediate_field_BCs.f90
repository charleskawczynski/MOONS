       module update_intermediate_field_BCs_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_discrete_mod

       implicit none
       private

       public :: update_intermediate_field_BCs

       contains

       subroutine update_intermediate_field_BCs(Xstar,X,m,temp_F)
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         call grad_component(temp_F,X,m)
         call assign_Neumann_BCs(Xstar,temp_F)
       end subroutine

       end module