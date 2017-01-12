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

       subroutine update_intermediate_field_BCs(Xstar,X,grad_phi,m,temp_X)
         implicit none
         type(VF),intent(inout) :: Xstar,temp_X
         type(VF),intent(in) :: X,grad_phi
         type(mesh),intent(in) :: m
         call assign(temp_X,X)
         call add(temp_X,grad_phi)
         call assign_Neumann_BCs(Xstar,temp_X)
       end subroutine

       subroutine update_intermediate_field_BCs2(Xstar,phi,m,dt,tempX)
         implicit none
         type(VF),intent(inout) :: Xstar,tempX
         type(SF),intent(in) :: phi
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         call lap_component(tempX,phi,m)
         ! call multiply(tempX,dt)
         call assign_Neumann_BCs(Xstar,tempX)
       end subroutine

       end module