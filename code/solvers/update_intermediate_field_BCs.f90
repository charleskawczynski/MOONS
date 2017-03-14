       module update_intermediate_field_BCs_mod ! Most modern copy
       use current_precision_mod
       use mesh_mod
       use ops_del_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none
       private

       public :: update_intermediate_field_BCs

       contains

       subroutine update_intermediate_field_BCs(Xstar,phi,scale,m,temp_F,temp_CC)
         ! Update intermediate field BCs as described in Equation of
         !         Kim, J. & Moin, P. Application of a
         !         Fractional-Step Method to Incompressible
         !         Naview-Stokes Equations. J. Comput. Phys.
         !         323, 308–323 (1985).
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F
         real(cp),intent(in) :: scale
         type(SF),intent(in) :: phi
         type(SF),intent(inout) :: temp_CC
         type(mesh),intent(in) :: m
         type(del) :: d
         ! Dirichlet / Periodic
         call grad(temp_F,phi,m)
         call multiply(temp_F,scale)
         call assign_Dirichlet_BCs(Xstar,temp_F)
         call assign_Periodic_BCs(Xstar,temp_F)
         ! Neumann
         if (get_any_Neumann(Xstar)) then
           call d%assign(temp_CC,phi,m,1,1,0); call grad(temp_F,temp_CC,m)
           call assign_Neumann_BCs_wall_normal(Xstar%x,temp_F)
           call d%assign(temp_CC,phi,m,1,2,0); call grad(temp_F,temp_CC,m)
           call assign_Neumann_BCs_wall_normal(Xstar%y,temp_F)
           call d%assign(temp_CC,phi,m,1,3,0); call grad(temp_F,temp_CC,m)
           call assign_Neumann_BCs_wall_normal(Xstar%z,temp_F)
           call multiply_Neumann_BCs(Xstar,scale)
         endif
         call multiply_BCs_by_nhat(Xstar)
         call update_BC_vals(Xstar)
       end subroutine

       end module