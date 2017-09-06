       module update_intermediate_field_BCs_mod ! Most modern copy
       use current_precision_mod
       use mesh_extend_mod
       use ops_del_mod
       use SF_extend_mod
       use VF_mod
       use TF_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none
       private

       public :: update_intermediate_field_BCs
       public :: update_correction_field_BCs

       contains

       subroutine update_intermediate_field_BCs(Xstar,phi,scale,m,temp_F,temp_E,temp_CC)
         ! Update intermediate field BCs as described in Equation of
         !         Kim, J. & Moin, P. Application of a
         !         Fractional-Step Method to Incompressible
         !         Naview-Stokes Equations. J. Comput. Phys.
         !         323, 308–323 (1985).
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F,temp_E
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
           call d%assign(temp_E%y,temp_F%x,m,1,3,0); call assign_Neumann_BCs_wall_normal(Xstar%x,temp_E%y,3)
           call d%assign(temp_E%z,temp_F%x,m,1,2,0); call assign_Neumann_BCs_wall_normal(Xstar%x,temp_E%z,2)

           call d%assign(temp_E%x,temp_F%y,m,1,3,0); call assign_Neumann_BCs_wall_normal(Xstar%y,temp_E%x,3)
           call d%assign(temp_E%z,temp_F%y,m,1,1,0); call assign_Neumann_BCs_wall_normal(Xstar%y,temp_E%z,1)

           call d%assign(temp_E%x,temp_F%z,m,1,2,0); call assign_Neumann_BCs_wall_normal(Xstar%z,temp_E%x,2)
           call d%assign(temp_E%y,temp_F%z,m,1,1,0); call assign_Neumann_BCs_wall_normal(Xstar%z,temp_E%y,1)

           call d%assign(temp_CC,phi,m,2,1,0); call cellCenter2Face(temp_F%x,temp_CC,m,1)
           call assign_Neumann_BCs_wall_normal(Xstar%x,temp_F%x,1)
           call d%assign(temp_CC,phi,m,2,2,0); call cellCenter2Face(temp_F%y,temp_CC,m,2)
           call assign_Neumann_BCs_wall_normal(Xstar%y,temp_F%y,2)
           call d%assign(temp_CC,phi,m,2,3,0); call cellCenter2Face(temp_F%z,temp_CC,m,3)
           call assign_Neumann_BCs_wall_normal(Xstar%z,temp_F%z,3)

           call multiply_Neumann_BCs(Xstar,scale)
         endif
         call multiply_BCs_by_nhat(Xstar)
         call update_BC_vals(Xstar)
       end subroutine

       subroutine update_correction_field_BCs(phi,Xstar,scale,temp_F1)
         ! Apply Neumann BCs as described in Equation 16 of:
         !        Bandaru, V., Boeck, T., Krasnov, D. & Schumacher, J.
         !        A hybrid finite difference-boundary element procedure
         !        for the simulation of turbulent MHD duct flow at finite
         !        magnetic Reynolds number. J. Comput. Phys. 304, 320–339 (2016).
         implicit none
         type(SF),intent(inout) :: phi
         type(VF),intent(inout) :: temp_F1
         type(VF),intent(in) :: Xstar
         real(cp),intent(in) :: scale
         call multiply(temp_F1,Xstar,scale)
         call multiply_nhat(temp_F1,Xstar)
         call assign_Neumann_BCs_wall_normal(phi,temp_F1) ! Only normal components
       end subroutine

       end module