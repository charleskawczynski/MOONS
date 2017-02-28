       module update_intermediate_field_BCs_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_interp_mod
       use apply_periodic_BCs_mod
       use ops_discrete_mod

       implicit none
       private

       public :: update_intermediate_field_BCs
       public :: update_correction_field_BCs

       contains

       subroutine update_intermediate_field_BCs(Xstar,X,phi,m,temp_F1,temp_F2,temp_CC)
         ! Update intermediate field BCs as described in Equation of
         !         Kim, J. & Moin, P. Application of a
         !         Fractional-Step Method to Incompressible
         !         Naview-Stokes Equations. J. Comput. Phys.
         !         323, 308–323 (1985).
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F1,temp_F2,temp_CC
         type(SF),intent(in) :: phi
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         ! Dirichlet
         call grad(temp_F1,phi,m)           ! phi-component
         call add(temp_F1,X)                ! U  -component
         call assign_Dirichlet_BCs(Xstar,temp_F1)
         call assign_Periodic_BCs(Xstar,temp_F1)

         ! Neumann
         call lap_component(temp_CC,phi,m)
         call extrap(temp_CC,m)
         call cellCenter2Face(temp_F2,temp_CC,m)
         call grad_component(temp_F1,X,m) ! U  -component
         call add(temp_F1,temp_F2)        ! phi-component
         call assign_Neumann_BCs(Xstar,temp_F1)
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
         call assign_Neumann_BCs(phi,temp_F1)
       end subroutine

       end module