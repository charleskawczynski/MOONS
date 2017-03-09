       module update_intermediate_field_BCs_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none
       private

       public :: update_intermediate_field_BCs
       public :: update_intermediate_field_BCs_gen
       public :: update_correction_field_BCs

       contains

       subroutine update_intermediate_field_BCs(Xstar,X,phi,scale,m,temp_F1,temp_F2,temp_CC)
         ! Update intermediate field BCs as described in Equation of
         !         Kim, J. & Moin, P. Application of a
         !         Fractional-Step Method to Incompressible
         !         Naview-Stokes Equations. J. Comput. Phys.
         !         323, 308–323 (1985).
         implicit none
         type(VF),intent(inout) :: Xstar,temp_F1,temp_F2,temp_CC
         type(SF),intent(in) :: phi
         real(cp),intent(in) :: scale
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         ! Dirichlet
         call grad(temp_F1,phi,m)           ! phi-component
         call multiply(temp_F1,scale)
         call add(temp_F1,X)                ! U  -component
         call assign_Dirichlet_BCs(Xstar,temp_F1)
         call assign_Periodic_BCs(Xstar,temp_F1)

         ! Neumann
         call lap_component(temp_CC,phi,m)
         call extrap(temp_CC,m)
         call cellCenter2Face(temp_F2,temp_CC,m)
         call grad_component(temp_F1,X,m) ! U  -component
         call add(temp_F1,temp_F2)        ! phi-component
         call assign_Neumann_BCs(Xstar%x,temp_F1)
         call assign_Neumann_BCs(Xstar%y,temp_F1)
         call assign_Neumann_BCs(Xstar%z,temp_F1)
       end subroutine

       subroutine update_intermediate_field_BCs_gen(Xstar,X,phi,scale,&
         m,temp_F,TF_Face1,TF_Face2,TF_CC_edge)
         ! Update intermediate field BCs as described in Equation of
         !         Kim, J. & Moin, P. Application of a
         !         Fractional-Step Method to Incompressible
         !         Naview-Stokes Equations. J. Comput. Phys.
         !         323, 308–323 (1985).
         implicit none
         type(TF),intent(inout) :: TF_Face1,TF_Face2,TF_CC_edge
         type(VF),intent(inout) :: Xstar,temp_F
         real(cp),intent(in) :: scale
         type(SF),intent(in) :: phi
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         ! Dirichlet / Periodic
         call grad(temp_F,phi,m)
         call multiply(temp_F,scale)
         call assign_Dirichlet_BCs(Xstar,temp_F)
         call assign_Periodic_BCs(Xstar,temp_F)

         ! Correction contribution (scale*partial_n partial_i phi)
         if (get_any_Robin(Xstar).or.get_any_Neumann(Xstar)) then
           call grad(temp_F,phi,m)
           call compute_PD_n_grad_X(TF_Face1,temp_F,m,TF_CC_edge)
           call multiply(TF_Face1,scale)
         endif

         ! if (get_any_Robin(Xstar).or.get_any_Neumann(Xstar)) then
         !   call grad(temp_CC_VF,phi,m)
         !   call multiply(temp_CC_VF,scale)
         !   call grad(TF_Face1,temp_CC_VF,m)
         !   call assign_Neumann_BCs(Xstar,TF_Face1)
         ! endif

         if (get_any_Robin(Xstar)) then ! Robin
           call assign(temp_F,X)
           call face2Face(TF_Face2,temp_F,m,TF_CC_edge%x%x) ! Need X on boundary
           call assign(TF_Face2%x%x,X%x)
           call assign(TF_Face2%y%y,X%y)
           call assign(TF_Face2%z%z,X%z)
           call face2Edge(TF_CC_edge%x%y,X%x,m,1,3)
           call face2Edge(TF_CC_edge%x%z,X%x,m,1,2)
           call face2Edge(TF_CC_edge%y%x,X%y,m,2,3)
           call face2Edge(TF_CC_edge%y%z,X%y,m,2,1)
           call face2Edge(TF_CC_edge%z%x,X%z,m,3,2)
           call face2Edge(TF_CC_edge%z%y,X%z,m,3,1)
           call multiply(TF_Face2,-1.0_cp/0.1_cp)
           call multiply(TF_CC_edge,-1.0_cp/0.1_cp)
           call add(TF_Face2,TF_Face1)
           call assign_Robin_BCs(Xstar%x,TF_Face2%x%x,1) ! x-faces for x-component
           call assign_Robin_BCs(Xstar%x,TF_CC_edge%x%z,2) ! y-faces for x-component
           call assign_Robin_BCs(Xstar%x,TF_CC_edge%x%y,3) ! z-faces for x-component

           call assign_Robin_BCs(Xstar%y,TF_CC_edge%y%z,1) ! x-faces for y-component
           call assign_Robin_BCs(Xstar%y,TF_Face2%y%y,2) ! y-faces for y-component
           call assign_Robin_BCs(Xstar%y,TF_CC_edge%y%x,3) ! z-faces for y-component

           call assign_Robin_BCs(Xstar%z,TF_CC_edge%z%y,1) ! x-faces for y-component
           call assign_Robin_BCs(Xstar%z,TF_CC_edge%z%x,2) ! y-faces for y-component
           call assign_Robin_BCs(Xstar%z,TF_Face2%z%z,3) ! z-faces for y-component
         endif

         if (get_any_Neumann(Xstar)) then ! Neumann
           call assign_Neumann_BCs(Xstar,TF_Face1)
         endif
         call update_BC_vals(Xstar)
       end subroutine

       subroutine compute_PD_n_grad_X(PD_n_grad_X,X,m,TF_CC_edge)
         ! Computes partial_n grad(X)
         ! PD_n_grad_X%x --> grad(u) on x,y,z faces
         ! PD_n_grad_X%y --> grad(v) on x,y,z faces
         ! PD_n_grad_X%z --> grad(w) on x,y,z faces
         implicit none
         type(TF),intent(inout) :: PD_n_grad_X,TF_CC_edge
         type(VF),intent(in) :: X
         type(mesh),intent(in) :: m
         call grad(TF_CC_edge,X,m)

         call cellCenter2Face(PD_n_grad_X%x%x,TF_CC_edge%x%x,m,1)
         call edge2Face(      PD_n_grad_X%x%y,TF_CC_edge%y%x,m,3,2)
         call edge2Face(      PD_n_grad_X%x%z,TF_CC_edge%z%x,m,2,3)

         call cellCenter2Face(PD_n_grad_X%y%y,TF_CC_edge%y%y,m,2)
         call edge2Face(      PD_n_grad_X%y%x,TF_CC_edge%x%y,m,3,1)
         call edge2Face(      PD_n_grad_X%y%z,TF_CC_edge%z%y,m,1,3)

         call cellCenter2Face(PD_n_grad_X%z%z,TF_CC_edge%z%z,m,3)
         call edge2Face(      PD_n_grad_X%z%x,TF_CC_edge%x%z,m,2,1)
         call edge2Face(      PD_n_grad_X%z%y,TF_CC_edge%y%z,m,1,2)
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