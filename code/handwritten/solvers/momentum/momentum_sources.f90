       module momentum_sources_mod
       use current_precision_mod
       use SF_extend_mod
       use VF_extend_mod
       use export_raw_processed_mod
       use TF_extend_mod
       use ops_embedExtract_mod
       use time_marching_params_mod
       use mesh_domain_mod
       use mesh_extend_mod
       use apply_BCs_mod
       use IO_export_mod
       use ops_aux_mod
       use ops_advect_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_norms_mod
       use dimensionless_params_mod
       use data_location_mod
       use matrix_free_params_mod

       implicit none

       private
       public :: compute_add_pressure_grad
       public :: compute_add_advection_divergence
       public :: compute_add_advection_convection
       public :: compute_add_advection_base_flow
       public :: compute_add_diffusion
       public :: compute_add_MPG
       public :: compute_add_JCrossB
       public :: compute_add_JCrossB_div_free
       public :: compute_add_Q2D_JCrossB
       public :: compute_add_buoyancy
       public :: compute_add_gravity

       contains

       subroutine compute_add_pressure_grad(F,m,p,scale,temp_F)
         implicit none
         type(VF),intent(inout) :: F,temp_F
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         call grad(temp_F,p,m)
         call add_product(F,temp_F,scale)
       end subroutine

       subroutine compute_add_advection_divergence(F,m,U,U_E,scale,temp_F,temp_E,temp_CC)
         implicit none
         type(VF),intent(inout) :: F,temp_F
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         type(SF),intent(inout) :: temp_CC
         type(VF),intent(in) :: U
         type(VF),intent(inout) :: temp_E
         type(TF),intent(inout) :: U_E
         call advect_U_divergence(temp_F,U,U_E,m,.false.,temp_E,temp_CC)
         call add_product(F,temp_F,scale)
       end subroutine

       subroutine compute_add_advection_convection(F,m,U,U_E,scale,temp_F,temp_F1,temp_F2,temp_CC)
         implicit none
         type(VF),intent(inout) :: F,temp_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         real(cp),intent(in) :: scale
         type(VF),intent(in) :: U
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(TF),intent(inout) :: U_E
         call advect_U_convection(temp_F,U,U_E,m,.false.,temp_F1,temp_F2,temp_CC)
         call add_product(F,temp_F,scale)
       end subroutine

       subroutine compute_add_advection_base_flow(F,m,U_base,U,U_E,scale,temp_F,temp_F1,temp_F2,temp_CC)
         implicit none
         type(VF),intent(inout) :: F,temp_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         real(cp),intent(in) :: scale
         type(VF),intent(in) :: U,U_base
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(TF),intent(inout) :: U_E
         call advect_U_convection(temp_F,U_base,U,U_E,m,temp_F1,temp_F2,temp_CC)
         call add_product(F,temp_F,scale)
       end subroutine

       subroutine compute_add_diffusion(F,m,U,scale,temp_F)
         implicit none
         type(VF),intent(inout) :: F,temp_F
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U
         real(cp),intent(in) :: scale
         call lap(temp_F,U,m)
         call add_product(F,temp_F,scale)
       end subroutine

       subroutine compute_add_MPG(U,TMP,scale,dir)
         type(VF),intent(inout) :: U
         type(time_marching_params),intent(in) :: TMP
         real(cp),intent(in) :: scale
         integer,intent(in) :: dir
         select case (dir)
         case (0);
         case (1); call add(U%x,scale*TMP%dt); call apply_BCs(U)
         case (2); call add(U%y,scale*TMP%dt); call apply_BCs(U)
         case (3); call add(U%z,scale*TMP%dt); call apply_BCs(U)
         case default; stop 'Error: dir must = 0:3 in compute_MPG in induction_aux.f90'
         end select
       end subroutine

       subroutine compute_add_JCrossB(F,jCrossB,B,B0,J,m,D_fluid,scale,finite_Rem,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF)
         ! computes
         !
         !     finite Rem:  scale J x (B0 + B_induced)
         !     low    Rem:  scale J x (B0)
         !
         implicit none
         type(VF),intent(inout) :: F,jCrossB,temp_F
         type(VF),intent(in) :: B,B0,J
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: D_fluid
         real(cp),intent(in) :: scale
         logical,intent(in) :: finite_Rem
         type(SF),intent(inout) :: temp_CC
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF
         call edge2Face_no_diag(temp_F1_TF,J,m)
         if (finite_Rem) then; call add(temp_F,B0,B); call face2Face_no_diag(temp_F2_TF,temp_F,m,temp_CC)
         else;                                        call face2Face_no_diag(temp_F2_TF,B0    ,m,temp_CC)
         endif
         call cross_product(temp_F,temp_F1_TF,temp_F2_TF)
         call extractFace(jCrossB,temp_F,D_fluid)
         call assign_ghost_XPeriodic(jCrossB,0.0_cp)
         call add_product(F,jCrossB,scale) ! Since J includes Rem
       end subroutine

       subroutine compute_add_JCrossB_div_free(F,jCrossB,B,B0,m,D_fluid,scale,finite_Rem,&
         temp_CC,temp_F,temp_F1,temp_F2,temp_F3,TF_E)
         ! computes divergence-free Lorentz force using
         ! jxB = B•∇(B/μ) - ∇ (B²/μ)
         !
         !     finite Rem:  Ha^2/Re jxB = B •∇(B        /μ)
         !     low    Rem:  Ha^2/Re jxB = B0•∇(B_induced/μ)
         !
         implicit none
         type(VF),intent(inout) :: F,jCrossB,temp_F,temp_F1,temp_F2,temp_F3
         type(VF),intent(in) :: B,B0
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: D_fluid
         real(cp),intent(in) :: scale
         logical,intent(in) :: finite_Rem
         type(SF),intent(inout) :: temp_CC
         type(TF),intent(inout) :: TF_E
         if (finite_Rem) then
           call add(temp_F1,B0,B)
           call advect_U_convection(temp_F,temp_F1,temp_F1,TF_E,m,temp_F2,temp_F3,temp_CC)
         else
           call advect_U_convection(temp_F,B0     ,B      ,TF_E,m,temp_F2,temp_F3,temp_CC)
         endif
         call extractFace(jCrossB,temp_F,D_fluid)
         call assign_ghost_XPeriodic(jCrossB,0.0_cp)
         call add_product(F,jCrossB,scale)
       end subroutine

       subroutine compute_add_Q2D_JCrossB(F,Q2D_JCrossB,U,scale)
         ! computes: Q2D_JCrossB = -U/tau, tau = Re/Ha
         implicit none
         type(VF),intent(inout) :: F,Q2D_JCrossB
         type(VF),intent(in) :: U
         real(cp),intent(in) :: scale
         call assign(Q2D_JCrossB,U)
         call add_product(F,Q2D_JCrossB,scale)
       end subroutine

       subroutine compute_add_buoyancy(F,buoyancy,T,gravity,scale,m,MD,temp_F,temp_CC)
         ! Computes
         !            Gr
         !           ---  T g
         !           Re^2
         implicit none
         type(VF),intent(inout) :: F,buoyancy,temp_F,temp_CC
         type(SF),intent(in) :: T
         type(VF),intent(in) :: gravity
         real(cp),intent(in) :: scale
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call assign(temp_CC,T)
         call multiply(temp_CC,gravity)
         call cellCenter2Face(temp_F,temp_CC,m)
         call extractFace(buoyancy,temp_F,MD)
         call add_product(F,buoyancy,scale)
       end subroutine

       subroutine compute_add_gravity(F,gravity,g,scale,m,MD,temp_F,temp_CC)
         ! Computes
         !            1
         !           --- g
         !           Fr^2
         implicit none
         type(VF),intent(inout) :: F,gravity,temp_F,temp_CC
         type(VF),intent(in) :: g
         real(cp),intent(in) :: scale
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call assign(temp_CC,g)
         call cellCenter2Face(temp_F,temp_CC,m)
         call extractFace(gravity,temp_F,MD)
         call add_product(F,gravity,scale)
       end subroutine

       end module