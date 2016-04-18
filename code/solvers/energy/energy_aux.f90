       module energy_aux_mod
       use current_precision_mod
       use SF_mod
       use VF_mod

       use ops_embedExtract_mod
       use domain_mod
       use mesh_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none

       private
       public :: compute_AddBuoyancy
       public :: compute_AddGravity
       public :: computeBuoyancy
       public :: computeGravity
       public :: compute_Q
       public :: compute_divQ
       public :: embed_velocity_F

       contains

       subroutine computeBuoyancy(buoyancy,T,gravity,Gr,Re,m,D,temp_F)
         ! Computes
         ! 
         !            Gr
         !           ---  T g
         !           Re^2
         implicit none
         type(VF),intent(inout) :: buoyancy,temp_F
         type(SF),intent(in) :: T
         type(VF),intent(in) :: gravity
         real(cp),intent(in) :: Gr,Re
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         call assign(buoyancy,T)
         call multiply(buoyancy,Gr/(Re**2.0_cp))
         call multiply(buoyancy,gravity)
         call cellCenter2Face(temp_F,buoyancy,m)
         call extractFace(buoyancy,temp_F,D)
       end subroutine

       subroutine compute_AddBuoyancy(buoyancy,T,gravity,Gr,Re,m,D,temp_F,temp_buoyancy)
         implicit none
         type(VF),intent(inout) :: buoyancy,temp_F,temp_buoyancy
         type(SF),intent(in) :: T
         type(VF),intent(in) :: gravity
         real(cp),intent(in) :: Gr,Re
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         call computeBuoyancy(temp_buoyancy,T,gravity,Gr,Re,m,D,temp_F)
         call add(buoyancy,temp_buoyancy)
       end subroutine

       subroutine computeGravity(gravity,g,Fr,m,D,temp_F,temp_CC)
         ! Computes
         ! 
         !            1   
         !           --- g
         !           Fr^2 
         implicit none
         type(VF),intent(inout) :: gravity,temp_F,temp_CC
         type(VF),intent(in) :: g
         real(cp),intent(in) :: Fr
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         call assign(temp_CC,g)
         call divide(temp_CC,Fr**2.0_cp)
         call cellCenter2Face(temp_F,temp_CC,m)
         call extractFace(gravity,temp_F,D)
       end subroutine

       subroutine compute_AddGravity(gravity,g,Fr,m,D,temp_F,temp_CC,temp_gravity)
         implicit none
         type(VF),intent(inout) :: gravity,temp_F,temp_CC,temp_gravity
         type(VF),intent(in) :: g
         real(cp),intent(in) :: Fr
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         call computeGravity(temp_gravity,g,Fr,m,D,temp_F,temp_CC)
         call add(gravity,temp_gravity)
       end subroutine

       subroutine compute_Q(Q,T,k,m)
         implicit none
         type(VF),intent(inout) :: Q
         type(SF),intent(in) :: T
         type(VF),intent(in) :: k
         type(mesh),intent(in) :: m
         call grad(Q,T,m)
         call multiply(Q,k)
         call multiply(Q,-1.0_cp)
       end subroutine

       subroutine compute_divQ(divQ,Q,m)
         implicit none
         type(SF),intent(inout) :: divQ
         type(VF),intent(in) :: Q
         type(mesh),intent(in) :: m
         call div(divQ,Q,m)
       end subroutine

       subroutine embed_velocity_F(U_Ft,U_Fi,D)
         implicit none
         type(VF),intent(inout) :: U_Ft
         type(VF),intent(in) :: U_Fi ! Raw momentum velocity
         type(domain),intent(in) :: D
         call embedFace(U_Ft,U_Fi,D)
       end subroutine

       end module