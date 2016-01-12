       module momentum_aux_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       
       implicit none
       private
       
       public :: compute_TKE
       public :: compute_divU
       public :: compute_CoFoRe_grid
       public :: addMeanPressureGrad

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine compute_TKE(K_energy,U_CC,m)
         implicit none
         real(cp),intent(inout) :: K_energy
         type(VF),intent(in) :: U_CC
         type(mesh),intent(in) :: m
         call totalEnergy(K_energy,U_CC,m)
       end subroutine

       subroutine compute_CoFoRe_grid(Co_grid,Fo_grid,Re_grid,U_CC,m,dt,Re)
         implicit none
         type(SF),intent(inout) :: Co_grid,Fo_grid,Re_grid
         type(VF),intent(in) :: U_CC
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt,Re
         call stabilityTerms(Co_grid,U_CC,m,1)
         call multiply(Co_grid,dt)
         call stabilityTerms(Fo_grid,U_CC,m,2)
         call multiply(Fo_grid,dt)
         call stabilityTerms(Re_grid,U_CC,m,-1)
         call multiply(Re_grid,Re)
       end subroutine

       subroutine compute_divU(divU,U,m)
         implicit none
         type(SF),intent(inout) :: divU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call div(divU,U,m)
       end subroutine

       subroutine addMeanPressureGrad(f,mpg,dir)
         implicit none
         type(VF),intent(inout) :: f
         real(cp),intent(in) :: mpg
         integer,intent(in) :: dir
         select case (dir)
         case (1); call subtract(f%x,mpg)
         case (2); call subtract(f%y,mpg)
         case (3); call subtract(f%z,mpg)
         case default; stop 'Error: dir must = 1,2,3 in addMeanPressureGrad in momentum_aux.f90'
         end select
       end subroutine

       end module