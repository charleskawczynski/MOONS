       module momentum_aux_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_mod
       use VF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_norms_mod

       implicit none
       private

       public :: compute_TKE
       public :: compute_TKE_2C
       public :: compute_CoFoRe_grid
       public :: addMeanPressureGrad

       contains

       subroutine compute_TKE(K_energy,U_CC,m,scale)
         implicit none
         real(cp),intent(inout) :: K_energy
         type(VF),intent(inout) :: U_CC
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         call assign_ghost_XPeriodic(U_CC,0.0_cp) ! norms now includes ghost points
         call Ln(K_energy,U_CC,2.0_cp,m)
         K_energy = scale*0.5_cp*K_energy ! KE = 1/2 int(u^2) dV
       end subroutine

       subroutine compute_TKE_2C(K_energy,A,B,m,scale,temp)
         implicit none
         real(cp),intent(inout) :: K_energy
         type(SF),intent(inout) :: temp
         type(SF),intent(in) :: A,B
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         call add(temp,A,B)
         call assign_ghost_XPeriodic(temp,0.0_cp) ! norms now includes ghost points
         call Ln(K_energy,temp,2.0_cp,m)
         K_energy = scale*0.5_cp*K_energy ! KE = 1/2 int(u^2) dV
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