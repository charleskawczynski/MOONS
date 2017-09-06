       module compute_energy_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_mod
       use ops_interp_mod
       use ops_norms_mod
       implicit none

       private
       public :: compute_energy

       contains

       subroutine compute_energy(e,A_F,B_F,m,temp_F,temp_CC,compute_norms)
         ! Computes  ∫∫∫ A_F•B_F dV
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: A_F,B_F
         type(VF),intent(inout) :: temp_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         real(cp) :: temp
         if (compute_norms) then
           call multiply(temp_F,A_F,B_F)
           e = 0.0_cp
           call face2CellCenter(temp_CC,temp_F%x,m,1)
           call assign_ghost_XPeriodic(temp_CC,0.0_cp)
           call Ln(temp,temp_CC,1.0_cp,m); e = temp
           call face2CellCenter(temp_CC,temp_F%y,m,2)
           call assign_ghost_XPeriodic(temp_CC,0.0_cp)
           call Ln(temp,temp_CC,1.0_cp,m); e = e + temp
           call face2CellCenter(temp_CC,temp_F%z,m,3)
           call assign_ghost_XPeriodic(temp_CC,0.0_cp)
           call Ln(temp,temp_CC,1.0_cp,m); e = e + temp
         endif
       end subroutine

       end module