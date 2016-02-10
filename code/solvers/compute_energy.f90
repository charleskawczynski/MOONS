       module compute_energy_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_interp_mod
       use ops_norms_mod
       implicit none

       private
       public :: compute_energy

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

       subroutine compute_energy(e,F,F_term,m,temp_F,temp_CC,compute_norms)
         ! Computes  ∫∫∫ F•F_term dV
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: F,F_term
         type(VF),intent(inout) :: temp_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp_CC
         logical,intent(in) :: compute_norms
         real(cp) :: temp
         if (compute_norms) then
           call multiply(temp_F,F,F_term)
           e = 0.0_cp
           call face2CellCenter(temp_CC,temp_F%x,m,1)
           call Ln(temp,temp_CC,1.0_cp,m); e = temp
           call face2CellCenter(temp_CC,temp_F%y,m,2)
           call Ln(temp,temp_CC,1.0_cp,m); e = e + temp
           call face2CellCenter(temp_CC,temp_F%z,m,3)
           call Ln(temp,temp_CC,1.0_cp,m); e = e + temp
         endif
       end subroutine

       end module