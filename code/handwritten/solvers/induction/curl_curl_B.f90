       module curl_curl_B_mod
       use current_precision_mod
       use mesh_mod
       use VF_mod
       use ops_discrete_mod

       implicit none

       private
       public :: curl_curl_B_matrix_free

       contains

       subroutine curl_curl_B_matrix_free(curl_curl_B,J,B,sigmaInv_E,m,temp_E)
         implicit none
         type(VF),intent(inout) :: curl_curl_B,J
         type(VF),intent(in) :: B
         type(VF),intent(in) :: sigmaInv_E
         type(VF),intent(inout) :: temp_E
         type(mesh),intent(in) :: m
         call curl(J,B,m)
         call multiply(temp_E,J,sigmaInv_E)
         call curl(curl_curl_B,temp_E,m)
       end subroutine

       end module