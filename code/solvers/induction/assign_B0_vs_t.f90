       module assign_B0_vs_t_mod
       use current_precision_mod
       use time_marching_params_mod
       use Mike_Ulrickson_data_mod
       use VF_mod
       implicit none

       private
       public :: assign_B0_vs_t

       contains

       subroutine assign_B0_vs_t(B0,TMP)
         implicit none
         type(VF),intent(inout) :: B0
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(58) :: B_all,t_all
         real(cp) :: B,t,t_total
         integer :: i
         t_total = (real(TMP%n_step_stop,cp)-real(TMP%n_step_start,cp))*TMP%dt
         i = floor(real(TMP%n_step,cp)/real(TMP%n_step_stop,cp))
         i = maxval((/i,1/))
         i = minval((/i,58/))
         call time(t_all); t = t_all(i)

         call B_r_mean(B_all); B = B_all(i)
         call assign(B0%y,B_all(i))
         call B_z_mean(B_all); B = B_all(i)
         call assign(B0%z,B_all(i))
       end subroutine


       end module