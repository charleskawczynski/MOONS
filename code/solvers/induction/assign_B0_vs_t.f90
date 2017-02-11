       module assign_B0_vs_t_mod
       use current_precision_mod
       use time_marching_params_mod
       use Mike_Ulrickson_data_mod
       use VF_mod
       implicit none

       private
       public :: assign_B0_vs_t
       public :: assign_dB0_dt_vs_t
       integer,parameter :: n_data_points = 58

       contains

       subroutine assign_B0_vs_t(B0,TMP)
         implicit none
         type(VF),intent(inout) :: B0
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(n_data_points) :: B_p_all,B_r_all,t_all
         ! Non-dimensionalize
         call time_normalized(t_all)
         ! call B_r_mean_normalized(B_r_all)
         call B_p_mean_normalized(B_p_all)
         call assign(B0%x,5.0_cp)
         ! call assign(B0%y,get_B_from_t(t_all,B_r_all,TMP%t))
         call assign(B0%y,0.0_cp)
         call assign(B0%z,get_B_from_t(t_all,B_p_all,TMP%t))
       end subroutine

       subroutine assign_dB0_dt_vs_t(dB0_dt,TMP)
         implicit none
         type(VF),intent(inout) :: dB0_dt
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(n_data_points) :: B_p_all,B_r_all,t_all
         call time_normalized(t_all)
         ! call B_r_mean_normalized(B_r_all)
         call B_p_mean_normalized(B_p_all)
         call assign(dB0_dt%x,0.0_cp)
         ! call assign(dB0_dt%y,get_dB0_dt_from_t(t_all,B_r_all,TMP%t))
         call assign(dB0_dt%y,0.0_cp)
         call assign(dB0_dt%z,get_dB0_dt_from_t(t_all,B_p_all,TMP%t))
       end subroutine

       function get_dB0_dt_from_t(t_all,B_all,t) result(dB0_dt)
         implicit none
         real(cp),dimension(n_data_points),intent(in) :: t_all,B_all
         real(cp),intent(in) :: t
         real(cp) :: dB0_dt
         integer :: i,n
         n = n_data_points
         dB0_dt = (B_all(2)-B_all(1))/(t_all(2)-t_all(1))
         do i=1,n-1
          if ((t.ge.t_all(i)).and.(t.le.t_all(i+1))) then
           dB0_dt = (B_all(i+1)-B_all(i))/(t_all(i+1)-t_all(i))
          endif
         enddo
         if (t.gt.t_all(n)) then
          dB0_dt = 0.0_cp
         endif
       end function

       function get_B_from_t(t_all,B_all,t) result(B)
         implicit none
         real(cp),dimension(n_data_points),intent(in) :: t_all,B_all
         real(cp),intent(in) :: t
         real(cp) :: B
         integer :: i,n
         n = n_data_points
         B = B_all(1)
         do i=1,n-1
          if ((t.ge.t_all(i)).and.(t.le.t_all(i+1))) then
           B = interp_simple(B_all(i),t_all(i),B_all(i+1),t_all(i+1),t)
          endif
         enddo
         if (t.ge.t_all(n)) B = B_all(n)
       end function

       function interp_simple(B1,t1,B2,t2,t) result(B)
         implicit none
         real(cp),intent(in) :: B1,t1,B2,t2,t
         real(cp) :: B
         B = B1 + (B2-B1)*(t-t1)/(t2-t1)
       end function

       end module