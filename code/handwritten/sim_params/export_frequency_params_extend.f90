       module export_frequency_params_extend_mod
       use export_frequency_params_mod
       use current_precision_mod
       use time_marching_params_mod
       use IO_tools_mod
       implicit none
       private
       public :: init
       public :: update

       interface init;    module procedure init_EFP;      end interface
       interface update;  module procedure update_EFP;    end interface

       contains

       subroutine init_EFP(EFP,export_ever,export_first_step,N_points_in_window,&
         t_window_start,t_window_stop,dt_window_factor)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         logical,intent(in) :: export_ever,export_first_step
         integer,intent(in) :: N_points_in_window
         real(cp),intent(in) :: t_window_start,t_window_stop,dt_window_factor
         EFP%export_ever       = export_ever
         EFP%export_now        = .false.
         EFP%export_first_step = export_first_step
         EFP%N_points_in_window= N_points_in_window
         EFP%t_window_start    = t_window_start
         EFP%t_window_stop     = t_window_stop
         EFP%dt_window_factor  = dt_window_factor
       end subroutine

       function in_window_range(t,t_star,range) result(L)
         implicit none
         real(cp),intent(in) :: t,t_star,range
         logical :: L
         L = (t.gt.t_star-range).and.(t.lt.t_star+range)
       end function

       function in_window_AB(t,a,b,dt) result(L)
         implicit none
         real(cp),intent(in) :: t,a,b,dt
         logical :: L
         L = (t.gt.a-0.01_cp*dt).and.(t.lt.b+0.01_cp*dt)
       end function

       function get_export_now(t,dt,t_star_left,t_star_right,dt_star_range) result(export_now)
         ! Two possible cases:
         !      between 3 tested points, either 1 or 2 points
         !      will fall between t*-.5 dt*w and t*+.5 dt*w.
         !      We choose to export when either
         !           1) The center point alone is within the window
         !           2) The left  and center points are within the window
         !           3) The right and center points are within the window
         ! The particular case can be easily tested for a given set of parameters
         !
         ! Case 1 - 1 point inside
         !                          dt     t
         !           |       |       |     |                     |                     |
         !           |       |<------|---->|                     |                     |
         !           |       |       |     |                     |                     |
         ! ----------|---------------------|---------------------|---------------------|
         !           |          |          |          |          |                     |
         !           |          |<---------|--------->|          |                     |
         !           |          |          |          |          |                     |
         !                            dt*w t*
         !
         ! Case 2 - 2 points inside
         !                         t     dt
         !           |             |       |     |               |                     |
         !           |             |<------|---->|               |                     |
         !           |             |       |     |               |                     |
         ! ----------|---------------------|---------------------|---------------------|
         !           |          |          |          |          |                     |
         !           |          |<---------|--------->|          |                     |
         !           |          |          |          |          |                     |
         !                            dt*w t*
         implicit none
         real(cp),intent(in) :: t,dt,t_star_left,t_star_right,dt_star_range
         logical,dimension(3) :: L
         logical,dimension(3) :: condition_set
         logical :: export_now
         real(cp) :: t_star
         ! Compare with nearest t_star:
         if (abs(t_star_left-t).gt.abs(t_star_right-t)) then
          t_star = t_star_right
         else
          t_star = t_star_left
         endif
         L(1) =  in_window_range(t-dt,t_star,dt_star_range)
         L(2) =  in_window_range( t  ,t_star,dt_star_range)
         L(3) =  in_window_range(t+dt,t_star,dt_star_range)
         condition_set(1) = L(2).and.(.not.L(1)).and.(.not.L(3)) ! single (center only) point inside
         condition_set(2) = L(2).and.(     L(1)).and.(.not.L(3)) ! two                  point inside
         condition_set(3) = L(2).and.(     L(3)).and.(.not.L(1)) ! two                  point inside
         if (any(condition_set)) then
           export_now = .true.
         else
           export_now = .false.
         endif
       end function

       subroutine update_EFP(EFP,TMP,substep)
         implicit none
         type(export_frequency_params),intent(inout) :: EFP
         type(time_marching_params),intent(in) :: TMP
         logical,intent(in) :: substep
         logical :: first_step,last_step,past_first_step
         if ((EFP%export_ever).and.(in_window_AB(TMP%t,EFP%t_window_start,EFP%t_window_stop,TMP%TS%dt))) then
           if (EFP%N_points_in_window.gt.0) then

             EFP%dt_star = (EFP%t_window_stop-EFP%t_window_start)/real(EFP%N_points_in_window,cp)
             if (TMP%TS%dt.gt.EFP%dt_star) then
               EFP%dt_star = TMP%TS%dt ! Avoid undersampling for short sims
               EFP%N_points_in_window = ceiling((EFP%t_window_stop-EFP%t_window_start)/EFP%dt_star)
             endif
             EFP%dt_star_range = 0.5_cp*TMP%TS%dt*(1.0_cp+EFP%dt_window_factor)
             EFP%left_point_export = floor((TMP%t - EFP%t_window_start)/EFP%dt_star)
             EFP%right_point_export = ceiling((TMP%t - EFP%t_window_start)/EFP%dt_star)

             EFP%t_star_left = EFP%t_window_start  + EFP%dt_star*EFP%left_point_export
             EFP%t_star_right = EFP%t_window_start + EFP%dt_star*EFP%right_point_export

             EFP%export_now = get_export_now(TMP%t,TMP%TS%dt,EFP%t_star_left,EFP%t_star_right,EFP%dt_star_range)

             first_step = in_window_range(TMP%t,EFP%t_window_start,EFP%dt_star_range)
             last_step  = in_window_range(TMP%t,EFP%t_window_stop,EFP%dt_star_range)
             past_first_step = .not.(first_step)
             if (EFP%export_first_step) then; EFP%export_now = EFP%export_now.or.first_step.or.last_step
             else;                            EFP%export_now = EFP%export_now.and.past_first_step.or.last_step
             endif
           endif
         else; EFP%export_now = .false.
         endif
         if (substep) EFP%export_now = .false.
       end subroutine

       end module