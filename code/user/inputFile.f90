       module inputFile_mod
       use current_precision_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use sim_params_mod
       use IO_tools_mod
       use iter_solver_params_mod
       use time_marching_params_mod
       implicit none
       private
       public :: readInputFile

       contains

       subroutine readInputFile(SP,DT,Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         coupled,TMP_U,TMP_B,TMP_T,ISP_U,ISP_B,ISP_T,ISP_P,ISP_phi,&
         tw,sig_local_over_sig_f,include_vacuum)
         implicit none
         type(sim_params),intent(in) :: SP
         type(dir_tree),intent(in) :: DT
         type(iter_solver_params),intent(inout) :: ISP_U,ISP_B,ISP_T,ISP_P,ISP_phi
         type(time_marching_params),intent(inout) :: coupled,TMP_U,TMP_B,TMP_T
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Rem,tw,sig_local_over_sig_f
         logical,intent(inout) :: finite_Rem,include_vacuum
         real(cp) :: time,dtime,tol_abs,delta_Ha,dh_min
         logical :: coupled_time_step
         integer(li) :: nstep_stop
         ! ***************** DEFAULT VALUES *****************
         Re         = 1000.0_cp
         Ha         = 20.0_cp
         Rem        = 100.0_cp
         tw = 0.05_cp
         include_vacuum = .true.
         finite_Rem = .true.
         coupled_time_step = .true.
         ! sig_local_over_sig_f = 1.0_cp             ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-1.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-2.0_cp) ! sigma* = sigma_wall/sigma_l
         sig_local_over_sig_f = 10.0_cp**(-5.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-4.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-5.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-6.0_cp) ! sigma* = sigma_wall/sigma_l

         Gr         = 10.0_cp**(8.0_cp)
         Pr         = 0.01_cp
         Fr         = 1.0_cp
         Ec         = 0.0_cp
         tol_abs = 10.0_cp**(-13.0_cp)
         ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res)
         delta_Ha = 1.0_cp/Ha
         dh_min = delta_Ha/5.0_cp
         call init(ISP_B  , 10000, 10.0_cp**(-5.0_cp) , 1.0_cp*10.0_cp**(-6.0_cp) , 100, str(DT%ISP),'ISP_B')
         call init(ISP_U  ,   5  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_U')
         call init(ISP_p  ,   5  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_p')
         call init(ISP_T  ,   5  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_T')
         call init(ISP_phi,   5  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_phi')

         time  = 1.0_cp*10.0_cp**(-1.0_cp)
         ! dtime = 1.0_cp*10.0_cp**(-6.0_cp) ! Implicit time marching

         dtime = 1.0_cp*10.0_cp**(-5.0_cp)*Rem*sig_local_over_sig_f ! Explicit time marching estimate

         ! call init(TMP,nstep_stop,dtime,dir,name)
         call init(coupled,ceiling(time/dtime,li),dtime,str(DT%TMP), 'TMP_coupled')
         ! call init(coupled,1000000000,dtime,str(DT%TMP), 'TMP_coupled')

         call init(TMP_B, coupled%n_step_stop, coupled%dt, str(DT%TMP), 'TMP_B')
         call init(TMP_U, coupled%n_step_stop, coupled%dt, str(DT%TMP), 'TMP_U')
         call init(TMP_T, coupled%n_step_stop, coupled%dt, str(DT%TMP), 'TMP_T')

         if (coupled%n_step_stop.lt.1) stop 'Error: coupled%n_step_stop<1 in inputFile.f90'

         if (coupled_time_step) then
           TMP_U%dt = coupled%dt; TMP_U%n_step_stop = coupled%n_step_stop
           TMP_B%dt = coupled%dt; TMP_B%n_step_stop = coupled%n_step_stop
           TMP_T%dt = coupled%dt; TMP_T%n_step_stop = coupled%n_step_stop
         endif
         ! Stopping criteria for iterative solvers:
         !         ||Ax - b||
         !         ----------- < tol_rel
         !         ||Ax⁰ - b||
         ! NOTE: tol_rel must be > 10^(-10)
         ! for the PPE, div(u) -> 0 as t-> infinity.
         ! Which means b and r⁰ -> 0 as t-> infinity.
         if (SP%restartU)      call import(TMP_U)
         if (.not.SP%restartU) call export(TMP_U)
         if (SP%restartU)      call import(ISP_U)
         if (.not.SP%restartU) call export(ISP_U)
         if (SP%restartU)      call import(ISP_P)
         if (.not.SP%restartU) call export(ISP_P)

         if (SP%restartB)      call import(TMP_B)
         if (.not.SP%restartB) call export(TMP_B)
         if (SP%restartB)      call import(ISP_B)
         if (.not.SP%restartB) call export(ISP_B)
         if (SP%restartB)      call import(ISP_phi)
         if (.not.SP%restartB) call export(ISP_phi)

         if (SP%restartT)      call import(TMP_T)
         if (.not.SP%restartT) call export(TMP_T)
         if (SP%restartT)      call import(ISP_T)
         if (.not.SP%restartT) call export(ISP_T)
       end subroutine

!          Re         = 400.0_cp
!          Ha         = 100.0_cp
!          Rem        = 1.0_cp
!          tw = 0.5_cp
!          dt_eng     = 1.0_cp*10.0_cp**(-4.0_cp)
!          dt_mom     = 1.0_cp*10.0_cp**(-4.0_cp)
!          dt_ind     = 1.0_cp*10.0_cp**(-4.0_cp)
!          include_vacuum = .true.
!          N_ind         = 100      ! Number of iterations to solve induction equation (if iterative solver is used)
!          N_PPE         = 5        ! Number of iterations to solve PPE steps
!          N_cleanB      = 5        ! Number of iterations to solve Poisson equation to clean B
!          tol_ind       = 10.0_cp**(-6.0_cp)
!          tol_PPE       = 10.0_cp**(-10.0_cp)
!          tol_cleanB    = 10.0_cp**(-10.0_cp)

!          ! sig_local_over_sig_f = 1.0_cp             ! sigma* = sigma_wall/sigma_l
!          ! sig_local_over_sig_f = 10.0_cp**(-1.0_cp) ! sigma* = sigma_wall/sigma_l
!          sig_local_over_sig_f = 10.0_cp**(-3.0_cp) ! sigma* = sigma_wall/sigma_l
!          ! sig_local_over_sig_f = 10.0_cp**(-4.0_cp) ! sigma* = sigma_wall/sigma_l
!          ! sig_local_over_sig_f = 10.0_cp**(-5.0_cp) ! sigma* = sigma_wall/sigma_l
!          ! sig_local_over_sig_f = 10.0_cp**(-6.0_cp) ! sigma* = sigma_wall/sigma_l

!          Gr         = 10.0_cp**(8.0_cp)
!          Pr         = 0.01_cp
!          Fr         = 1.0_cp
!          Ec         = 0.0_cp
!          finite_Rem = .true.
!          t          = 10.0_cp
!          n_dt_start = 0
!          n_dt_stop = ceiling(t/dt_mom)
!          ! n_dt_stop       = 1000000000
!          if (n_dt_stop.lt.1) stop 'Error: n_dt_stop<1 in inputFile.f90'
!          N_nrg         = 10       ! Number of iterations to solve energy    equation (if iterative solver is used)
!          N_mom         = 10       ! Number of iterations to solve momentum  equation (if iterative solver is used)
!          ! Stopping criteria for iterative solvers:
!          !         ||Ax - b||
!          !         ----------- < tol
!          !         ||Ax⁰ - b||
!          ! NOTE: tol must be > 10^(-10)
!          ! For the PPE, div(u) -> 0 as t-> infinity.
!          ! Which means b and r⁰ -> 0 as t-> infinity.
!          tol_nrg       = 10.0_cp**(-10.0_cp)
!          tol_mom       = 10.0_cp**(-10.0_cp)


       end module
