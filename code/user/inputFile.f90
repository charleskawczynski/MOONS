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
         real(cp) :: time,dtime
         ! ***************** DEFAULT VALUES *****************
         ! Re         = 1000.0_cp
         Re         = 100.0_cp
         Ha         = 10.0_cp
         Rem        = 1.0_cp
         tw         = 0.5_cp

         include_vacuum = .false.
         finite_Rem = .false.
         sig_local_over_sig_f = 1.0_cp             ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-1.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-2.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-3.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-4.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-5.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-6.0_cp) ! sigma* = sigma_wall/sigma_l

         Gr         = 0.0_cp
         Pr         = 0.01_cp
         Fr         = 1.0_cp
         Ec         = 0.0_cp

         ! call init(ISP,iter_max,tol_rel,tol_abs,n_skip_check_res)
         ! call init(ISP_B  , 10000, 10.0_cp**(-5.0_cp),  1.0_cp*10.0_cp**(-7.0_cp) , 100, str(DT%ISP),'ISP_B')
         call init(ISP_B  ,   5  , 10.0_cp**(-5.0_cp),  1.0_cp*10.0_cp**(-7.0_cp) , 100, str(DT%ISP),'ISP_B')
         call init(ISP_U  ,  40  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_U')
         call init(ISP_p  ,   5  , 10.0_cp**(-6.0_cp) , 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_p')
         call init(ISP_T  ,   5  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_T')
         call init(ISP_phi,   5  , 10.0_cp**(-10.0_cp), 1.0_cp*10.0_cp**(-13.0_cp), 100, str(DT%ISP),'ISP_phi')

         ! BMC 102
         ! time  = 10.0_cp
         time  = 100.0_cp
         ! dtime = 1.0_cp*10.0_cp**(-3.0_cp) ! Implicit time marching
         ! dtime = 1.0_cp*10.0_cp**(-2.0_cp) ! Implicit time marching
         dtime = 16.0_cp*10.0_cp**(-2.0_cp) ! Implicit time marching
         ! dtime = 3.0_cp*10.0_cp**(-4.0_cp) ! Implicit time marching

         ! time  = 100.0_cp
         ! dtime = 1.0_cp*10.0_cp**(-4.0_cp) ! Implicit time marching
         ! dtime = 5.0_cp*10.0_cp**(-6.0_cp) ! Implicit time marching
         ! dtime = get_dt_NME(21)

         ! dtime = 1.0_cp*10.0_cp**(-5.0_cp)*Rem*sig_local_over_sig_f ! Explicit time marching estimate

         ! call init(TMP,n_step_stop,dtime,dir,name)
         call init(coupled,ceiling(time/dtime,li),dtime,str(DT%TMP), 'TMP_coupled')
         ! call init(coupled,1000000000,dtime,str(DT%TMP), 'TMP_coupled')

         call init(TMP_B, coupled%n_step_stop, coupled%dt/100.0_cp, str(DT%TMP), 'TMP_B')
         call init(TMP_U, coupled%n_step_stop, coupled%dt, str(DT%TMP), 'TMP_U')
         call init(TMP_T, coupled%n_step_stop, coupled%dt, str(DT%TMP), 'TMP_T')

         if (coupled%n_step_stop.lt.1) stop 'Error: coupled%n_step_stop<1 in inputFile.f90'

         if (SP%coupled_time_step) then
           call couple_time_step(TMP_U,coupled)
           call couple_time_step(TMP_B,coupled)
           call couple_time_step(TMP_T,coupled)
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

       function get_dt_NME(NME) result(dtime)
         implicit none
         integer,intent(in) :: NME
         real(cp) :: dtime
         select case (NME)
         case (1);  dtime = 5.0_cp*10.0_cp**(-5.0_cp) ! checked
         case (2);  dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (3);  dtime = 5.0_cp*10.0_cp**(-5.0_cp)
         case (4);  dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (5);  dtime = 5.0_cp*10.0_cp**(-5.0_cp)
         case (6);  dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (7);  dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (8);  dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (9);  dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (10); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (11); dtime = 5.0_cp*10.0_cp**(-6.0_cp)
         case (12); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (13); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (14); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (15); dtime = 5.0_cp*10.0_cp**(-6.0_cp)
         case (16); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (17); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (18); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (19); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (20); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (21); dtime = 1.0_cp*10.0_cp**(-4.0_cp) ! checked
         case (22); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (23); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (24); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (25); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (26); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (27); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (28); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (29); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (30); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (31); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case (32); dtime = 1.0_cp*10.0_cp**(-4.0_cp)
         case default; stop 'Error: bad NME in inputFile.f90'
         end select
       end function

       end module
