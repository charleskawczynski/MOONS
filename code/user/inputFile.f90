       module inputFile_mod
       use current_precision_mod
       use iter_solver_params_mod
       use time_marching_params_mod
       implicit none
       private
       public :: readInputFile

       contains

       subroutine readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,n_dt_start,n_dt_stop,N_nrg,tol_nrg,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_ind,tol_ind,N_cleanB,tol_cleanB,tw,sig_local_over_sig_f,include_vacuum)
         implicit none
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Rem,tw,sig_local_over_sig_f
         logical,intent(inout) :: finite_Rem,include_vacuum
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         real(cp),intent(inout) :: tol_nrg,tol_PPE,tol_mom,tol_ind,tol_cleanB
         integer,intent(inout) :: n_dt_start,n_dt_stop,N_nrg,N_mom,N_PPE,N_ind,N_cleanB
         real(cp) :: t
         ! ***************** DEFAULT VALUES *****************
         Re         = 400.0_cp
         Ha         = 100.0_cp
         Rem        = 1.0_cp
         tw = 0.5_cp
         dt_eng     = 1.0_cp*10.0_cp**(-4.0_cp)
         dt_mom     = 1.0_cp*10.0_cp**(-4.0_cp)
         dt_ind     = 1.0_cp*10.0_cp**(-4.0_cp)
         include_vacuum = .true.
         N_ind         = 100      ! Number of iterations to solve induction equation (if iterative solver is used)
         N_PPE         = 5        ! Number of iterations to solve PPE steps
         N_cleanB      = 5        ! Number of iterations to solve Poisson equation to clean B
         tol_ind       = 10.0_cp**(-6.0_cp)
         tol_PPE       = 10.0_cp**(-10.0_cp)
         tol_cleanB    = 10.0_cp**(-10.0_cp)

         ! sig_local_over_sig_f = 1.0_cp             ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-1.0_cp) ! sigma* = sigma_wall/sigma_l
         sig_local_over_sig_f = 10.0_cp**(-3.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-4.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-5.0_cp) ! sigma* = sigma_wall/sigma_l
         ! sig_local_over_sig_f = 10.0_cp**(-6.0_cp) ! sigma* = sigma_wall/sigma_l

         Gr         = 10.0_cp**(8.0_cp)
         Pr         = 0.01_cp
         Fr         = 1.0_cp
         Ec         = 0.0_cp
         finite_Rem = .true.
         t          = 10.0_cp
         n_dt_start = 0
         n_dt_stop = ceiling(t/dt_mom)
         ! n_dt_stop       = 1000000000
         if (n_dt_stop.lt.1) stop 'Error: n_dt_stop<1 in inputFile.f90'
         N_nrg         = 10       ! Number of iterations to solve energy    equation (if iterative solver is used)
         N_mom         = 10       ! Number of iterations to solve momentum  equation (if iterative solver is used)
         ! Stopping criteria for iterative solvers:
         !         ||Ax - b||
         !         ----------- < tol
         !         ||Ax⁰ - b||
         ! NOTE: tol must be > 10^(-10)
         ! For the PPE, div(u) -> 0 as t-> infinity.
         ! Which means b and r⁰ -> 0 as t-> infinity.
         tol_nrg       = 10.0_cp**(-10.0_cp)
         tol_mom       = 10.0_cp**(-10.0_cp)
       end subroutine

       end module
