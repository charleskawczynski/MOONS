       module inputFile_mod
       use current_precision_mod
       implicit none
       private
       public :: readInputFile

       contains

       subroutine readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Rem,finite_Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,N_nrg,tol_nrg,N_mom,tol_mom,&
         N_PPE,tol_PPE,N_ind,tol_ind,N_cleanB,tol_cleanB)
         implicit none
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         logical,intent(inout) :: finite_Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         real(cp),intent(inout) :: tol_nrg,tol_PPE,tol_mom,tol_ind,tol_cleanB
         integer,intent(inout) :: NmaxMHD,N_nrg,N_mom,N_PPE,N_ind,N_cleanB
         real(cp) :: t
         ! ***************** DEFAULT VALUES *****************
         Re         = 400.0_cp
         Ha         = 100.0_cp

         Rem        = 100.0_cp
         Gr         = 0.0_cp
         Fr         = 0.0_cp
         Pr         = 0.71_cp
         Ec         = 0.0_cp

         finite_Rem = .true.
         dt_eng     = 5.0_cp*10.0_cp**(-4.0_cp)
         dt_mom     = 5.0_cp*10.0_cp**(-4.0_cp)
         dt_ind     = 5.0_cp*10.0_cp**(-4.0_cp)
         t          = 200.0_cp
         ! t          = 1.0_cp
         ! NmaxMHD       = ceiling(t/dt_eng)
         NmaxMHD       = ceiling(t/dt_mom)
         ! NmaxMHD       = ceiling(t/dt_ind)
         ! NmaxMHD       = maxval((/ceiling(t/dt_eng),ceiling(t/dt_mom),ceiling(t/dt_ind)/))
         ! NmaxMHD       = 1500

         N_nrg         = 1000     ! Number of iterations to solve energy    equation (if iterative solver is used)
         N_mom         = 100      ! Number of iterations to solve momentum  equation (if iterative solver is used)
         N_ind         = 100000   ! Number of iterations to solve induction equation (if iterative solver is used)
         N_PPE         = 5        ! Number of iterations to solve PPE steps
         N_cleanB      = 5        ! Number of iterations to solve Poisson equation to clean B

         ! Stopping criteria for iterative solvers:
         !         ||Ax - b||
         !         ----------- < tol
         !         ||Ax⁰ - b||
         ! NOTE: tol must be > 10^(-10)
         ! For the PPE, div(u) -> 0 as t-> infinity.
         ! Which means b and r⁰ -> 0 as t-> infinity.
         tol_nrg       = 10.0_cp**(-10.0_cp)
         tol_mom       = 10.0_cp**(-10.0_cp)
         tol_ind       = 10.0_cp**(-6.0_cp)
         tol_PPE       = 10.0_cp**(-10.0_cp)
         tol_cleanB    = 10.0_cp**(-10.0_cp)
       end subroutine

       end module
