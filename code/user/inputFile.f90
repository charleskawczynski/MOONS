       module inputFile_mod

       implicit none

       private

       public :: readInputFile

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

       subroutine readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,N_energy,N_mom,N_PPE,N_induction,N_cleanB)
         implicit none
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         integer,intent(inout) :: NmaxMHD,N_energy,N_mom,N_PPE,N_induction,N_cleanB
         real(cp) :: t
         ! ***************** DEFAULT VALUES *****************
         Re = 100.0d0
         Ha = 10.0d0
         Rem = 1.0d0
         Gr = 0.0_cp
         Fr = 0.0d0
         Pr = 0.71d0
         Ec = 0.0d0
         t = 80.0
         dt_eng = 1.0d-4
         dt_mom = 1.0d-2
         dt_ind = 1.0d-6

         ! NmaxMHD     = ceiling(t/dt_eng)
         ! NmaxMHD     = ceiling(t/dt_mom)
         NmaxMHD     = ceiling(t/dt_ind)
         ! NmaxMHD     = maxval((/ceiling(t/dt_eng),ceiling(t/dt_mom),ceiling(t/dt_ind)/))
         N_energy    = 1000 ! Number of iterations to solve energy    equation (if iterative solver is used)
         N_mom       = 1000 ! Number of iterations to solve momentum  equation (if iterative solver is used)
         N_induction = 1000 ! Number of iterations to solve induction equation (if iterative solver is used)
         N_PPE       = 5    ! Number of iterations to solve PPE steps
         N_cleanB    = 5    ! Number of iterations to solve Poisson equation to clean B
       end subroutine

       end module
