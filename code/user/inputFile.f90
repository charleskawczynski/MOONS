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
         t = 10000.0
         dt_eng = 1.0d-4
         dt_mom = 1.0d-2
         dt_ind = 1.0d-4
         NmaxMHD = ceiling(t/dt_mom)
         NmaxMHD = 4000
         N_energy = 5
         N_mom      = 1000
         N_PPE    = 5 ! Number of PPE steps
         N_induction      = 5 ! Number of Steps for Low Rem approx to solve B
         N_cleanB = 5 ! Number of Steps to clean B
       end subroutine

       end module
