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

       subroutine readInputFile(Re,Ha,Gr,Fr,Pr,Ec,Al,Rem,&
         dt_eng,dt_mom,dt_ind,NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB)
         implicit none
         real(cp),intent(inout) :: Re,Ha,Gr,Fr,Pr,Ec,Al,Rem
         real(cp),intent(inout) :: dt_eng,dt_mom,dt_ind
         integer,intent(inout) :: NmaxMHD,NmaxPPE,NmaxB,NmaxCleanB
         real(cp) :: t
         ! ***************** DEFAULT VALUES *****************
         Re = 400.0d0
         Ha = 20.0d0
         Gr = 0.0_cp
         Fr = 0.0d0
         Pr = 0.71d0
         Ec = 0.0d0
         Al = 0.0d0
         Rem = 100.0d0
         t = 10000.0
         dt_eng = 5.0d-3
         dt_mom = 5.0d-3
         dt_ind = 5.0d-3
         NmaxMHD = ceiling(t/dt_mom)
         ! NmaxMHD = 80000
         NmaxPPE    = 5 ! Number of PPE steps
         NmaxB      = 5 ! Number of Steps for Low Rem approx to solve B
         NmaxCleanB = 5 ! Number of Steps to clean B
       end subroutine

       end module
