      module myPoisson_mod
      ! call myPoisson(u,f,u_bcs,gd,ss,displayTF) 
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), griddata (gd)
      ! and solver settings (ss) using the uncommented method below.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     gd           = contains grid information
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     displayTF    = print residuals to screen (T,F)
      ! 

      ! 
      ! Good Poisson solution examples:
      ! http://www.physics.buffalo.edu/phy410-505/topic6/
      !
      ! Fixes / Improvements:
      ! - Add multigrid subroutine
      !
      !
      ! Some references:
      ! http://demonstrations.wolfram.com/SolvingThe2DPoissonPDEByEightDifferentMethods/
      ! http://ocw.mit.edu/courses/mechanical-engineering/2-29-numerical-fluid-mechanics-fall-2011/lecture-notes/MIT2_29F11_lect_16.pdf
      ! http://12000.org/my_courses/UC_davis/fall_2010/math_228a/HWs/HW3/Neumman_BC/Neumman_BC.htm
      ! http://www.physics.buffalo.edu/phy410-505/topic6/
      ! http://www.ldeo.columbia.edu/~mspieg/mmm/BVPs.pdf
      ! http://www.serc.iisc.ernet.in/graduation-theses/Karthik_M.Tech._project_report.pdf
      ! http://people.sc.fsu.edu/~jburkardt/f_src/fftpack5.1/fftpack5.1.html

      use constants_mod
      use solverSettings_mod
      use griddata_mod
      use BCs_mod
      use myError_mod

      use mySOR_mod
      use myADI_mod
!      use myMG_mod
      implicit none

      private

      public :: myPoisson

      interface myPoisson
        module procedure myPoisson_SOR
!         module procedure myPoisson_MG
        module procedure myPoisson_ADI
      end interface

      contains

      subroutine myPoisson_SOR(SOR,u,f,ab,gd,ss,err,gridType,displayTF)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: ab
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        call solve(SOR,u,f,ab,gd,ss,err,gridType,displayTF)
      end subroutine

      subroutine myPoisson_ADI(ADI,u,f,ab,gd,ss,err,gridType,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: ab
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in) :: displayTF
        call solve(ADI,u,f,ab,gd,ss,err,gridType,displayTF)
      end subroutine

!       subroutine myPoisson_ADI(ADI,u,f,ab,gd,ss,err,gridType,displayTF)
!         implicit none
!         type(myADI),intent(inout) :: ADI
!         real(dpn),dimension(:,:,:),intent(inout) :: u
!         real(dpn),dimension(:,:,:),intent(in) :: f
!         type(BCs),intent(in) :: ab
!         type(griddata),intent(in) :: gd
!         type(myError),intent(inout) :: err
!         integer,intent(in) :: gridType
!         type(solverSettings),intent(inout) :: ss
!         logical,intent(in) :: displayTF
!         call solve(ADI,u,f,ab,gd,ss,err,gridType,displayTF)
!       end subroutine


      end module