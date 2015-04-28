      module myPoisson_mod
      ! call myPoisson(method,u,f,u_bcs,g,ss,err,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using the method defined by the first object.
      !
      ! Input:
      !     method       = Method used to solve poisson equation (SOR,ADI,MG)
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     g            = contains grid information
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

      use solverSettings_mod
      use grid_mod
      use BCs_mod
      use myError_mod

      use myJacobi_mod
      use mySOR_mod
      use myADI_mod
      use myMG_mod

      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: myPoisson

      interface myPoisson;    module procedure myPoisson_Jacobi;  end interface
      interface myPoisson;    module procedure myPoisson_SOR;     end interface
      interface myPoisson;    module procedure myPoisson_ADI;     end interface
      interface myPoisson;    module procedure myPoisson_MG;      end interface

      contains

      subroutine myPoisson_Jacobi(Jacobi,u,f,u_bcs,g,ss,err,displayTF)
        implicit none
        type(myJacobi),intent(inout) :: Jacobi
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        logical,intent(in) :: displayTF
        call solve(Jacobi,u,f,u_bcs,g,ss,err,displayTF)
      end subroutine

      subroutine myPoisson_SOR(SOR,u,f,u_bcs,g,ss,err,displayTF)
        implicit none
        type(mySOR),intent(inout) :: SOR
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        logical,intent(in) :: displayTF
        call solve(SOR,u,f,u_bcs,g,ss,err,displayTF)
      end subroutine

      ! subroutine myPoisson_pseudoTimeStep(PTS,u,f,u_bcs,g,ss,err,displayTF)
      !   implicit none
      !   type(myPTS),intent(inout) :: PTS
      !   real(cp),dimension(:,:,:),intent(inout) :: u
      !   real(cp),dimension(:,:,:),intent(in) :: f
      !   type(BCs),intent(in) :: u_bcs
      !   type(grid),intent(in) :: g
      !   type(solverSettings),intent(inout) :: ss
      !   type(myError),intent(inout) :: err
      !   logical,intent(in) :: displayTF
      !   call solve(PTS,u,f,u_bcs,g,ss,err,displayTF)
      ! end subroutine

      subroutine myPoisson_ADI(ADI,u,f,u_bcs,g,ss,err,displayTF)
        implicit none
        type(myADI),intent(inout) :: ADI
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        logical,intent(in) :: displayTF
        call solve(ADI,u,f,u_bcs,g,ss,err,displayTF)
      end subroutine

      subroutine myPoisson_MG(MG,u,f,u_bcs,g,ss,err,displayTF)
        implicit none
        type(multiGrid),dimension(:),intent(inout) :: MG
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        logical,intent(in) :: displayTF
        call solve(MG,u,f,u_bcs,g,ss,err,displayTF)
      end subroutine

      end module