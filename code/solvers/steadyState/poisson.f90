      module poisson_mod
      ! call poisson(method,u,f,u_bcs,m,ss,err,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), mesh (m)
      ! and solver settings (ss) using the method defined by the first object.
      !
      ! Input:
      !     method       = Method used to solve poisson equation (SOR,ADI,MG)
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     m            = contains mesh information
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
      use mesh_mod
      use norms_mod
      use SF_mod

      use FFT_poisson_mod
      ! use PSE_mod
      ! use jacobi_mod
      use SOR_mod
      ! use ADI_mod
      ! use MG_mod

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
      public :: poisson

      interface poisson;    module procedure poisson_FFT;     end interface
      ! interface poisson;    module procedure poisson_PSE;     end interface
      ! interface poisson;    module procedure poisson_Jacobi;  end interface
      interface poisson;    module procedure poisson_SOR;     end interface
      ! interface poisson;    module procedure poisson_ADI;     end interface
      ! interface poisson;    module procedure poisson_MG;      end interface

      contains

      subroutine poisson_FFT(FT,u,f,m,ss,err,displayTF,dir)
        implicit none
        type(FFTSolver),intent(inout) :: FT
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: err
        logical,intent(in) :: displayTF
        integer,intent(in) :: dir
        call solve(FT,u,f,m,ss,err,displayTF,dir)
      end subroutine

      ! subroutine poisson_PSE(PSE,u,f,m,ss,err,displayTF)
      !   implicit none
      !   type(PseudoTimeSolver),intent(inout) :: PSE
      !   real(cp),dimension(:,:,:),intent(inout) :: u
      !   real(cp),dimension(:,:,:),intent(in) :: f
      !   type(mesh),intent(in) :: m
      !   type(solverSettings),intent(inout) :: ss
      !   type(norms),intent(inout) :: err
      !   logical,intent(in) :: displayTF
      !   call solve(PSE,u,f,m,ss,err,displayTF)
      ! end subroutine

      ! subroutine poisson_Jacobi(JAC,u,f,m,ss,err,displayTF)
      !   implicit none
      !   type(jacobi),intent(inout) :: JAC
      !   real(cp),dimension(:,:,:),intent(inout) :: u
      !   real(cp),dimension(:,:,:),intent(in) :: f
      !   type(mesh),intent(in) :: m
      !   type(solverSettings),intent(inout) :: ss
      !   type(norms),intent(inout) :: err
      !   logical,intent(in) :: displayTF
      !   call solve(JAC,u,f,m,ss,err,displayTF)
      ! end subroutine

      subroutine poisson_SOR(SOR,u,f,m,ss,err,displayTF)
        implicit none
        type(SORSolver),intent(inout) :: SOR
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(solverSettings),intent(inout) :: ss
        type(norms),intent(inout) :: err
        logical,intent(in) :: displayTF
        call solve(SOR,u,f,m,ss,err,displayTF)
      end subroutine

      ! subroutine poisson_ADI(ADI,u,f,m,ss,err,displayTF)
      !   implicit none
      !   type(myADI),intent(inout) :: ADI
      !   real(cp),dimension(:,:,:),intent(inout) :: u
      !   real(cp),dimension(:,:,:),intent(in) :: f
      !   type(mesh),intent(in) :: m
      !   type(solverSettings),intent(inout) :: ss
      !   type(norms),intent(inout) :: err
      !   logical,intent(in) :: displayTF
      !   call solve(ADI,u,f,m,ss,err,displayTF)
      ! end subroutine

      ! subroutine poisson_MG(MG,u,f,m,ss,err,displayTF)
      !   implicit none
      !   type(multiGrid),dimension(:),intent(inout) :: MG
      !   real(cp),dimension(:,:,:),intent(inout) :: u
      !   real(cp),dimension(:,:,:),intent(in) :: f
      !   type(mesh),intent(in) :: m
      !   type(solverSettings),intent(inout) :: ss
      !   type(norms),intent(inout) :: err
      !   logical,intent(in) :: displayTF
      !   call solve(MG,u,f,m,ss,err,displayTF)
      ! end subroutine

      end module