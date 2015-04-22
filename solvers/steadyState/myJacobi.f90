      module myJacobi_mod
      ! call myJacobi(u,f,u_bcs,g,ss,gridType,displayTF)
      ! solves the poisson equation:
      !     u_xx + u_yy + u_zz = f
      ! for a given f, boundary conditions for u (u_bcs), grid (g)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (Jacobi) method
      ! 
      ! Note that the variant of Gauss-Seidel/Jacobi called
      ! "red-black" Gauss-Seidel is used.
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     g            = contains grid information (dhc,dhn)
      !     ss           = solver settings (specifies max iterations, tolerance etc.)
      !     gridType     = (1,2) = (cell-based,node-based)
      !     displayTF    = print residuals to screen (T,F)
      ! 
      ! Fixes/improvements:
      ! - 'If ANY conditions are Dirichlet, then the Neumann conditions
      !   can be taken outside of the iteration loop becuase the matrix 
      !   is no longer singular.' --Gautam
      ! 
      ! - Clean up a bit, it seems some code is written twice

      ! Flags: (_PARALLELIZE_Jacobi_,_EXPORT_Jacobi_CONVERGENCE_)

      use grid_mod
      use BCs_mod
      use applyBCs_mod
      use myError_mod
      use delOps_mod
      use solverSettings_mod
#ifdef _EXPORT_Jacobi_CONVERGENCE_
      use IO_tools_mod
#endif
      implicit none

      private
      public :: myJacobi,solve
      private :: init,delete

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
      real(cp),parameter :: PI = 3.14159265358979

      logical, parameter :: useGaussSeidel = .true.

      type myJacobi
        character(len=6) :: name
        integer :: gridType
        real(cp),dimension(:),allocatable :: dxp,dyp,dzp
        real(cp),dimension(:),allocatable :: dxd,dyd,dzd
        real(cp),dimension(:,:,:),allocatable :: lapu,f,r ! f zeros mean
        real(cp),dimension(:,:,:),allocatable :: uTemp
        real(cp) :: omega
        integer,dimension(3) :: s
        integer :: gt
      end type
      
      interface init;        module procedure initJacobi;       end interface
      interface delete;      module procedure deleteJacobi;     end interface
      interface solve;       module procedure solveJacobi;      end interface

      contains

      subroutine initJacobi(Jacobi,s,g)
        implicit none
        type(myJacobi),intent(inout) :: Jacobi
        integer,dimension(3),intent(in) :: s
        type(grid),intent(in) :: g
        integer :: Nx,Ny,Nz
        
        Jacobi%s = s

            if (Jacobi%s(1).eq.g%c(1)%sc) then; Jacobi%gridType = 1
        elseif (Jacobi%s(1).eq.g%c(1)%sn) then; Jacobi%gridType = 2
        else; stop 'Error: gridType was not determined in Jacobi.f90'
        endif

        select case (Jacobi%gridType)
        case (1) ! Cell based (for pressure)
          Nx = g%c(1)%sc-1; Ny = g%c(2)%sc-1; Nz = g%c(3)%sc-1   ! number of cells (excluding ghost)
          allocate(Jacobi%dxp(Nx),Jacobi%dyp(Ny),Jacobi%dzp(Nz))          ! Primary grid
          Nx = g%c(1)%sn-1; Ny = g%c(2)%sn-1; Nz = g%c(3)%sn-1   ! number of cells (excluding ghost)
          allocate(Jacobi%dxd(Nx),Jacobi%dyd(Ny),Jacobi%dzd(Nz))          ! Dual grid
          Jacobi%dxp = g%c(1)%dhc
          Jacobi%dyp = g%c(2)%dhc
          Jacobi%dzp = g%c(3)%dhc
          Jacobi%dxd = g%c(1)%dhn
          Jacobi%dyd = g%c(2)%dhn
          Jacobi%dzd = g%c(3)%dhn
          Jacobi%gt = 1
        case (2) ! Node based (for magnetic field)
          Nx = g%c(1)%sn-1; Ny = g%c(2)%sn-1; Nz = g%c(3)%sn-1   ! number of cells (excluding ghost)
          allocate(Jacobi%dxp(Nx),Jacobi%dyp(Ny),Jacobi%dzp(Nz))          ! Primary grid
          Nx = g%c(1)%sc-1; Ny = g%c(2)%sc-1; Nz = g%c(3)%sc-1   ! number of cells (excluding ghost)
          allocate(Jacobi%dxd(Nx),Jacobi%dyd(Ny),Jacobi%dzd(Nz))          ! Dual grid
          Jacobi%dxd = g%c(1)%dhc
          Jacobi%dyd = g%c(2)%dhc
          Jacobi%dzd = g%c(3)%dhc
          Jacobi%dxp = g%c(1)%dhn
          Jacobi%dyp = g%c(2)%dhn
          Jacobi%dzp = g%c(3)%dhn
          Jacobi%gt = 0
        case default
          write(*,*) 'gridType in Jacobi must be 1 or 2. Terminating';stop
        end select
        allocate(Jacobi%lapu(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))
        allocate(Jacobi%f(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))
        allocate(Jacobi%r(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))
        allocate(Jacobi%uTemp(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))

        if (useGaussSeidel) then
          Jacobi%omega = real(1.0,cp)
          Jacobi%name = 'Jacobi'
        else
          Jacobi%omega = real(2.0,cp)/(real(1.0,cp) + sqrt(real(1.0,cp) - & 
           ((cos(PI/real(Nx+1,cp)) + cos(PI/real(Ny+1,cp)) + &
             cos(PI/real(Nz+1,cp)))/real(3.0,cp))**real(2.0,cp)))
          Jacobi%name = 'Jacobi'
        endif
      end subroutine

      subroutine deleteJacobi(Jacobi)
        implicit none
        type(myJacobi),intent(inout) :: Jacobi
        deallocate(Jacobi%dxp)
        deallocate(Jacobi%dyp)
        deallocate(Jacobi%dzp)
        deallocate(Jacobi%dxd)
        deallocate(Jacobi%dyd)
        deallocate(Jacobi%dzd)
        deallocate(Jacobi%lapu)
        deallocate(Jacobi%f)
        deallocate(Jacobi%r)
        deallocate(Jacobi%uTemp)

        ! write(*,*) 'Jacobi object deleted'
      end subroutine


      subroutine solveJacobi(Jacobi,u,f,u_bcs,g,ss,norms,displayTF)
        implicit none
        type(myJacobi),intent(inout) :: Jacobi
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: norms
        logical,intent(in) :: displayTF
        ! Locals
        integer,dimension(3) :: s
        integer :: i,j,k,ijk,gt
        real(cp) :: r
        logical :: TF,continueLoop,TF_allDirichlet
        integer :: maxIterations
#ifdef _EXPORT_Jacobi_CONVERGENCE_
        integer :: NU
#endif
        
        s = shape(f)
        call init(Jacobi,s,g)
        gt = Jacobi%gt

        TF_allDirichlet = getAllDirichlet(u_bcs)
        call solverSettingsSet(ss)
        ijk = 0

        ! Boundaries
        call applyAllBCs(u_bcs,u,g) ! Necessary with ghost nodes

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

        Jacobi%f = f ! CANNOT REMOVE MEAN FOR NEUMANN, RESULTS IN BAD RESIDUALS FOR Jacobi

#ifdef _EXPORT_Jacobi_CONVERGENCE_
        NU = newAndOpen('out\','norms_Jacobi')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1
#ifdef _PARALLELIZE_Jacobi_
          !$OMP DO PRIVATE(r)
#endif
          do k=2,s(3)-1,2
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = real(1.0,cp)/Jacobi%dxd(i-1+gt)*(real(1.0,cp)/Jacobi%dxp(i) + real(1.0,cp)/Jacobi%dxp(i-1)) + & 
                    real(1.0,cp)/Jacobi%dyd(j-1+gt)*(real(1.0,cp)/Jacobi%dyp(j) + real(1.0,cp)/Jacobi%dyp(j-1)) + & 
                    real(1.0,cp)/Jacobi%dzd(k-1+gt)*(real(1.0,cp)/Jacobi%dzp(k) + real(1.0,cp)/Jacobi%dzp(k-1))

               Jacobi%uTemp(i,j,k) = u(i,j,k)*(real(1.0,cp)-Jacobi%omega) + &

               Jacobi%omega*( u(i-1,j,k)/(Jacobi%dxp(i-1) * Jacobi%dxd(i-1+gt)) + &
                              u(i+1,j,k)/(Jacobi%dxp( i ) * Jacobi%dxd(i-1+gt)) + &
                              u(i,j-1,k)/(Jacobi%dyp(j-1) * Jacobi%dyd(j-1+gt)) + &
                              u(i,j+1,k)/(Jacobi%dyp( j ) * Jacobi%dyd(j-1+gt)) + &
                              u(i,j,k-1)/(Jacobi%dzp(k-1) * Jacobi%dzd(k-1+gt)) + &
                              u(i,j,k+1)/(Jacobi%dzp( k ) * Jacobi%dzd(k-1+gt)) &
                            - Jacobi%f(i,j,k) )/r
              enddo
            enddo
          enddo
#ifdef _PARALLELIZE_Jacobi_
          !$OMP END DO
#endif

#ifdef _PARALLELIZE_Jacobi_
          !$OMP DO PRIVATE(r)
#endif
          do k=3,s(3)-1,2 ! 3 is correct (odd numbers)
            do j=2,s(2)-1
              do i=2,s(1)-1
                r = real(1.0,cp)/Jacobi%dxd(i-1+gt)*(real(1.0,cp)/Jacobi%dxp(i) + real(1.0,cp)/Jacobi%dxp(i-1)) + & 
                    real(1.0,cp)/Jacobi%dyd(j-1+gt)*(real(1.0,cp)/Jacobi%dyp(j) + real(1.0,cp)/Jacobi%dyp(j-1)) + & 
                    real(1.0,cp)/Jacobi%dzd(k-1+gt)*(real(1.0,cp)/Jacobi%dzp(k) + real(1.0,cp)/Jacobi%dzp(k-1))

               Jacobi%uTemp(i,j,k) = u(i,j,k)*(real(1.0,cp)-Jacobi%omega) + &

               Jacobi%omega*( u(i-1,j,k)/(Jacobi%dxp(i-1) * Jacobi%dxd(i-1+gt)) + &
                              u(i+1,j,k)/(Jacobi%dxp( i ) * Jacobi%dxd(i-1+gt)) + &
                              u(i,j-1,k)/(Jacobi%dyp(j-1) * Jacobi%dyd(j-1+gt)) + &
                              u(i,j+1,k)/(Jacobi%dyp( j ) * Jacobi%dyd(j-1+gt)) + &
                              u(i,j,k-1)/(Jacobi%dzp(k-1) * Jacobi%dzd(k-1+gt)) + &
                              u(i,j,k+1)/(Jacobi%dzp( k ) * Jacobi%dzd(k-1+gt)) &
                            - Jacobi%f(i,j,k) )/r
              enddo
            enddo
          enddo
#ifdef _PARALLELIZE_Jacobi_
          !$OMP END DO
#endif
          u = Jacobi%uTemp

          call applyAllBCs(u_bcs,u,g) ! Necessary with ghost nodes

          if (getMinToleranceTF(ss)) then
            call lap(Jacobi%lapu,u,g)
            Jacobi%r = Jacobi%lapu - Jacobi%f
            call zeroGhostPoints(Jacobi%r)
            call compute(norms,real(0.0,cp),Jacobi%r)
            call setTolerance(ss,getL2Rel(norms))
          endif

#ifdef _EXPORT_Jacobi_CONVERGENCE_
            call lap(Jacobi%lapu,u,g)
            Jacobi%r = Jacobi%lapu - Jacobi%f
            call zeroGhostPoints(Jacobi%r)
            call compute(norms,real(0.0,cp),Jacobi%r)
            write(NU,*) getL1(norms),getL2(norms),getLinf(norms)
#endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo

#ifdef _EXPORT_Jacobi_CONVERGENCE_
        close(NU)
#endif
        
        ! Subtract mean (for Pressure Poisson)
        ! This step is not necessary if mean(f) = 0 and all BCs are Neumann.
        if (allNeumann(u_bcs)) then
          u = u - sum(u)/(max(1,size(u)))
        endif

        ! Okay for Jacobi alone when comparing with u_exact, but not okay for MG
        ! if (.not.allNeumann(u_bcs)) then
        !   u = u - sum(u)/(max(1,size(u)))
        ! endif

        if (displayTF) then
          write(*,*) 'Jacobi parameter = ',Jacobi%omega
          write(*,*) '(Final,max) '//Jacobi%name//' iteration = ',ijk,maxIterations

          call lap(Jacobi%lapu,u,g)
          Jacobi%r = Jacobi%lapu - Jacobi%f
          call zeroGhostPoints(Jacobi%r)
          call compute(norms,real(0.0,cp),Jacobi%r)
          call print(norms,Jacobi%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(Jacobi)
      end subroutine
      end module