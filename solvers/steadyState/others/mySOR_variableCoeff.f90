      module mySOR_mod
      ! call mySOR(u,f,u_bcs,gd,ss,gridType,displayTF)
      ! solves the poisson equation:
      !      ∇•(σ∇u)= f
      ! for a given f, boundary conditions for u (u_bcs), griddata (gd)
      ! and solver settings (ss) using the iterative Successive Over 
      ! Realxation (SOR) method
      !
      ! Input:
      !     u            = initial guess for u
      !     f            = RHS of above equation
      !     u_bcs        = boundary conditions for u. Refer to BCs_mod for more info.
      !     gd           = contains grid information (dhc,dhn)
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

      use constants_mod
      use simParams_mod
      use griddata_mod
      use BCs_mod
      use applyBCs_mod
      use myError_mod
      use vectorOps_mod
      use solverSettings_mod
      implicit none

      private

      public :: mySOR

      logical, parameter :: useGaussSeidel = .true.

      contains

      subroutine mySOR(u,f,sigma,u_bcs,gd,ss,err,gridType,displayTF)
        implicit none
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f,sigma
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in),optional :: displayTF
        call mySOR(u,f,sigma,u_bcs,gd,ss,err,gridType,displayTF)
      end subroutine

      subroutine mySOR(u,f,sigma,u_bcs,gd,ss,err,gridType,displayTF)
        implicit none
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f,sigma
        type(BCs),intent(in) :: u_bcs
        type(griddata),intent(in) :: gd
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: err
        integer,intent(in) :: gridType
        logical,intent(in),optional :: displayTF
        ! Locals
        logical :: tempDisplayTF
        real(dpn),dimension(:,:,:),allocatable :: lapu
        integer,dimension(3) :: s
        integer :: i,j,k,ijk,Nx,Ny,Nz
        real(dpn) :: r,omega,sig
        real(dpn),dimension(:),allocatable :: dx1,dy1,dz1
        real(dpn),dimension(:),allocatable :: dx2,dy2,dz2
        logical :: TF,continueLoop,TF_allDirichlet
        integer :: maxIterations
        s = shape(f)

        select case (gridType)
        case (1) ! Cell based (for pressure)
          Nx = s(1)-2; Ny = s(2)-2; Nz = s(3)-2
          allocate(dx1(Nx+1),dy1(Ny+1),dz1(Nz+1))
          allocate(dx2(Nx),dy2(Ny),dz2(Nz))
          call getDxcc(gd,dx1)
          call getDycc(gd,dy1)
          call getDzcc(gd,dz1)
          call getDxn(gd,dx2)
          call getDyn(gd,dy2)
          call getDzn(gd,dz2)
        case (2) ! Node based (for magnetic field)
          Nx = s(1)-1; Ny = s(2)-1; Nz = s(3)-1
          allocate(dx2(Nx+1),dy2(Ny+1),dz2(Nz+1))
          allocate(dx1(Nx),dy1(Ny),dz1(Nz))
          call getDxcc(gd,dx2)
          call getDycc(gd,dy2)
          call getDzcc(gd,dz2)
          call getDxn(gd,dx1)
          call getDyn(gd,dy1)
          call getDzn(gd,dz1)
        case default
          write(*,*) 'gridType in SOR must be 1 or 2. Terminating';stop
        end select

        omega = 2.0/(one + sqrt(one - & 
         ((cos(PI/dble(Nx+1)) + cos(PI/dble(Ny+1)) + cos(PI/dble(Nz+1)))/3.0)**2.0))

        if (useGaussSeidel) omega = 1.0 ! omega = 1 for Gauss Seidel

        if (present(displayTF)) then
          tempDisplayTF = displayTF
        else; tempDisplayTF = .false.
        endif
        TF_allDirichlet = getAllDirichlet(u_bcs)
        call solverSettingsSet(ss)

        allocate(lapu(s(1),s(2),s(3)))
        ijk = 0

        ! Boundaries
        call applyAllBCs(u_bcs,u,gd)

        if (getMaxIterationsTF(ss)) then
          maxIterations = getMaxIterations(ss)
          TF = (maxIterations.ge.1)
        else; TF = .true.
        endif
        continueLoop = .true.

        do while (continueLoop.and.TF)
          ijk = ijk + 1
          !$OMP PARALLEL PRIVATE(r)
          !$OMP DO
          do k=2,s(3)-1,2
            do j=2,s(2)-1
              do i=2,s(1)-1
                sig(1) = sigma(i,j,k)*(one-dx2(i-1)/(2.0d0*dx1(i-1)))+&
                                           dx2(i-1)/(2.0d0*dx1(i-1))*sigma(i-1,j,k)
                sig(2) = sigma(i,j,k)*(one-dx2( i )/(2.0d0*dx1( i )))+&
                                           dx2( i )/(2.0d0*dx1( i ))*sigma(i+1,j,k)
                sig(3) = sigma(i,j,k)*(one-dy2(j-1)/(2.0d0*dy1(j-1)))+&
                                           dy2(j-1)/(2.0d0*dy1(j-1))*sigma(i,j-1,k)
                sig(4) = sigma(i,j,k)*(one-dy2( j )/(2.0d0*dy1( j )))+&
                                           dy2( j )/(2.0d0*dy1( j ))*sigma(i,j+1,k)
                sig(5) = sigma(i,j,k)*(one-dz2(k-1)/(2.0d0*dz1(k-1)))+&
                                           dz2(k-1)/(2.0d0*dz1(k-1))*sigma(i,j,k-1)
                sig(6) = sigma(i,j,k)*(one-dz2( k )/(2.0d0*dz1( k )))+&
                                           dz2( k )/(2.0d0*dz1( k ))*sigma(i,j,k+1)

                r = one/dx2(i-1)*(sig(2)/dx1(i) + sig(1)/dx1(i-1)) + & 
                    one/dy2(j-1)*(sig(4)/dy1(j) + sig(3)/dy1(j-1)) + & 
                    one/dz2(k-1)*(sig(6)/dz1(k) + sig(5)/dz1(k-1))

                u(i,j,k) = u(i,j,k)*(1.0-omega) + &

                   omega*( u(i-1,j,k)*(sig(1)/(dx1(i-1) * dx2(i-1)) + &
                           u(i+1,j,k)*(sig(2)/(dx1( i ) * dx2(i-1)) + &
                           u(i,j-1,k)*(sig(3)/(dy1(j-1) * dy2(j-1)) + &
                           u(i,j+1,k)*(sig(4)/(dy1( j ) * dy2(j-1)) + &
                           u(i,j,k-1)*(sig(5)/(dz1(k-1) * dz2(k-1)) + &
                           u(i,j,k+1)*(sig(6)/(dz1( k ) * dz2(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
          !$OMP END DO

          !$OMP DO
          do k=3,s(3)-1,2
            do j=2,s(2)-1
              do i=2,s(1)-1
                sig(1) = sigma(i,j,k)*(one-dx2(i-1)/(2.0d0*dx1(i-1)))+&
                                           dx2(i-1)/(2.0d0*dx1(i-1))*sigma(i-1,j,k)
                sig(2) = sigma(i,j,k)*(one-dx2( i )/(2.0d0*dx1( i )))+&
                                           dx2( i )/(2.0d0*dx1( i ))*sigma(i+1,j,k)
                sig(3) = sigma(i,j,k)*(one-dy2(j-1)/(2.0d0*dy1(j-1)))+&
                                           dy2(j-1)/(2.0d0*dy1(j-1))*sigma(i,j-1,k)
                sig(4) = sigma(i,j,k)*(one-dy2( j )/(2.0d0*dy1( j )))+&
                                           dy2( j )/(2.0d0*dy1( j ))*sigma(i,j+1,k)
                sig(5) = sigma(i,j,k)*(one-dz2(k-1)/(2.0d0*dz1(k-1)))+&
                                           dz2(k-1)/(2.0d0*dz1(k-1))*sigma(i,j,k-1)
                sig(6) = sigma(i,j,k)*(one-dz2( k )/(2.0d0*dz1( k )))+&
                                           dz2( k )/(2.0d0*dz1( k ))*sigma(i,j,k+1)

                r = one/dx2(i-1)*(sig(2)/dx1(i) + sig(1)/dx1(i-1)) + & 
                    one/dy2(j-1)*(sig(4)/dy1(j) + sig(3)/dy1(j-1)) + & 
                    one/dz2(k-1)*(sig(6)/dz1(k) + sig(5)/dz1(k-1))

                u(i,j,k) = u(i,j,k)*(1.0-omega) + &

                   omega*( u(i-1,j,k)*(sig(1)/(dx1(i-1) * dx2(i-1)) + &
                           u(i+1,j,k)*(sig(2)/(dx1( i ) * dx2(i-1)) + &
                           u(i,j-1,k)*(sig(3)/(dy1(j-1) * dy2(j-1)) + &
                           u(i,j+1,k)*(sig(4)/(dy1( j ) * dy2(j-1)) + &
                           u(i,j,k-1)*(sig(5)/(dz1(k-1) * dz2(k-1)) + &
                           u(i,j,k+1)*(sig(6)/(dz1( k ) * dz2(k-1)) &
                         - f(i,j,k) )/r
              enddo
            enddo
          enddo
          !$OMP END DO
          !$OMP END PARALLEL

          if (.not.TF_allDirichlet) then
            call applyAllBCs(u_bcs,u,gd)
          endif

          if (getMinToleranceTF(ss)) then
            select case (gridType)
            case (1) ! Cell based (for pressure)
              call myCC2CCLap(lapu,u,gd)
            case (2) ! Node based (for magnetic field)
              call myNodeLap(lapu,u,gd)
            end select
            call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
            call setTolerance(ss,getL2Rel(err))
          endif

          call setIteration(ss,ijk)

          ! ********************************* CHECK TO EXIT ************************************
          call checkCondition(ss,continueLoop)
          if (.not.continueLoop) exit
          ! ************************************************************************************
        enddo
        
        ! Subtract mean (for Pressure Poisson)
        if (getSubtractMean(ss)) u = u - sum(u)/(max(1,size(u)))

        if (getAnyDirichlet(u_bcs)) then
          call applyAllBCs(u_bcs,u,gd)
        endif

        if (tempDisplayTF) then
          if (.not.useGaussSeidel) then
            write(*,*) 'SOR parameter = ',omega
            write(*,*) '(Final,max) SOR iteration = ',ijk,maxIterations

            select case (gridType)
            case (1) ! Cell based (for pressure)
              call myCC2CCLap(lapu,u,gd)
            case (2) ! Node based (for magnetic field)
              call myNodeLap(lapu,u,gd)
            end select
            call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
            call printMyError(err,'SOR Residuals for '//trim(adjustl(getName(ss))))
          else
            write(*,*) '(Final,max) Gauss Seidel iteration = ',ijk,maxIterations
            select case (gridType)
            case (1) ! Cell based (for pressure)
              call myCC2CCLap(lapu,u,gd)
            case (2) ! Node based (for magnetic field)
              call myNodeLap(lapu,u,gd)
            end select
            call computeError(err,f(2:s(1)-1,2:s(2)-1,2:s(3)-1),lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
            ! write(*,*) 'maxval = ',maxval(f - lapu)
            ! write(*,*) 'maxloc = ',maxloc(f - lapu)
            ! write(*,*) 'dx = ',dx(s(1)-1)
            ! write(*,*) 'dy = ',dy(s(2)-1)
            ! write(*,*) 'dz = ',dz(s(3)-1)
            ! write(*,*) 'maxval(dh) = ',maxval(dx),maxval(dy),maxval(dz)
            ! write(*,*) 'minval(dh) = ',minval(dx),minval(dy),minval(dz)
            ! pause
            call printMyError(err,'Gauss Seidel Residuals for '//trim(adjustl(getName(ss))))
          endif
        endif

        deallocate(dx1,dy1,dz1)
        deallocate(dx2,dy2,dz2)
        deallocate(lapu)
      end subroutine


      end module