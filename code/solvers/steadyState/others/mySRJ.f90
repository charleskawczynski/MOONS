      module mySRJ_mod
      ! [u] = mySRJ(f,bc,bctype,gd) solves the poisson equation:
      !     u_xx + u_yy = f
      ! for a given f and set of boundary conditions and griddata gd.
      ! using the Scheduled Relaxation Jacobi iterative method described in 
      ! "Acceleration of the Jacobi iterative method by factors exceeding
      ! 100 using scheduled relaxation".
      !
      ! Input: 
      ! f            = RHS of above equation
      ! bc           = boundary conditions of u (u_bc = bc)
      ! bctype       = (dirichlet,neumann,robin) = (TF,TF,TF)
      ! gd           = (dx,dy,dz)
      !
      ! Note, to get the boundary condition type use:
      !           TF = getDirichlet(bctype)
      !           TF = getNeumann(bctype)
      !           TF = getRobin(bctype)
      ! 
      ! Fixes/improvements:
      ! - 'If ANY conditions are Dirichlet, then the Neumann conditions
      !   can be taken outside of the iteration loop becuase the matrix 
      !   is no longer singular.' --Gautam

      use constants_mod
      use griddata_mod
      use allBCs_mod
      use applyBCs_mod
      use myError_mod
      use vectorOps_mod
      implicit none

      private

      public :: mySRJ

      real(dpn),parameter :: tol = 1.0e-6 ! (Compared with Linf norm)
      integer,parameter :: maxIterations = 30

      interface mySRJ
!         module procedure mySRJ1D
        module procedure mySRJ2D
        module procedure mySRJ3D
      end interface

      contains

      subroutine mySRJ2D(u,f,ab,gd)
        implicit none
        real(dpn),dimension(:,:),intent(inout) :: u
        real(dpn),dimension(:,:),intent(in) :: f
        type(allBCs),intent(in) :: ab
        type(griddata),intent(in) :: gd
        real(dpn),dimension(:,:),allocatable :: lapu
        integer,dimension(2) :: s
        integer :: i,j,ijk
        real(dpn) :: dx2,dy2,Nx,Ny,r,omega
        logical :: TF
        type(myError) :: err
        s = shape(f)
        dx2 = getDx(gd)**2.0
        dy2 = getDy(gd)**2.0
        Nx = getNx(gd)
        Ny = getNy(gd)

        r = dx2*dy2/(dx2 + dy2)
        omega = 2.0/(1.0 + sqrt(1.0- & 
         ((cos(PI/(Nx-1)) + cos(PI/(Nx-1)))/2.0)**2.0))

        allocate(lapu(s(1),s(2)))
        TF = .true.
        ijk = 0

        ! Boundaries
        call applyAllBCs(ab,u,gd)

        do while (TF.and.(ijk.lt.maxIterations))
          do i=2,s(1)-1
            do j=2,s(2)-1
              u(i,j) = u(i,j)*(1.0-omega) + &
                 omega*0.5*( (u(i+1,j)+u(i-1,j))/(1.0+dx2/dy2)+ & 
                               (u(i,j+1)+u(i,j-1))/(dy2/dx2+1.0)-r*f(i,j) )
            enddo
          enddo

          if (.not.getAllDirichlet(ab)) then
            call applyAllBCs(ab,u,gd)
          endif

          call myLap(lapu,u,gd)

          ijk = ijk + 1
          err = computeError(f,lapu)
          if (getLinf(err).lt.tol) TF = .false.
        enddo
        write(*,*) 'Final 2D SOR iteration = ',ijk
          err = computeError(f(2:s(1)-1,2:s(2)-1),lapu(2:s(1)-1,2:s(2)-1))
        call printMyError(err,'2D SOR solution')
        if (ijk.eq.maxIterations) then
          write(*,*) 'Maximum 2D SOR iterations reached.'
        endif
        deallocate(lapu)
      end subroutine

      subroutine mySRJ3D(u,f,ab,gd)
        implicit none
        real(dpn),dimension(:,:,:),intent(inout) :: u
        real(dpn),dimension(:,:,:),intent(in) :: f
        type(allBCs),intent(in) :: ab
        type(griddata),intent(in) :: gd
        real(dpn),dimension(:,:,:),allocatable :: lapu
        integer,dimension(3) :: s
        integer :: i,j,k,ijk
        real(dpn) :: dx2,dy2,dz2,Nx,Ny,Nz,r,omega
        logical :: TF
        type(myError) :: err
        s = shape(f)
        dx2 = getDx(gd)**2.0
        dy2 = getDy(gd)**2.0
        dz2 = getDz(gd)**2.0
        Nx = getNx(gd)
        Ny = getNy(gd)
        Nz = getNz(gd)

        r = dx2*dy2*dz2/(dy2*dz2 + dx2*dz2 + dx2*dy2)
        omega = 2.0/(1.0 + sqrt(1.0 - & 
         ((cos(PI/(Nx-1)) + cos(PI/(Nx-1)) + cos(PI/(Nz-1)))/3.0)**2.0))

        allocate(lapu(s(1),s(2),s(3)))
        TF = .true.
        ijk = 0

        ! Boundaries
        call applyAllBCs(ab,u,gd)

        do while (TF.and.(ijk.lt.maxIterations))
          do i=2,s(1)-1
            do j=2,s(2)-1
              do k=2,s(3)-1
                u(i,j,k) = u(i,j,k)*(1.0-omega) + &
                   omega*0.5*( (u(i+1,j,k)+u(i-1,j,k))/(1.0+dx2/dy2+dx2/dz2)+ & 
                                 (u(i,j+1,k)+u(i,j-1,k))/(dy2/dx2+1.0+dy2/dz2)+ & 
                                 (u(i,j,k+1)+u(i,j,k-1))/(dz2/dx2+dz2/dy2+1.0)-r*f(i,j,k) )
              enddo
            enddo
          enddo

          if (.not.getAllDirichlet(ab)) then
            call applyAllBCs(ab,u,gd)
          endif

          call myLap(lapu,u,gd)

          ijk = ijk + 1
          err = computeError(f(2:s(1)-1,2:s(2)-1,2:s(3)-1),lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
          if (getLinf(err).lt.tol) TF = .false.
        enddo
        write(*,*) 'Final 3D SRJ iteration = ',ijk
        err = computeError(f(2:s(1)-1,2:s(2)-1,2:s(3)-1),lapu(2:s(1)-1,2:s(2)-1,2:s(3)-1))
        call printMyError(err,'3D SRJ solution')
        if (ijk.eq.maxIterations) then
          write(*,*) 'Maximum 3D SRJ iterations reached.'
        endif
        deallocate(lapu)
      end subroutine

      end module