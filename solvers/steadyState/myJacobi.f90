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
      ! Flags: (_PARALLELIZE_Jacobi_,
      !         _EXPORT_Jacobi_CONVERGENCE_)

      use grid_mod
      use BCs_mod
      use applyBCs_mod
      use myError_mod
      use ops_discrete_mod
      use ops_aux_mod

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
        character(len=5) :: name
        type(grid) :: p,d ! Primary / Dual grids
        real(cp),dimension(:,:,:),allocatable :: lapu,f,res ! f zeros mean
        real(cp),dimension(:,:,:),allocatable :: uTemp
        real(cp) :: omega
        integer,dimension(3) :: gt,s
        integer :: gridType ! (1,2,3) = (CC,N,Face)
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
        integer :: Nx,Ny,Nz,i
        
        Jacobi%s = s

        do i=1,3
          if (Jacobi%s(i).eq.g%c(i)%sc) then
            call init(Jacobi%p,g%c(i)%hc,i,2) ! grid made from cc --> p%dhn is dhc
            call init(Jacobi%d,g%c(i)%hn,i,2) ! grid made from n --> d%dhn is dhn
            Jacobi%gt(i) = 1
          elseif(Jacobi%s(i).eq.g%c(i)%sn) then
            call init(Jacobi%p,g%c(i)%hn,i,2) ! grid made from n --> p%dhn is dhn
            call init(Jacobi%d,g%c(i)%hc,i,2) ! grid made from cc --> d%dhn is dhc
            Jacobi%gt(i) = 0
          else
            write(*,*) 's = ',Jacobi%s
            write(*,*) 'sc = ',(/g%c(1)%sc,g%c(2)%sc,g%c(3)%sc/)
            write(*,*) 'sn = ',(/g%c(1)%sn,g%c(2)%sn,g%c(3)%sn/)
            stop 'Error: grid type was not determined in Jacobi.f90'
          endif
        enddo

        if (all((/(s(i).eq.g%c(i)%sc, i=1,3)/))) then ! Node data
        Jacobi%gridType = 1

        elseif (all((/(s(i).eq.g%c(i)%sn, i=1,3)/))) then ! CC data
        Jacobi%gridType = 2

        ! Face data
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sc/))) then
        Jacobi%gridType = 3
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
        Jacobi%gridType = 3
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
        Jacobi%gridType = 3

        ! Edge Data
        elseif (all((/s(1).eq.g%c(1)%sc,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sn/))) then
          stop 'Error: edge data not yet supported in Jacobi.f90'
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sc,s(3).eq.g%c(3)%sn/))) then
          stop 'Error: edge data not yet supported in Jacobi.f90'
        elseif (all((/s(1).eq.g%c(1)%sn,s(2).eq.g%c(2)%sn,s(3).eq.g%c(3)%sc/))) then
          stop 'Error: edge data not yet supported in Jacobi.f90'
        else
          write(*,*) 's = ',s
          write(*,*) 'g%sn = ',(/(g%c(i)%sn, i=1,3)/)
          write(*,*) 'g%sc = ',(/(g%c(i)%sc, i=1,3)/)
          stop 'Error: in grid size compared to input field in Jacobi.f90.'
        endif


        allocate(Jacobi%lapu(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))
        allocate(Jacobi%f(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))
        allocate(Jacobi%res(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))
        allocate(Jacobi%uTemp(Jacobi%s(1),Jacobi%s(2),Jacobi%s(3)))

        if (useGaussSeidel) then
          Jacobi%omega = real(1.0,cp)
          Jacobi%name = 'Jacobi'
        else
          Nx = s(1); Ny = s(2); Nz = s(3)
          Jacobi%omega = real(2.0,cp)/(real(1.0,cp) + sqrt(real(1.0,cp) - & 
           ((cos(PI/real(Nx+1,cp)) + cos(PI/real(Ny+1,cp)) + &
             cos(PI/real(Nz+1,cp)))/real(3.0,cp))**real(2.0,cp)))
          Jacobi%name = 'Jacobi'
        endif
      end subroutine

      subroutine deleteJacobi(Jacobi)
        implicit none
        type(myJacobi),intent(inout) :: Jacobi
        call delete(Jacobi%p)
        call delete(Jacobi%d)
        deallocate(Jacobi%lapu)
        deallocate(Jacobi%f)
        deallocate(Jacobi%res)
        deallocate(Jacobi%uTemp)

        ! write(*,*) 'Jacobi object deleted'
      end subroutine


      subroutine solveJacobi(jac,u,f,u_bcs,g,ss,norms,displayTF)
        implicit none
        type(myJacobi),intent(inout) :: jac
        real(cp),dimension(:,:,:),intent(inout) :: u
        real(cp),dimension(:,:,:),intent(in) :: f
        type(BCs),intent(in) :: u_bcs
        type(grid),intent(in) :: g
        type(solverSettings),intent(inout) :: ss
        type(myError),intent(inout) :: norms
        logical,intent(in) :: displayTF
        ! Locals
        integer :: ijk
        logical :: TF,continueLoop
        integer :: maxIterations
#ifdef _EXPORT_Jacobi_CONVERGENCE_
        integer :: NU
#endif
        
        call init(jac,shape(f),g)

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

        jac%f = f ! CANNOT REMOVE MEAN FOR NEUMANN, RESULTS IN BAD RESIDUALS FOR Jacobi

#ifdef _EXPORT_Jacobi_CONVERGENCE_
        NU = newAndOpen('out\','norms_Jacobi')
#endif

        do while (continueLoop.and.TF)
          ijk = ijk + 1

          jac%uTemp = u
          ! THE ORDER OF THESE ROUTINE CALLS IS IMPORTANT. DO NOT CHANGE.

#ifdef _PARALLELIZE_Jacobi_
          !$OMP PARALLEL

#endif
          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/0,0,0/)) ! Even in odd plane

          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/1,0,0/)) ! Even in even plane
          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/0,1,0/)) ! Even in even plane
          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/0,0,1/)) ! Even in even plane


#ifdef _PARALLELIZE_Jacobi_
          !$OMP END PARALLEL
          !$OMP PARALLEL

#endif
          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/1,1,1/)) ! Odd in odd plane

          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/0,1,1/)) ! Odd in even plane
          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/1,0,1/)) ! Odd in even plane
          call redBlack(jac%uTemp,u,jac%f,jac%s,jac%p%c(1)%dhn,jac%p%c(2)%dhn,jac%p%c(3)%dhn,&
          jac%d%c(1)%dhn,jac%d%c(2)%dhn,jac%d%c(3)%dhn,jac%omega,jac%gt,(/1,1,0/)) ! Odd in even plane

#ifdef _PARALLELIZE_Jacobi_
          !$OMP END PARALLEL

#endif

          call applyAllBCs(u_bcs,jac%uTemp,g)

          if (getMinToleranceTF(ss)) then
            call lap(jac%lapu,jac%uTemp,g)
            jac%res = jac%lapu - jac%f
            call zeroGhostPoints(jac%res)
            call compute(norms,real(0.0,cp),jac%res)
            call setTolerance(ss,getL2Rel(norms))
          endif

#ifdef _EXPORT_Jacobi_CONVERGENCE_
            call lap(jac%lapu,jac%uTemp,g)
            jac%res = jac%lapu - jac%f
            call zeroGhostPoints(jac%res)
            call compute(norms,real(0.0,cp),jac%res)
            write(NU,*) getL1(norms),getL2(norms),getLinf(norms)
#endif

          call setIteration(ss,ijk)
          u = jac%uTemp

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
          write(*,*) 'Jacobi parameter = ',jac%omega
          write(*,*) '(Final,max) '//jac%name//' iteration = ',ijk,maxIterations

          call lap(jac%lapu,u,g)
          jac%res = jac%lapu - jac%f
          call zeroGhostPoints(jac%res)
          call compute(norms,real(0.0,cp),jac%res)
          call print(norms,jac%name//' Residuals for '//trim(adjustl(getName(ss))))
        endif

        call delete(jac)
      end subroutine

      subroutine redBlack(u_out,u,f,s,dxp,dyp,dzp,dxd,dyd,dzd,omega,gt,odd)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: u_out
        real(cp),dimension(:,:,:),intent(in) :: f,u
        integer,dimension(3) :: s,odd
        real(cp),dimension(:),intent(in) :: dxp,dyp,dzp,dxd,dyd,dzd
        real(cp),intent(in) :: omega
        integer,dimension(3),intent(in) :: gt
        integer :: i,j,k
        real(cp) :: r

#ifdef _PARALLELIZE_Jacobi_
        !$OMP DO PRIVATE(r)

#endif

        do k=2+odd(3),s(3)-1,2
          do j=2+odd(2),s(2)-1,2
            do i=2+odd(1),s(1)-1,2

              r = real(1.0,cp)/dxd(i-1+gt(1))*(real(1.0,cp)/dxp(i) + real(1.0,cp)/dxp(i-1)) + & 
                  real(1.0,cp)/dyd(j-1+gt(2))*(real(1.0,cp)/dyp(j) + real(1.0,cp)/dyp(j-1)) + & 
                  real(1.0,cp)/dzd(k-1+gt(3))*(real(1.0,cp)/dzp(k) + real(1.0,cp)/dzp(k-1))

          u_out(i,j,k) = u(i,j,k)*(real(1.0,cp)-omega) + &
                 omega*( u(i-1,j,k)/(dxp(i-1) * dxd(i-1+gt(1))) + &
                         u(i+1,j,k)/(dxp( i ) * dxd(i-1+gt(1))) + &
                         u(i,j-1,k)/(dyp(j-1) * dyd(j-1+gt(2))) + &
                         u(i,j+1,k)/(dyp( j ) * dyd(j-1+gt(2))) + &
                         u(i,j,k-1)/(dzp(k-1) * dzd(k-1+gt(3))) + &
                         u(i,j,k+1)/(dzp( k ) * dzd(k-1+gt(3))) &
                       - f(i,j,k) )/r

            enddo
          enddo
        enddo

#ifdef _PARALLELIZE_Jacobi_
        !$OMP END DO

#endif
      end subroutine

      end module