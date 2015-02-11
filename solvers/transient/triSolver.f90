      module myTriSolve_mod
      ! Tridiagonal matrix solver:
      ! call myTrisolve(x,d,a,b,c,d,dir[,circTF]) solves the 
      ! system of equations:
      !
      !        | b1  c2            |   | x1 |   | d1 |
      !        | a2  b2  c3        |   | x2 |   | d2 |
      !        |     .   .   .     | * | .  | = | .  |
      !        |         .   .  cn |   | .  |   | .  |
      !        |             an bn |   | xn |   | dn |
      !
      ! Inputs: 
      !  a - sub-diagonal          size = size(d) - 1
      !  b - the main diagonal     size = size(d)
      !  c - super-diagonal        size = size(d) - 1
      !  d - right part            size = size(d)
      !  dir      = (1,2,3)
      !  circTF   = .true./.false.
      ! 
      ! Output:
      !  x - the solution          size = size(d)
      ! 
      ! Notes:
      ! - Coefficients may be arrays OR constants
      ! - Type specifies whether the tridiagnoal matrix is regular or circTFulant
      !   where a cirulant matrix has periodicity:
      !
      !              | b  c  0  .  .  0  a  |
      !              | a  b  c           0  |
      !              | 0     .  .        .  |
      !              | .     .  .  .     .  |
      !              | .        .  .  .  0  |
      !              | 0           .  .  c  |
      !              | c  0  .  .  0  a  b  |
      ! 
      ! Where the eigenvalues are already known, so the solution can be computed
      ! using an ifft.
      !
      !
      ! NOTE myTrisolve_vec
      ! 
      ! The myTrisolve_vec subroutines always solve the systems along the first index
      ! (or x direction) for every location on the j,k location (or y-z plane).

      use constants_mod
      implicit none
      
      private
      public :: myTrisolve,myTrisolve_vec
       ! Fixes/improvements:
       ! - Make interface for constant coefficients (a,b,c)
       ! - include regular and circTFulant matrices
       ! - make matrix version for simpler interface

      interface myTrisolve_vec ! myTrisolve_vec(x,d,a,b,c,dir[,circTF])
        module procedure myTrisolveReg_vec3D
        module procedure myTrisolveRegConstCoeffs_vec3D
      end interface

      interface myTrisolve
        module procedure myTrisolveReg
        module procedure myTrisolveRegConstCoeffs
      end interface

      contains

      subroutine myTrisolveReg_vec3D(x,d,a,b,c,dir,circTF)
        implicit none
        real(dpn),dimension(:,:,:),intent(inout) :: x
        real(dpn),dimension(:,:,:),intent(in) :: d
        real(dpn),dimension(:),intent(in) :: a,b,c
        integer,intent(in) :: dir
        logical,intent(in),optional :: circTF
        integer,dimension(3) :: s
        integer :: i,j,k
        logical :: circTFDummy
        s = shape(d)
        if (present(circTF)) then; circTFDummy = circTF
        else; circTFDummy = .false.
        endif

        if (.not.circTFDummy) then
          if (dir.eq.1) then
            do j=1,s(2)
              do k=1,s(3)
                call myTrisolve(x(:,j,k),d(:,j,k),a,b,c)
              enddo
            enddo
          elseif (dir.eq.2) then
            do i=1,s(1)
              do k=1,s(3)
                call myTrisolve(x(i,:,k),d(i,:,k),a,b,c)
              enddo
            enddo
          elseif (dir.eq.2) then
            do i=1,s(1)
              do j=1,s(2)
                call myTrisolve(x(i,j,:),d(i,j,:),a,b,c)
              enddo
            enddo
          endif
        else
          write(*,*) 'Criculant TDMA not yet developed.';stop
        endif
      end subroutine

      subroutine myTrisolveRegConstCoeffs_vec3D(x,d,a,b,c,dir,circTF)
        implicit none
        real(dpn),dimension(:,:,:),intent(inout) :: x
        real(dpn),dimension(:,:,:),intent(in) :: d
        real(dpn),intent(in) :: a,b,c
        integer,intent(in) :: dir
        logical,intent(in),optional :: circTF
        integer,dimension(3) :: s
        integer :: i,j,k
        logical :: circTFDummy
        s = shape(d)
        if (present(circTF)) then; circTFDummy = circTF
        else; circTFDummy = .false.
        endif

        if (.not.circTFDummy) then
          if (dir.eq.1) then
            do j=1,s(2)
              do k=1,s(3)
                call myTrisolve(x(:,j,k),d(:,j,k),a,b,c)
              enddo
            enddo
          elseif (dir.eq.2) then
            do i=1,s(1)
              do k=1,s(3)
                call myTrisolve(x(i,:,k),d(i,:,k),a,b,c)
              enddo
            enddo
          elseif (dir.eq.2) then
            do i=1,s(1)
              do j=1,s(2)
                call myTrisolve(x(i,j,:),d(i,j,:),a,b,c)
              enddo
            enddo
          endif
        else
          write(*,*) 'Criculant TDMA not yet developed.';stop
        endif
      end subroutine

      !---------------------Low level Tridiagonal solver---------------------

      subroutine myTrisolveReg(x,d,a,b,c)
        implicit none
        real(dpn),dimension(:),intent(inout) :: x
        real(dpn),dimension(:),intent(in) :: d
        real(dpn),dimension(:),intent(in) :: a,b,c
        call triSolveOnline(x,d,a,b,c,size(d))
      end subroutine

      subroutine myTrisolveRegConstCoeffs(x,d,a,b,c)
        implicit none
        real(dpn),dimension(:),intent(inout) :: x
        real(dpn),dimension(:),intent(in) :: d
        real(dpn),intent(in) :: a,b,c
        call triSolveOnlineConstCoeffs(x,d,a,b,c,size(d))
      end subroutine

! REQUIRES FFT / IFFT.. Must develop those first.
!       function myTrisolvecircTFulant(a,b,c,d) result(x)
!         real(dpn),dimension(:),intent(in),target :: a,b,c,d
!         real(dpn),dimension(:) :: cp,dp,x
!         real(dpn),dimension() :: m
!         real(dpn) :: a,b,c
!         integer :: n,s
!         s = shape(d)

!          ! For a circTFulant matrix, get the eigenvalues
!          m = (/(j,j=0,s(1)-1)/)
!          do j = 1,s(1)-1
!            lam(j) = b + (a+c)*cos(2.0*PI*m(j)/dble(s(1))) - &
!            1i*(a-c)*sin(2.0*PI*m(j)/dble(s(1)))
!          enddo
!          xhat = (/(0.0,j=1,size(d))/)
         
!          ! Transform the rhs (fhat = V^-1*d).
!          fhat = fft(d)/dble(s(1))
         
!          ! Get xhat = Lam^-1*fhat.
!          do i=1,s(1)
!            do j = 1,s(2)
!                xhat(i,j) = fhat(i,j)/lam(i)
!            enddo
!          enddo
         
!          ! Get x = V*xhat by transforming back.
!          x = real(dble(s(1))*ifft(xhat))
!        end function


      subroutine triSolveOnline(x,d,a,b,c,n)
        implicit none
        integer,intent(in) :: n
        real(dpn),dimension(n),intent(inout) :: x
        real(dpn),dimension(n),intent(in) :: a,b,c,d
        real(dpn),dimension(n) :: cp,dp
        real(dpn) :: m
        integer i
        cp(1) = c(1)/b(1)
        dp(1) = d(1)/b(1)
        do i = 2,n
          m = b(i)-cp(i-1)*a(i)
          cp(i) = c(i)/m
          dp(i) = (d(i)-dp(i-1)*a(i))/m
        enddo
        x(n) = dp(n)
        do i = n-1, 1, -1
          x(i) = dp(i)-cp(i)*x(i+1)
        end do
      end subroutine

      subroutine triSolveOnlineConstCoeffs(x,d,a,b,c,n)
        implicit none
        integer,intent(in) :: n
        real(dpn),dimension(n),intent(inout) :: x
        real(dpn),dimension(n),intent(in) :: d
        real(dpn),intent(in) :: a,b,c
        real(dpn),dimension(n) :: cp,dp
        real(dpn) :: m
        integer i
        cp(1) = c/b
        dp(1) = d(1)/b
        do i = 2,n
          m = b-cp(i-1)*a
          cp(i) = c/m
          dp(i) = (d(i)-dp(i-1)*a)/m
        enddo
        x(n) = dp(n)
        do i = n-1, 1, -1
          x(i) = dp(i)-cp(i)*x(i+1)
        end do
      end subroutine

      end module
