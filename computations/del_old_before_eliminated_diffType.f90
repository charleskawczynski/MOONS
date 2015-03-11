      module del_mod
      ! Returns an nth-derivative of the scalar field, f, 
      ! along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_DEL_)
      ! 
      ! Implementation:
      ! type(del) :: d
      ! call d%add     (dfdh,f,g,n,dir,diffType,pad) --> dfdh = dfdh + d/dh (f)
      ! call d%subtract(dfdh,f,g,n,dir,diffType,pad) --> dfdh = dfdh - d/dh (f)
      ! call d%multiply(dfdh,f,g,n,dir,diffType,pad) --> dfdh = dfdh * d/dh (f)
      ! call d%divide  (dfdh,f,g,n,dir,diffType,pad) --> dfdh = dfdh / d/dh (f)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     g            = grid (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     n            = nth derivative (n=1,2 supported)
      !     dir          = direction along which to take the derivative (1,2,3)
      !     diffType     = (1,2,3,4) derivative type, refer to myDiff for more details
      !     pad          = (1,0) = (exclude,include) boundary calc along derivative direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at del for implementation details  
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 5/15/2014

      use grid_mod
      implicit none

      private
      public :: del 
      public :: assign,add,subtract,multiply,divide

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type del
        contains
        procedure,nopass :: assign
        procedure,nopass :: add
        procedure,nopass :: subtract
        procedure,nopass :: multiply
        procedure,nopass :: divide
      end type

      contains

      subroutine myDiff(dfdh,f,dhc,dhn,n,diffType,s,genType)
        implicit none
        real(cp),dimension(:),intent(in) :: f
        real(cp),dimension(:),intent(in) :: dhc,dhn
        integer,intent(in) :: n,diffType,s,genType
        real(cp),dimension(:),intent(inout) :: dfdh

        select case(genType)
        case (1) ! Assign
        select case (diffType)
        case (1); call collocatedAssign(dfdh,f,dhc,dhn,n,s)    ! Collocated CellCenter derivative
        case (2); call collocatedAssign(dfdh,f,dhn,dhc,n,s)    ! Collocated Node derivative
        case (3); call staggeredAssign(dfdh,f,dhc,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredAssign(dfdh,f,dhn,s,1)         ! Node centered staggered derivative
        end select
        case (2) ! add
        select case (diffType)
        case (1); call collocatedAdd(dfdh,f,dhc,dhn,n,s)    ! Collocated CellCenter derivative
        case (2); call collocatedAdd(dfdh,f,dhn,dhc,n,s)    ! Collocated Node derivative
        case (3); call staggeredAdd(dfdh,f,dhc,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredAdd(dfdh,f,dhn,s,1)         ! Node centered staggered derivative
        end select
        case (3) ! subtract
        select case (diffType)
        case (1); call collocatedSubtract(dfdh,f,dhc,dhn,n,s)    ! Collocated CellCenter derivative
        case (2); call collocatedSubtract(dfdh,f,dhn,dhc,n,s)    ! Collocated Node derivative
        case (3); call staggeredSubtract(dfdh,f,dhc,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredSubtract(dfdh,f,dhn,s,1)         ! Node centered staggered derivative
        end select
        case (4) ! multiply
        select case (diffType)
        case (1); call collocatedMultiply(dfdh,f,dhc,dhn,n,s)    ! Collocated CellCenter derivative
        case (2); call collocatedMultiply(dfdh,f,dhn,dhc,n,s)    ! Collocated Node derivative
        case (3); call staggeredMultiply(dfdh,f,dhc,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredMultiply(dfdh,f,dhn,s,1)         ! Node centered staggered derivative
        end select
        case (5) ! divide
        select case (diffType)
        case (1); call collocatedDivide(dfdh,f,dhc,dhn,n,s)    ! Collocated CellCenter derivative
        case (2); call collocatedDivide(dfdh,f,dhn,dhc,n,s)    ! Collocated Node derivative
        case (3); call staggeredDivide(dfdh,f,dhc,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredDivide(dfdh,f,dhn,s,1)         ! Node centered staggered derivative
        end select
        case default
          stop 'Error: genType must = 1,2,3,4 in diff.'
        end select
      end subroutine

      ! ********************** STAGGERED DERIVATIVES *************************

      function staggeredFunc(f,dh,s,gt) result(dfdh)
        ! f    lives on CC
        ! dfdh lives on N
        ! The entire array dfdh is defined here
        ! 
        ! gt = 0 :  f-CC | dfdh-N
        !      1 :  f-N  | dfdh-CC
        ! 
        implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: dfdh
        integer :: i
        ! If f \in N, then dfdh \in CC
        dfdh(1) = real(0.0,cp); dfdh(s-1+2*gt) = real(0.0,cp)
        dfdh(1+gt:s-1+gt) = (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end function

      subroutine staggeredAssign(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredAdd(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh + staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredSubtract(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh - staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredMultiply(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh * staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredDivide(dfdh,f,dh,s,gt); implicit none
        ! NOTE THAT THIS WILL RESULT IN DIVISION BY ZERO IF f
        ! LIVES ON NODES
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh / staggeredFunc(f,dh,s,gt)
      end subroutine

      ! ********************** COLLOCATED DERIVATIVES *************************

      function collocatedFunc(f,dhp,dhd,n,s) result(dfdh)
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: n,s
        integer :: i,j,k
        real(cp) :: alpha,beta

        select case (n)
        case (1)
          ! Interior
          k = -1; j = 1
          do i=2,s-1
            alpha = -dhp(i-1); beta = dhp(i)
            dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                      f(i+k)*beta/alpha + &
                      f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo

          ! Forward difference
          i = 1; k = 1; j = 2
          alpha = dhp(1); beta = dhp(1) + dhp(2)
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)

          ! Backward difference
          i = s; k = -1; j = -2
          alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2))
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)

        case (2)
          ! Interior
          dfdh(1) = real(0.0,cp); dfdh(s) = real(0.0,cp)
          do i=2,s-1
            dfdh(i) = ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
          enddo
        case default
        stop 'Error: n must = 1,2 in collocatedFunc.'
        end select
      end function

      function collocatedFunc2(f,dhp,dhd,s) result(dfdh)
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: s
        integer :: i
        dfdh(1) = real(0.0,cp); dfdh(s) = real(0.0,cp)
        do i=2,s-1
          dfdh(i) = ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
        enddo
      end function

      function collocatedFunc2Var(f,k,dhp,dhd,s) result(dfdh)
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f,k
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: s
        integer :: i
        dfdh(1) = real(0.0,cp); dfdh(s) = real(0.0,cp)
        do i=2,s-1
          dfdh(i) = ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
        enddo
      end function

      function collocatedFunc1(f,dhp,dhd,s) result(dfdh)
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: s
        integer :: i,j,k
        real(cp) :: alpha,beta
        ! Interior
        k = -1; j = 1
        do i=2,s-1
          alpha = -dhp(i-1); beta = dhp(i)
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)
        enddo
        ! Forward difference
        i = 1; k = 1; j = 2
        alpha = dhp(1); beta = dhp(1) + dhp(2)
        dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
        ! Backward difference
        i = s; k = -1; j = -2
        alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2))
        dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
      end function

      subroutine collocatedAssign(dfdh,f,dhp,dhd,n,s); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        dfdh = collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedAdd(dfdh,f,dhp,dhd,n,s); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        dfdh = dfdh + collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedSubtract(dfdh,f,dhp,dhd,n,s); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        dfdh = dfdh - collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedMultiply(dfdh,f,dhp,dhd,n,s); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        dfdh = dfdh * collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedDivide(dfdh,f,dhp,dhd,n,s); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        dfdh = dfdh / collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine delGen(dfdh,f,g,nth,dir,diffType,pad,genType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad,genType
        integer,dimension(3) :: s
        integer :: i,j,k
        s = shape(f)
#ifdef _DEBUG_DEL_
        call checkOrthogonalDimensions(shape(f),shape(dfdh),dir)
#endif
        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
              call myDiff(dfdh(:,j,k),f(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(1),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
              call myDiff(dfdh(i,:,k),f(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(2),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
              call myDiff(dfdh(i,j,:),f(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(3),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen.'
        end select
      end subroutine


      ! ******************* OPERATOR TYPES ****************************

      subroutine assign(dfdh,f,g,nth,dir,diffType,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        call delGen(dfdh,f,g,nth,dir,diffType,pad,1)
      end subroutine

      subroutine add(dfdh,f,g,nth,dir,diffType,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        call delGen(dfdh,f,g,nth,dir,diffType,pad,2)
      end subroutine

      subroutine subtract(dfdh,f,g,nth,dir,diffType,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        call delGen(dfdh,f,g,nth,dir,diffType,pad,3)
      end subroutine

      subroutine multiply(dfdh,f,g,nth,dir,diffType,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        call delGen(dfdh,f,g,nth,dir,diffType,pad,4)
      end subroutine

      subroutine divide(dfdh,f,g,nth,dir,diffType,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        call delGen(dfdh,f,g,nth,dir,diffType,pad,5)
      end subroutine

#ifdef _DEBUG_DEL_
      subroutine checkOrthogonalDimensions(s1,s2,dir)
        ! This routine makes sure that the shapes s1 and s2 
        ! are equal for orthogonal directions to dir, which
        ! must be the case for all derivatives in del.
        implicit none
        integer,dimension(3),intent(in) :: s1,s2
        integer,intent(in) :: dir
        select case (dir)
        case (1); if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 1 in del'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 2 in del'
        case (2); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 3 in del'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 4 in del'
        case (3); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 5 in del'
                  if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 6 in del'
        case default
        stop 'Error: dir must = 1,2,3'
        end select
        if (s1(dir).eq.s2(dir)) then         ! Ok (collocated)
        elseif (s1(dir).eq.(s2(dir)+1)) then ! Ok (N and CC)
        elseif ((s1(dir)+1).eq.s2(dir)) then ! Ok (CC and N)
        else
          stop 'Error: shape mismatch 7 in del'
        endif
      end subroutine
#endif


      end module