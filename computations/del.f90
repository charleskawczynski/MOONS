      module del_mod
      ! Returns an n-derivative of the scalar field, f, 
      ! along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_DEL_)
      ! 
      ! Implementation:
      ! type(del) :: d
      ! call d%assign  (dfdh,f,g,n,dir,diffType,pad) --> dfdh = d/dh (f), 0 if not defined.
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
      !     diffType     = (1,2,3,4) derivative type, refer to diff for more details
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
      use stencils_mod
      implicit none

      private
      public :: del 

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
        generic,public :: assign => assignDel
        generic,public :: add => addDel
        generic,public :: subtract => subtractDel
        generic,public :: multiply => multiplyDel
        generic,public :: divide => divideDel
        procedure,private,nopass :: assignDel
        procedure,private,nopass :: addDel
        procedure,private,nopass :: subtractDel
        procedure,private,nopass :: multiplyDel
        procedure,private,nopass :: divideDel
      end type

      contains

      subroutine diff(dfdh,f,dh1,dh2,n,diffType,s,genType)
        implicit none
        real(cp),dimension(:),intent(in) :: f
        real(cp),dimension(:),intent(in) :: dh1,dh2
        integer,intent(in) :: n,diffType,s,genType
        real(cp),dimension(:),intent(inout) :: dfdh

        select case(genType)
        case (1) ! Assign
        select case (diffType)
        case (1); call collocatedAssign(dfdh,f,dh1,dh2,n,s,0)    ! Collocated CellCenter derivative
        case (2); call collocatedAssign(dfdh,f,dh2,dh1,n,s,1)    ! Collocated Node derivative
        case (3); call staggeredAssign(dfdh,f,dh1,s,0)           ! Cell centered staggered derivative
        case (4); call staggeredAssign(dfdh,f,dh2,s,1)           ! Node centered staggered derivative
        end select
        case (2) ! add
        select case (diffType)
        case (1); call collocatedAdd(dfdh,f,dh1,dh2,n,s,0)       ! Collocated CellCenter derivative
        case (2); call collocatedAdd(dfdh,f,dh2,dh1,n,s,1)       ! Collocated Node derivative
        case (3); call staggeredAdd(dfdh,f,dh1,s,0)              ! Cell centered staggered derivative
        case (4); call staggeredAdd(dfdh,f,dh2,s,1)              ! Node centered staggered derivative
        end select
        case (3) ! subtract
        select case (diffType)
        case (1); call collocatedSubtract(dfdh,f,dh1,dh2,n,s,0)  ! Collocated CellCenter derivative
        case (2); call collocatedSubtract(dfdh,f,dh2,dh1,n,s,1)  ! Collocated Node derivative
        case (3); call staggeredSubtract(dfdh,f,dh1,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredSubtract(dfdh,f,dh2,s,1)         ! Node centered staggered derivative
        end select
        case (4) ! multiply
        select case (diffType)
        case (1); call collocatedMultiply(dfdh,f,dh1,dh2,n,s,0)  ! Collocated CellCenter derivative
        case (2); call collocatedMultiply(dfdh,f,dh2,dh1,n,s,1)  ! Collocated Node derivative
        case (3); call staggeredMultiply(dfdh,f,dh1,s,0)         ! Cell centered staggered derivative
        case (4); call staggeredMultiply(dfdh,f,dh2,s,1)         ! Node centered staggered derivative
        end select
        case (5) ! divide
        select case (diffType)
        case (1); call collocatedDivide(dfdh,f,dh1,dh2,n,s,0)    ! Collocated CellCenter derivative
        case (2); call collocatedDivide(dfdh,f,dh2,dh1,n,s,1)    ! Collocated Node derivative
        case (3); call staggeredDivide(dfdh,f,dh1,s,0)           ! Cell centered staggered derivative
        case (4); call staggeredDivide(dfdh,f,dh2,s,1)           ! Node centered staggered derivative
        end select
        case default
          stop 'Error: genType must = 1,2,3,4 in diff.'
        end select
      end subroutine

      ! ********************** STAGGERED DERIVATIVES *************************

      subroutine staggeredAssign(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = staggered(f,dh,s,gt)
      end subroutine

      subroutine staggeredAdd(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh + staggered(f,dh,s,gt)
      end subroutine

      subroutine staggeredSubtract(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh - staggered(f,dh,s,gt)
      end subroutine

      subroutine staggeredMultiply(dfdh,f,dh,s,gt); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh * staggered(f,dh,s,gt)
      end subroutine

      subroutine staggeredDivide(dfdh,f,dh,s,gt); implicit none
        ! NOTE THAT THIS WILL RESULT IN DIVISION BY ZERO IF f
        ! LIVES ON NODES
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh / staggered(f,dh,s,gt)
      end subroutine

      ! ********************** COLLOCATED DERIVATIVES *************************

      subroutine collocatedAssign(dfdh,f,dhp,dhd,n,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = collocated(f,dhp,s)
        case (2); dfdh = collocated(f,dhp,dhd,s,gt)
        end select
      end subroutine

      subroutine collocatedAdd(dfdh,f,dhp,dhd,n,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh + collocated(f,dhp,s)
        case (2); dfdh = dfdh + collocated(f,dhp,dhd,s,gt)
        end select
      end subroutine

      subroutine collocatedSubtract(dfdh,f,dhp,dhd,n,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh - collocated(f,dhp,s)
        case (2); dfdh = dfdh - collocated(f,dhp,dhd,s,gt)
        end select
      end subroutine

      subroutine collocatedMultiply(dfdh,f,dhp,dhd,n,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh * collocated(f,dhp,s)
        case (2); dfdh = dfdh * collocated(f,dhp,dhd,s,gt)
        end select
      end subroutine

      subroutine collocatedDivide(dfdh,f,dhp,dhd,n,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh / collocated(f,dhp,s)
        case (2); dfdh = dfdh / collocated(f,dhp,dhd,s,gt)
        end select
      end subroutine

      subroutine delGen(dfdh,f,g,n,dir,pad,genType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType
        integer,dimension(3) :: s
        integer :: i,j,k,diffType

        s = shape(f)

#ifdef _DEBUG_DEL_
        call checkDimensions(shape(f),shape(dfdh),dir)
#endif

        diffType = getDiffType(s,shape(dfdh),g%c(dir)%sn,g%c(dir)%sc,dir)

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
              call diff(dfdh(:,j,k),f(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s(1),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
              call diff(dfdh(i,:,k),f(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s(2),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
              call diff(dfdh(i,j,:),f(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s(3),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen.'
        end select
      end subroutine


      ! ******************* OPERATOR TYPES ****************************

      subroutine assignDel(dfdh,f,g,n,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,1)
      end subroutine

      subroutine addDel(dfdh,f,g,n,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,2)
      end subroutine

      subroutine subtractDel(dfdh,f,g,n,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,3)
      end subroutine

      subroutine multiplyDel(dfdh,f,g,n,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,4)
      end subroutine

      subroutine divideDel(dfdh,f,g,n,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,5)
      end subroutine

      function getDiffType(sf,sdfdh,sn,sc,dir) result(diffType)
        implicit none
        integer,dimension(3),intent(in) :: sf,sdfdh
        integer,intent(in) :: sn,sc,dir
        integer :: diffType
            if ((sf(dir).eq.sdfdh(dir)).and.(sf(dir).eq.sc)) then
          diffType = 1 ! Collocated derivative (CC)
        elseif ((sf(dir).eq.sdfdh(dir)).and.(sf(dir).eq.sn)) then
          diffType = 2 ! Collocated derivative (N)
        elseif ((sf(dir).eq.(sdfdh(dir)+1))) then
          diffType = 3 ! Staggered derivative (CC->N)
        elseif ((sf(dir).eq.(sdfdh(dir)-1))) then
          diffType = 4 ! Staggered derivative (N->CC)
        else
          stop 'Error: diffType undetermined.'
        endif
      end function

#ifdef _DEBUG_DEL_
      subroutine checkDimensions(s1,s2,dir)
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
        else; stop 'Error: shape mismatch 7 in del'
        endif
      end subroutine
#endif


      end module