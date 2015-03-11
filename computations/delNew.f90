      module delNew_mod
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
      public :: delNew
      public :: assign
      ! public :: assign,add,subtract,multiply,divide

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type delNew
        contains
        procedure,nopass :: assign
        ! procedure,nopass :: assign,add,subtract,multiply,divide
      end type

      contains

      subroutine assign(dfdh,f,alpha,g,n,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        real(cp),dimension(:,:,:),intent(in),optional :: alpha
        ! type(grid),intent(in) :: g
        type(grid),intent(in),target :: g
        integer,intent(in) :: n,dir,pad
        integer,dimension(3) :: s
        integer :: i,j,k,diffType,gt
        ! real(cp),dimension(:),allocatable :: dh1,dh2
        real(cp),dimension(:), pointer :: dh1,dh2

        s = shape(f)
#ifdef _DEBUG_DEL_
        call checkDimensions(shape(f),shape(dfdh),dir)
#endif

        diffType = getDiffType(shape(f),shape(dfdh),g%c(dir)%sn,g%c(dir)%sc,dir)

        ! select case (diffType)
        ! case (1); allocate(dh1(g%c(dir)%sc-1)); allocate(dh2(g%c(dir)%sn-1))
        ! dh1 = g%c(dir)%dhc; dh2 = g%c(dir)%dhn; gt = 0
        ! case (2); allocate(dh1(g%c(dir)%sn-1)); allocate(dh2(g%c(dir)%sc-1))
        ! dh1 = g%c(dir)%dhn; dh2 = g%c(dir)%dhc; gt = 1
        ! case (3); allocate(dh1(g%c(dir)%sc-1)); allocate(dh2(g%c(dir)%sn-1))
        ! dh1 = g%c(dir)%dhc; dh2 = g%c(dir)%dhn; gt = 0
        ! case (4); allocate(dh1(g%c(dir)%sn-1)); allocate(dh2(g%c(dir)%sc-1))
        ! dh1 = g%c(dir)%dhn; dh2 = g%c(dir)%dhc; gt = 1
        ! case default
        ! stop 'Error: diffType must = 1,2,3,4 in assign'
        ! end select

        select case (diffType)
        case (1); dh1 => g%c(dir)%dhc; dh2 => g%c(dir)%dhn; gt = 0
        case (2); dh1 => g%c(dir)%dhn; dh2 => g%c(dir)%dhc; gt = 1
        case (3); dh1 => g%c(dir)%dhc; dh2 => g%c(dir)%dhn; gt = 0
        case (4); dh1 => g%c(dir)%dhn; dh2 => g%c(dir)%dhc; gt = 1
        case default
        stop 'Error: diffType must = 1,2,3,4 in assign'
        end select

        if ((n.eq.1).and.((diffType.eq.1).or.(diffType.eq.2))) then     ! n = 1, Collocated
          select case (dir)
          case (1)
            !$OMP PARALLEL DO
            do k=1+pad,s(3)-pad;do j=1+pad,s(2)-pad
              dfdh(:,j,k) = collocated(f(:,j,k),dh1,s(1))
            enddo;enddo
            !$OMP END PARALLEL DO
          case (2)
            !$OMP PARALLEL DO
            do k=1+pad,s(3)-pad;do i=1+pad,s(1)-pad
              dfdh(i,:,k) = collocated(f(i,:,k),dh1,s(2))
            enddo;enddo
            !$OMP END PARALLEL DO
          case (3)
            !$OMP PARALLEL DO
            do j=1+pad,s(2)-pad;do i=1+pad,s(1)-pad
              dfdh(i,j,:) = collocated(f(i,j,:),dh1,s(3))
            enddo;enddo
            !$OMP END PARALLEL DO
          case default
          stop 'Error: dir must = 1,2,3 in delGen.'
          end select
        elseif ((n.eq.1).and.((diffType.eq.3).or.(diffType.eq.4))) then ! n = 1, Staggered
          select case (dir)
          case (1)
            !$OMP PARALLEL DO
            do k=1+pad,s(3)-pad;do j=1+pad,s(2)-pad
              dfdh(:,j,k) = staggered(f(:,j,k),dh1,s(1),gt)
            enddo;enddo
            !$OMP END PARALLEL DO
          case (2)
            !$OMP PARALLEL DO
            do k=1+pad,s(3)-pad;do i=1+pad,s(1)-pad
              dfdh(i,:,k) = staggered(f(i,:,k),dh1,s(2),gt)
            enddo;enddo
            !$OMP END PARALLEL DO
          case (3)
            !$OMP PARALLEL DO
            do j=1+pad,s(2)-pad;do i=1+pad,s(1)-pad
              dfdh(i,j,:) = staggered(f(i,j,:),dh1,s(3),gt)
            enddo;enddo
            !$OMP END PARALLEL DO
          case default
          stop 'Error: dir must = 1,2,3 in delGen.'
          end select
        elseif ((n.eq.2).and.(.not.present(alpha))) then ! n = 2, Collocated, uniform alpha
          select case (dir)
          case (1)
            !$OMP PARALLEL DO
            do k=1+pad,s(3)-pad;do j=1+pad,s(2)-pad
              dfdh(:,j,k) = collocated(f(:,j,k),dh1,dh2,s(1),gt)
            enddo;enddo
            !$OMP END PARALLEL DO
          case (2)
            !$OMP PARALLEL DO
            do k=1+pad,s(3)-pad;do i=1+pad,s(1)-pad
              dfdh(i,:,k) = collocated(f(i,:,k),dh1,dh2,s(2),gt)
            enddo;enddo
            !$OMP END PARALLEL DO
          case (3)
            !$OMP PARALLEL DO
            do j=1+pad,s(2)-pad;do i=1+pad,s(1)-pad
              dfdh(i,j,:) = collocated(f(i,j,:),dh1,dh2,s(3),gt)
            enddo;enddo
            !$OMP END PARALLEL DO
          case default
          stop 'Error: dir must = 1,2,3 in delGen.'
          end select
        elseif ((n.eq.2).and.(present(alpha))) then      ! n = 2, Collocated, variable alpha
          ! select case (dir)
          ! case (1)
          !   !$OMP PARALLEL DO
          !   do k=1+pad,s(3)-pad;do j=1+pad,s(2)-pad
          !     dfdh(:,j,k) = collocated(f(:,j,k),alpha(:,j,k),dh1,dh2,s(1),gt)
          !   enddo;enddo
          !   !$OMP END PARALLEL DO
          ! case (2)
          !   !$OMP PARALLEL DO
          !   do k=1+pad,s(3)-pad;do i=1+pad,s(1)-pad
          !     dfdh(i,:,k) = collocated(f(i,:,k),alpha(i,:,k),dh1,dh2,s(2),gt)
          !   enddo;enddo
          !   !$OMP END PARALLEL DO
          ! case (3)
          !   !$OMP PARALLEL DO
          !   do j=1+pad,s(2)-pad;do i=1+pad,s(1)-pad
          !     dfdh(i,j,:) = collocated(f(i,j,:),alpha(i,j,:),dh1,dh2,s(3),gt)
          !   enddo;enddo
          !   !$OMP END PARALLEL DO
          ! case default
          ! stop 'Error: dir must = 1,2,3 in delGen.'
          ! end select
        else
        stop 'Error: no cases were correctly selected'
        endif

        ! deallocate(dh1,dh2)
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
        else
          stop 'Error: shape mismatch 7 in del'
        endif
      end subroutine
#endif


      end module