      module del_mod
      ! Returns an n-derivative of the scalar field, f, 
      ! along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_DEL_)
      ! 
      ! Implementation:
      ! type(del) :: d
      ! call d%assign  (dfdh,f,g,n,dir,pad) --> dfdh = d/dh (f), 0 if not defined.
      ! call d%add     (dfdh,f,g,n,dir,pad) --> dfdh = dfdh + d/dh (f)
      ! call d%subtract(dfdh,f,g,n,dir,pad) --> dfdh = dfdh - d/dh (f)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     g            = grid (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     n            = nth derivative (n=1,2 supported)
      !     dir          = direction along which to take the derivative (1,2,3)
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
      use SF_mod
      use triDiag_mod
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

      interface delGen;    module procedure delGen_temp_T;  end interface
      interface delGen;    module procedure delGen_given_T; end interface


      type del
        contains
        generic,public :: assign => assignDel
        generic,public :: add => addDel
        generic,public :: subtract => subtractDel
        procedure,private,nopass :: assignDel
        procedure,private,nopass :: addDel
        procedure,private,nopass :: subtractDel

        generic,public :: assign_T => assignDel_T
        generic,public :: add_T => addDel_T
        generic,public :: subtract_T => subtractDel_T
        procedure,private,nopass :: assignDel_T
        procedure,private,nopass :: addDel_T
        procedure,private,nopass :: subtractDel_T
      end type

      contains

      ! *********************************************************
      ! *********************** LOW LEVEL ***********************
      ! *********************************************************

      subroutine delGen_T(dfdh,f,T,dir,pad,genType,diffType,gt,s,sdfdh)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad,genType,diffType,gt
        integer,dimension(3),intent(in) :: s,sdfdh
        integer :: i,j,k
#ifdef _DEBUG_DEL_
        call checkSideDimensions(s,sdfdh,dir)
#endif
        select case (dir)
        case (1); 
        !$OMP PARALLEL DO
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
          call diff(dfdh(:,j,k),f(:,j,k),T,diffType,s(1),sdfdh(1),genType,gt)
        enddo; enddo
        !$OMP END PARALLEL DO
        case (2); 
        !$OMP PARALLEL DO
        do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
          call diff(dfdh(i,:,k),f(i,:,k),T,diffType,s(2),sdfdh(2),genType,gt)
        enddo; enddo
        !$OMP END PARALLEL DO
        case (3); 
        !$OMP PARALLEL DO
        do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          call diff(dfdh(i,j,:),f(i,j,:),T,diffType,s(3),sdfdh(3),genType,gt)
        enddo; enddo
        !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen_T in del.f90.'
        end select
        if (genType.eq.1) then
          if (pad.gt.0) then
            select case (dir)
          case (1); dfdh(:,:,1) = 0.0_cp; dfdh(:,:,s(3)) = 0.0_cp
                    dfdh(:,1,:) = 0.0_cp; dfdh(:,s(2),:) = 0.0_cp
          case (2); dfdh(1,:,:) = 0.0_cp; dfdh(s(1),:,:) = 0.0_cp
                    dfdh(:,:,1) = 0.0_cp; dfdh(:,:,s(3)) = 0.0_cp
          case (3); dfdh(1,:,:) = 0.0_cp; dfdh(s(1),:,:) = 0.0_cp
                    dfdh(:,1,:) = 0.0_cp; dfdh(:,s(2),:) = 0.0_cp
          case default
            stop 'Error: dir must = 1,2,3 in delGen_T in del.f90.'
            end select
          endif
        endif
      end subroutine

      subroutine diff(dfdh,f,T,diffType,s,sdfdh,genType,gt)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: diffType,s,sdfdh,genType,gt
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        select case(genType)
        case (1) ;select case (diffType) ! Assign
                  case (1); dfdh = collocated(f,T,s,gt)              ! Collocated derivative
                  case (2); dfdh = staggered(f,T,s,sdfdh,gt)         ! Staggered derivative
                  end select
        case (2) ;select case (diffType) ! add
                  case (1); dfdh = dfdh + collocated(f,T,s,gt)       ! Collocated derivative
                  case (2); dfdh = dfdh + staggered(f,T,s,sdfdh,gt)  ! Staggered derivative
                  end select
        case (3) ;select case (diffType) ! subtract
                  case (1); dfdh = dfdh - collocated(f,T,s,gt)       ! Collocated derivative
                  case (2); dfdh = dfdh - staggered(f,T,s,sdfdh,gt)  ! Staggered derivative
                  end select
        case default
          stop 'Error: genType must = 1,2,3 in diff in del.f90'
        end select
      end subroutine

      ! *********************************************************
      ! *********************** MED LEVEL ***********************
      ! *********************************************************

      subroutine delGen_temp_T(dfdh,f,g,n,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType
        integer :: i
        do i=1,dfdh%s
          call delGen_RF_given_g(dfdh%RF(i)%f,f%RF(i)%f,g,n,dir,pad,genType,f%RF(i)%s,dfdh%RF(i)%s)
        enddo
      end subroutine

      subroutine delGen_given_T(dfdh,f,g,T,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad,genType
        integer :: i
        do i=1,dfdh%s
          call delGen_RF_given_T(dfdh%RF(i)%f,f%RF(i)%f,g,T,dir,pad,genType,f%RF(i)%s,dfdh%RF(i)%s)
        enddo
      end subroutine

      subroutine delGen_RF_given_g(dfdh,f,g,n,dir,pad,genType,s,sdfdh)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType
        integer,dimension(3),intent(in) :: s,sdfdh
        integer :: diffType,gt
        diffType = getDiffType(s,sdfdh,g%c(dir)%sn,g%c(dir)%sc,dir)
        select case (diffType)
        case (1); gt = 1
                  select case (n)
                  case (1); call delGen_T(dfdh,f,g%c(dir)%colCC,dir,pad,genType,1,gt,s,sdfdh)
                  case (2); call delGen_T(dfdh,f,g%c(dir)%LapCC,dir,pad,genType,1,gt,s,sdfdh)
                  end select
        case (2); gt = 0
                  select case (n)
                  case (1); call delGen_T(dfdh,f,g%c(dir)%colN,dir,pad,genType,1,gt,s,sdfdh)
                  case (2); call delGen_T(dfdh,f,g%c(dir)%LapN,dir,pad,genType,1,gt,s,sdfdh)
                  end select
        case (3); gt = 1; call delGen_T(dfdh,f,g%c(dir)%stagCC2N,dir,pad,genType,2,gt,s,sdfdh)
        case (4); gt = 0; call delGen_T(dfdh,f,g%c(dir)%stagN2CC,dir,pad,genType,2,gt,s,sdfdh)
        end select
      end subroutine

      subroutine delGen_RF_given_T(dfdh,f,g,T,dir,pad,genType,s,sdfdh)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad,genType
        integer,dimension(3),intent(in) :: s,sdfdh
        integer :: diffType,gt
        diffType = getDiffType(s,sdfdh,g%c(dir)%sn,g%c(dir)%sc,dir)
        select case (diffType)
        case (1,3); gt = 1
        case (2,4); gt = 0
        end select
        call delGen_T(dfdh,f,T,dir,pad,genType,diffType,gt,s,sdfdh)
      end subroutine


      ! *********************************************************
      ! ********************** HIGH LEVEL ***********************
      ! *********************************************************

      subroutine assignDel(dfdh,f,g,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,1)
      end subroutine

      subroutine addDel(dfdh,f,g,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,2)
      end subroutine

      subroutine subtractDel(dfdh,f,g,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,3)
      end subroutine

      subroutine assignDel_T(dfdh,f,g,T,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,g,T,dir,pad,1)
      end subroutine

      subroutine addDel_T(dfdh,f,g,T,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,g,T,dir,pad,2)
      end subroutine

      subroutine subtractDel_T(dfdh,f,g,T,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,g,T,dir,pad,3)
      end subroutine

      ! *********************************************************
      ! *********************** AUX / DEBUG *********************
      ! *********************************************************

      function getDiffType(sf,sdfdh,sn,sc,dir) result(diffType)
        implicit none
        integer,dimension(3),intent(in) :: sf,sdfdh
        integer,intent(in) :: sn,sc,dir
        integer :: diffType
            if ((sf(dir).eq.sc).and.(sdfdh(dir).eq.sc)) then
          diffType = 1 ! Collocated derivative (CC)
        elseif ((sf(dir).eq.sn).and.(sdfdh(dir).eq.sn)) then
          diffType = 2 ! Collocated derivative (N)
        elseif ((sf(dir).eq.sc).and.(sdfdh(dir).eq.sn)) then
          diffType = 3 ! Staggered derivative (CC->N)
        elseif ((sf(dir).eq.sn).and.(sdfdh(dir).eq.sc)) then
          diffType = 4 ! Staggered derivative (N->CC)
        else
          write(*,*) 'sf = ',sf
          write(*,*) 'sdfdh = ',sdfdh
          write(*,*) 'sn = ',sn
          write(*,*) 'sc = ',sc
          write(*,*) 'dir = ',dir
          stop 'Error: diffType undetermined in del.f90.'
        endif
      end function

#ifdef _DEBUG_DEL_
      subroutine checkSideDimensions(s1,s2,dir)
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
        stop 'Error: dir must = 1,2,3 in del.f90'
        end select
      end subroutine
#endif

      end module