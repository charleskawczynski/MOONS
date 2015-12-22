      module ops_del_mod
      ! Returns an n-derivative of the scalar field, f, 
      ! along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_DEL_)
      ! 
      ! Implementation:
      !      type(del) :: d
      !      type(SF) :: f,dfdh
      !      type(mesh) :: m
      !      integer :: n,dir,pad
      !      call d%assign  (dfdh,f,m,n,dir,pad) --> dfdh = d/dh (f), 0 if not defined.
      !      call d%add     (dfdh,f,m,n,dir,pad) --> dfdh = dfdh + d/dh (f)
      !      call d%subtract(dfdh,f,m,n,dir,pad) --> dfdh = dfdh - d/dh (f)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     m            = mesh containing grids
      !     n            = nth derivative (n=1,2 supported)
      !     dir          = direction along which to take the derivative (1,2,3)
      !     pad          = (1,0) = (exclude,include) boundary calc along derivative direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at del for implementation details  
      !                    |0000000|     |-------|
      !
      ! CharlieKawczynski@gmail.com
      ! 5/15/2014

      use grid_mod
      use mesh_mod
      use SF_mod
      use triDiag_mod
      use stencils_explicit_mod
      use apply_stitches_mod
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

      interface delGen;    module procedure delGen_given_g; end interface

      type del
        contains
        generic,public :: assign => assignDel
        generic,public :: add => addDel
        generic,public :: subtract => subtractDel
        procedure,private,nopass :: assignDel
        procedure,private,nopass :: addDel
        procedure,private,nopass :: subtractDel
      end type

      contains

      ! *********************************************************
      ! *********************** LOW LEVEL ***********************
      ! *********************************************************

      subroutine delGen_T(operator,dfdh,f,T,dir,pad,genType,s,sdfdh)
        implicit none
        external :: operator
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad
        integer,dimension(3),intent(in) :: s,sdfdh
        integer :: i,j,k
#ifdef _DEBUG_DEL_
        call checkSideDimensions(s,sdfdh,dir)
#endif
        if (genType.eq.1) dfdh = 0.0_cp
        select case (dir)
        case (1); 
        !$OMP PARALLEL DO
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
          call operator(dfdh(:,j,k),f(i,j,k),T,s(1),sdfdh(1))
        enddo; enddo
        !$OMP END PARALLEL DO
        case (2); 
        !$OMP PARALLEL DO
        do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
          call operator(dfdh(i,:,k),f(i,j,k),T,s(2),sdfdh(2))
        enddo; enddo
        !$OMP END PARALLEL DO
        case (3); 
        !$OMP PARALLEL DO
        do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          call operator(dfdh(i,j,:),f(i,j,k),T,s(3),sdfdh(3))
        enddo; enddo
        !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen_T in ops_del.f90.'
        end select
      end subroutine


      ! *********************************************************
      ! *********************** MED LEVEL ***********************
      ! *********************************************************

      public :: stag_CC2N_add
      public :: stag_CC2N_ass
      public :: stag_CC2N_sub

      public :: collocated_centered
      public :: collocated_1_sided_CC
      public :: collocated_1_sided_N

      subroutine delGen_RF_given_g(P,dfdh,f,g,n,dir,pad,genType,s,sdfdh)
        implicit none
        type(PEMDAS),intent(in) :: P
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType,pad1,pad2
        integer,dimension(3),intent(in) :: s,sdfdh
        logical,intent(in) :: CC
        type(triDiag) :: T
        integer :: diffType
        diffType = getDiffType(s,sdfdh,g%c(dir)%sn,g%c(dir)%sc,dir)
        select case (genType)
        case (1)
          select case (diffType)
          case (1); select case (n)
                    case (1); call delGen_T(P%collocated_centered,dfdh,f,g%c(dir)%colCC,dir,pad,s,sdfdh)
                    case (2); call delGen_T(P%collocated_centered,dfdh,f,g%c(dir)%LapCC,dir,pad,s,sdfdh)
                    end select
          case (2); select case (n)
                    case (1); call delGen_T(P%collocated_centered,dfdh,f,g%c(dir)%colN,dir,pad,s,sdfdh)
                    case (2); call delGen_T(P%collocated_centered,dfdh,f,g%c(dir)%LapN,dir,pad,s,sdfdh)
                    end select
          case (3); call delGen_T(P%stag_CC2N,dfdh,f,g%c(dir)%stagCC2N,dir,pad,s,sdfdh)
          case (4); call delGen_T(P%stag_N2CC,dfdh,f,g%c(dir)%stagN2CC,dir,pad,s,sdfdh)
          end select


        select case (diffType)
        case (1); select case (n)
                  case (1); call init(T,g%c(dir)%colCC); P=>colCC
                  case (2); call init(T,g%c(dir)%LapCC); P=>LapCC
                  end select
        case (2); select case (n)
                  case (1); call init(T,g%c(dir)%colN); P=>colN
                  case (2); call init(T,g%c(dir)%LapN); P=>LapN
                  end select
        case (3); call init(T,g%c(dir)%stagCC2N); P=>stagCC2N
        case (4); call init(T,g%c(dir)%stagN2CC); P=>stagN2CC
        end select

        select case (genType)
        case (1); call delGen_T(P%assign  ,dfdh,f,T,dir,pad,s,sdfdh)
        case (2); call delGen_T(P%add     ,dfdh,f,T,dir,pad,s,sdfdh)
        case (3); call delGen_T(P%subtract,dfdh,f,T,dir,pad,s,sdfdh)
        end select
        call delete(T)
      end subroutine

      subroutine delGen_given_g(dfdh,f,m,n,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad,genType
        integer :: i
        do i=1,m%s
          call delGen_RF_given_g(dfdh%RF(i)%f,f%RF(i)%f,m%g(i),&
            n,dir,pad,genType,f%RF(i)%s,dfdh%RF(i)%s,CC_along(f,dir))
        enddo
      end subroutine

      ! *********************************************************
      ! *********************** HIGH LEVEL **********************
      ! *********************************************************

      subroutine assignDel(dfdh,f,m,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,m,n,dir,pad,1)
      end subroutine

      subroutine addDel(dfdh,f,m,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,m,n,dir,pad,2)
      end subroutine

      subroutine subtractDel(dfdh,f,m,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad
        type(subtract) :: stencil
        call delGen(dfdh,f,m,n,dir,pad,stencil)
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
          stop 'Error: diffType undetermined in ops_del.f90.'
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
        case (1); if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 1 in checkSideDimensions in ops_del.f90'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 2 in checkSideDimensions in ops_del.f90'
        case (2); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 3 in checkSideDimensions in ops_del.f90'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 4 in checkSideDimensions in ops_del.f90'
        case (3); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 5 in checkSideDimensions in ops_del.f90'
                  if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 6 in checkSideDimensions in ops_del.f90'
        case default
        stop 'Error: dir must = 1,2,3 in ops_del.f90'
        end select
      end subroutine
#endif

      end module