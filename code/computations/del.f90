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
      ! call d%multiply(dfdh,f,g,n,dir,pad) --> dfdh = dfdh * d/dh (f)
      ! call d%divide  (dfdh,f,g,n,dir,pad) --> dfdh = dfdh / d/dh (f)
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
      use del_old_mod
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

      subroutine diff(dfdh,f,g,dir,n,diffType,s,sdfdh,genType)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,n,diffType,s,sdfdh,genType
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        ! real(cp),dimension(sdfdh) :: dfdh_old

        select case(genType)
        case (1) ! Assign
        select case (diffType)
        case (1); call collocatedAssign(dfdh,f,g%c(dir)%colCC,g%c(dir)%lapCC,n,s,1)   ! Collocated CellCenter derivative
        case (2); call collocatedAssign(dfdh,f,g%c(dir)%colN,g%c(dir)%lapN,n,s,0)     ! Collocated Node derivative
        case (3); call staggeredAssign(dfdh,f,g%c(dir)%stagCC2N,s,sdfdh,1)            ! Staggered derivative (CC->N)
        case (4); call staggeredAssign(dfdh,f,g%c(dir)%stagN2CC,s,sdfdh,0)            ! Staggered derivative (N->CC)
        end select
        case (2) ! add
        select case (diffType)
        case (1); call collocatedAdd(dfdh,f,g%c(dir)%colCC,g%c(dir)%lapCC,n,s,1)      ! Collocated CellCenter derivative
        case (2); call collocatedAdd(dfdh,f,g%c(dir)%colN,g%c(dir)%lapN,n,s,0)        ! Collocated Node derivative
        case (3); call staggeredAdd(dfdh,f,g%c(dir)%stagCC2N,s,sdfdh,1)               ! Staggered derivative (CC->N)
        case (4); call staggeredAdd(dfdh,f,g%c(dir)%stagN2CC,s,sdfdh,0)               ! Staggered derivative (N->CC)
        end select
        case (3) ! subtract
        select case (diffType)
        case (1); call collocatedSubtract(dfdh,f,g%c(dir)%colCC,g%c(dir)%lapCC,n,s,1) ! Collocated CellCenter derivative
        case (2); call collocatedSubtract(dfdh,f,g%c(dir)%colN,g%c(dir)%lapN,n,s,0)   ! Collocated Node derivative
        case (3); call staggeredSubtract(dfdh,f,g%c(dir)%stagCC2N,s,sdfdh,1)          ! Staggered derivative (CC->N)
        case (4); call staggeredSubtract(dfdh,f,g%c(dir)%stagN2CC,s,sdfdh,0)          ! Staggered derivative (N->CC)
        end select
        case (4) ! multiply
        select case (diffType)
        case (1); call collocatedMultiply(dfdh,f,g%c(dir)%colCC,g%c(dir)%lapCC,n,s,1) ! Collocated CellCenter derivative
        case (2); call collocatedMultiply(dfdh,f,g%c(dir)%colN,g%c(dir)%lapN,n,s,0)   ! Collocated Node derivative
        case (3); call staggeredMultiply(dfdh,f,g%c(dir)%stagCC2N,s,sdfdh,1)          ! Staggered derivative (CC->N)
        case (4); call staggeredMultiply(dfdh,f,g%c(dir)%stagN2CC,s,sdfdh,0)          ! Staggered derivative (N->CC)
        end select
        case (5) ! divide
        select case (diffType)
        case (1); call collocatedDivide(dfdh,f,g%c(dir)%colCC,g%c(dir)%lapCC,n,s,1)   ! Collocated CellCenter derivative
        case (2); call collocatedDivide(dfdh,f,g%c(dir)%colN,g%c(dir)%lapN,n,s,0)     ! Collocated Node derivative
        case (3); call staggeredDivide(dfdh,f,g%c(dir)%stagCC2N,s,sdfdh,1)            ! Staggered derivative (CC->N)
        case (4); call staggeredDivide(dfdh,f,g%c(dir)%stagN2CC,s,sdfdh,0)            ! Staggered derivative (N->CC)
        end select
        case default
          stop 'Error: genType must = 1,2,3,4 in diff in del.f90'
        end select
        ! if (genType.eq.1) then
        !   call diff_old(dfdh_old,f,g%c(dir)%dhc,g%c(dir)%dhn,n,diffType,s,sdfdh,genType)
        !   call compare(dfdh,dfdh_old,f,g,dir,n,diffType,s,sdfdh,genType)
        ! endif
      end subroutine

      subroutine compare(dfdh,dfdh_old,f,g,dir,n,diffType,s,sdfdh,genType)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,n,diffType,s,sdfdh,genType
        real(cp),dimension(sdfdh),intent(in) :: dfdh,dfdh_old
        real(cp) :: tol,err
        ! tol = 10.0_cp**(-1.0_cp)
        tol = 5.0_cp
        err = maxval(abs(dfdh_old - dfdh))
        if (err.gt.tol) then
          write(*,*) 'Error: results mismatch in del vs del_old.'
          write(*,*) 'error = ',maxval(abs(dfdh_old - dfdh))
          write(*,*) 'tol = ',tol
          write(*,*) 'n = ',n
          write(*,*) 's,sdfdh = ',s,sdfdh
          write(*,*) 'diffType = ',diffType
          write(*,*) 'genType = ',genType
          write(*,*) 'f = ',f
          write(*,*) 'dfdh = ',dfdh
          write(*,*) 'dfdh_old = ',dfdh_old
          select case (diffType)
          case (1); if (n.eq.1) then; call print(g%c(dir)%colCC)
                    else;             call print(g%c(dir)%lapCC)
                    endif
          case (2); if (n.eq.1) then; call print(g%c(dir)%colN)
                    else;             call print(g%c(dir)%lapN)
                    endif
          case (3); call print(g%c(dir)%stagCC2N)
          case (4); call print(g%c(dir)%stagN2CC)
          end select
          stop 'Done'
        endif
      end subroutine


      ! ********************** STAGGERED DERIVATIVES *************************

      subroutine staggeredAssign(dfdh,f,T,s,sdfdh,gt); implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        integer,intent(in) :: s,sdfdh,gt
        dfdh = staggered(f,T,s,sdfdh,gt)
      end subroutine

      subroutine staggeredAdd(dfdh,f,T,s,sdfdh,gt); implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        integer,intent(in) :: s,sdfdh,gt
        dfdh = dfdh + staggered(f,T,s,sdfdh,gt)
      end subroutine

      subroutine staggeredSubtract(dfdh,f,T,s,sdfdh,gt); implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        integer,intent(in) :: s,sdfdh,gt
        dfdh = dfdh - staggered(f,T,s,sdfdh,gt)
      end subroutine

      subroutine staggeredMultiply(dfdh,f,T,s,sdfdh,gt); implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        integer,intent(in) :: s,sdfdh,gt
        dfdh = dfdh * staggered(f,T,s,sdfdh,gt)
      end subroutine

      subroutine staggeredDivide(dfdh,f,T,s,sdfdh,gt); implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        integer,intent(in) :: s,sdfdh,gt
        dfdh = dfdh / staggered(f,T,s,sdfdh,gt)
      end subroutine

      ! ********************** COLLOCATED DERIVATIVES *************************

      subroutine collocatedAssign(dfdh,f,T1,T2,n,s,gt); implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T1,T2
        real(cp),dimension(s),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = collocated(f,T1,s,gt)
        case (2); dfdh = collocated(f,T2,s,gt)
        end select
      end subroutine

      subroutine collocatedAdd(dfdh,f,T1,T2,n,s,gt); implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T1,T2
        real(cp),dimension(s),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh + collocated(f,T1,s,gt)
        case (2); dfdh = dfdh + collocated(f,T2,s,gt)
        end select
      end subroutine

      subroutine collocatedSubtract(dfdh,f,T1,T2,n,s,gt); implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T1,T2
        real(cp),dimension(s),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh - collocated(f,T1,s,gt)
        case (2); dfdh = dfdh - collocated(f,T2,s,gt)
        end select
      end subroutine

      subroutine collocatedMultiply(dfdh,f,T1,T2,n,s,gt); implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T1,T2
        real(cp),dimension(s),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh * collocated(f,T1,s,gt)
        case (2); dfdh = dfdh * collocated(f,T2,s,gt)
        end select
      end subroutine

      subroutine collocatedDivide(dfdh,f,T1,T2,n,s,gt); implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T1,T2
        real(cp),dimension(s),intent(inout) :: dfdh
        integer,intent(in) :: n,s,gt
        select case (n)
        case (1); dfdh = dfdh / collocated(f,T1,s,gt)
        case (2); dfdh = dfdh / collocated(f,T2,s,gt)
        end select
      end subroutine

      subroutine delGen(dfdh,f,g,n,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType
        integer :: i
        do i=1,dfdh%s
          call delGen_RF(dfdh%RF(i)%f,f%RF(i)%f,g,n,dir,pad,genType,f%RF(i)%s,dfdh%RF(i)%s)
        enddo
      end subroutine

      subroutine delGen_RF(dfdh,f,g,n,dir,pad,genType,s,sdfdh)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType
        integer,dimension(3),intent(in) :: s,sdfdh
        integer :: i,j,k,diffType

#ifdef _DEBUG_DEL_
        call checkDimensions(s,sdfdh,dir)
#endif

        diffType = getDiffType(s,sdfdh,g%c(dir)%sn,g%c(dir)%sc,dir)

        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
              call diff(dfdh(:,j,k),f(:,j,k),g,dir,n,diffType,s(1),sdfdh(1),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
              call diff(dfdh(i,:,k),f(i,:,k),g,dir,n,diffType,s(2),sdfdh(2),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
              call diff(dfdh(i,j,:),f(i,j,:),g,dir,n,diffType,s(3),sdfdh(3),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
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
            stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
            end select
          endif
        endif

      end subroutine


      ! ******************* OPERATOR TYPES ****************************

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

      subroutine multiplyDel(dfdh,f,g,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,4)
      end subroutine

      subroutine divideDel(dfdh,f,g,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad
        call delGen(dfdh,f,g,n,dir,pad,5)
      end subroutine

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
        stop 'Error: dir must = 1,2,3 in del.f90'
        end select
        if (s1(dir).eq.s2(dir)) then         ! Ok (collocated)
        elseif (s1(dir).eq.(s2(dir)+1)) then ! Ok (N and CC)
        elseif ((s1(dir)+1).eq.s2(dir)) then ! Ok (CC and N)
        else; stop 'Error: shape mismatch 7 in del.f90'
        endif
      end subroutine
#endif


      end module