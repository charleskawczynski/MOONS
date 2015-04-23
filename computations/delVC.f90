      module delVC_mod
      ! This file is almost exactly the same as the del.f90
      ! 
      ! If this file ever becomes corrupt or outdated, copy the del.f90 
      ! file and add the variable coefficient capability
      ! 
      use grid_mod
      use stencils_mod
      implicit none

      private
      public :: delVC

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type delVC
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

      subroutine diff(dfdh,f,k,dhc,dhn,diffType,s,genType)
        implicit none
        real(cp),dimension(:),intent(in) :: f,k
        real(cp),dimension(:),intent(in) :: dhc,dhn
        integer,intent(in) :: diffType,s,genType
        real(cp),dimension(:),intent(inout) :: dfdh

        select case(genType)
        case (1) ! Assign
        select case (diffType)
        case (1); call collocatedAssign(dfdh,f,k,dhc,dhn,s,1)    ! Collocated CellCenter derivative
        case (2); call collocatedAssign(dfdh,f,k,dhn,dhc,s,0)    ! Collocated Node derivative
        end select
        case (2) ! add
        select case (diffType)
        case (1); call collocatedAdd(dfdh,f,k,dhc,dhn,s,1)       ! Collocated CellCenter derivative
        case (2); call collocatedAdd(dfdh,f,k,dhn,dhc,s,0)       ! Collocated Node derivative
        end select
        case (3) ! subtract
        select case (diffType)
        case (1); call collocatedSubtract(dfdh,f,k,dhc,dhn,s,1)  ! Collocated CellCenter derivative
        case (2); call collocatedSubtract(dfdh,f,k,dhn,dhc,s,0)  ! Collocated Node derivative
        end select
        case (4) ! multiply
        select case (diffType)
        case (1); call collocatedMultiply(dfdh,f,k,dhc,dhn,s,1)  ! Collocated CellCenter derivative
        case (2); call collocatedMultiply(dfdh,f,k,dhn,dhc,s,0)  ! Collocated Node derivative
        end select
        case (5) ! divide
        select case (diffType)
        case (1); call collocatedDivide(dfdh,f,k,dhc,dhn,s,1)    ! Collocated CellCenter derivative
        case (2); call collocatedDivide(dfdh,f,k,dhn,dhc,s,0)    ! Collocated Node derivative
        end select
        case default
          stop 'Error: genType must = 1,2,3,4 in diff in delVC.f90'
        end select
      end subroutine

      ! ********************** COLLOCATED DERIVATIVES *************************

      subroutine collocatedAssign(dfdh,f,k,dhp,dhd,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,k,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = collocated(f,k,dhp,dhd,s,gt)
      end subroutine

      subroutine collocatedAdd(dfdh,f,k,dhp,dhd,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,k,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh + collocated(f,k,dhp,dhd,s,gt)
      end subroutine

      subroutine collocatedSubtract(dfdh,f,k,dhp,dhd,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,k,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh - collocated(f,k,dhp,dhd,s,gt)
      end subroutine

      subroutine collocatedMultiply(dfdh,f,k,dhp,dhd,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,k,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh * collocated(f,k,dhp,dhd,s,gt)
      end subroutine

      subroutine collocatedDivide(dfdh,f,k,dhp,dhd,s,gt); implicit none
        real(cp),intent(in),dimension(:) :: f,k,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        dfdh = dfdh / collocated(f,k,dhp,dhd,s,gt)
      end subroutine

      subroutine delGen(dfdh,f,sig,g,dir,pad,genType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,sig
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,pad,genType
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
              call diff(dfdh(:,j,k),f(:,j,k),sig(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,diffType,s(1),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
              call diff(dfdh(i,:,k),f(i,:,k),sig(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,diffType,s(2),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
              call diff(dfdh(i,j,:),f(i,j,:),sig(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,diffType,s(3),genType)
          enddo; enddo
          !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
        end select

        if (genType.eq.1) then
          if (pad.gt.0) then
            select case (dir)
          case (1); dfdh(:,:,1) = real(0.0,cp); dfdh(:,:,s(3)) = real(0.0,cp)
                    dfdh(:,1,:) = real(0.0,cp); dfdh(:,s(2),:) = real(0.0,cp)
          case (2); dfdh(:,:,1) = real(0.0,cp); dfdh(:,:,s(3)) = real(0.0,cp)
                    dfdh(1,:,:) = real(0.0,cp); dfdh(s(1),:,:) = real(0.0,cp)
          case (3); dfdh(1,:,:) = real(0.0,cp); dfdh(s(1),:,:) = real(0.0,cp)
                    dfdh(:,1,:) = real(0.0,cp); dfdh(:,s(2),:) = real(0.0,cp)
          case default
            stop 'Error: dir must = 1,2,3 in delGen in del.f90.'
            end select
          endif
        endif

      end subroutine


      ! ******************* OPERATOR TYPES ****************************

      subroutine assignDel(dfdh,f,k,g,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,k
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,k,g,dir,pad,1)
      end subroutine

      subroutine addDel(dfdh,f,k,g,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,k
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,k,g,dir,pad,2)
      end subroutine

      subroutine subtractDel(dfdh,f,k,g,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,k
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,k,g,dir,pad,3)
      end subroutine

      subroutine multiplyDel(dfdh,f,k,g,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,k
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,k,g,dir,pad,4)
      end subroutine

      subroutine divideDel(dfdh,f,k,g,dir,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,k
        type(grid),intent(in) :: g
        integer,intent(in) :: dir,pad
        call delGen(dfdh,f,k,g,dir,pad,5)
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
        ! elseif ((sdfdh(dir).eq.(sf(dir)+1))) then
        !   diffType = 3 ! Staggered derivative (CC->N)
        ! elseif ((sdfdh(dir).eq.(sf(dir)-1))) then
        !   diffType = 4 ! Staggered derivative (N->CC)
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