      module ops_del_local_mod
      ! This module is the same as del,
      ! but was developed to operate on 
      ! unit vectors and avoid unecessary 
      ! calculations

      use grid_mod
      use mesh_mod
      use SF_mod
      use triDiag_mod
      use stencils_mod
      use apply_stitches_mod
      implicit none

      private
      public :: del_local

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

      type del_local
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

      subroutine diff(dfdh,f,T,diffType,s,sdfdh,genType,gt,CC,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: diffType,s,sdfdh,genType,gt,pad1,pad2
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        logical,intent(in) :: CC
        select case(genType)
        case (1) ;select case (diffType) ! Assign
                  case (1); dfdh = collocated(f,T,s,CC,pad1,pad2)         ! Collocated derivative
                  case (2); dfdh = staggered(f,T,s,sdfdh,gt)              ! Staggered derivative
                  end select
        case (2) ;select case (diffType) ! add
                  case (1); dfdh = dfdh + collocated(f,T,s,CC,pad1,pad2)  ! Collocated derivative
                  case (2); dfdh = dfdh + staggered(f,T,s,sdfdh,gt)       ! Staggered derivative
                  end select
        case (3) ;select case (diffType) ! subtract
                  case (1); dfdh = dfdh - collocated(f,T,s,CC,pad1,pad2)  ! Collocated derivative
                  case (2); dfdh = dfdh - staggered(f,T,s,sdfdh,gt)       ! Staggered derivative
                  end select
        case default
          stop 'Error: genType must = 1,2,3 in diff in del.f90'
        end select
      end subroutine

      subroutine delGen_T(dfdh,f,T,dir,pad,genType,diffType,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad,genType,diffType,gt,pad1,pad2,i_3D,j_3D,k_3D
        integer,dimension(3),intent(in) :: s,sdfdh
        logical,intent(in) :: CC
        integer :: i,j,k
        integer :: i_padl,i_padr,j_padl,j_padr,k_padl,k_padr
#ifdef _DEBUG_DEL_
        call checkSideDimensions(s,sdfdh,dir)
#endif

        ! if (genType.eq.1) dfdh = 0.0_cp

        i_padl = 2; i_padr = 2
        j_padl = 2; j_padr = 2
        k_padl = 2; k_padr = 2

        if (i_3D.eq.1) i_padl=0; if (i_3D.eq.s(1)) i_padr=0
        if (j_3D.eq.1) j_padl=0; if (j_3D.eq.s(2)) j_padr=0
        if (k_3D.eq.1) k_padl=0; if (k_3D.eq.s(3)) k_padr=0
        
        if (i_3D.eq.2) i_padl=1; if (i_3D.eq.s(1)-1) i_padr=1
        if (j_3D.eq.2) j_padl=1; if (j_3D.eq.s(2)-1) j_padr=1
        if (k_3D.eq.2) k_padl=1; if (k_3D.eq.s(3)-1) k_padr=1

        select case (dir)
        case (1)
        do k=k_3D-k_padl,k_3D+k_padr; do j=j_3D-j_padl,j_3D+j_padr
          call diff(dfdh(:,j,k),(/(f(i,j,k),i=1,s(1))/),T,diffType,s(1),sdfdh(1),genType,gt,CC,pad1,pad2)
        enddo; enddo
        case (2); 
        do k=k_3D-k_padl,k_3D+k_padr; do i=i_3D-i_padl,i_3D+i_padr
          call diff(dfdh(i,:,k),(/(f(i,j,k),j=1,s(2))/),T,diffType,s(2),sdfdh(2),genType,gt,CC,pad1,pad2)
        enddo; enddo
        case (3); 
        do j=j_3D-j_padl,j_3D+j_padr; do i=i_3D-i_padl,i_3D+i_padr
          call diff(dfdh(i,j,:),(/(f(i,j,k),k=1,s(3))/),T,diffType,s(3),sdfdh(3),genType,gt,CC,pad1,pad2)
        enddo; enddo
        case default
        stop 'Error: dir must = 1,2,3 in delGen_T in ops_del.f90.'
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
            stop 'Error: dir must = 1,2,3 in delGen_T in ops_del.f90.'
            end select
          endif
        endif
      end subroutine


      ! *********************************************************
      ! *********************** MED LEVEL ***********************
      ! *********************************************************

      subroutine delGen_RF_given_g(dfdh,f,g,n,dir,pad,genType,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType,pad1,pad2,i_3D,j_3D,k_3D
        integer,dimension(3),intent(in) :: s,sdfdh
        logical,intent(in) :: CC
        integer :: diffType,gt
        diffType = getDiffType(s,sdfdh,g%c(dir)%sn,g%c(dir)%sc,dir)
        select case (diffType)
        case (1); gt = 1
                  select case (n)
                  case (1); call delGen_T(dfdh,f,g%c(dir)%colCC,dir,pad,genType,1,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
                  case (2); call delGen_T(dfdh,f,g%c(dir)%LapCC,dir,pad,genType,1,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
                  end select
        case (2); gt = 0
                  select case (n)
                  case (1); call delGen_T(dfdh,f,g%c(dir)%colN,dir,pad,genType,1,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
                  case (2); call delGen_T(dfdh,f,g%c(dir)%LapN,dir,pad,genType,1,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
                  end select
        case (3); gt = 1; call delGen_T(dfdh,f,g%c(dir)%stagCC2N,dir,pad,genType,2,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
        case (4); gt = 0; call delGen_T(dfdh,f,g%c(dir)%stagN2CC,dir,pad,genType,2,gt,s,sdfdh,CC,pad1,pad2,i_3D,j_3D,k_3D)
        end select
      end subroutine

      subroutine delGen_given_g(dfdh,f,m,n,dir,pad,genType,index_1D)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad,genType,index_1D
        integer :: i,pad1,pad2,i_3D,j_3D,k_3D,t_3D
        logical :: CC
        CC = CC_along(f,dir)
        ! Node = any((/ f%is_Node , f%is_Face.and.(f%face.ne.dir) , f%is_Edge.and.(f%edge.eq.dir) /))
        call get_3D_index(i_3D,j_3D,k_3D,t_3D,f,index_1D)
        do i=1,t_3D
          if (m%g(i)%st_face%hmin(dir)) then; pad1 = 1;
          else; pad1 = 0; endif
          if (m%g(i)%st_face%hmax(dir)) then; pad2 = 1;
          else; pad2 = 0; endif
          call delGen_RF_given_g(dfdh%RF(i)%f,f%RF(i)%f,m%g(i),&
            n,dir,pad,genType,f%RF(i)%s,dfdh%RF(i)%s,CC,pad1,pad2,i_3D,j_3D,k_3D)
        enddo
        ! call applyStitches(dfdh,m)
      end subroutine

      ! *********************************************************
      ! *********************** HIGH LEVEL **********************
      ! *********************************************************

      subroutine assignDel(dfdh,f,m,n,dir,pad,index_1D)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad,index_1D
        call delGen(dfdh,f,m,n,dir,pad,1,index_1D)
      end subroutine

      subroutine addDel(dfdh,f,m,n,dir,pad,index_1D)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad,index_1D
        call delGen(dfdh,f,m,n,dir,pad,2,index_1D)
      end subroutine

      subroutine subtractDel(dfdh,f,m,n,dir,pad,index_1D)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad,index_1D
        call delGen(dfdh,f,m,n,dir,pad,3,index_1D)
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