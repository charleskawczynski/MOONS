      module ops_split_mod
      ! Returns an n-derivative of the scalar field, f, 
      ! along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! Flags: (fopenmp,_DEBUG_DEL_)
      ! 
      ! Implementation:
      ! type(split) :: d
      ! call d%assign  (dfdh,f,m,n,dir,pad) --> dfdh = d/dh (f), 0 if not defined.
      ! call d%add     (dfdh,f,m,n,dir,pad) --> dfdh = dfdh + d/dh (f)
      ! call d%subtract(dfdh,f,m,n,dir,pad) --> dfdh = dfdh - d/dh (f)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     m            = mesh containing grids (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     n            = nth derivative (n=1,2 supported)
      !     dir          = direction along which to take the derivative (1,2,3)
      !     pad          = (1,0) = (exclude,include) boundary calc along derivative direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at split for implementation details  
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 5/15/2014

      use grid_mod
      use mesh_mod
      use SF_mod
      use triDiag_mod
      use stencils_mod
      implicit none

      private
      public :: split

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type split
        contains
        generic,public :: assign_D => assignSplit_D
        generic,public :: add_D => addSplit_D
        generic,public :: subtract_D => subtractSplit_D
        procedure,private,nopass :: assignSplit_D
        procedure,private,nopass :: addSplit_D
        procedure,private,nopass :: subtractSplit_D

        generic,public :: assign_LU => assignSplit_LU
        generic,public :: add_LU => addSplit_LU
        generic,public :: subtract_LU => subtractSplit_LU
        procedure,private,nopass :: assignSplit_LU
        procedure,private,nopass :: addSplit_LU
        procedure,private,nopass :: subtractSplit_LU
      end type

      contains

      ! *********************************************************
      ! *********************** LOW LEVEL ***********************
      ! *********************************************************

      subroutine diff(dfdh,f,k,D1,U1,D2,U2,diffType,s,sk,genType,gt)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        real(cp),dimension(sk),intent(in) :: k
        type(triDiag),intent(in) :: D1,D2,U1,U2
        integer,intent(in) :: diffType,s,sk,genType,gt
        real(cp),dimension(s),intent(inout) :: dfdh
        select case(genType)
        case (1) ;select case (diffType) ! Assign
                  case (1); dfdh = collocated_LU(f,k,D1,U1,D2,U2,s,sk,gt)
                  case (2); dfdh =  collocated_D(f,k,D1,U1,D2,U2,s,sk,gt)
                  end select
        case (2) ;select case (diffType) ! add
                  case (1); dfdh = dfdh + collocated_LU(f,k,D1,U1,D2,U2,s,sk,gt)
                  case (2); dfdh = dfdh +  collocated_D(f,k,D1,U1,D2,U2,s,sk,gt)
                  end select
        case (3) ;select case (diffType) ! subtract
                  case (1); dfdh = dfdh - collocated_LU(f,k,D1,U1,D2,U2,s,sk,gt)
                  case (2); dfdh = dfdh -  collocated_D(f,k,D1,U1,D2,U2,s,sk,gt)
                  end select
        case default
          stop 'Error: genType must = 1,2,3 in diff in split.f90'
        end select
      end subroutine

      subroutine splitGen_T(dfdh,f,sig,D1,U1,D2,U2,dir,pad,genType,diffType,gt,s,sk)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f,sig
        type(triDiag),intent(in) :: D1,U1,D2,U2
        integer,intent(in) :: dir,pad,genType,diffType,gt
        integer,dimension(3),intent(in) :: s,sk
        integer :: i,j,k
#ifdef _DEBUG_DEL_
        call checkSideDimensions(s,sk,dir)
#endif
        select case (dir)
        case (1); 
        !$OMP PARALLEL DO
        do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
          call diff(dfdh(:,j,k),f(:,j,k),sig(:,j,k),D1,U1,D2,U2,diffType,s(1),sk(1),genType,gt)
        enddo; enddo
        !$OMP END PARALLEL DO
        case (2); 
        !$OMP PARALLEL DO
        do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
          call diff(dfdh(i,:,k),f(i,:,k),sig(i,:,k),D1,U1,D2,U2,diffType,s(2),sk(2),genType,gt)
        enddo; enddo
        !$OMP END PARALLEL DO
        case (3); 
        !$OMP PARALLEL DO
        do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
          call diff(dfdh(i,j,:),f(i,j,:),sig(i,j,:),D1,U1,D2,U2,diffType,s(3),sk(3),genType,gt)
        enddo; enddo
        !$OMP END PARALLEL DO
        case default
        stop 'Error: dir must = 1,2,3 in splitGen_T in split.f90.'
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
            stop 'Error: dir must = 1,2,3 in splitGen_T in split.f90.'
            end select
          endif
        endif
      end subroutine

      ! *********************************************************
      ! *********************** MED LEVEL ***********************
      ! *********************************************************

      subroutine splitGen_D(dfdh,f,sig,m,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad,genType
        integer :: i
        logical :: CC,Node
        CC   = any((/ f%is_CC   , f%is_Face.and.(f%face.ne.dir) , f%is_Edge.and.(f%edge.eq.dir) /))
        Node = any((/ f%is_Node , f%is_Face.and.(f%face.ne.dir) , f%is_Edge.and.(f%edge.eq.dir) /))
        ! if (CC.eq.Node) stop 'Error: CC = Node in splitGen_D in ops_split.f90'

        ! function collocated_D(f,k,D1,U1,D2,U2,s,sk,gt)
        !   ! Computes
        !   !    U2(σD1) + D2(σU1)
        if (CC) then
          do i=1,f%s
            call splitGen_T(dfdh%RF(i)%f,f%RF(i)%f,sig%RF(i)%f,&
              m%g(i)%c(dir)%D_CC2N,m%g(i)%c(dir)%U_CC2N,&
              m%g(i)%c(dir)%D_N2CC,m%g(i)%c(dir)%U_N2CC,&
              dir,pad,genType,2,1,f%RF(i)%s,sig%RF(i)%s)
          enddo
        elseif (Node) then
          do i=1,f%s
            call splitGen_T(dfdh%RF(i)%f,f%RF(i)%f,sig%RF(i)%f,&
              m%g(i)%c(dir)%D_N2CC,m%g(i)%c(dir)%U_N2CC,&
              m%g(i)%c(dir)%D_CC2N,m%g(i)%c(dir)%U_CC2N,&
              dir,pad,genType,2,0,f%RF(i)%s,sig%RF(i)%s)
          enddo
        else; stop 'Error: SF not fully defined in splitGen_D in ops_split.f90'
        endif
      end subroutine

      subroutine splitGen_LU(dfdh,f,sig,m,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad,genType
        integer :: i
        logical :: CC,Node
        CC   = any((/ f%is_CC   , f%is_Face.and.(f%face.ne.dir) , f%is_Edge.and.(f%edge.eq.dir) /))
        Node = any((/ f%is_Node , f%is_Face.and.(f%face.ne.dir) , f%is_Edge.and.(f%edge.eq.dir) /))
        ! if (CC.eq.Node) stop 'Error: CC = Node in splitGen_D in ops_split.f90'

        ! function collocated_LU(f,k,D1,U1,D2,U2,s,sk,gt)
        !   ! Computes
        !   !    D2(σD1) + U2(σU1)
        if (CC) then
          do i=1,f%s
            call splitGen_T(dfdh%RF(i)%f,f%RF(i)%f,sig%RF(i)%f,&
              m%g(i)%c(dir)%D_CC2N,m%g(i)%c(dir)%U_CC2N,&
              m%g(i)%c(dir)%D_N2CC,m%g(i)%c(dir)%U_N2CC,&
              dir,pad,genType,1,1,f%RF(i)%s,sig%RF(i)%s)
          enddo
        elseif (Node) then
          do i=1,f%s
            call splitGen_T(dfdh%RF(i)%f,f%RF(i)%f,sig%RF(i)%f,&
              m%g(i)%c(dir)%D_N2CC,m%g(i)%c(dir)%U_N2CC,&
              m%g(i)%c(dir)%D_CC2N,m%g(i)%c(dir)%U_CC2N,&
              dir,pad,genType,1,0,f%RF(i)%s,sig%RF(i)%s)
          enddo
        else; stop 'Error: SF not fully defined in splitGen_D in ops_split.f90'
        endif
      end subroutine

      ! *********************************************************
      ! *********************** HIGH LEVEL **********************
      ! *********************************************************

      subroutine assignSplit_D(dfdh,f,sig,m,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad
        call splitGen_D(dfdh,f,sig,m,dir,pad,1)
      end subroutine

      subroutine addSplit_D(dfdh,f,sig,m,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad
        call splitGen_D(dfdh,f,sig,m,dir,pad,2)
      end subroutine

      subroutine subtractSplit_D(dfdh,f,sig,m,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad
        call splitGen_LU(dfdh,f,sig,m,dir,pad,3)
      end subroutine

      subroutine assignSplit_LU(dfdh,f,sig,m,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad
        call splitGen_LU(dfdh,f,sig,m,dir,pad,1)
      end subroutine

      subroutine addSplit_LU(dfdh,f,sig,m,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad
        call splitGen_LU(dfdh,f,sig,m,dir,pad,2)
      end subroutine

      subroutine subtractSplit_LU(dfdh,f,sig,m,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f,sig
        type(mesh),intent(in) :: m
        integer,intent(in) :: dir,pad
        call splitGen_LU(dfdh,f,sig,m,dir,pad,3)
      end subroutine

      ! *********************************************************
      ! *********************** AUX / DEBUG *********************
      ! *********************************************************

#ifdef _DEBUG_DEL_
      subroutine checkSideDimensions(s1,s2,dir)
        ! This routine makes sure that the shapes s1 and s2 
        ! are equal for orthogonal directions to dir, which
        ! must be the case for all derivatives in split.
        implicit none
        integer,dimension(3),intent(in) :: s1,s2
        integer,intent(in) :: dir
        select case (dir)
        case (1); if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 1 in split'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 2 in split'
        case (2); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 3 in split'
                  if (s1(3).ne.s2(3)) stop 'Error: Shape mismatch 4 in split'
        case (3); if (s1(1).ne.s2(1)) stop 'Error: Shape mismatch 5 in split'
                  if (s1(2).ne.s2(2)) stop 'Error: Shape mismatch 6 in split'
        case default
        stop 'Error: dir must = 1,2,3 in split.f90'
        end select
      end subroutine
#endif

      end module