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

      use current_precision_mod
      use grid_mod
      use face_edge_corner_indexing_mod
      use mesh_mod
      use GF_mod
      use SF_mod
      use triDiag_mod
      use stencils_mod
      implicit none

      private
      public :: del 

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

      subroutine diff_stag(operator,dfdh,f,T,dir,pad,gt)
        implicit none
        procedure(stencils_stag) :: operator
        type(grid_field),intent(inout) :: dfdh
        type(grid_field),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad,gt
        integer :: i,j,k
        select case (dir)
        case (1)
#ifdef _PARALLELIZE_DEL_
        !$OMP PARALLEL DO SHARED(T,f,gt)

#endif
        do k=1+pad,f%s(3)-pad; do j=1+pad,f%s(2)-pad
          call operator(dfdh%f(:,j,k),f%f(:,j,k),T,f%s(dir),dfdh%s(dir),gt)
        enddo; enddo
#ifdef _PARALLELIZE_DEL_
        !$OMP END PARALLEL DO

#endif
        case (2)
#ifdef _PARALLELIZE_DEL_
        !$OMP PARALLEL DO SHARED(T,f,gt)

#endif
        do k=1+pad,f%s(3)-pad; do i=1+pad,f%s(1)-pad
          call operator(dfdh%f(i,:,k),f%f(i,:,k),T,f%s(dir),dfdh%s(dir),gt)
        enddo; enddo
#ifdef _PARALLELIZE_DEL_
        !$OMP END PARALLEL DO

#endif
        case (3)
#ifdef _PARALLELIZE_DEL_
        !$OMP PARALLEL DO SHARED(T,f,gt)

#endif
        do j=1+pad,f%s(2)-pad; do i=1+pad,f%s(1)-pad
          call operator(dfdh%f(i,j,:),f%f(i,j,:),T,f%s(dir),dfdh%s(dir),gt)
        enddo; enddo
#ifdef _PARALLELIZE_DEL_
        !$OMP END PARALLEL DO

#endif
        end select
      end subroutine

      subroutine diff_col(operator,dfdh,f,T,dir,pad,pad1,pad2)
        implicit none
        procedure(stencils_col) :: operator
        type(grid_field),intent(inout) :: dfdh
        type(grid_field),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: dir,pad,pad1,pad2
        integer :: i,j,k
        select case (dir)
        case (1)
#ifdef _PARALLELIZE_DEL_
        !$OMP PARALLEL DO SHARED(T,f,pad1,pad2)

#endif
        do k=1+pad,f%s(3)-pad; do j=1+pad,f%s(2)-pad
          call operator(dfdh%f(:,j,k),f%f(:,j,k),T,f%s(dir),pad1,pad2)
        enddo; enddo
#ifdef _PARALLELIZE_DEL_
        !$OMP END PARALLEL DO

#endif
        case (2)
#ifdef _PARALLELIZE_DEL_
        !$OMP PARALLEL DO SHARED(T,f,pad1,pad2)

#endif
        do k=1+pad,f%s(3)-pad; do i=1+pad,f%s(1)-pad
          call operator(dfdh%f(i,:,k),f%f(i,:,k),T,f%s(dir),pad1,pad2)
        enddo; enddo
#ifdef _PARALLELIZE_DEL_
        !$OMP END PARALLEL DO

#endif
        case (3)
#ifdef _PARALLELIZE_DEL_
        !$OMP PARALLEL DO SHARED(T,f,pad1,pad2)

#endif
        do j=1+pad,f%s(2)-pad; do i=1+pad,f%s(1)-pad
          call operator(dfdh%f(i,j,:),f%f(i,j,:),T,f%s(dir),pad1,pad2)
        enddo; enddo
#ifdef _PARALLELIZE_DEL_
        !$OMP END PARALLEL DO

#endif
        end select
      end subroutine


      ! *********************************************************
      ! *********************** MED LEVEL ***********************
      ! *********************************************************

      subroutine diff_tree_search(dfdh,f,g,n,dir,pad,genType,diffType,pad1,pad2)
        implicit none
        type(grid_field),intent(inout) :: dfdh
        type(grid_field),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: n,dir,pad,genType,pad1,pad2
        integer,intent(in) :: diffType
#ifdef _DEBUG_DEL_
        call checkSideDimensions(f%s,dfdh%s,dir)
#endif
        select case (genType)
        case (1); select case (diffType)
                  case (3); call diff_stag(stag_assign ,dfdh,f,g%c(dir)%stagCC2N,dir,pad,1)
                  case (4); call diff_stag(stag_assign ,dfdh,f,g%c(dir)%stagN2CC,dir,pad,0)
                  case (1); call diff_col(col_CC_assign,dfdh,f,g%c(dir)%colCC(n),dir,pad,pad1,pad2)
                  case (2); call diff_col(col_N_assign ,dfdh,f,g%c(dir)%colN(n) ,dir,pad,pad1,pad2)
                  end select
        case (3); select case (diffType)
                  case (3); call diff_stag(stag_subtract ,dfdh,f,g%c(dir)%stagCC2N,dir,pad,1)
                  case (4); call diff_stag(stag_subtract ,dfdh,f,g%c(dir)%stagN2CC,dir,pad,0)
                  case (1); call diff_col(col_CC_subtract,dfdh,f,g%c(dir)%colCC(n),dir,pad,pad1,pad2)
                  case (2); call diff_col(col_N_subtract ,dfdh,f,g%c(dir)%colN(n) ,dir,pad,pad1,pad2)
                  end select
        case (2); select case (diffType)
                  case (3); call diff_stag(stag_add ,dfdh,f,g%c(dir)%stagCC2N,dir,pad,1)
                  case (4); call diff_stag(stag_add ,dfdh,f,g%c(dir)%stagN2CC,dir,pad,0)
                  case (1); call diff_col(col_CC_add,dfdh,f,g%c(dir)%colCC(n),dir,pad,pad1,pad2)
                  case (2); call diff_col(col_N_add ,dfdh,f,g%c(dir)%colN(n) ,dir,pad,pad1,pad2)
                  end select
        case default; stop 'Error: genType must = 1,2,3 in diff_tree_search in ops_del.f90'
        end select
      end subroutine

      subroutine diff(dfdh,f,m,n,dir,pad,genType)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad,genType
        integer :: i,diffType
        ! integer :: pad1,pad2
        ! integer,dimension(2) :: faces
        ! d%assign(lapU,u,m,2,1,0)

        diffType = getDiffType(f,dfdh,dir)
        do i=1,m%s
          ! faces = normal_faces_given_dir(dir)
          ! if (m%B(i)%g%st_faces(faces(1))%TF) then; pad1 = 1; else; pad1 = 0; endif
          ! if (m%B(i)%g%st_faces(faces(2))%TF) then; pad2 = 1; else; pad2 = 0; endif
          call diff_tree_search(dfdh%BF(i)%GF,f%BF(i)%GF,m%B(i)%g,n,dir,pad,genType,diffType,0,0)
        enddo
        ! if (n.eq.2) then; call assign_ghost(dfdh,0.0_cp)
        ! elseif ((genType.eq.1).and.(pad.gt.0)) then
        if ((genType.eq.1).and.(pad.gt.0)) then
          if (n.eq.2) write(*,*) 'n,pad,dir,genType = ',n,pad,dir,genType
          select case (dir)
          case (1); call zero_ghost_ymin_ymax(dfdh)
                    call zero_ghost_zmin_zmax(dfdh)
          case (2); call zero_ghost_xmin_xmax(dfdh)
                    call zero_ghost_zmin_zmax(dfdh)
          case (3); call zero_ghost_xmin_xmax(dfdh)
                    call zero_ghost_ymin_ymax(dfdh)
          case default; stop 'Error: dir must = 1,2,3 in delGen_T in ops_del.f90.'
          end select
        endif
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
        call diff(dfdh,f,m,n,dir,pad,1)
      end subroutine

      subroutine addDel(dfdh,f,m,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad
        call diff(dfdh,f,m,n,dir,pad,2)
      end subroutine

      subroutine subtractDel(dfdh,f,m,n,dir,pad)
        implicit none
        type(SF),intent(inout) :: dfdh
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,dir,pad
        call diff(dfdh,f,m,n,dir,pad,3)
      end subroutine

      ! *********************************************************
      ! *********************** AUX / DEBUG *********************
      ! *********************************************************

      function getDiffType(f,dfdh,dir) result(diffType)
        implicit none
        type(SF),intent(in) :: f,dfdh
        integer,intent(in) :: dir
        integer :: diffType
            if (f%CC_along(dir).and.dfdh%CC_along(dir)) then
          diffType = 1 ! Collocated derivative (CC)
        elseif (f%N_along(dir).and.dfdh%N_along(dir)) then
          diffType = 2 ! Collocated derivative (N)
        elseif (f%CC_along(dir).and.dfdh%N_along(dir)) then
          diffType = 3 ! Staggered derivative (CC->N)
        elseif (f%N_along(dir).and.dfdh%CC_along(dir)) then
          diffType = 4 ! Staggered derivative (N->CC)
        else
          write(*,*) 'f%CC_along(dir) = ',f%CC_along(dir)
          write(*,*) 'f%Node_along(dir) = ',f%N_along(dir)
          write(*,*) 'dfdh%CC_along(dir) = ',dfdh%CC_along(dir)
          write(*,*) 'dfdh%Node_along(dir) = ',dfdh%N_along(dir)
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
        case default; stop 'Error: dir must = 1,2,3 in checkSideDimensions in ops_del.f90'
        end select
      end subroutine
#endif

      end module