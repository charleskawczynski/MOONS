      module sparse_mod
      ! Stores elements of a sparse matrix A
      ! 
      ! type(sparse) :: A ! Instantiate sparse matrix
      ! call init(A,d,0)  ! Sets       diagonal
      ! call init(A,d,1)  ! Sets super-diagonal
      ! call init(A,d,-1) ! Sets sub  -diagonal
      ! call delete(A)    ! delete sparse matrix
      ! 
      ! call mult(SF,SP)
      ! 
      ! Contains:
      !            S%A(i)%f
      use array_mod
      implicit none

      private
      public :: sparse
      public :: init,delete
      public :: print,check

      public :: staggered, collocated

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif      

      type sparse
        type(array),dimension(:),allocatable :: A
        integer,dimension(:),allocatable :: elem  ! indexes for allocated L,D,U ect.
        integer :: s
      end type

      interface init;       module procedure init_sparse;           end interface
      interface init;       module procedure init_Copy;             end interface
      interface delete;     module procedure delete_sparse;         end interface
      interface delete;     module procedure delete_sparse_element; end interface
      interface print;      module procedure print_sparse;          end interface
      interface check;      module procedure check_sparse;          end interface
      interface staggered;  module procedure staggered;             end interface
      interface collocated; module procedure collocated_uniform;    end interface
      interface collocated; module procedure collocated_var;        end interface

      contains

      subroutine init_sparse(S,f,i)
        implicit none
        type(sparse),intent(inout) :: S
        real(cp),dimension(:),intent(in) :: f
        integer,dimension(:),allocatable :: temp
        integer :: i
        call delete(S,i)
        call init(S%A(i),f)
        if (.not.allocated(S%elem)) then
          allocate(S%elem(1)); S%elem = i
        else
          allocate(temp(size(S%elem)+1))
          temp = (/S%elem,i/)
          deallocate(S%elem); allocate(S%elem(size(temp)))
          S%elem = temp
          deallocate(temp)
        endif
        S%s = size(S%elem)
      end subroutine

      subroutine delete_sparse_element(S,i)
        implicit none
        type(sparse),intent(inout) :: S
        integer,intent(in) :: i
        if (allocated(S%A(i)%f)) call delete(S%A(i))
      end subroutine

      subroutine init_Copy(S_out,S_in)
        implicit none
        type(sparse),intent(inout) :: S_out
        type(sparse),intent(in) :: S_in
        integer :: i
        if (S_in%s.le.0) then
          stop 'Error: input sparse matrix not allocated in init_Copy in sparse.f90'
        endif
        call delete(S_out)
        do i=1,S_in%s
          call init(S_out%A(S_in%elem(i)),S_in%A(S_in%elem(i)))
        enddo
        if (allocated(S_out%elem)) deallocate(S_out%elem)
        S_out%elem = S_in%elem
        S_out%s = S_in%s
      end subroutine

      subroutine delete_sparse(S)
        implicit none
        type(sparse),intent(inout) :: S
        integer :: i
        do i=1,S%s
          call delete(S%A(i))
        enddo
        if (allocated(S%elem)) deallocate(S%elem)
        S%s = 0
      end subroutine

      subroutine print_sparse(S)
        implicit none
        type(sparse),intent(in) :: S
        integer :: i
        do i=1,S%s
          write(*,*) 'Element = ',S%elem(i)
          call print(S%A(S%elem(i)))
        enddo
      end subroutine

      subroutine check_sparse(S)
        implicit none
        type(sparse),intent(in) :: S
        integer :: i,j
        real(cp) :: rowSum
        rowSum = 0.0_cp
        do j=1,S%s
          write(*,*) 'Element = ',S%elem(j)
          rowSum = 0.0_cp
          do i=3,S%A(j)%s-2
            rowSum = rowSum +S%A(j)%f(i-1) + S%A(j)%f(i-1) + S%A(j)%f(i-1)
          enddo
          write(*,*) 'sum(row) = ',rowSum
        enddo
      end subroutine

      function staggered(f,SP,s,sdfdh,gt) result(dfdh)
        ! This routine computes the 1st derivative of f on the primary
        ! grid. The result lives on the dual grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 1 :  f {CC} , dfdh {N}   ,  NOTE: dfdh = d/dh (f) {interior}
        !      0 :  f {N}  , dfdh {CC}  ,  NOTE: dfdh = d/dh (f) {everywhere}
        ! 
        ! NOTE:
        !  f {CC} , dfdh {N}
        !  f {N}  , dfdh {CC}
        ! 
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer,intent(in) :: s,sdfdh,gt
        real(cp),dimension(sdfdh) :: dfdh
        integer :: i,j
        ! Interior
        do i=3,s-2
          dfdh(i+gt) = (/(f(i+SP%elem(j)),j=1,SP%s)/)*(/( SP%A( SP%elem(j) )%f(i+SP%elem(j)) ,j=1,SP%s)/)
        enddo
        ! Boundaries
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end function

      function collocated_uniform(f,T,s,gt) result(dfdh)
        ! This routine computes the 1st or 2nd derivative (depending
        ! on the triDiag, T) of f on the primary grid. The result 
        ! lives on the primary grid. gt indicates whether f lives on 
        ! the cell center or node of the grid.
        ! gt = 1 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      0 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: gt,s
        real(cp),dimension(s) :: dfdh
        integer :: i
        ! Interior
        do i=3,s-2
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        ! Boundaries
        if (gt.eq.1) then
        ! Collocated cell-center derivative, half cell from boundary
        ! Linear interpolation is used to obtain boundary value
        dfdh(2) = 0.5_cp*(f(1)+f(2))*T%L(1) + &
                               f(2) *T%D(1) + &
                               f(3) *T%U(1)
        dfdh(s-1) =  f(s-2) *T%L(s-2) + &
                     f(s-1) *T%D(s-2) + &
        0.5_cp*(f(s)+f(s-1))*T%U(s-2)
        else
        ! Collocated cell-corner derivative, on boundary
        dfdh(2) = f(2)*T%L(1) + &
                  f(3)*T%D(1) + &
                  f(4)*T%U(1)
        dfdh(s-1) = f(s-3)*T%L(s-2) + &
                    f(s-2)*T%D(s-2) + &
                    f(s-1)*T%U(s-2)
        endif
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp ! Ghost points
      end function

      function collocated_var(f,k,T1,T2,s,stemp,gt) result(dfdh)
        implicit none
        real(cp),intent(in),dimension(s) :: f
        real(cp),dimension(stemp),intent(in) :: k
        type(triDiag),intent(in) :: T1,T2
        integer,intent(in) :: s,stemp,gt
        real(cp),dimension(s) :: dfdh
        real(cp),dimension(stemp) :: dfdh_temp
        ! First derivative
        dfdh_temp = staggered(f,T1,s,stemp,gt)
        ! Second derivative
        if (gt.eq.1) then; dfdh = staggered(k*dfdh_temp,T2,stemp,s,0)
        else;              dfdh = staggered(k*dfdh_temp,T2,stemp,s,1)
        endif
      end function

      end module
