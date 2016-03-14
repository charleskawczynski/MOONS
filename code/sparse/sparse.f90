      module sparse_mod
      ! Stores elements of a sparse matrix A
      ! 
      ! type(sparse) :: A ! Instantiate sparse matrix
      ! call init(A,d,0)  ! Sets       diagonal
      ! call init(A,d,1)  ! Sets super-diagonal
      ! call init(A,d,-1) ! Sets sub  -diagonal
      ! call delete(A)    ! delete sparse matrix
      ! 
      ! Contains:
      !            S%A(i)%f
      use array_mod
      implicit none

      private
      public :: sparse
      public :: init,delete
      public :: print,check

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

      interface init;    module procedure init_sparse;           end interface
      interface init;    module procedure init_Copy;             end interface
      interface delete;  module procedure delete_sparse;         end interface
      interface delete;  module procedure delete_sparse_element; end interface
      interface print;   module procedure print_sparse;          end interface
      interface check;   module procedure check_sparse;          end interface

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

      end module
