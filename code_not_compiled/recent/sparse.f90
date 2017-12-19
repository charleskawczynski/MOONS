      module sparse_mod
      ! Stores rows of a sparse matrix A and the columns of each element
      ! 
      ! type(sparse) :: A
      ! 
      !                 --                                     --
      ! A =    row(1)   | *               *                     |
      !        row(2)   |    *               *                  |
      !           .     |       *               *               |
      !           .     |          *               *            |
      !           .     |             *               *         |
      !        row(i-1) |                *               *      |
      !        row(i)   |*                  *               *   |
      !        row(i+1) |   *                  *               *|
      !           .     |      *                  *             |
      !           .     |         *                  *          |
      !           .     |            *                  *       |
      !        row(s-1) |               *                  *    |
      !        row(s)   |                  *                  * |
      !                 --                                     --
      !            
      !          
      !        
      !      
      !    
      !  
      ! 
      ! 
      ! 
      ! 
      ! 
      ! 
      ! type(sparse) :: A ! Instantiate sparse matrix
      ! call delete(A)    ! delete sparse matrix
      ! 
      ! call mult(v_out,A,v_in)
      ! 
      use current_precision_mod
      use array_mod
      implicit none

      private
      public :: sparse
      public :: init,delete
      public :: print,check

      public :: staggered, collocated

      type sparse
        type(array),dimension(:),allocatable :: row ! row values
        integer,dimension(:),allocatable :: r       ! row index
        integer,dimension(:),allocatable :: c       ! column index
        logical :: SPD                              ! Symmetric Positive Definite
        integer :: s                                ! size of full matrix (full and empty elements)
        real(cp) :: MV                              ! Minimum value greater than zero
      end type

      interface init;       module procedure init_sparse;           end interface
      interface init;       module procedure init_Copy;             end interface
      interface delete;     module procedure delete_sparse;         end interface
      interface print;      module procedure print_sparse;          end interface
      interface check;      module procedure check_sparse;          end interface
      interface staggered;  module procedure staggered;             end interface
      interface collocated; module procedure collocated_uniform;    end interface
      interface collocated; module procedure collocated_var;        end interface

      contains

      subroutine compute_A(Aun,un,m)
        implicit none
        type(SF),intent(inout) :: Aun
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: un
        call lap(Aun,un,m)
      end subroutine

      subroutine init_from_operator_SF(S,x,m)
        implicit none
        type(sparse),intent(inout) :: S
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        type(SF) :: un,Aun
        integer :: i,newU
        call init(un,x); call init(Aun,x)
        call assign(un,0.0_cp)

        S%MV = 0.0_cp
        do i=1,un%numEl ! Get minimum value in matrix
          call unitVector(un,i)
          call zeroGhostPoints(un)
          if (sum(un).gt.0.5_cp) then
            call compute_A(Aun,un,m)
            S%MV = get_MV(Aun,S%MV)
          endif
        enddo

        do i=1,un%numEl ! Get size
          call unitVector(un,i)
          call zeroGhostPoints(un)
          if (sum(un).gt.0.5_cp) then
            call compute_A(Aun,un,m)
            call init_col_size(S,Aun,i)
          endif
        enddo

        do i=1,un%numEl ! Get sparse matrix
          call unitVector(un,i)
          call zeroGhostPoints(un)
          if (sum(un).gt.0.5_cp) then
            call compute_A(Aun,un,m)
            call extract_row(S,Aun)
          endif
        enddo
        call delete(un)
        call delete(Aun)
        call delete(temp)
      end subroutine

      function get_MV(U,MV_in) result(MV)
        implicit none
        type(SF),intent(in) :: U
        real(cp),intent(in) :: MV_in
        integer :: i,j,k,t
        real(cp) :: MV,v
        MV = MV_in
        do t=1,U%s; do k=2,U%RF(t)%s(3)-1; do j=2,U%RF(t)%s(2)-1; do i=2,U%RF(t)%s(1)-1
        v = abs(U%RF(t)%f(i,j,k))
        if ((v.gt.0.0_cp).and.(v.lt.MV)) MV = v
        enddo; enddo; enddo; enddo
      end function

      function init_col_size(S,U,un) result(n)
        implicit none
        type(sparse),intent(inout) :: S
        type(SF),intent(in) :: U
        integer,intent(in) :: un
        integer :: n
        real(cp) :: v
        integer :: i,j,k,t
        n = 0
        do t=1,U%s; do k=2,U%RF(t)%s(3)-1; do j=2,U%RF(t)%s(2)-1; do i=2,U%RF(t)%s(1)-1
          v = U%RF(t)%f(i,j,k)
          if (abs(v).gt.S%MV) then
            n = n+1
          endif
        enddo; enddo; enddo; enddo
      end subroutine

      subroutine extract_row_SF(S,U,un)
        implicit none
        type(sparse),intent(inout) :: S
        type(SF),intent(in) :: U
        integer,intent(in) :: un
        real(cp) :: v
        integer :: i,j,k,t
        do t=1,U%s; do k=2,U%RF(t)%s(3)-1; do j=2,U%RF(t)%s(2)-1; do i=2,U%RF(t)%s(1)-1
          v = U%RF(t)%f(i,j,k)
          if (abs(v).gt.S%MV) then
            C(S%col()) = U%RF(t)%f(i,j,k)
          endif
        enddo; enddo; enddo; enddo
      end subroutine



      subroutine delete_sparse(S)
        implicit none
        type(sparse),intent(inout) :: S
        integer :: i
        do i=1,S%s
          call delete(S%row(i))
        enddo
        if (allocated(S%r)) deallocate(S%r)
        if (allocated(S%c)) deallocate(S%c)
        S%s = 0
      end subroutine

      subroutine init_Copy(S_out,S_in)
        implicit none
        type(sparse),intent(inout) :: S_out
        type(sparse),intent(in) :: S_in
        integer :: i
        if (S_in%s.lt.1) then
          stop 'Error: input sparse matrix not allocated in init_Copy in sparse.f90'
        endif
        call delete(S_out)
        do i=1,S_in%s
          call init(S_out%row(i),S_in%row(i))
        enddo
        allocate(S_out%c(size(S_in%c)))
        allocate(S_out%r(size(S_in%r)))
        S_out%c = S_in%c
        S_out%r = S_in%r
        S_out%s = S_in%s
      end subroutine

      subroutine print_sparse(S)
        implicit none
        type(sparse),intent(in) :: S
        integer :: i
        write(*,*) 'A(sparse) = '
        do i=1,S%s
          call print(S%row(i))
        enddo
      end subroutine

      end module