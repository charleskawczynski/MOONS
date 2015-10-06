      module array_mod
      ! A real array
      ! 
      ! type(array) :: a
      ! call init(a,f)
      ! call delete(a)

      implicit none

      private
      public :: array
      public :: init,delete
      public :: print

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif      

      type array
        real(cp),dimension(:),allocatable :: f
        integer :: s
      end type

      interface init;    module procedure init_array;            end interface
      interface init;    module procedure init_array_Copy;       end interface
      interface delete;  module procedure delete_array;          end interface
      interface print;   module procedure print_array;           end interface

      contains

      subroutine init_array(A,f)
        implicit none
        type(array),intent(inout) :: A
        real(cp),dimension(:),intent(in) :: f
        call delete(A)
        A%s = size(f)
        allocate(A%f(A%s))
        A%f = f
      end subroutine

      subroutine init_array_copy(A_out,A_in)
        implicit none
        type(array),intent(inout) :: A_out
        type(array),intent(in) :: A_in
        if (.not.allocated(A_in%f)) stop 'Error: input array is not allocated in array.f90'
        call delete(A_out)
        A_out%s = A_in%s
        allocate(A_out%f(A_out%s))
        A_out%f = A_in%f
      end subroutine

      subroutine delete_array(A)
        implicit none
        type(array),intent(inout) :: A
        if (allocated(A%f)) deallocate(A%f)
        A%s = 0
      end subroutine

      subroutine print_array(A)
        implicit none
        type(array),intent(in) :: A
        integer :: i
        write(*,*) 'A%s = ',A%s
        write(*,*) 'f = '
        if (allocated(A%f)) then
          do i=1,A%s; write(*,*) A%f(i); enddo
        else
          stop 'Error: array is not allocated in print_array in array.f90'
        endif
      end subroutine

      end module
