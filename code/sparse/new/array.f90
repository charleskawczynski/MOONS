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

      interface init;    module procedure init_size_array;       end interface
      interface init;    module procedure init_array;            end interface
      interface init;    module procedure init_element_array;    end interface
      interface init;    module procedure init_array_sparse;     end interface
      interface init;    module procedure init_array_Copy;       end interface
      interface delete;  module procedure delete_array;          end interface
      interface print;   module procedure print_array;           end interface
      interface export;  module procedure export_array;          end interface

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

      subroutine init_size_array(A,s)
        implicit none
        type(array),intent(inout) :: A
        integer,intent(in) :: s
        A%s = s
        if (allocated(A%f)) deallocate(A%f)
        allocate(A%f(A%s))
      end subroutine

      subroutine init_element_array(A,e,i)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: e
        integer,intent(in) :: i
        A%f(i) = e
      end subroutine

      subroutine init_array_sparse(A,f,MV)
        implicit none
        type(array),intent(inout) :: A
        real(cp),dimension(:),intent(in) :: f
        real(cp),intent(in) :: MV ! min value above which to accept array
        integer :: i,s,n
        s = size(f)
        n = 0
        do i=1,s
          if (f(i).gt.MV) then
          n = n+1
          endif
        enddo
        if (n.lt.1) then
          write(*,*) 'Minimum value above which to accept array elements = ',MV
          write(*,*) 'Input array = ',f
          stop 'Error: input array is empty in array.f90'
        endif
        call delete(A)
        A%s = n
        allocate(A%f(A%s))
        do i=1,s
          if (f(i).gt.MV) then
          A%f(i) = f(i)
          endif
        enddo
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
        call export(A,6)
      end subroutine

      subroutine export_array(A,un)
        implicit none
        type(array),intent(in) :: A
        integer,intent(in) :: un
        integer :: i
        if (allocated(A%f)) then
          do i=1,A%s; write(un,*) A%f(i); enddo
        else
          stop 'Error: array is not allocated in print_array in array.f90'
        endif
      end subroutine

      end module
