      module array_mod
      use current_precision_mod
      implicit none
      private
      public :: array
      public :: init,delete,display,print,export,import

      public :: assign,add
      public :: insist_allocated

      type array
        integer :: N
        real(cp),dimension(:),allocatable :: f
      end type

      interface init;               module procedure init_array_size;            end interface
      interface init;               module procedure init_array_value;           end interface
      interface init;               module procedure init_array;                 end interface
      interface init;               module procedure init_array_copy;            end interface
      interface delete;             module procedure delete_array;               end interface
      interface display;            module procedure display_array;              end interface
      interface print;              module procedure print_array;                end interface
      interface export;             module procedure export_array;               end interface
      interface import;             module procedure import_array;               end interface

      interface insist_allocated;   module procedure insist_allocated_array;     end interface
      interface check_bounds;       module procedure check_bounds_array;         end interface
      interface add;                module procedure add_array;                  end interface
      interface assign;             module procedure assign_array;               end interface

      contains

      ! ***************************************************************

      subroutine init_array_size(A,N)
        implicit none
        type(array),intent(inout) :: A
        integer,intent(in) :: N
#ifdef _DEBUG_ARRAY_
        if (N.lt.1) stop 'Error: array size must be > 0 in init_array in array.f90'
#endif
        call delete(A)
        A%N = N
        allocate(A%f(N))
      end subroutine

      subroutine init_array_value(A,v,i)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: v
        integer,intent(in) :: i
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'init_array_value')
        call check_bounds(A,i,'init_array_value')
#endif
        A%f(i) = v
      end subroutine

      subroutine init_array(A,f,N)
        implicit none
        type(array),intent(inout) :: A
        integer,intent(in) :: N
        real(cp),dimension(N),intent(in) :: f
        call delete(A)
#ifdef _DEBUG_ARRAY_
        if (N.ne.size(f)) then
          write(*,*) 'Error: bad size input to init_array in array.f90'
          write(*,*) 'N = ',N
          write(*,*) 'size(f) = ',size(f)
          write(*,*) 'f = ',f
          stop 'Done'
        endif
#endif
        call init(A,N)
        A%f = f
      end subroutine

      subroutine init_array_copy(A,A_in)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: A_in
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A_in,'init_array_copy')
#endif
        call delete(A)
        A%N = A_in%N
        allocate(A%f(A%N))
        A%f = A_in%f
      end subroutine

      subroutine delete_array(A)
        implicit none
        type(array),intent(inout) :: A
        if (allocated(A%f)) deallocate(A%f)
        A%N = 0
      end subroutine

      subroutine display_array(A,un)
        implicit none
        type(array),intent(in) :: A
        integer,intent(in) :: un
        integer :: i
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'display_array')
#endif
        write(un,*) 'A%N = ',A%N
        write(un,*) 'f = '
        do i=1,A%N; write(un,*) A%f(i); enddo
      end subroutine

      subroutine print_array(A)
        implicit none
        type(array),intent(in) :: A
        call display(A,6)
      end subroutine

      subroutine export_array(A,un)
        implicit none
        type(array),intent(in) :: A
        integer,intent(in) :: un
        integer :: i
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'export_array')
#endif
        write(un,*) 'A%N = '; write(un,*) A%N
        write(un,*) 'f = '
        do i=1,A%N; write(un,*) A%f(i); enddo
      end subroutine

      subroutine import_array(A,un)
        implicit none
        type(array),intent(inout) :: A
        integer,intent(in) :: un
        integer :: i,N
        read(un,*); read(un,*) N
        read(un,*);
        call init(A,N)
        do i=1,A%N; read(un,*) A%f(i); enddo
      end subroutine

      ! ***************************************************************

      subroutine insist_allocated_array(A,caller)
        implicit none
        type(array),intent(in) :: A
        character(len=*),intent(in) :: caller
        if (.not.allocated(A%f)) then
         write(*,*) 'Error: input array is not allocated in ',caller,' in array.f90'
         stop 'Done'
        endif
      end subroutine

      subroutine check_bounds_array(A,i,caller)
        implicit none
        type(array),intent(in) :: A
        integer,intent(in) :: i
        character(len=*),intent(in) :: caller
        logical,dimension(2) :: L
        L(1) = i.ge.1
        L(2) = i.le.A%N
        if (.not.all(L)) then
         write(*,*) 'Error: bounds error in ',caller,' in array.f90'
         stop 'Done'
        endif
      end subroutine

      subroutine assign_array(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(B,'assign_array (2)')
#endif
        if (.not.allocated(A%f)) call init(A,B%N)
        A%f = B%f
      end subroutine

      subroutine add_array(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'add_array (1)')
        call insist_allocated(B,'add_array (2)')
        if (A%N.ne.B%N) then
          stop 'Error: size mismatch in add_array in array.f90'
        endif
#endif
        A%f = A%f + B%f
      end subroutine

      end module
