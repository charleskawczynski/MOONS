      module array_mod
      use current_precision_mod
      implicit none
      private
      public :: array
      public :: init,delete,display,print,export,import

      public :: assign,add,multiply
      public :: insert
      public :: append,prepend
      public :: pop,snip

      public :: reverse

      public :: insist_allocated

      type array
        integer :: N = 0
        real(cp),dimension(:),allocatable :: f
      end type

      interface init;               module procedure init_array_size;            end interface
      interface init;               module procedure init_array_value;           end interface
      interface init;               module procedure init_array;                 end interface
      interface init;               module procedure init_array_defferred;       end interface
      interface init;               module procedure init_array_copy;            end interface
      interface delete;             module procedure delete_array;               end interface
      interface display;            module procedure display_array;              end interface
      interface print;              module procedure print_array;                end interface
      interface export;             module procedure export_array;               end interface
      interface import;             module procedure import_array;               end interface

      interface assign;             module procedure assign_array;               end interface
      interface assign;             module procedure assign_array_cp;            end interface
      interface assign;             module procedure assign_array_element;       end interface
      interface add;                module procedure add_array_cp;               end interface
      interface add;                module procedure add_array;                  end interface
      interface add;                module procedure add_array_2;                end interface
      interface multiply;           module procedure multiply_array_cp;          end interface
      interface multiply;           module procedure multiply_array;             end interface
      interface multiply;           module procedure multiply_array_2;           end interface

      interface insert;             module procedure insert_element_array;       end interface
      interface append;             module procedure append_element_array;       end interface
      interface append;             module procedure append_A_A;                 end interface
      interface append;             module procedure append_A_cp;                end interface
      interface prepend;            module procedure prepend_A_A;                end interface
      interface prepend;            module procedure prepend_A_cp;               end interface
      interface pop;                module procedure pop_element_array;          end interface
      interface snip;               module procedure snip_element_array;         end interface

      interface reverse;            module procedure reverse_array;              end interface

      interface check_bounds;       module procedure check_bounds_array;         end interface
      interface insist_allocated;   module procedure insist_allocated_array;     end interface
      interface insist_equal_sizes; module procedure insist_equal_sizes_array;   end interface

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

      subroutine init_array_defferred(A,f)
        implicit none
        type(array),intent(inout) :: A
        real(cp),dimension(:),intent(in) :: f
        call init(A,f,size(f))
      end subroutine

      subroutine init_array_copy(A,A_in)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: A_in
#ifdef _DEBUG_ARRAY_
        ! call insist_allocated(A_in,'init_array_copy')

#endif
        call delete(A)
        if (A_in%N.gt.0) then
          A%N = A_in%N
          allocate(A%f(A%N))
          A%f = A_in%f
        endif
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

      subroutine assign_array(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(B,'assign_array')
#endif
        if (.not.allocated(A%f)) call init(A,B%N)
        A%f = B%f
      end subroutine

      subroutine assign_array_cp(A,B)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'assign_array_cp')
#endif
        A%f = B
      end subroutine
      subroutine assign_array_element(A,B,i)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: B
        integer,intent(in) :: i
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'assign_array_element')
        call check_bounds_array(A,i,'assign_array_element')
#endif
        A%f(i) = B
      end subroutine

      subroutine add_array_cp(A,B)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'add_array_cp (A)')
#endif
        A%f = A%f + B
      end subroutine
      subroutine add_array(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'add_array (A)')
        call insist_allocated(B,'add_array (B)')
        call insist_equal_sizes(A,B,'add_array')
#endif
        A%f = A%f + B%f
      end subroutine
      subroutine add_array_2(A,B,C)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B,C
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'add_array_2 (A)')
        call insist_allocated(B,'add_array_2 (B)')
        call insist_allocated(C,'add_array_2 (C)')
        call insist_equal_sizes(A,B,'add_array_2 (A,B)')
        call insist_equal_sizes(A,C,'add_array_2 (A,C)')
#endif
        A%f = B%f + C%f
      end subroutine

      subroutine multiply_array_cp(A,B)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'multiply_array_cp (A)')
#endif
        A%f = A%f*B
      end subroutine
      subroutine multiply_array(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'multiply_array (A)')
        call insist_allocated(B,'multiply_array (B)')
        call insist_equal_sizes(A,B,'multiply_array')
#endif
        A%f = A%f*B%f
      end subroutine
      subroutine multiply_array_2(A,B,C)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B,C
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'multiply_array (A)')
        call insist_allocated(B,'multiply_array (B)')
        call insist_allocated(C,'multiply_array (C)')
        call insist_equal_sizes(A,B,'multiply_array (A,B)')
        call insist_equal_sizes(A,C,'multiply_array (A,C)')
#endif
        A%f = B%f*C%f
      end subroutine

      subroutine append_element_array(A,value)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: value
        type(array) :: temp
        call init(temp,A)
        call init(A,(/temp%f,value/),temp%N+1)
        call delete(temp)
      end subroutine

      subroutine append_A_A(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
        type(array) :: temp
        call init(temp,A)
        call init(A,(/temp%f,B%f/),temp%N+B%N)
        call delete(temp)
      end subroutine

      subroutine prepend_A_A(A,B)
        implicit none
        type(array),intent(inout) :: A
        type(array),intent(in) :: B
        type(array) :: temp
        call init(temp,A)
        call init(A,(/B%f,temp%f/),temp%N+B%N)
        call delete(temp)
      end subroutine

      subroutine append_A_cp(A,B)
        implicit none
        type(array),intent(inout) :: A
        real(cp),dimension(:),intent(in) :: B
        type(array) :: temp
        call init(temp,A)
        call init(A,(/temp%f,B/),temp%N+size(B))
        call delete(temp)
      end subroutine

      subroutine prepend_A_cp(A,B)
        implicit none
        type(array),intent(inout) :: A
        real(cp),dimension(:),intent(in) :: B
        type(array) :: temp
        call init(temp,A)
        call init(A,(/B,temp%f/),temp%N+size(B))
        call delete(temp)
      end subroutine

      subroutine insert_element_array(A,value)
        implicit none
        type(array),intent(inout) :: A
        real(cp),intent(in) :: value
        type(array) :: temp
        call init(temp,A)
        call init(A,(/value,temp%f/),temp%N+1)
        call delete(temp)
      end subroutine

      subroutine pop_element_array(A)
        implicit none
        type(array),intent(inout) :: A
        type(array) :: temp
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'pop_element_array')
#endif
        if (A%N.eq.1) then
          call delete(A)
        else
          call init(temp,A)
          call init(A,(/temp%f(1:temp%N-1)/),temp%N-1)
          call delete(temp)
        endif
      end subroutine

      subroutine snip_element_array(A)
        implicit none
        type(array),intent(inout) :: A
        type(array) :: temp
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'snip_element_array')
#endif
        if (A%N.eq.1) then
          call delete(A)
        else
          call init(temp,A)
          call init(A,(/temp%f(2:temp%N)/),temp%N-1)
          call delete(temp)
        endif
      end subroutine

      subroutine reverse_array(A)
        implicit none
        type(array),intent(inout) :: A
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'snip_element_array')
#endif
        A%f(1:A%N) = A%f(A%N:1:-1)
      end subroutine

      ! ***************************************************************
      ! *********************** DEBUG *********************************
      ! ***************************************************************

      subroutine insist_allocated_array(A,caller)
        implicit none
        type(array),intent(in) :: A
        character(len=*),intent(in) :: caller
        if (.not.allocated(A%f)) then
         write(*,*) 'Error: input array is not allocated in ',caller,' in array.f90'
         write(*,*) 'A%N = ',A%N
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

      subroutine insist_equal_sizes_array(A,B,caller)
        implicit none
        type(array),intent(in) :: A,B
        character(len=*),intent(in) :: caller
#ifdef _DEBUG_ARRAY_
        call insist_allocated(A,'insist_equal_sizes_array in '//caller//' (A)')
        call insist_allocated(B,'insist_equal_sizes_array in '//caller//' (B)')
        if (A%N.ne.B%N) then
          write(*,*) 'Error: N mismatch in ',caller,' in array.f90'
          write(*,*) 'A%N = ',A%N
          write(*,*) 'B%N = ',B%N
          stop 'Done'
        endif
        if (size(A%f).ne.size(B%f)) then
          write(*,*) 'Error: size mismatch in ',caller,' in array.f90'
          write(*,*) 'A%N = ',size(A%f)
          write(*,*) 'B%N = ',size(B%f)
          stop 'Done'
        endif
#endif
      end subroutine

      end module
