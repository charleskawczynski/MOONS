       module dynamic_index_mod
       use current_precision_mod
       implicit none

       private
       public :: dynamic_index
       public :: init,delete
       public :: print,export

       type dynamic_index
         integer,allocatable :: i1,i2
         integer :: s1,s2
       end type

       interface init;          module procedure init_DI_single;       end interface
       interface init;          module procedure init_DI_array;        end interface
       interface init;          module procedure init_DI_copy;         end interface
       interface delete;        module procedure delete_DI;            end interface
       interface print;         module procedure print_DI;             end interface
       interface export;        module procedure export_DI;            end interface
       interface display;       module procedure display_DI;           end interface

       contains

      subroutine init_DI_single(DI,i1,i2)
        implicit none
        type(dynamic_index),intent(inout) :: DI
        integer,intent(in) :: i1,i2
        call delete(DI)
#ifdef _DEBUG_DYNAMIC_INDEX_
        call check_allocated(DI%i1,DI%i2,'init_DI_single')
#endif
        allocate(DI%i1(1)); DI%i1 = i1
        allocate(DI%i2(1)); DI%i2 = i2
        DI%s1 = 1
        DI%s2 = 1
      end subroutine

      subroutine init_DI_array(DI,i1,i2,s1,s2)
        implicit none
        integer,intent(in) :: s1
        integer,intent(in) :: s2
        type(dynamic_index),intent(inout) :: DI
        integer,dimension(s1),intent(in) :: i1
        integer,dimension(s2),intent(in) :: i2
        call delete(DI)
#ifdef _DEBUG_DYNAMIC_INDEX_
        call check_allocated(i1,i2,'init_DI_array')
        call check_size(s1,s2,'init_DI_array')
#endif
        allocate(DI%i1(s1)); DI%i1 = i1
        allocate(DI%i2(s2)); DI%i2 = i2
        DI%s1 = 1
        DI%s2 = 1
      end subroutine

      subroutine init_DI_copy(DI_out,DI_in)
        implicit none
        type(dynamic_index),intent(inout) :: DI_out
        type(dynamic_index),intent(in) :: DI_in
        call delete(DI_out)
#ifdef _DEBUG_DYNAMIC_INDEX_
        call check_allocated(DI_in%i1,DI_in%i2,'init_DI_copy')
        call check_size(DI_in%s1,DI_in%s2,'init_DI_copy')
#endif
        call init(DI_out,DI_in%i1,DI_in%i2)
      end subroutine

      subroutine delete_DI(DI)
        implicit none
        type(dynamic_index),intent(inout) :: DI
        if (allocated(DI%i1)) deallocate(DI%i1)
        if (allocated(DI%i2)) deallocate(DI%i2)
        s1 = 0
        s2 = 0
      end subroutine

      subroutine export_DI(DI,un)
        implicit none
        type(dynamic_index),intent(in) :: DI
        integer,intent(in) :: DI
#ifdef _DEBUG_DYNAMIC_INDEX_
        call check_allocated(DI,'export_DI')
#endif
        write(un,*) 's1 = '
        write(un,*) DI%s1
        write(un,*) 's2 = '
        write(un,*) DI%s2
        write(un,*) 'i1 = '
        do i=1,DI%s1; write(un,*) DI%i1(i); enddo
        write(un,*) 'i2 = '
        do i=1,DI%s2; write(un,*) DI%i2(i); enddo
      end subroutine

      subroutine print_DI(DI)
        implicit none
        type(dynamic_index),intent(in) :: DI
        call export_DI(DI,6)
      end subroutine


#ifdef _DEBUG_DYNAMIC_INDEX_
      subroutine check_allocated(i1,i2,caller)
        implicit none
        integer,dimension(:),intent(in) :: i1,i2
        character(len=*),intent(in) :: caller
        if (.not.allocated(i1)) stop 'Error: i1 unallocated in '//caller//' in dynamic_index.f90'
        if (.not.allocated(i2)) stop 'Error: i2 unallocated in '//caller//' in dynamic_index.f90'
      end subroutine

      subroutine check_size(s1,s2,caller)
        implicit none
        integer,intent(in) :: s1,s2
        character(len=*),intent(in) :: caller
        if (s1.lt.1) stop 'Error: s1.lt.1 in '//caller//' in dynamic_index.f90'
        if (s2.lt.1) stop 'Error: s2.lt.1 in '//caller//' in dynamic_index.f90'
      end subroutine
#endif

       end module