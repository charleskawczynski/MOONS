       module associative_array_mod
       use current_precision_mod
       use key_value_pair_mod
       use string_mod
       use IO_tools_mod
       implicit none

       private
       public :: associative_array
       public :: init,delete,export,import,display,print

       public :: add
       public :: set_all_false
       public :: get_value

       type associative_array
         integer :: n = 0
         type(key_value_pair),dimension(:),allocatable :: KVP
       end type

       interface init;          module procedure init_AA;          end interface
       interface init;          module procedure init_size_AA;     end interface
       interface init;          module procedure init_copy_AA;     end interface
       interface add;           module procedure add_KVP_AA;       end interface
       interface delete;        module procedure delete_AA;        end interface
       interface export;        module procedure export_AA;        end interface
       interface import;        module procedure import_AA;        end interface
       interface display;       module procedure display_AA;       end interface
       interface print;         module procedure print_AA;         end interface

       interface set_all_false; module procedure set_all_false_AA; end interface
       interface get_value;     module procedure get_value_AA;     end interface

       contains

       ! **********************************************************
       ! ********************* EAAENTIALS *************************
       ! **********************************************************

       subroutine init_AA(AA,KVP)
         implicit none
         type(associative_array),intent(inout) :: AA
         type(key_value_pair),intent(in) :: KVP
         call init(AA,1)
         call init(AA%KVP(1),KVP)
       end subroutine

       subroutine init_size_AA(AA,n)
         implicit none
         type(associative_array),intent(inout) :: AA
         integer,intent(in) :: n
         call delete(AA)
         AA%n = n
         allocate(AA%KVP(AA%n))
       end subroutine

       subroutine add_KVP_AA(AA,KVP)
         implicit none
         type(associative_array),intent(inout) :: AA
         type(key_value_pair),intent(in) :: KVP
         type(associative_array) :: temp
         integer :: i
         call init(temp,AA)
         call init(AA,temp%n+1)
         do i=1,temp%n
         call init(AA%KVP(i),temp%KVP(i))
         enddo
         call init(AA%KVP(AA%n),KVP)
         call delete(temp)
       end subroutine

       subroutine init_copy_AA(AA,AA_in)
         implicit none
         type(associative_array),intent(inout) :: AA
         type(associative_array),intent(in) :: AA_in
         integer :: i
         call delete(AA)
         if (AA_in%n.gt.0) then
           AA%n = AA_in%n
           allocate(AA%KVP(AA_in%n))
           do i=1,AA_in%n
             call init(AA%KVP(i),AA_in%KVP(i))
           enddo
         endif
       end subroutine

       subroutine delete_AA(AA)
         implicit none
         type(associative_array),intent(inout) :: AA
         integer :: i
         if (AA%n.gt.0) then
           do i=1,AA%n
             call delete(AA%KVP(i))
           enddo
           deallocate(AA%KVP)
           AA%n = 0
         endif
       end subroutine

       subroutine export_AA(AA,un)
         implicit none
         type(associative_array),intent(in) :: AA
         integer,intent(in) :: un
         integer :: i
         write(un,*) AA%n
         do i=1,AA%n
           call export(AA%KVP(i),un)
         enddo
       end subroutine

       subroutine import_AA(AA,un)
         implicit none
         type(associative_array),intent(inout) :: AA
         integer,intent(in) :: un
         integer :: i,n
         call delete(AA)
         write(un,*) n
         call init(AA,n)
         do i=1,AA%n
           call import(AA%KVP(i),un)
         enddo
       end subroutine

       subroutine display_AA(AA,un)
         implicit none
         type(associative_array),intent(in) :: AA
         integer,intent(in) :: un
         integer :: i
         if (AA%n.gt.0) then
           do i=1,AA%n; call display(AA%KVP(i),un); enddo
         endif
       end subroutine

       subroutine print_AA(AA)
         implicit none
         type(associative_array),intent(inout) :: AA
         call display(AA,6)
       end subroutine

       subroutine set_all_false_AA(AA)
         implicit none
         type(associative_array),intent(inout) :: AA
         integer :: i
         do i=1,AA%n
           call set_false(AA%KVP(i))
         enddo
       end subroutine

       function get_value_AA(AA,key) result(L)
         implicit none
         type(associative_array),intent(in) :: AA
         character(len=*),intent(in) :: key
         integer :: i
         logical :: L
         L = .false.
         if (AA%n.gt.0) then
           do i=1,AA%n
             if (is_key(AA%KVP(i),key)) L = get_value(AA%KVP(i))
           enddo
         endif
       end function

       end module