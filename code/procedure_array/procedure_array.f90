       module procedure_array_mod
       use IO_tools_mod
       use single_procedure_mod
       use apply_BCs_faces_bridge_mod

       implicit none
       private
       public :: procedure_array
       public :: init,delete,display,print,export,import

       public :: add

       type procedure_array
         type(single_procedure),dimension(:),allocatable :: SP
         integer :: N
         logical :: defined = .false.
       end type

       interface init;           module procedure init_PA;           end interface
       interface init;           module procedure init_copy_PA;      end interface
       interface delete;         module procedure delete_PA;         end interface
       interface display;        module procedure display_PA;        end interface
       interface print;          module procedure print_PA;          end interface
       interface export;         module procedure export_PA;         end interface
       interface import;         module procedure import_PA;         end interface
       interface export;         module procedure export_PA_wrapper; end interface
       interface import;         module procedure import_PA_wrapper; end interface

       interface insist_defined; module procedure insist_defined_PA; end interface

       interface add;            module procedure add_PA;            end interface

       contains

       subroutine init_PA(PA,N)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: N
         call delete(PA)
         if (N.lt.1) stop 'Error: N must > 1 in init_PA in procedure_array.f90'
         allocate(PA%SP(N))
         PA%N = N
         PA%defined = .false.
       end subroutine

       subroutine init_copy_PA(PA,PA_in)
         implicit none
         type(procedure_array),intent(inout) :: PA
         type(procedure_array),intent(in) :: PA_in
         integer :: i
         ! call insist_defined(PA_in,'init_copy_PA')
         call delete(PA)
         call init(PA,PA_in%N)
         do i=1,PA%N
          call init(PA%SP(i),PA_in%SP(i))
         enddo
         PA%defined = PA_in%defined
         PA%N = PA_in%N
       end subroutine

       subroutine delete_PA(PA)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer :: i
         if (allocated(PA%SP)) then
           do i=1,size(PA%SP); call delete(PA%SP(i)); enddo
           deallocate(PA%SP)
         endif
         PA%N = 0
         PA%defined = .false.
       end subroutine

       subroutine display_PA(PA,un)
         implicit none
         type(procedure_array),intent(in) :: PA
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'PA%N = ',PA%N
         do i=1,PA%N; call display(PA%SP(i),un); enddo
       end subroutine

       subroutine print_PA(PA)
         implicit none
         type(procedure_array),intent(in) :: PA
         call display(PA,6)
       end subroutine

       subroutine export_PA(PA,un)
         implicit none
         type(procedure_array),intent(in) :: PA
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'PA%N = '
         write(un,*) PA%N
         do i=1,PA%N; call export(PA%SP(i),un); enddo
       end subroutine

       subroutine import_PA(PA,un)
         implicit none
         type(procedure_array),intent(inout) :: PA
         integer,intent(in) :: un
         integer :: i
         read(un,*) 
         read(un,*) PA%N
         do i=1,PA%N; call import(PA%SP(i),un); enddo
       end subroutine

       subroutine export_PA_wrapper(PA,dir,name)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(PA,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_PA_wrapper(PA,dir,name)
         implicit none
         type(procedure_array),intent(inout) :: PA
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(PA,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! *****************************************************************
       ! ***************************** OTHER *****************************
       ! *****************************************************************

       subroutine add_PA(PA,P,ID)
         implicit none
         type(procedure_array),intent(inout) :: PA
         procedure(apply_BC_op) :: P
         integer,intent(in) :: ID
         type(procedure_array) :: temp
         integer :: i
         if (PA%defined) then
           call init(temp,PA)
           call init(PA,temp%N+1)
           do i=1,temp%N; call init(PA%SP(i),temp%SP(i)); enddo
           i = PA%N
           call init(PA%SP(i),P,ID)
           call delete(temp)
         else
           call init(PA,1)
           call init(PA%SP(1),P,ID)
         endif
         PA%defined = .true.
       end subroutine

       ! subroutine apply_PA(PA,BF,B)
       !   implicit none
       !   type(procedure_array),intent(in) :: PA
       !   type(block_field),intent(inout) :: BF
       !   type(block),intent(in) :: B
       !   integer :: i
       !   do i=1,PA%N
       !   call PA%SP(i)%P(BF,B)
       !   enddo
       ! end subroutine

       subroutine insist_defined_PA(PA,caller)
         implicit none
         type(procedure_array),intent(in) :: PA
         character(len=*),intent(in) :: caller
         integer :: i
         do i=1,PA%N
         call insist_defined(PA%SP(i),caller)
         enddo
       end subroutine

       end module