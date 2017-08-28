       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module procedure_array_mod
       use IO_tools_mod
       use single_procedure_mod
       implicit none

       private
       public :: procedure_array
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_procedure_array;           end interface
       interface delete; module procedure delete_procedure_array;         end interface
       interface display;module procedure display_procedure_array;        end interface
       interface display;module procedure display_wrapper_procedure_array;end interface
       interface print;  module procedure print_procedure_array;          end interface
       interface export; module procedure export_procedure_array;         end interface
       interface import; module procedure import_procedure_array;         end interface
       interface export; module procedure export_wrapper_procedure_array; end interface
       interface import; module procedure import_wrapper_procedure_array; end interface

       type procedure_array
         integer :: n = 0
         type(single_procedure),dimension(:),allocatable :: sp
         logical :: defined = .false.
       end type

       contains

       subroutine init_procedure_array(this,that)
         implicit none
         type(procedure_array),intent(inout) :: this
         type(procedure_array),intent(in) :: that
         integer :: i_sp
         integer :: s_sp
         call delete(this)
         this%n = that%n
         if (allocated(that%sp)) then
           s_sp = size(that%sp)
           if (s_sp.gt.0) then
             allocate(this%sp(s_sp))
             do i_sp=1,s_sp
               call init(this%sp(i_sp),that%sp(i_sp))
             enddo
           endif
         endif
         this%defined = that%defined
       end subroutine

       subroutine delete_procedure_array(this)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer :: i_sp
         integer :: s_sp
         this%n = 0
         if (allocated(this%sp)) then
           s_sp = size(this%sp)
           do i_sp=1,s_sp
             call delete(this%sp(i_sp))
           enddo
           deallocate(this%sp)
         endif
         this%defined = .false.
       end subroutine

       subroutine display_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- procedure_array'
         integer :: i_sp
         integer :: s_sp
         write(un,*) 'n       = ',this%n
         if (allocated(this%sp)) then
           s_sp = size(this%sp)
           do i_sp=1,s_sp
             call display(this%sp(i_sp),un)
           enddo
         endif
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine print_procedure_array(this)
         implicit none
         type(procedure_array),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_sp
         integer :: s_sp
         write(un,*) 'n        = ';write(un,*) this%n
         if (allocated(this%sp)) then
           s_sp = size(this%sp)
           write(un,*) s_sp
           do i_sp=1,s_sp
             call export(this%sp(i_sp),un)
           enddo
         endif
         write(un,*) 'defined  = ';write(un,*) this%defined
       end subroutine

       subroutine import_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_sp
         integer :: s_sp
         call delete(this)
         read(un,*); read(un,*) this%n
         if (allocated(this%sp)) then
           read(un,*) s_sp
           do i_sp=1,s_sp
             call import(this%sp(i_sp),un)
           enddo
         endif
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrapper_procedure_array(this,dir,name)
         implicit none
         type(procedure_array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_procedure_array(this,dir,name)
         implicit none
         type(procedure_array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_procedure_array(this,dir,name)
         implicit none
         type(procedure_array),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module