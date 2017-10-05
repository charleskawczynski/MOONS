       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module procedure_array_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use single_procedure_mod
       use string_mod
       implicit none

       private
       public :: procedure_array
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_procedure_array;          end interface
       interface delete;           module procedure delete_procedure_array;             end interface
       interface display;          module procedure display_procedure_array;            end interface
       interface display_short;    module procedure display_short_procedure_array;      end interface
       interface display;          module procedure display_wrap_procedure_array;       end interface
       interface print;            module procedure print_procedure_array;              end interface
       interface print_short;      module procedure print_short_procedure_array;        end interface
       interface export;           module procedure export_procedure_array;             end interface
       interface export_primitives;module procedure export_primitives_procedure_array;  end interface
       interface import;           module procedure import_procedure_array;             end interface
       interface export_structured;module procedure export_structured_D_procedure_array;end interface
       interface import_structured;module procedure import_structured_D_procedure_array;end interface
       interface import_primitives;module procedure import_primitives_procedure_array;  end interface
       interface export;           module procedure export_wrap_procedure_array;        end interface
       interface import;           module procedure import_wrap_procedure_array;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_procedure_array;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_procedure_array;        end interface
       interface suppress_warnings;module procedure suppress_warnings_procedure_array;  end interface

       type procedure_array
         integer :: N = 0
         type(single_procedure),dimension(:),allocatable :: SP
         logical :: defined = .false.
       end type

       contains

       subroutine init_copy_procedure_array(this,that)
         implicit none
         type(procedure_array),intent(inout) :: this
         type(procedure_array),intent(in) :: that
         integer :: i_SP
         integer :: s_SP
         call delete(this)
         this%N = that%N
         if (allocated(that%SP)) then
           s_SP = size(that%SP)
           if (s_SP.gt.0) then
             allocate(this%SP(s_SP))
             do i_SP=1,s_SP
               call init(this%SP(i_SP),that%SP(i_SP))
             enddo
           endif
         endif
         this%defined = that%defined
       end subroutine

       subroutine delete_procedure_array(this)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer :: i_SP
         integer :: s_SP
         this%N = 0
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           do i_SP=1,s_SP
             call delete(this%SP(i_SP))
           enddo
           deallocate(this%SP)
         endif
         this%defined = .false.
       end subroutine

       subroutine display_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SP
         integer :: s_SP
         write(un,*) 'N       = ',this%N
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           do i_SP=1,s_SP
             call display(this%SP(i_SP),un)
           enddo
         endif
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_short_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SP
         integer :: s_SP
         write(un,*) 'N       = ',this%N
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           do i_SP=1,s_SP
             call display(this%SP(i_SP),un)
           enddo
         endif
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_wrap_procedure_array(this,dir,name)
         implicit none
         type(procedure_array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_procedure_array(this)
         implicit none
         type(procedure_array),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_procedure_array(this)
         implicit none
         type(procedure_array),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SP
         integer :: s_SP
         call export_primitives(this,un)
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           write(un,*) s_SP
           if (s_SP.gt.0) then
             do i_SP=1,s_SP
               call export(this%SP(i_SP),un)
             enddo
           else
             write(un,*) 0
           endif
         endif
       end subroutine

       subroutine import_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_SP
         integer :: s_SP
         call delete(this)
         call import_primitives(this,un)
         read(un,*) s_SP
         if (s_SP.gt.0) then
           allocate(this%SP(s_SP))
           do i_SP=1,s_SP
             call import(this%SP(i_SP),un)
           enddo
         endif
       end subroutine

       subroutine export_primitives_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N        = ';write(un,*) this%N
         write(un,*) 'defined  = ';write(un,*) this%defined
       end subroutine

       subroutine import_primitives_procedure_array(this,un)
         implicit none
         type(procedure_array),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%N
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine export_wrap_procedure_array(this,dir,name)
         implicit none
         type(procedure_array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_procedure_array(this,dir,name)
         implicit none
         type(procedure_array),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_procedure_array(this,dir)
         implicit none
         type(procedure_array),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SP
         integer :: s_SP
         call suppress_warnings(this)
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           do i_SP=1,s_SP
             call set_IO_dir(this%SP(i_SP),&
             dir//'SP_'//int2str(i_SP)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine make_IO_dir_procedure_array(this,dir)
         implicit none
         type(procedure_array),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SP
         integer :: s_SP
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           do i_SP=1,s_SP
             call make_IO_dir(this%SP(i_SP),&
             dir//'SP_'//int2str(i_SP)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine export_structured_D_procedure_array(this,dir)
         implicit none
         type(procedure_array),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SP
         integer :: s_SP
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         if (allocated(this%SP)) then
           s_SP = size(this%SP)
           write(un,*) s_SP
           do i_SP=1,s_SP
             call export_structured(this%SP(i_SP),&
             dir//'SP_'//int2str(i_SP)//fortran_PS)
           enddo
         else
           write(un,*) 0
         endif
         close(un)
       end subroutine

       subroutine import_structured_D_procedure_array(this,dir)
         implicit none
         type(procedure_array),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_SP
         integer :: s_SP
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         read(un,*) s_SP
         if (s_SP.gt.0) then
           allocate(this%SP(s_SP))
           s_SP = size(this%SP)
           do i_SP=1,s_SP
             call import_structured(this%SP(i_SP),&
             dir//'SP_'//int2str(i_SP)//fortran_PS)
           enddo
         endif
         close(un)
       end subroutine

       subroutine suppress_warnings_procedure_array(this)
         implicit none
         type(procedure_array),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module