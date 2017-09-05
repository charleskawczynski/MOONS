       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module boundary_mod
       use IO_tools_mod
       use BC_logicals_mod
       use single_boundary_mod
       use string_mod
       implicit none

       private
       public :: boundary
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_bo;    end interface
       interface delete;       module procedure delete_bo;       end interface
       interface display;      module procedure display_bo;      end interface
       interface display_short;module procedure display_short_bo;end interface
       interface display;      module procedure display_wrap_bo; end interface
       interface print;        module procedure print_bo;        end interface
       interface print_short;  module procedure print_short_bo;  end interface
       interface export;       module procedure export_bo;       end interface
       interface import;       module procedure import_bo;       end interface
       interface export;       module procedure export_wrap_bo;  end interface
       interface import;       module procedure import_wrap_bo;  end interface

       type boundary
         integer :: n = 0
         type(single_boundary),dimension(:),allocatable :: SB
         type(string) :: name
         type(BC_logicals) :: BCL
       end type

       contains

       subroutine init_copy_bo(this,that)
         implicit none
         type(boundary),intent(inout) :: this
         type(boundary),intent(in) :: that
         integer :: i_SB
         integer :: s_SB
         call delete(this)
         this%n = that%n
         if (allocated(that%SB)) then
           s_SB = size(that%SB)
           if (s_SB.gt.0) then
             allocate(this%SB(s_SB))
             do i_SB=1,s_SB
               call init(this%SB(i_SB),that%SB(i_SB))
             enddo
           endif
         endif
         call init(this%name,that%name)
         call init(this%BCL,that%BCL)
       end subroutine

       subroutine delete_bo(this)
         implicit none
         type(boundary),intent(inout) :: this
         integer :: i_SB
         integer :: s_SB
         this%n = 0
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call delete(this%SB(i_SB))
           enddo
           deallocate(this%SB)
         endif
         call delete(this%name)
         call delete(this%BCL)
       end subroutine

       subroutine display_bo(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         write(un,*) ' -------------------- boundary'
         write(un,*) 'n    = ',this%n
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call display(this%SB(i_SB),un)
           enddo
         endif
         call display(this%name,un)
         call display(this%BCL,un)
       end subroutine

       subroutine display_short_bo(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         write(un,*) 'n    = ',this%n
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           do i_SB=1,s_SB
             call display(this%SB(i_SB),un)
           enddo
         endif
         call display(this%name,un)
         call display(this%BCL,un)
       end subroutine

       subroutine print_bo(this)
         implicit none
         type(boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_bo(this)
         implicit none
         type(boundary),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_bo(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         write(un,*) 'n     = ';write(un,*) this%n
         if (allocated(this%SB)) then
           s_SB = size(this%SB)
           write(un,*) s_SB
           do i_SB=1,s_SB
             call export(this%SB(i_SB),un)
           enddo
         endif
         call export(this%name,un)
         call export(this%BCL,un)
       end subroutine

       subroutine import_bo(this,un)
         implicit none
         type(boundary),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_SB
         integer :: s_SB
         call delete(this)
         read(un,*); read(un,*) this%n
         if (allocated(this%SB)) then
           read(un,*) s_SB
           do i_SB=1,s_SB
             call import(this%SB(i_SB),un)
           enddo
         endif
         call import(this%name,un)
         call import(this%BCL,un)
       end subroutine

       subroutine display_wrap_bo(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_bo(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_bo(this,dir,name)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module