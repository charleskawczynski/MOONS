       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_lines_mod
       use IO_tools_mod
       use export_line_mod
       implicit none

       private
       public :: export_lines
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_export_lines;      end interface
       interface delete;       module procedure delete_export_lines;         end interface
       interface display;      module procedure display_export_lines;        end interface
       interface display_short;module procedure display_short_export_lines;  end interface
       interface display;      module procedure display_wrapper_export_lines;end interface
       interface print;        module procedure print_export_lines;          end interface
       interface print_short;  module procedure print_short_export_lines;    end interface
       interface export;       module procedure export_export_lines;         end interface
       interface import;       module procedure import_export_lines;         end interface
       interface export;       module procedure export_wrapper_export_lines; end interface
       interface import;       module procedure import_wrapper_export_lines; end interface

       type export_lines
         type(export_line),dimension(:),allocatable :: EL
         integer :: N = 0
       end type

       contains

       subroutine init_copy_export_lines(this,that)
         implicit none
         type(export_lines),intent(inout) :: this
         type(export_lines),intent(in) :: that
         integer :: i_EL
         integer :: s_EL
         call delete(this)
         if (allocated(that%EL)) then
           s_EL = size(that%EL)
           if (s_EL.gt.0) then
             allocate(this%EL(s_EL))
             do i_EL=1,s_EL
               call init(this%EL(i_EL),that%EL(i_EL))
             enddo
           endif
         endif
         this%N = that%N
       end subroutine

       subroutine delete_export_lines(this)
         implicit none
         type(export_lines),intent(inout) :: this
         integer :: i_EL
         integer :: s_EL
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           do i_EL=1,s_EL
             call delete(this%EL(i_EL))
           enddo
           deallocate(this%EL)
         endif
         this%N = 0
       end subroutine

       subroutine display_export_lines(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_EL
         integer :: s_EL
         write(un,*) ' -------------------- export_lines'
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           do i_EL=1,s_EL
             call display(this%EL(i_EL),un)
           enddo
         endif
         write(un,*) 'N  = ',this%N
       end subroutine

       subroutine display_short_export_lines(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_EL
         integer :: s_EL
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           do i_EL=1,s_EL
             call display(this%EL(i_EL),un)
           enddo
         endif
         write(un,*) 'N  = ',this%N
       end subroutine

       subroutine print_export_lines(this)
         implicit none
         type(export_lines),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_lines(this)
         implicit none
         type(export_lines),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_export_lines(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_EL
         integer :: s_EL
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           write(un,*) s_EL
           do i_EL=1,s_EL
             call export(this%EL(i_EL),un)
           enddo
         endif
         write(un,*) 'N   = ';write(un,*) this%N
       end subroutine

       subroutine import_export_lines(this,un)
         implicit none
         type(export_lines),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_EL
         integer :: s_EL
         call delete(this)
         if (allocated(this%EL)) then
           read(un,*) s_EL
           do i_EL=1,s_EL
             call import(this%EL(i_EL),un)
           enddo
         endif
         read(un,*); read(un,*) this%N
       end subroutine

       subroutine display_wrapper_export_lines(this,dir,name)
         implicit none
         type(export_lines),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_lines(this,dir,name)
         implicit none
         type(export_lines),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_lines(this,dir,name)
         implicit none
         type(export_lines),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module