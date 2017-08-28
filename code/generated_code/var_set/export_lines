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

       interface init;   module procedure init_export_lines;           end interface
       interface delete; module procedure delete_export_lines;         end interface
       interface display;module procedure display_export_lines;        end interface
       interface display;module procedure display_wrapper_export_lines;end interface
       interface print;  module procedure print_export_lines;          end interface
       interface export; module procedure export_export_lines;         end interface
       interface import; module procedure import_export_lines;         end interface
       interface export; module procedure export_wrapper_export_lines; end interface
       interface import; module procedure import_wrapper_export_lines; end interface

       type export_lines
         type(export_line),dimension(:),allocatable :: el
         integer :: n = 0
       end type

       contains

       subroutine init_export_lines(this,that)
         implicit none
         type(export_lines),intent(inout) :: this
         type(export_lines),intent(in) :: that
         integer :: i_el
         integer :: s_el
         call delete(this)
         if (allocated(that%el)) then
           s_el = size(that%el)
           if (s_el.gt.0) then
             allocate(this%el(s_el))
             do i_el=1,s_el
               call init(this%el(i_el),that%el(i_el))
             enddo
           endif
         endif
         this%n = that%n
       end subroutine

       subroutine delete_export_lines(this)
         implicit none
         type(export_lines),intent(inout) :: this
         integer :: i_el
         integer :: s_el
         if (allocated(this%el)) then
           s_el = size(this%el)
           do i_el=1,s_el
             call delete(this%el(i_el))
           enddo
           deallocate(this%el)
         endif
         this%n = 0
       end subroutine

       subroutine display_export_lines(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_lines'
         integer :: i_el
         integer :: s_el
         if (allocated(this%el)) then
           s_el = size(this%el)
           do i_el=1,s_el
             call display(this%el(i_el),un)
           enddo
         endif
         write(un,*) 'n  = ',this%n
       end subroutine

       subroutine print_export_lines(this)
         implicit none
         type(export_lines),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_lines(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_el
         integer :: s_el
         if (allocated(this%el)) then
           s_el = size(this%el)
           write(un,*) s_el
           do i_el=1,s_el
             call export(this%el(i_el),un)
           enddo
         endif
         write(un,*) 'n   = ';write(un,*) this%n
       end subroutine

       subroutine import_export_lines(this,un)
         implicit none
         type(export_lines),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_el
         integer :: s_el
         call delete(this)
         if (allocated(this%el)) then
           read(un,*) s_el
           do i_el=1,s_el
             call import(this%el(i_el),un)
           enddo
         endif
         read(un,*); read(un,*) this%n
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
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module