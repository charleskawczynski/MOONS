       module segment_mod
       use current_precision_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: segment
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_segment;          end interface
       interface init;   module procedure init_many_segment;     end interface
       interface delete; module procedure delete_segment;        end interface
       interface delete; module procedure delete_many_segment;   end interface
       interface display;module procedure display_segment;       end interface
       interface display;module procedure display_many_segment;  end interface
       interface print;  module procedure print_segment;         end interface
       interface print;  module procedure print_many_segment;    end interface
       interface export; module procedure export_segment;        end interface
       interface export; module procedure export_many_segment;   end interface
       interface import; module procedure import_segment;        end interface
       interface import; module procedure import_many_segment;   end interface
       interface export; module procedure export_wrapper_segment;end interface
       interface import; module procedure import_wrapper_segment;end interface

       type segment
         integer :: n_cells = 0
         type(string) :: distribution
         real(cp) :: hmax = 0.0_cp
         real(cp) :: hmin = 0.0_cp
         real(cp) :: l = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: yc = 0.0_cp
         integer :: dir = 0
       end type

       contains

       subroutine init_segment(this,that)
         implicit none
         type(segment),intent(inout) :: this
         type(segment),intent(in) :: that
         call delete(this)
         this%n_cells = that%n_cells
         call init(this%distribution,that%distribution)
         this%hmax = that%hmax
         this%hmin = that%hmin
         this%l = that%l
         this%tau = that%tau
         this%yc = that%yc
         this%dir = that%dir
       end subroutine

       subroutine init_many_segment(this,that)
         implicit none
         type(segment),dimension(:),intent(inout) :: this
         type(segment),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_segment(this)
         implicit none
         type(segment),intent(inout) :: this
         this%n_cells = 0
         call delete(this%distribution)
         this%hmax = 0.0_cp
         this%hmin = 0.0_cp
         this%l = 0.0_cp
         this%tau = 0.0_cp
         this%yc = 0.0_cp
         this%dir = 0
       end subroutine

       subroutine delete_many_segment(this)
         implicit none
         type(segment),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_cells      = ',this%n_cells
         call display(this%distribution,un)
         write(un,*) 'hmax         = ',this%hmax
         write(un,*) 'hmin         = ',this%hmin
         write(un,*) 'l            = ',this%l
         write(un,*) 'tau          = ',this%tau
         write(un,*) 'yc           = ',this%yc
         write(un,*) 'dir          = ',this%dir
       end subroutine

       subroutine display_many_segment(this,un)
         implicit none
         type(segment),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_segment(this)
         implicit none
         type(segment),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_segment(this)
         implicit none
         type(segment),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_segment(this,un)
         implicit none
         type(segment),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%n_cells
         call export(this%distribution,un)
         write(un,*) this%hmax
         write(un,*) this%hmin
         write(un,*) this%l
         write(un,*) this%tau
         write(un,*) this%yc
         write(un,*) this%dir
       end subroutine

       subroutine export_many_segment(this,un)
         implicit none
         type(segment),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_segment(this,un)
         implicit none
         type(segment),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%n_cells
         call import(this%distribution,un)
         read(un,*) this%hmax
         read(un,*) this%hmin
         read(un,*) this%l
         read(un,*) this%tau
         read(un,*) this%yc
         read(un,*) this%dir
       end subroutine

       subroutine import_many_segment(this,un)
         implicit none
         type(segment),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_segment(this,dir,name)
         implicit none
         type(segment),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_segment(this,dir,name)
         implicit none
         type(segment),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module