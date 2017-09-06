       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module block_mod
       use IO_tools_mod
       use grid_mod
       use grid_field_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_bl;    end interface
       interface delete;       module procedure delete_bl;       end interface
       interface display;      module procedure display_bl;      end interface
       interface display_short;module procedure display_short_bl;end interface
       interface display;      module procedure display_wrap_bl; end interface
       interface print;        module procedure print_bl;        end interface
       interface print_short;  module procedure print_short_bl;  end interface
       interface export;       module procedure export_bl;       end interface
       interface import;       module procedure import_bl;       end interface
       interface export;       module procedure export_wrap_bl;  end interface
       interface import;       module procedure import_wrap_bl;  end interface

       type block
         type(grid) :: g
         type(grid),dimension(:),allocatable :: f
         type(grid),dimension(:),allocatable :: e
         type(grid),dimension(:),allocatable :: c
         type(grid),dimension(:),allocatable :: fb
         type(grid),dimension(:),allocatable :: eb
         type(grid),dimension(:),allocatable :: cb
         type(grid_field),dimension(:),allocatable :: vol
         integer,dimension(6) :: apply_BC_order = 0
       end type

       contains

       subroutine init_copy_bl(this,that)
         implicit none
         type(block),intent(inout) :: this
         type(block),intent(in) :: that
         integer :: i_f
         integer :: i_e
         integer :: i_c
         integer :: i_fb
         integer :: i_eb
         integer :: i_cb
         integer :: i_vol
         integer :: s_f
         integer :: s_e
         integer :: s_c
         integer :: s_fb
         integer :: s_eb
         integer :: s_cb
         integer :: s_vol
         call delete(this)
         call init(this%g,that%g)
         if (allocated(that%f)) then
           s_f = size(that%f)
           if (s_f.gt.0) then
             allocate(this%f(s_f))
             do i_f=1,s_f
               call init(this%f(i_f),that%f(i_f))
             enddo
           endif
         endif
         if (allocated(that%e)) then
           s_e = size(that%e)
           if (s_e.gt.0) then
             allocate(this%e(s_e))
             do i_e=1,s_e
               call init(this%e(i_e),that%e(i_e))
             enddo
           endif
         endif
         if (allocated(that%c)) then
           s_c = size(that%c)
           if (s_c.gt.0) then
             allocate(this%c(s_c))
             do i_c=1,s_c
               call init(this%c(i_c),that%c(i_c))
             enddo
           endif
         endif
         if (allocated(that%fb)) then
           s_fb = size(that%fb)
           if (s_fb.gt.0) then
             allocate(this%fb(s_fb))
             do i_fb=1,s_fb
               call init(this%fb(i_fb),that%fb(i_fb))
             enddo
           endif
         endif
         if (allocated(that%eb)) then
           s_eb = size(that%eb)
           if (s_eb.gt.0) then
             allocate(this%eb(s_eb))
             do i_eb=1,s_eb
               call init(this%eb(i_eb),that%eb(i_eb))
             enddo
           endif
         endif
         if (allocated(that%cb)) then
           s_cb = size(that%cb)
           if (s_cb.gt.0) then
             allocate(this%cb(s_cb))
             do i_cb=1,s_cb
               call init(this%cb(i_cb),that%cb(i_cb))
             enddo
           endif
         endif
         if (allocated(that%vol)) then
           s_vol = size(that%vol)
           if (s_vol.gt.0) then
             allocate(this%vol(s_vol))
             do i_vol=1,s_vol
               call init(this%vol(i_vol),that%vol(i_vol))
             enddo
           endif
         endif
         this%apply_BC_order = that%apply_BC_order
       end subroutine

       subroutine delete_bl(this)
         implicit none
         type(block),intent(inout) :: this
         integer :: i_f
         integer :: i_e
         integer :: i_c
         integer :: i_fb
         integer :: i_eb
         integer :: i_cb
         integer :: i_vol
         integer :: s_f
         integer :: s_e
         integer :: s_c
         integer :: s_fb
         integer :: s_eb
         integer :: s_cb
         integer :: s_vol
         call delete(this%g)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call delete(this%f(i_f))
           enddo
           deallocate(this%f)
         endif
         if (allocated(this%e)) then
           s_e = size(this%e)
           do i_e=1,s_e
             call delete(this%e(i_e))
           enddo
           deallocate(this%e)
         endif
         if (allocated(this%c)) then
           s_c = size(this%c)
           do i_c=1,s_c
             call delete(this%c(i_c))
           enddo
           deallocate(this%c)
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call delete(this%fb(i_fb))
           enddo
           deallocate(this%fb)
         endif
         if (allocated(this%eb)) then
           s_eb = size(this%eb)
           do i_eb=1,s_eb
             call delete(this%eb(i_eb))
           enddo
           deallocate(this%eb)
         endif
         if (allocated(this%cb)) then
           s_cb = size(this%cb)
           do i_cb=1,s_cb
             call delete(this%cb(i_cb))
           enddo
           deallocate(this%cb)
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           do i_vol=1,s_vol
             call delete(this%vol(i_vol))
           enddo
           deallocate(this%vol)
         endif
         this%apply_BC_order = 0
       end subroutine

       subroutine display_bl(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_e
         integer :: i_c
         integer :: i_fb
         integer :: i_eb
         integer :: i_cb
         integer :: i_vol
         integer :: s_f
         integer :: s_e
         integer :: s_c
         integer :: s_fb
         integer :: s_eb
         integer :: s_cb
         integer :: s_vol
         write(un,*) ' -------------------- block'
         call display(this%g,un)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call display(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%e)) then
           s_e = size(this%e)
           do i_e=1,s_e
             call display(this%e(i_e),un)
           enddo
         endif
         if (allocated(this%c)) then
           s_c = size(this%c)
           do i_c=1,s_c
             call display(this%c(i_c),un)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call display(this%fb(i_fb),un)
           enddo
         endif
         if (allocated(this%eb)) then
           s_eb = size(this%eb)
           do i_eb=1,s_eb
             call display(this%eb(i_eb),un)
           enddo
         endif
         if (allocated(this%cb)) then
           s_cb = size(this%cb)
           do i_cb=1,s_cb
             call display(this%cb(i_cb),un)
           enddo
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           do i_vol=1,s_vol
             call display(this%vol(i_vol),un)
           enddo
         endif
         write(un,*) 'apply_BC_order = ',this%apply_BC_order
       end subroutine

       subroutine display_short_bl(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_e
         integer :: i_c
         integer :: i_fb
         integer :: i_eb
         integer :: i_cb
         integer :: i_vol
         integer :: s_f
         integer :: s_e
         integer :: s_c
         integer :: s_fb
         integer :: s_eb
         integer :: s_cb
         integer :: s_vol
         call display(this%g,un)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call display(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%e)) then
           s_e = size(this%e)
           do i_e=1,s_e
             call display(this%e(i_e),un)
           enddo
         endif
         if (allocated(this%c)) then
           s_c = size(this%c)
           do i_c=1,s_c
             call display(this%c(i_c),un)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call display(this%fb(i_fb),un)
           enddo
         endif
         if (allocated(this%eb)) then
           s_eb = size(this%eb)
           do i_eb=1,s_eb
             call display(this%eb(i_eb),un)
           enddo
         endif
         if (allocated(this%cb)) then
           s_cb = size(this%cb)
           do i_cb=1,s_cb
             call display(this%cb(i_cb),un)
           enddo
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           do i_vol=1,s_vol
             call display(this%vol(i_vol),un)
           enddo
         endif
         write(un,*) 'apply_BC_order = ',this%apply_BC_order
       end subroutine

       subroutine print_bl(this)
         implicit none
         type(block),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_bl(this)
         implicit none
         type(block),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_bl(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_e
         integer :: i_c
         integer :: i_fb
         integer :: i_eb
         integer :: i_cb
         integer :: i_vol
         integer :: s_f
         integer :: s_e
         integer :: s_c
         integer :: s_fb
         integer :: s_eb
         integer :: s_cb
         integer :: s_vol
         call export(this%g,un)
         if (allocated(this%f)) then
           s_f = size(this%f)
           write(un,*) s_f
           do i_f=1,s_f
             call export(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%e)) then
           s_e = size(this%e)
           write(un,*) s_e
           do i_e=1,s_e
             call export(this%e(i_e),un)
           enddo
         endif
         if (allocated(this%c)) then
           s_c = size(this%c)
           write(un,*) s_c
           do i_c=1,s_c
             call export(this%c(i_c),un)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           write(un,*) s_fb
           do i_fb=1,s_fb
             call export(this%fb(i_fb),un)
           enddo
         endif
         if (allocated(this%eb)) then
           s_eb = size(this%eb)
           write(un,*) s_eb
           do i_eb=1,s_eb
             call export(this%eb(i_eb),un)
           enddo
         endif
         if (allocated(this%cb)) then
           s_cb = size(this%cb)
           write(un,*) s_cb
           do i_cb=1,s_cb
             call export(this%cb(i_cb),un)
           enddo
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           write(un,*) s_vol
           do i_vol=1,s_vol
             call export(this%vol(i_vol),un)
           enddo
         endif
         write(un,*) 'apply_BC_order  = ';write(un,*) this%apply_BC_order
       end subroutine

       subroutine import_bl(this,un)
         implicit none
         type(block),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_e
         integer :: i_c
         integer :: i_fb
         integer :: i_eb
         integer :: i_cb
         integer :: i_vol
         integer :: s_f
         integer :: s_e
         integer :: s_c
         integer :: s_fb
         integer :: s_eb
         integer :: s_cb
         integer :: s_vol
         call delete(this)
         call import(this%g,un)
         if (allocated(this%f)) then
           read(un,*) s_f
           do i_f=1,s_f
             call import(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%e)) then
           read(un,*) s_e
           do i_e=1,s_e
             call import(this%e(i_e),un)
           enddo
         endif
         if (allocated(this%c)) then
           read(un,*) s_c
           do i_c=1,s_c
             call import(this%c(i_c),un)
           enddo
         endif
         if (allocated(this%fb)) then
           read(un,*) s_fb
           do i_fb=1,s_fb
             call import(this%fb(i_fb),un)
           enddo
         endif
         if (allocated(this%eb)) then
           read(un,*) s_eb
           do i_eb=1,s_eb
             call import(this%eb(i_eb),un)
           enddo
         endif
         if (allocated(this%cb)) then
           read(un,*) s_cb
           do i_cb=1,s_cb
             call import(this%cb(i_cb),un)
           enddo
         endif
         if (allocated(this%vol)) then
           read(un,*) s_vol
           do i_vol=1,s_vol
             call import(this%vol(i_vol),un)
           enddo
         endif
         read(un,*); read(un,*) this%apply_BC_order
       end subroutine

       subroutine display_wrap_bl(this,dir,name)
         implicit none
         type(block),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_bl(this,dir,name)
         implicit none
         type(block),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_bl(this,dir,name)
         implicit none
         type(block),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module