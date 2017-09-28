       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module block_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use grid_mod
       use grid_field_mod
       use string_mod
       implicit none

       private
       public :: block
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_block;        end interface
       interface delete;           module procedure delete_block;           end interface
       interface display;          module procedure display_block;          end interface
       interface display_short;    module procedure display_short_block;    end interface
       interface display;          module procedure display_wrap_block;     end interface
       interface print;            module procedure print_block;            end interface
       interface print_short;      module procedure print_short_block;      end interface
       interface export;           module procedure export_block;           end interface
       interface export_primitives;module procedure export_primitives_block;end interface
       interface export_restart;   module procedure export_restart_block;   end interface
       interface import;           module procedure import_block;           end interface
       interface import_restart;   module procedure import_restart_block;   end interface
       interface import_primitives;module procedure import_primitives_block;end interface
       interface export;           module procedure export_wrap_block;      end interface
       interface import;           module procedure import_wrap_block;      end interface
       interface make_restart_dir; module procedure make_restart_dir_block; end interface
       interface suppress_warnings;module procedure suppress_warnings_block;end interface

       type block
         type(grid) :: g
         type(grid),dimension(:),allocatable :: f
         type(grid),dimension(:),allocatable :: fb
         type(grid_field),dimension(:),allocatable :: vol
         integer,dimension(6) :: apply_BC_order = 0
       end type

       contains

       subroutine init_copy_block(this,that)
         implicit none
         type(block),intent(inout) :: this
         type(block),intent(in) :: that
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
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
         if (allocated(that%fb)) then
           s_fb = size(that%fb)
           if (s_fb.gt.0) then
             allocate(this%fb(s_fb))
             do i_fb=1,s_fb
               call init(this%fb(i_fb),that%fb(i_fb))
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

       subroutine delete_block(this)
         implicit none
         type(block),intent(inout) :: this
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         call delete(this%g)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call delete(this%f(i_f))
           enddo
           deallocate(this%f)
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call delete(this%fb(i_fb))
           enddo
           deallocate(this%fb)
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

       subroutine display_block(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         call display(this%g,un)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call display(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call display(this%fb(i_fb),un)
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

       subroutine display_short_block(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         call display(this%g,un)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call display(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call display(this%fb(i_fb),un)
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

       subroutine display_wrap_block(this,dir,name)
         implicit none
         type(block),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_block(this)
         implicit none
         type(block),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_block(this)
         implicit none
         type(block),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_block(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'apply_BC_order  = ';write(un,*) this%apply_BC_order
       end subroutine

       subroutine export_block(this,un)
         implicit none
         type(block),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         call export(this%g,un)
         if (allocated(this%f)) then
           s_f = size(this%f)
           write(un,*) s_f
           do i_f=1,s_f
             call export(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           write(un,*) s_fb
           do i_fb=1,s_fb
             call export(this%fb(i_fb),un)
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

       subroutine import_primitives_block(this,un)
         implicit none
         type(block),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%apply_BC_order
       end subroutine

       subroutine import_block(this,un)
         implicit none
         type(block),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         call delete(this)
         call import(this%g,un)
         if (allocated(this%f)) then
           read(un,*) s_f
           do i_f=1,s_f
             call import(this%f(i_f),un)
           enddo
         endif
         if (allocated(this%fb)) then
           read(un,*) s_fb
           do i_fb=1,s_fb
             call import(this%fb(i_fb),un)
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

       subroutine export_wrap_block(this,dir,name)
         implicit none
         type(block),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_block(this,dir,name)
         implicit none
         type(block),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_block(this,dir)
         implicit none
         type(block),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%g,dir//'g'//fortran_PS)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call make_restart_dir(this%f(i_f),&
             dir//'f_'//int2str(i_f)//fortran_PS)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call make_restart_dir(this%fb(i_fb),&
             dir//'fb_'//int2str(i_fb)//fortran_PS)
           enddo
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           do i_vol=1,s_vol
             call make_restart_dir(this%vol(i_vol),&
             dir//'vol_'//int2str(i_vol)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine export_restart_block(this,dir)
         implicit none
         type(block),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%g,dir//'g'//fortran_PS)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call export_restart(this%f(i_f),&
             dir//'f_'//int2str(i_f)//fortran_PS)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call export_restart(this%fb(i_fb),&
             dir//'fb_'//int2str(i_fb)//fortran_PS)
           enddo
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           do i_vol=1,s_vol
             call export_restart(this%vol(i_vol),&
             dir//'vol_'//int2str(i_vol)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine import_restart_block(this,dir)
         implicit none
         type(block),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_f
         integer :: i_fb
         integer :: i_vol
         integer :: s_f
         integer :: s_fb
         integer :: s_vol
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%g,dir//'g'//fortran_PS)
         if (allocated(this%f)) then
           s_f = size(this%f)
           do i_f=1,s_f
             call import_restart(this%f(i_f),&
             dir//'f_'//int2str(i_f)//fortran_PS)
           enddo
         endif
         if (allocated(this%fb)) then
           s_fb = size(this%fb)
           do i_fb=1,s_fb
             call import_restart(this%fb(i_fb),&
             dir//'fb_'//int2str(i_fb)//fortran_PS)
           enddo
         endif
         if (allocated(this%vol)) then
           s_vol = size(this%vol)
           do i_vol=1,s_vol
             call import_restart(this%vol(i_vol),&
             dir//'vol_'//int2str(i_vol)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine suppress_warnings_block(this)
         implicit none
         type(block),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module