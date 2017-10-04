       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_lines_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use export_line_mod
       use string_mod
       implicit none

       private
       public :: export_lines
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_lines;          end interface
       interface delete;           module procedure delete_export_lines;             end interface
       interface display;          module procedure display_export_lines;            end interface
       interface display_short;    module procedure display_short_export_lines;      end interface
       interface display;          module procedure display_wrap_export_lines;       end interface
       interface print;            module procedure print_export_lines;              end interface
       interface print_short;      module procedure print_short_export_lines;        end interface
       interface export;           module procedure export_export_lines;             end interface
       interface export_primitives;module procedure export_primitives_export_lines;  end interface
       interface import;           module procedure import_export_lines;             end interface
       interface export_structured;module procedure export_structured_D_export_lines;end interface
       interface import_structured;module procedure import_structured_D_export_lines;end interface
       interface import_primitives;module procedure import_primitives_export_lines;  end interface
       interface export;           module procedure export_wrap_export_lines;        end interface
       interface import;           module procedure import_wrap_export_lines;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_export_lines;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_export_lines;        end interface
       interface suppress_warnings;module procedure suppress_warnings_export_lines;  end interface

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

       subroutine display_wrap_export_lines(this,dir,name)
         implicit none
         type(export_lines),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
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
           if (s_EL.gt.0) then
             do i_EL=1,s_EL
               call export(this%EL(i_EL),un)
             enddo
           else
             write(un,*) 0
           endif
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
         read(un,*) s_EL
         if (s_EL.gt.0) then
           allocate(this%EL(s_EL))
           do i_EL=1,s_EL
             call import(this%EL(i_EL),un)
           enddo
         endif
         read(un,*); read(un,*) this%N
       end subroutine

       subroutine export_primitives_export_lines(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N   = ';write(un,*) this%N
       end subroutine

       subroutine import_primitives_export_lines(this,un)
         implicit none
         type(export_lines),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%N
       end subroutine

       subroutine export_wrap_export_lines(this,dir,name)
         implicit none
         type(export_lines),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_lines(this,dir,name)
         implicit none
         type(export_lines),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_export_lines(this,dir)
         implicit none
         type(export_lines),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EL
         integer :: s_EL
         call suppress_warnings(this)
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           do i_EL=1,s_EL
             call set_IO_dir(this%EL(i_EL),&
             dir//'EL_'//int2str(i_EL)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine make_IO_dir_export_lines(this,dir)
         implicit none
         type(export_lines),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EL
         integer :: s_EL
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           do i_EL=1,s_EL
             call make_IO_dir(this%EL(i_EL),&
             dir//'EL_'//int2str(i_EL)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine export_structured_D_export_lines(this,dir)
         implicit none
         type(export_lines),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EL
         integer :: s_EL
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         if (allocated(this%EL)) then
           s_EL = size(this%EL)
           write(un,*) s_EL
           do i_EL=1,s_EL
             call export_structured(this%EL(i_EL),&
             dir//'EL_'//int2str(i_EL)//fortran_PS)
           enddo
         else
           write(un,*) 0
         endif
       end subroutine

       subroutine import_structured_D_export_lines(this,dir)
         implicit none
         type(export_lines),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EL
         integer :: s_EL
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         read(un,*) s_EL
         if (s_EL.gt.0) then
           allocate(this%EL(s_EL))
           s_EL = size(this%EL)
           do i_EL=1,s_EL
             call import_structured(this%EL(i_EL),&
             dir//'EL_'//int2str(i_EL)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine suppress_warnings_export_lines(this)
         implicit none
         type(export_lines),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module