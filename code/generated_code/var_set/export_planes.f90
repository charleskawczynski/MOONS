       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_planes_mod
       use export_plane_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: export_planes
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_export_planes;              end interface
       interface delete;                 module procedure delete_export_planes;                 end interface
       interface display;                module procedure display_export_planes;                end interface
       interface display_short;          module procedure display_short_export_planes;          end interface
       interface display;                module procedure display_wrap_export_planes;           end interface
       interface print;                  module procedure print_export_planes;                  end interface
       interface print_short;            module procedure print_short_export_planes;            end interface
       interface export;                 module procedure export_export_planes;                 end interface
       interface export_primitives;      module procedure export_primitives_export_planes;      end interface
       interface import;                 module procedure import_export_planes;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_export_planes;end interface
       interface export_structured;      module procedure export_structured_D_export_planes;    end interface
       interface import_structured;      module procedure import_structured_D_export_planes;    end interface
       interface import_primitives;      module procedure import_primitives_export_planes;      end interface
       interface export;                 module procedure export_wrap_export_planes;            end interface
       interface import;                 module procedure import_wrap_export_planes;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_export_planes;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_export_planes;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_export_planes;      end interface

       type export_planes
         type(export_plane),dimension(:),allocatable :: EP
         integer :: N = 0
       end type

       contains

       subroutine init_copy_export_planes(this,that)
         implicit none
         type(export_planes),intent(inout) :: this
         type(export_planes),intent(in) :: that
         integer :: i_EP
         integer :: s_EP
         call delete(this)
         if (allocated(that%EP)) then
           s_EP = size(that%EP)
           if (s_EP.gt.0) then
             allocate(this%EP(s_EP))
             do i_EP=1,s_EP
               call init(this%EP(i_EP),that%EP(i_EP))
             enddo
           endif
         endif
         this%N = that%N
       end subroutine

       subroutine delete_export_planes(this)
         implicit none
         type(export_planes),intent(inout) :: this
         integer :: i_EP
         integer :: s_EP
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           do i_EP=1,s_EP
             call delete(this%EP(i_EP))
           enddo
           deallocate(this%EP)
         endif
         this%N = 0
       end subroutine

       subroutine display_export_planes(this,un)
         implicit none
         type(export_planes),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_EP
         integer :: s_EP
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           do i_EP=1,s_EP
             call display(this%EP(i_EP),un)
           enddo
         endif
         write(un,*) 'N  = ',this%N
       end subroutine

       subroutine display_short_export_planes(this,un)
         implicit none
         type(export_planes),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_EP
         integer :: s_EP
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           do i_EP=1,s_EP
             call display(this%EP(i_EP),un)
           enddo
         endif
         write(un,*) 'N  = ',this%N
       end subroutine

       subroutine display_wrap_export_planes(this,dir,name)
         implicit none
         type(export_planes),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_planes(this)
         implicit none
         type(export_planes),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_planes(this)
         implicit none
         type(export_planes),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_export_planes(this,un)
         implicit none
         type(export_planes),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_EP
         integer :: s_EP
         call export_primitives(this,un)
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           write(un,*) s_EP
           if (s_EP.gt.0) then
             do i_EP=1,s_EP
               call export(this%EP(i_EP),un)
             enddo
           else
             write(un,*) 0
           endif
         endif
       end subroutine

       subroutine import_export_planes(this,un)
         implicit none
         type(export_planes),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_EP
         integer :: s_EP
         call delete(this)
         call import_primitives(this,un)
         read(un,*) s_EP
         if (s_EP.gt.0) then
           allocate(this%EP(s_EP))
           do i_EP=1,s_EP
             call import(this%EP(i_EP),un)
           enddo
         endif
       end subroutine

       subroutine export_primitives_export_planes(this,un)
         implicit none
         type(export_planes),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N   = ';write(un,*) this%N
       end subroutine

       subroutine import_primitives_export_planes(this,un)
         implicit none
         type(export_planes),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%N
       end subroutine

       subroutine export_wrap_export_planes(this,dir,name)
         implicit none
         type(export_planes),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_planes(this,dir,name)
         implicit none
         type(export_planes),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_export_planes(this,dir)
         implicit none
         type(export_planes),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EP
         integer :: s_EP
         call suppress_warnings(this)
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           do i_EP=1,s_EP
             call set_IO_dir(this%EP(i_EP),&
             dir//'EP_'//int2str(i_EP)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine make_IO_dir_export_planes(this,dir)
         implicit none
         type(export_planes),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EP
         integer :: s_EP
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           do i_EP=1,s_EP
             call make_IO_dir(this%EP(i_EP),&
             dir//'EP_'//int2str(i_EP)//fortran_PS)
           enddo
         endif
       end subroutine

       subroutine export_folder_structure_export_planes(this,dir)
         implicit none
         type(export_planes),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EP
         integer :: s_EP
         integer :: un
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           write(un,*) s_EP
           do i_EP=1,s_EP
             call export_structured(this%EP(i_EP),&
             dir//'EP_'//int2str(i_EP)//fortran_PS)
           enddo
         else
           write(un,*) 0
         endif
       end subroutine

       subroutine export_structured_D_export_planes(this,dir)
         implicit none
         type(export_planes),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EP
         integer :: s_EP
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         if (allocated(this%EP)) then
           s_EP = size(this%EP)
           write(un,*) s_EP
           do i_EP=1,s_EP
             call export_structured(this%EP(i_EP),&
             dir//'EP_'//int2str(i_EP)//fortran_PS)
           enddo
         else
           write(un,*) 0
         endif
         close(un)
       end subroutine

       subroutine import_structured_D_export_planes(this,dir)
         implicit none
         type(export_planes),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_EP
         integer :: s_EP
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         read(un,*) s_EP
         if (s_EP.gt.0) then
           if (.not.allocated(this%EP)) then
             allocate(this%EP(s_EP))
           endif
           do i_EP=1,s_EP
             call import_structured(this%EP(i_EP),&
             dir//'EP_'//int2str(i_EP)//fortran_PS)
           enddo
         endif
         close(un)
       end subroutine

       subroutine suppress_warnings_export_planes(this)
         implicit none
         type(export_planes),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module