       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_params_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use mesh_quality_params_mod
       use segment_mod
       use string_mod
       implicit none

       private
       public :: mesh_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_mesh_params;        end interface
       interface delete;           module procedure delete_mesh_params;           end interface
       interface display;          module procedure display_mesh_params;          end interface
       interface display_short;    module procedure display_short_mesh_params;    end interface
       interface display;          module procedure display_wrap_mesh_params;     end interface
       interface print;            module procedure print_mesh_params;            end interface
       interface print_short;      module procedure print_short_mesh_params;      end interface
       interface export;           module procedure export_mesh_params;           end interface
       interface export_primitives;module procedure export_primitives_mesh_params;end interface
       interface export_restart;   module procedure export_restart_mesh_params;   end interface
       interface import;           module procedure import_mesh_params;           end interface
       interface import_restart;   module procedure import_restart_mesh_params;   end interface
       interface import_primitives;module procedure import_primitives_mesh_params;end interface
       interface export;           module procedure export_wrap_mesh_params;      end interface
       interface import;           module procedure import_wrap_mesh_params;      end interface
       interface make_restart_dir; module procedure make_restart_dir_mesh_params; end interface
       interface suppress_warnings;module procedure suppress_warnings_mesh_params;end interface

       type mesh_params
         type(mesh_quality_params) :: MQP
         type(segment),dimension(:),allocatable :: s_base
         type(segment),dimension(:),allocatable :: s_ext
         integer :: N_base = 0
         integer :: N_ext = 0
       end type

       contains

       subroutine init_copy_mesh_params(this,that)
         implicit none
         type(mesh_params),intent(inout) :: this
         type(mesh_params),intent(in) :: that
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call delete(this)
         call init(this%MQP,that%MQP)
         if (allocated(that%s_base)) then
           s_s_base = size(that%s_base)
           if (s_s_base.gt.0) then
             allocate(this%s_base(s_s_base))
             do i_s_base=1,s_s_base
               call init(this%s_base(i_s_base),that%s_base(i_s_base))
             enddo
           endif
         endif
         if (allocated(that%s_ext)) then
           s_s_ext = size(that%s_ext)
           if (s_s_ext.gt.0) then
             allocate(this%s_ext(s_s_ext))
             do i_s_ext=1,s_s_ext
               call init(this%s_ext(i_s_ext),that%s_ext(i_s_ext))
             enddo
           endif
         endif
         this%N_base = that%N_base
         this%N_ext = that%N_ext
       end subroutine

       subroutine delete_mesh_params(this)
         implicit none
         type(mesh_params),intent(inout) :: this
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call delete(this%MQP)
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           do i_s_base=1,s_s_base
             call delete(this%s_base(i_s_base))
           enddo
           deallocate(this%s_base)
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           do i_s_ext=1,s_s_ext
             call delete(this%s_ext(i_s_ext))
           enddo
           deallocate(this%s_ext)
         endif
         this%N_base = 0
         this%N_ext = 0
       end subroutine

       subroutine display_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call display(this%MQP,un)
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           do i_s_base=1,s_s_base
             call display(this%s_base(i_s_base),un)
           enddo
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           do i_s_ext=1,s_s_ext
             call display(this%s_ext(i_s_ext),un)
           enddo
         endif
         write(un,*) 'N_base = ',this%N_base
         write(un,*) 'N_ext  = ',this%N_ext
       end subroutine

       subroutine display_short_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call display(this%MQP,un)
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           do i_s_base=1,s_s_base
             call display(this%s_base(i_s_base),un)
           enddo
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           do i_s_ext=1,s_s_ext
             call display(this%s_ext(i_s_ext),un)
           enddo
         endif
         write(un,*) 'N_base = ',this%N_base
         write(un,*) 'N_ext  = ',this%N_ext
       end subroutine

       subroutine display_wrap_mesh_params(this,dir,name)
         implicit none
         type(mesh_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_mesh_params(this)
         implicit none
         type(mesh_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mesh_params(this)
         implicit none
         type(mesh_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N_base  = ';write(un,*) this%N_base
         write(un,*) 'N_ext   = ';write(un,*) this%N_ext
       end subroutine

       subroutine export_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call export(this%MQP,un)
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           write(un,*) s_s_base
           do i_s_base=1,s_s_base
             call export(this%s_base(i_s_base),un)
           enddo
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           write(un,*) s_s_ext
           do i_s_ext=1,s_s_ext
             call export(this%s_ext(i_s_ext),un)
           enddo
         endif
         write(un,*) 'N_base  = ';write(un,*) this%N_base
         write(un,*) 'N_ext   = ';write(un,*) this%N_ext
       end subroutine

       subroutine import_primitives_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%N_base
         read(un,*); read(un,*) this%N_ext
       end subroutine

       subroutine import_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call delete(this)
         call import(this%MQP,un)
         if (allocated(this%s_base)) then
           read(un,*) s_s_base
           do i_s_base=1,s_s_base
             call import(this%s_base(i_s_base),un)
           enddo
         endif
         if (allocated(this%s_ext)) then
           read(un,*) s_s_ext
           do i_s_ext=1,s_s_ext
             call import(this%s_ext(i_s_ext),un)
           enddo
         endif
         read(un,*); read(un,*) this%N_base
         read(un,*); read(un,*) this%N_ext
       end subroutine

       subroutine export_wrap_mesh_params(this,dir,name)
         implicit none
         type(mesh_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mesh_params(this,dir,name)
         implicit none
         type(mesh_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_mesh_params(this,dir)
         implicit none
         type(mesh_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%MQP,dir//fortran_PS//'MQP')
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           do i_s_base=1,s_s_base
             call make_restart_dir(this%s_base(i_s_base),&
             dir//fortran_PS//'s_base_'//int2str(i_s_base))
           enddo
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           do i_s_ext=1,s_s_ext
             call make_restart_dir(this%s_ext(i_s_ext),&
             dir//fortran_PS//'s_ext_'//int2str(i_s_ext))
           enddo
         endif
       end subroutine

       subroutine export_restart_mesh_params(this,dir)
         implicit none
         type(mesh_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%MQP,dir//fortran_PS//'MQP')
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           do i_s_base=1,s_s_base
             call export_restart(this%s_base(i_s_base),&
             dir//fortran_PS//'s_base_'//int2str(i_s_base))
           enddo
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           do i_s_ext=1,s_s_ext
             call export_restart(this%s_ext(i_s_ext),&
             dir//fortran_PS//'s_ext_'//int2str(i_s_ext))
           enddo
         endif
       end subroutine

       subroutine import_restart_mesh_params(this,dir)
         implicit none
         type(mesh_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_s_base
         integer :: i_s_ext
         integer :: s_s_base
         integer :: s_s_ext
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%MQP,dir//fortran_PS//'MQP')
         if (allocated(this%s_base)) then
           s_s_base = size(this%s_base)
           do i_s_base=1,s_s_base
             call import_restart(this%s_base(i_s_base),&
             dir//fortran_PS//'s_base_'//int2str(i_s_base))
           enddo
         endif
         if (allocated(this%s_ext)) then
           s_s_ext = size(this%s_ext)
           do i_s_ext=1,s_s_ext
             call import_restart(this%s_ext(i_s_ext),&
             dir//fortran_PS//'s_ext_'//int2str(i_s_ext))
           enddo
         endif
       end subroutine

       subroutine suppress_warnings_mesh_params(this)
         implicit none
         type(mesh_params),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module