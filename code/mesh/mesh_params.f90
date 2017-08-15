       module mesh_params_mod
       use IO_tools_mod
       use segment_mod
       use mesh_quality_params_mod
       implicit none

       private
       public :: mesh_params
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh_params;          end interface
       interface init;   module procedure init_many_mesh_params;     end interface
       interface delete; module procedure delete_mesh_params;        end interface
       interface delete; module procedure delete_many_mesh_params;   end interface
       interface display;module procedure display_mesh_params;       end interface
       interface display;module procedure display_many_mesh_params;  end interface
       interface print;  module procedure print_mesh_params;         end interface
       interface print;  module procedure print_many_mesh_params;    end interface
       interface export; module procedure export_mesh_params;        end interface
       interface export; module procedure export_many_mesh_params;   end interface
       interface import; module procedure import_mesh_params;        end interface
       interface import; module procedure import_many_mesh_params;   end interface
       interface export; module procedure export_wrapper_mesh_params;end interface
       interface import; module procedure import_wrapper_mesh_params;end interface

       type mesh_params
         type(mesh_quality_params) :: mqp
         type(segment),dimension(:),allocatable :: s_base
         type(segment),dimension(:),allocatable :: s_ext
         integer :: n_base = 0
         integer :: n_ext = 0
       end type

       contains

       subroutine init_mesh_params(this,that)
         implicit none
         type(mesh_params),intent(inout) :: this
         type(mesh_params),intent(in) :: that
         integer :: i_iter
         call delete(this)
         call init(this%mqp,that%mqp)
         if (allocated(that%s_base)) then
           allocate(this%s_base(size(that%s_base)))
           do i_iter=1,size(this%s_base)
             call init(this%s_base(i_iter),that%s_base(i_iter))
           enddo
         endif
         if (allocated(that%s_ext)) then
           allocate(this%s_ext(size(that%s_ext)))
           do i_iter=1,size(this%s_ext)
             call init(this%s_ext(i_iter),that%s_ext(i_iter))
           enddo
         endif
         this%n_base = that%n_base
         this%n_ext = that%n_ext
       end subroutine

       subroutine init_many_mesh_params(this,that)
         implicit none
         type(mesh_params),dimension(:),intent(inout) :: this
         type(mesh_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_mesh_params(this)
         implicit none
         type(mesh_params),intent(inout) :: this
         integer :: i_iter
         call delete(this%mqp)
         if (allocated(this%s_base)) then
           do i_iter=1,size(this%s_base)
             call delete(this%s_base(i_iter))
           enddo
           deallocate(this%s_base)
         endif
         if (allocated(this%s_ext)) then
           do i_iter=1,size(this%s_ext)
             call delete(this%s_ext(i_iter))
           enddo
           deallocate(this%s_ext)
         endif
         this%n_base = 0
         this%n_ext = 0
       end subroutine

       subroutine delete_many_mesh_params(this)
         implicit none
         type(mesh_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%mqp,un)
         if (allocated(this%s_base)) then
         call display(this%s_base,un)
         endif
         if (allocated(this%s_ext)) then
         call display(this%s_ext,un)
         endif
         write(un,*) 'n_base = ',this%n_base
         write(un,*) 'n_ext  = ',this%n_ext
       end subroutine

       subroutine display_many_mesh_params(this,un)
         implicit none
         type(mesh_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_mesh_params(this)
         implicit none
         type(mesh_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_mesh_params(this)
         implicit none
         type(mesh_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(in) :: this
         integer,intent(in) :: un
         call export(this%mqp,un)
         if (allocated(this%s_base)) then
         call export(this%s_base,un)
         endif
         if (allocated(this%s_ext)) then
         call export(this%s_ext,un)
         endif
         write(un,*) this%n_base
         write(un,*) this%n_ext
       end subroutine

       subroutine export_many_mesh_params(this,un)
         implicit none
         type(mesh_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_mesh_params(this,un)
         implicit none
         type(mesh_params),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%mqp,un)
         if (allocated(this%s_base)) then
         call import(this%s_base,un)
         endif
         if (allocated(this%s_ext)) then
         call import(this%s_ext,un)
         endif
         read(un,*) this%n_base
         read(un,*) this%n_ext
       end subroutine

       subroutine import_many_mesh_params(this,un)
         implicit none
         type(mesh_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_mesh_params(this,dir,name)
         implicit none
         type(mesh_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_mesh_params(this,dir,name)
         implicit none
         type(mesh_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module