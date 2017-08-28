       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module mesh_mod
       use IO_tools_mod
       use block_mod
       use mesh_props_mod
       implicit none

       private
       public :: mesh
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh;           end interface
       interface delete; module procedure delete_mesh;         end interface
       interface display;module procedure display_mesh;        end interface
       interface display;module procedure display_wrapper_mesh;end interface
       interface print;  module procedure print_mesh;          end interface
       interface export; module procedure export_mesh;         end interface
       interface import; module procedure import_mesh;         end interface
       interface export; module procedure export_wrapper_mesh; end interface
       interface import; module procedure import_wrapper_mesh; end interface

       type mesh
         type(block),dimension(:),allocatable :: b
         type(mesh_props) :: mp
         logical :: defined = .false.
       end type

       contains

       subroutine init_mesh(this,that)
         implicit none
         type(mesh),intent(inout) :: this
         type(mesh),intent(in) :: that
         integer :: i_b
         integer :: s_b
         call delete(this)
         if (allocated(that%b)) then
           s_b = size(that%b)
           if (s_b.gt.0) then
             allocate(this%b(s_b))
             do i_b=1,s_b
               call init(this%b(i_b),that%b(i_b))
             enddo
           endif
         endif
         call init(this%mp,that%mp)
         this%defined = that%defined
       end subroutine

       subroutine delete_mesh(this)
         implicit none
         type(mesh),intent(inout) :: this
         integer :: i_b
         integer :: s_b
         if (allocated(this%b)) then
           s_b = size(this%b)
           do i_b=1,s_b
             call delete(this%b(i_b))
           enddo
           deallocate(this%b)
         endif
         call delete(this%mp)
         this%defined = .false.
       end subroutine

       subroutine display_mesh(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- mesh'
         integer :: i_b
         integer :: s_b
         if (allocated(this%b)) then
           s_b = size(this%b)
           do i_b=1,s_b
             call display(this%b(i_b),un)
           enddo
         endif
         call display(this%mp,un)
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine print_mesh(this)
         implicit none
         type(mesh),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_mesh(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_b
         integer :: s_b
         if (allocated(this%b)) then
           s_b = size(this%b)
           write(un,*) s_b
           do i_b=1,s_b
             call export(this%b(i_b),un)
           enddo
         endif
         call export(this%mp,un)
         write(un,*) 'defined  = ';write(un,*) this%defined
       end subroutine

       subroutine import_mesh(this,un)
         implicit none
         type(mesh),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_b
         integer :: s_b
         call delete(this)
         if (allocated(this%b)) then
           read(un,*) s_b
           do i_b=1,s_b
             call import(this%b(i_b),un)
           enddo
         endif
         call import(this%mp,un)
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrapper_mesh(this,dir,name)
         implicit none
         type(mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_mesh(this,dir,name)
         implicit none
         type(mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_mesh(this,dir,name)
         implicit none
         type(mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module