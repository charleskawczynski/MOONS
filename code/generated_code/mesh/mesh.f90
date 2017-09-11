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
       public :: display_short,print_short

       interface init;         module procedure init_copy_mesh;    end interface
       interface delete;       module procedure delete_mesh;       end interface
       interface display;      module procedure display_mesh;      end interface
       interface display_short;module procedure display_short_mesh;end interface
       interface display;      module procedure display_wrap_mesh; end interface
       interface print;        module procedure print_mesh;        end interface
       interface print_short;  module procedure print_short_mesh;  end interface
       interface export;       module procedure export_mesh;       end interface
       interface import;       module procedure import_mesh;       end interface
       interface export;       module procedure export_wrap_mesh;  end interface
       interface import;       module procedure import_wrap_mesh;  end interface

       type mesh
         type(block),dimension(:),allocatable :: B
         type(mesh_props) :: MP
         logical :: defined = .false.
         integer :: s = 0
       end type

       contains

       subroutine init_copy_mesh(this,that)
         implicit none
         type(mesh),intent(inout) :: this
         type(mesh),intent(in) :: that
         integer :: i_B
         integer :: s_B
         call delete(this)
         if (allocated(that%B)) then
           s_B = size(that%B)
           if (s_B.gt.0) then
             allocate(this%B(s_B))
             do i_B=1,s_B
               call init(this%B(i_B),that%B(i_B))
             enddo
           endif
         endif
         call init(this%MP,that%MP)
         this%defined = that%defined
         this%s = that%s
       end subroutine

       subroutine delete_mesh(this)
         implicit none
         type(mesh),intent(inout) :: this
         integer :: i_B
         integer :: s_B
         if (allocated(this%B)) then
           s_B = size(this%B)
           do i_B=1,s_B
             call delete(this%B(i_B))
           enddo
           deallocate(this%B)
         endif
         call delete(this%MP)
         this%defined = .false.
         this%s = 0
       end subroutine

       subroutine display_mesh(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_B
         integer :: s_B
         if (allocated(this%B)) then
           s_B = size(this%B)
           do i_B=1,s_B
             call display(this%B(i_B),un)
           enddo
         endif
         call display(this%MP,un)
         write(un,*) 'defined = ',this%defined
         write(un,*) 's       = ',this%s
       end subroutine

       subroutine display_short_mesh(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_B
         integer :: s_B
         if (allocated(this%B)) then
           s_B = size(this%B)
           do i_B=1,s_B
             call display(this%B(i_B),un)
           enddo
         endif
         call display(this%MP,un)
         write(un,*) 'defined = ',this%defined
         write(un,*) 's       = ',this%s
       end subroutine

       subroutine print_mesh(this)
         implicit none
         type(mesh),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mesh(this)
         implicit none
         type(mesh),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_mesh(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_B
         integer :: s_B
         if (allocated(this%B)) then
           s_B = size(this%B)
           write(un,*) s_B
           do i_B=1,s_B
             call export(this%B(i_B),un)
           enddo
         endif
         call export(this%MP,un)
         write(un,*) 'defined  = ';write(un,*) this%defined
         write(un,*) 's        = ';write(un,*) this%s
       end subroutine

       subroutine import_mesh(this,un)
         implicit none
         type(mesh),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_B
         integer :: s_B
         call delete(this)
         if (allocated(this%B)) then
           read(un,*) s_B
           do i_B=1,s_B
             call import(this%B(i_B),un)
           enddo
         endif
         call import(this%MP,un)
         read(un,*); read(un,*) this%defined
         read(un,*); read(un,*) this%s
       end subroutine

       subroutine display_wrap_mesh(this,dir,name)
         implicit none
         type(mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_mesh(this,dir,name)
         implicit none
         type(mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mesh(this,dir,name)
         implicit none
         type(mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module