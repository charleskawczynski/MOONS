       module refine_mesh_mod
       use datatype_conversion_mod
       use string_mod
       use string_aux_mod
       use IO_tools_mod
       implicit none
       private
       public :: refine_mesh
       public :: init,delete,export,import
       public :: update
       public :: prolongate
       public :: get_dir

       interface init;        module procedure init_RM;         end interface
       interface delete;      module procedure delete_RM;       end interface
       interface export;      module procedure export_RM;       end interface
       interface import;      module procedure import_RM;       end interface
       interface update;      module procedure update_RM;       end interface

       interface prolongate;  module procedure prolongate_RM;   end interface
       interface get_dir;     module procedure get_dir_RM;      end interface


       type step
         logical :: this = .false.
         logical :: next = .false.
       end type

       type refine_mesh
         type(step) :: all,x,y,z
         type(step) :: x_plane
         type(step) :: y_plane
         type(step) :: z_plane
         type(string) :: dir,name,level,level_last
         logical :: any_next = .false.
         integer :: un
         integer :: i_level,i_level_last
       end type

       contains

       subroutine update_step(s)
         implicit none
         type(step),intent(inout) :: s
         s%this = s%next
         s%next = .false.
       end subroutine

       subroutine delete_step(s)
         implicit none
         type(step),intent(inout) :: s
         s%this = .false.
         s%next = .false.
       end subroutine

       subroutine init_RM(RM,dir,name)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         character(len=*),intent(in) :: dir,name
         call delete_step(RM%all)
         call delete_step(RM%x)
         call delete_step(RM%y)
         call delete_step(RM%z)
         call delete_step(RM%x_plane)
         call delete_step(RM%y_plane)
         call delete_step(RM%z_plane)
         RM%any_next = .false.
         RM%i_level = 0
         RM%i_level_last = 0
         call init(RM%level,'0')
         call init(RM%level_last,'0')

         call init(RM%dir,dir)
         call init(RM%name,name)
       end subroutine

       subroutine delete_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         call delete_step(RM%all)
         call delete_step(RM%x)
         call delete_step(RM%y)
         call delete_step(RM%z)
         call delete_step(RM%x_plane)
         call delete_step(RM%y_plane)
         call delete_step(RM%z_plane)
         RM%any_next = .false.
         RM%i_level = 0
         RM%i_level_last = 0
         call init(RM%level,'0')
         call init(RM%level_last,'0')

         call delete(RM%dir)
         call delete(RM%name)
       end subroutine

       subroutine export_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         integer :: un
         un = new_and_open(str(RM%dir),str(RM%name))
         write(un,*) 'all_next = ';     write(un,*) RM%all%next
         write(un,*) 'x_next = ';       write(un,*) RM%x%next
         write(un,*) 'y_next = ';       write(un,*) RM%y%next
         write(un,*) 'z_next = ';       write(un,*) RM%z%next
         write(un,*) 'x_plane_next = '; write(un,*) RM%x_plane%next
         write(un,*) 'y_plane_next = '; write(un,*) RM%y_plane%next
         write(un,*) 'z_plane_next = '; write(un,*) RM%z_plane%next
         close(un)
       end subroutine

       subroutine import_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         integer :: un
         un = open_to_read(str(RM%dir),str(RM%name))
         read(un,*) ; read(un,*) RM%all%next
         read(un,*) ; read(un,*) RM%x%next
         read(un,*) ; read(un,*) RM%y%next
         read(un,*) ; read(un,*) RM%z%next
         read(un,*) ; read(un,*) RM%x_plane%next
         read(un,*) ; read(un,*) RM%y_plane%next
         read(un,*) ; read(un,*) RM%z_plane%next
         close(un)
       end subroutine

       subroutine update_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         RM%any_next = any((/RM%all%next,RM%x%next,RM%y%next,RM%z%next,&
         RM%x_plane%next,RM%y_plane%next,RM%z_plane%next/))
         call update_step(RM%all)
         call update_step(RM%x)
         call update_step(RM%y)
         call update_step(RM%z)
         call update_step(RM%x_plane)
         call update_step(RM%y_plane)
         call update_step(RM%z_plane)
       end subroutine

       subroutine prolongate_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         RM%i_level_last = RM%i_level
         call init(RM%level_last,int2str(RM%i_level_last))
         call remove_leading_zeros(RM%level_last)

         RM%i_level = RM%i_level + 1
         call init(RM%level,int2str(RM%i_level))
         call remove_leading_zeros(RM%level)
       end subroutine

       function get_dir_RM(RM) result(dir)
        implicit none
        type(refine_mesh),intent(in) :: RM
        integer,dimension(3) :: dir
         dir = 0
         if (RM%x%this) then; dir(1) = 1; endif
         if (RM%y%this) then; dir(2) = 2; endif
         if (RM%z%this) then; dir(3) = 3; endif
         if (RM%x_plane%this) then; dir=(/1,2,3/); dir(1) = 0; endif
         if (RM%y_plane%this) then; dir=(/1,2,3/); dir(2) = 0; endif
         if (RM%z_plane%this) then; dir=(/1,2,3/); dir(3) = 0; endif
         if (RM%all%this) then; dir = (/1,2,3/); endif
       end function

       end module