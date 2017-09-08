       module refine_mesh_extend_mod
       use step_mod
       use step_extend_mod
       use refine_mesh_mod
       use datatype_conversion_mod
       use string_mod
       use string_aux_mod
       use IO_tools_mod
       implicit none
       private
       public :: refine_mesh
       public :: init,delete,export,import
       public :: delete_levels
       public :: update
       public :: prolongate
       public :: get_dir

       interface init;          module procedure init_RM;          end interface
       interface update;        module procedure update_RM;        end interface
       interface delete_levels; module procedure delete_levels_RM; end interface

       interface prolongate;    module procedure prolongate_RM;    end interface
       interface get_dir;       module procedure get_dir_RM;       end interface

       contains

       subroutine init_RM(RM,dir,name)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         character(len=*),intent(in) :: dir,name
         call delete(RM%all)
         call delete(RM%x)
         call delete(RM%y)
         call delete(RM%z)
         call delete(RM%x_plane)
         call delete(RM%y_plane)
         call delete(RM%z_plane)
         RM%any_next = .false.
         RM%i_level = 0
         RM%i_level_last = 0
         call init(RM%level,'0')
         call init(RM%level_last,'0')

         call init(RM%dir,dir)
         call init(RM%name,name)
       end subroutine

       subroutine delete_levels_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         call init(RM%level,'0')
         call init(RM%level_last,'0')
       end subroutine

       subroutine update_RM(RM)
         implicit none
         type(refine_mesh),intent(inout) :: RM
         RM%any_next = any((/RM%all%next,RM%x%next,RM%y%next,RM%z%next,&
         RM%x_plane%next,RM%y_plane%next,RM%z_plane%next/))
         call update(RM%all)
         call update(RM%x)
         call update(RM%y)
         call update(RM%z)
         call update(RM%x_plane)
         call update(RM%y_plane)
         call update(RM%z_plane)
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