      module IO_import_mod
      use current_precision_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      use base_import_mod
      use IO_tools_mod
      use time_marching_params_mod
      use datatype_conversion_mod
      implicit none

      private
      public :: import_3D_1C
      public :: import_3D_3C

      contains

      subroutine import_3D_1C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(SF),intent(inout) :: U
        integer :: un
        un = open_to_read(dir,name)
        call imp_3D_1C(m,pad,un,U)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine import_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(inout) :: U
        integer :: un
        un = open_to_read(dir,name)
        call imp_3D_3C(m,pad,un,U)
        call close_and_message(un,dir,name)
      end subroutine

      end module