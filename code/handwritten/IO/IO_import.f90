      module IO_import_mod
      use current_precision_mod
      use mesh_extend_mod
      use SF_extend_mod
      use VF_mod
      use base_import_mod
      use IO_tools_mod
      use string_mod
      use time_marching_params_mod
      use datatype_conversion_mod
      use construct_suffix_mod
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
        type(string) :: s
        integer :: un
        call construct_suffix(s,name,get_DL(U))
        un = new_and_open(dir,str(s))
        call imp_3D_1C(m,pad,un,U)
        call close_and_message(un,dir,str(s))
        call delete(s)
      end subroutine

      subroutine import_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(inout) :: U
        call import_3D_1C(m,U%x,dir,name,pad)
        call import_3D_1C(m,U%y,dir,name,pad)
        call import_3D_1C(m,U%z,dir,name,pad)
      end subroutine

      end module