      module IO_SF_mod
      use current_precision_mod
      use mesh_mod
      use SF_mod
      use export_SF_mod
      use import_SF_mod
      use IO_tools_mod
      use fmt_mod
      implicit none

      private
      public :: export_3D_1C,export_2D_1C
      public :: import_3D_1C

      public :: export_mesh

      contains

      subroutine export_3D_1C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(SF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name)
        call exp_3D_1C(m,pad,un,arrfmt,name,U)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_2D_1C(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(SF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name)
        call exp_2D_1C(m,pad,un,arrfmt,name,U,direction)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine import_3D_1C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(SF),intent(inout) :: U
        type(mesh) :: temp
        integer :: un
        un = open_to_read(dir,name)
        call init(temp,m)
        call imp_3D_1C(temp,pad,un,arrfmt,name,U)
        call delete(temp)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_mesh(m,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        integer :: un
        un = new_and_open(dir,name)
        call exp_mesh_SF(m,pad,un,arrfmt,name)
        call close_and_message(un,dir,name)
      end subroutine

      end module