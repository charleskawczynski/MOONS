      module IO_VF_mod
      use current_precision_mod
      use mesh_mod
      use VF_mod
      use export_VF_mod
      use import_VF_mod
      use IO_tools_mod
      use fmt_mod
      use time_marching_params_mod
      use datatype_conversion_mod

      implicit none

      private
      public :: export_3D_3C
      public :: export_2D_2C
      public :: export_2D_3C
      public :: export_2D_2C_transient
      public :: export_2D_3C_transient
      public :: import_3D_3C

      public :: export_unsteady

      abstract interface
        subroutine export_unsteady(m,U,dir,name,pad,direction,TMP)
          import mesh,VF,time_marching_params
          implicit none
          character(len=*),intent(in) :: dir,name
          type(mesh),intent(in) :: m
          integer,intent(in) :: pad,direction
          type(time_marching_params),intent(in) :: TMP
          type(VF),intent(in) :: U
        end subroutine
      end interface

      contains

      subroutine export_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name)
        call exp_3D_3C(m,pad,un,arrfmt,name,U)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_2D_2C_transient(m,U,dir,name,pad,direction,TMP)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name//cp2str(TMP%t))
        call exp_2D_2C(m,pad,un,arrfmt,name,U,direction)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_2D_3C_transient(m,U,dir,name,pad,direction,TMP)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(time_marching_params),intent(in) :: TMP
        type(VF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name//cp2str(TMP%t))
        call exp_2D_3C(m,pad,un,arrfmt,name,U,direction)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_2D_2C(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name)
        call exp_2D_2C(m,pad,un,arrfmt,name,U,direction)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_2D_3C(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        integer :: un
        un = new_and_open(dir,name)
        call exp_2D_3C(m,pad,un,arrfmt,name,U,direction)
        call close_and_message(un,dir,name)
      end subroutine

      subroutine import_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad
        type(VF),intent(inout) :: U
        integer :: un
        un = open_to_read(dir,name)
        call imp_3D_3C(m,pad,un,arrfmt,name,U)
        call close_and_message(un,dir,name)
      end subroutine

      end module