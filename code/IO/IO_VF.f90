      module IO_VF_mod
      use current_precision_mod
      use mesh_mod
      use VF_mod
      use export_VF_mod
      use import_VF_mod
      use IO_tools_mod

      implicit none

      private
      public :: export_3D_3C
      public :: export_2D_2C
      public :: export_2D_3C
      public :: export_2D_2C_transient
      public :: export_2D_3C_transient
      public :: import_3D_3C

      contains

      subroutine export_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,name)
        call exp_3D_3C(m,pad,un,arrfmt,name,U)
        call closeAndMessage(un,dir,name)
      end subroutine

      subroutine export_2D_2C_transient(m,U,dir,name,pad,direction,nstep)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,nstep
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,name//int2str(nstep))
        call exp_2D_2C(m,pad,un,arrfmt,name,U,direction)
        call closeAndMessage(un,dir,name)
      end subroutine

      subroutine export_2D_3C_transient(m,U,dir,name,pad,direction,nstep)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,nstep
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,name//int2str(nstep))
        call exp_2D_3C(m,pad,un,arrfmt,name,U,direction)
        call closeAndMessage(un,dir,name)
      end subroutine

      subroutine export_2D_2C(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,name)
        call exp_2D_2C(m,pad,un,arrfmt,name,U,direction)
        call closeAndMessage(un,dir,name)
      end subroutine

      subroutine export_2D_3C(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,name)
        call exp_2D_3C(m,pad,un,arrfmt,name,U,direction)
        call closeAndMessage(un,dir,name)
      end subroutine

      subroutine import_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad
        type(VF),intent(inout) :: U
        integer :: un
        un = openToRead(dir,name)
        call imp_3D_3C(m,pad,un,arrfmt,name,U)
        call closeAndMessage(un,dir,name)
      end subroutine


      end module