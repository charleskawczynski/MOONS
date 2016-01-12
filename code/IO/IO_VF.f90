      module IO_VF_mod
      use mesh_mod
      use VF_mod
      use export_VF_mod
      use import_VF_mod
      use IO_tools_mod

      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: export_3D_3C
      public :: export_2D_2C
      public :: export_2D_2C_transient
      public :: import_3D_3C

      contains

      subroutine export_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,trim(adjustl(name)))
        call exp_3D_3C(m,pad,un,arrfmt,trim(adjustl(name)),U)
        call closeAndMessage(un,trim(adjustl(name)),dir)
      end subroutine

      subroutine export_2D_2C_transient(m,U,dir,name,pad,direction,nstep)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction,nstep
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,trim(adjustl(name//int2str(nstep))))
        call exp_2D_2C(m,pad,un,arrfmt,trim(adjustl(name)),U,direction)
        call closeAndMessage(un,trim(adjustl(name)),dir)
      end subroutine

      subroutine export_2D_2C(m,U,dir,name,pad,direction)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,direction
        type(VF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,trim(adjustl(name)))
        call exp_2D_2C(m,pad,un,arrfmt,trim(adjustl(name)),U,direction)
        call closeAndMessage(un,trim(adjustl(name)),dir)
      end subroutine

      subroutine import_3D_3C(m,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad
        type(VF),intent(inout) :: U
        integer :: un
        un = openToRead(dir,trim(adjustl(name)))
        call imp_3D_3C(m,pad,un,arrfmt,trim(adjustl(name)),U)
        call closeExisting(un,trim(adjustl(name)),dir)
      end subroutine


      end module