      module IO_SF_mod
      use grid_mod
      use SF_mod
      use export_SF_mod
      use import_SF_mod
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
      public :: export_1C_SF
      public :: export_grid

      contains

      subroutine export_1C_SF(g,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(in) :: g
        integer,intent(in) :: pad
        type(SF),intent(in) :: U
        integer :: un
        un = newAndOpen(dir,trim(adjustl(name)))
        call exp_1C_SF(g,pad,un,arrfmt,trim(adjustl(name)),U)
        call closeAndMessage(un,trim(adjustl(name)),dir)
      end subroutine

      subroutine import_1C_SF(g,U,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad
        type(SF),intent(inout) :: U
        integer :: un
        un = openToRead(dir,trim(adjustl(name)))
        call imp_1C_SF(g,pad,un,arrfmt,trim(adjustl(name)),U)
        call closeExisting(un,trim(adjustl(name)),dir)
      end subroutine

      subroutine export_grid(g,dir,name,pad)
        implicit none
        character(len=*),intent(in) :: dir,name
        type(grid),intent(in) :: g
        integer,intent(in) :: pad
        integer :: un
        un = newAndOpen(dir,trim(adjustl(name)))
        call exp_grid_SF(g,pad,un,arrfmt,trim(adjustl(name)))
        call closeAndMessage(un,trim(adjustl(name)),dir)
      end subroutine

      end module