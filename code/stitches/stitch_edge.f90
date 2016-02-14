      module stitch_edge_mod
      ! This routine is used in grid as
      !        minmin(i) {(y,z),(x,z),(x,y)}
      !        minmax(i) {(y,z),(x,z),(x,y)}
      !        maxmin(i) {(y,z),(x,z),(x,y)}
      !        maxmax(i) {(y,z),(x,z),(x,y)}
      ! for direction i, covering all 12 edge.
      implicit none

      private
      public :: stitch_edge
      public :: init,delete
      public :: es_ID

      type stitch_edge
        logical,dimension(3) :: minmin,minmax,maxmin,maxmax
        integer,dimension(3) :: minmin_id,minmax_id,maxmin_id,maxmax_id
      end type

      interface init;   module procedure init_copy;          end interface
      interface delete; module procedure delete_stitch_edge; end interface
      
      contains

      subroutine delete_stitch_edge(s)
        implicit none
        type(stitch_edge),intent(inout) :: s
        s%minmin = .false.; s%maxmax = .false.
        s%minmin_id = 0;    s%maxmax_id = 0
        s%minmax = .false.; s%maxmin = .false.
        s%minmax_id = 0;    s%maxmin_id = 0
      end subroutine

      subroutine init_copy(s_out,s_in)
        implicit none
        type(stitch_edge),intent(inout) :: s_out
        type(stitch_edge),intent(in) :: s_in
        s_out%minmin = s_in%minmin
        s_out%minmax = s_in%minmax
        s_out%maxmin = s_in%maxmin
        s_out%maxmax = s_in%maxmax
        s_out%minmin_id = s_in%minmin_id
        s_out%minmax_id = s_in%minmax_id
        s_out%maxmin_id = s_in%maxmin_id
        s_out%maxmax_id = s_in%maxmax_id
      end subroutine

      function es_ID(dir,edge2D) result(ID)
        implicit none
        integer,intent(in) :: dir,edge2D
        integer :: ID
        select case (dir)
        case (1); select case (edge2D)
                  case (1); ID = 1 ! minmin (x)
                  case (2); ID = 2 ! minmax (x)
                  case (3); ID = 3 ! maxmin (x)
                  case (4); ID = 4 ! maxmax (x)
                  case default; stop 'Error: edge2D must = 1,2,3,4 in edge_index in stitch_edge.f90'
                  end select
        case (2); select case (edge2D)
                  case (1); ID = 5 ! minmin (y)
                  case (2); ID = 6 ! minmax (y)
                  case (3); ID = 7 ! maxmin (y)
                  case (4); ID = 8 ! maxmax (y)
                  case default; stop 'Error: edge2D must = 1,2,3,4 in edge_index in stitch_edge.f90'
                  end select
        case (3); select case (edge2D)
                  case (1); ID = 9 ! minmin (z)
                  case (2); ID = 10 ! minmax (z)
                  case (3); ID = 11 ! maxmin (z)
                  case (4); ID = 12 ! maxmax (z)
                  case default; stop 'Error: edge2D must = 1,2,3,4 in edge_index in stitch_edge.f90'
                  end select
        case default; stop 'Error: dir must = 1,2,3 in edge_index in stitch_edge.f90'
        end select
      end function

      end module