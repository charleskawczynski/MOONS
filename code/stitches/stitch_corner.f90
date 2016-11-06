      module stitch_corner_mod
      ! This routine is used in grid as
      !        minmin    {(x,y,z)}
      !        minmax(i) {(y,z),(x,z),(x,y)}
      !        maxmin(i) {(y,z),(x,z),(x,y)}
      !        maxmax    {(x,y,z)}
      ! for direction i, covering all 8 corner.
      implicit none

      private
      public :: stitch_corner
      public :: init,delete

      type stitch_corner
        logical,dimension(3) :: minmax = .false.
        logical,dimension(3) :: maxmin = .false.
        logical :: minmin = .false.
        logical :: maxmax = .false.
        integer,dimension(3) :: minmax_id,maxmin_id
        integer :: minmin_id,maxmax_id
      end type

      interface init;   module procedure init_copy;            end interface
      interface delete; module procedure delete_stitch_corner; end interface
      
      contains

      subroutine delete_stitch_corner(s)
        implicit none
        type(stitch_corner),intent(inout) :: s
        s%minmax = .false.; s%maxmin = .false.
        s%minmin = .false.; s%maxmax = .false.
        s%minmax_id = 0; s%maxmin_id = 0
        s%minmin_id = 0; s%maxmax_id = 0
      end subroutine

      subroutine init_copy(s_out,s_in)
        implicit none
        type(stitch_corner),intent(inout) :: s_out
        type(stitch_corner),intent(in) :: s_in
        s_out%minmax = s_in%minmax
        s_out%maxmin = s_in%maxmin
        s_out%minmin = s_in%minmin
        s_out%maxmax = s_in%maxmax
        s_out%minmax_id = s_in%minmax_id
        s_out%maxmin_id = s_in%maxmin_id
        s_out%minmin_id = s_in%minmin_id
        s_out%maxmax_id = s_in%maxmax_id
      end subroutine

      end module