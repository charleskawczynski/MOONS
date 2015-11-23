      module stitch_face_mod
      ! This routine is used in grid as
      !        hmin(i) {(x,y,z)}
      !        hmax(1) {(x,y,z)}
      ! for direction i, covering all 6 faces.
      implicit none

      private
      public :: stitch_face
      public :: init,delete

      type stitch_face
        ! type(stitch),dimension(6) :: st
        logical,dimension(3) :: hmax,hmin
        integer,dimension(3) :: hmax_id,hmin_id
      end type

      interface init;   module procedure init_stitch_face;   end interface
      interface init;   module procedure init_copy;          end interface
      interface delete; module procedure delete_stitch_face; end interface
      
      contains

      subroutine delete_stitch_face(s)
        implicit none
        type(stitch_face),intent(inout) :: s
        s%hmin = .false.
        s%hmax = .false.
        s%hmin_id = 0
        s%hmax_id = 0
      end subroutine

      subroutine init_stitch_face(s,hmin,hmax)
        implicit none
        type(stitch_face),intent(inout) :: s
        logical,intent(in) :: hmin,hmax
        s%hmin = hmin
        s%hmax = hmax
      end subroutine

      subroutine init_copy(s_out,s_in)
        implicit none
        type(stitch_face),intent(inout) :: s_out
        type(stitch_face),intent(in) :: s_in
        s_out%hmin = s_in%hmin
        s_out%hmax = s_in%hmax
        s_out%hmin_id = s_in%hmin_id
        s_out%hmax_id = s_in%hmax_id
      end subroutine

      end module