      module stitch_mod
      implicit none

      private
      public :: stitch
      public :: init,delete

      type stitch
        logical :: hmax,hmin
      end type

      interface init;   module procedure init_stitch;   end interface
      interface init;   module procedure init_copy;     end interface
      interface delete; module procedure delete_stitch; end interface
      
      contains

      subroutine delete_stitch(s)
        implicit none
        type(stitch),intent(inout) :: s
        s%hmin = .false.
        s%hmax = .false.
      end subroutine

      subroutine init_stitch(s,hmin,hmax)
        implicit none
        type(stitch),intent(inout) :: s
        logical,intent(in) :: hmin,hmax
        s%hmin = hmin
        s%hmax = hmax
      end subroutine

      subroutine init_copy(s_out,s_in)
        implicit none
        type(stitch),intent(inout) :: s_out
        type(stitch),intent(in) :: s_in
        s_out%hmin = s_in%hmin
        s_out%hmax = s_in%hmax
      end subroutine

      end module