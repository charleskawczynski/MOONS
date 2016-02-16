      module stitch_mod
      implicit none

      private
      public :: stitch
      public :: init,delete

      type stitch
        logical :: TF
        integer :: ID
      end type

      interface init;   module procedure init_stitch;   end interface
      interface init;   module procedure init_copy;     end interface
      interface delete; module procedure delete_stitch; end interface
      
      contains

      subroutine delete_stitch(s)
        implicit none
        type(stitch),intent(inout) :: s
        s%TF = .false.
        s%ID = 0
      end subroutine

      subroutine init_stitch(s,ID)
        implicit none
        type(stitch),intent(inout) :: s
        integer,intent(in) :: ID
        s%TF = .true.
        s%ID = ID
      end subroutine

      subroutine init_copy(s_out,s_in)
        implicit none
        type(stitch),intent(inout) :: s_out
        type(stitch),intent(in) :: s_in
        s_out%TF = s_in%TF
        s_out%ID = s_in%ID
      end subroutine

      end module