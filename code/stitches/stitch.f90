      module stitch_mod
      implicit none

      private
      public :: stitch
      public :: init,delete

      type stitch
        logical :: TF
        integer :: id
      end type

      interface init;   module procedure init_stitch;   end interface
      interface init;   module procedure init_copy;     end interface
      interface delete; module procedure delete_stitch; end interface
      
      contains

      subroutine delete_stitch(s)
        implicit none
        type(stitch),intent(inout) :: s
        s%TF = .false.
        s%id = 0
      end subroutine

      subroutine init_stitch(s,TF)
        implicit none
        type(stitch),intent(inout) :: s
        logical,intent(in) :: TF
        s%TF = TF
      end subroutine

      subroutine init_copy(s_out,s_in)
        implicit none
        type(stitch),intent(inout) :: s_out
        type(stitch),intent(in) :: s_in
        s_out%TF = s_in%TF
        s_out%id = s_in%id
      end subroutine

      end module