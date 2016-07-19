      module stitch_mod
      implicit none

      private
      public :: stitch
      public :: init,delete
      public :: export,import,display

      type stitch
        logical :: TF
        integer :: ID
      end type

      interface init;      module procedure init_stitch;     end interface
      interface init;      module procedure init_copy;       end interface
      interface delete;    module procedure delete_stitch;   end interface
      interface export;    module procedure export_stitch;   end interface
      interface import;    module procedure import_stitch;   end interface
      interface display;   module procedure display_stitch;  end interface
      
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

      subroutine display_stitch(s,un)
        implicit none
        type(stitch),intent(in) :: s
        integer,intent(in) :: un
        write(un,*) 'stitch = ',s%TF,s%ID
      end subroutine

      subroutine export_stitch(s,un)
        implicit none
        type(stitch),intent(in) :: s
        integer,intent(in) :: un
        write(un,*) 'stitch'
        write(un,*) s%TF,s%ID
      end subroutine

      subroutine import_stitch(s,un)
        implicit none
        type(stitch),intent(inout) :: s
        integer,intent(in) :: un
        read(un,*); read(un,*) s%TF,s%ID
      end subroutine

      end module