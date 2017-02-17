     module induction_forces_mod
     use IO_tools_mod
     implicit none

     private
     public :: induction_forces
     public :: init,delete,display,print,export,import

     interface delete;  module procedure delete_MF;         end interface
     interface init;    module procedure init_MF_copy;      end interface
     interface display; module procedure display_MF;        end interface
     interface print;   module procedure print_MF;          end interface
     interface export;  module procedure export_MF;         end interface
     interface import;  module procedure import_MF;         end interface
     interface export;  module procedure export_MF_wrapper; end interface

     type induction_forces
       logical :: advection   = .false.
       logical :: diffusion   = .false.
       logical :: unsteady_B0 = .false.
     end type

     contains

     subroutine init_MF_copy(MF,MF_in)
       implicit none
       type(induction_forces),intent(inout) :: MF
       type(induction_forces),intent(in) :: MF_in
       MF%advection   = MF_in%advection
       MF%diffusion   = MF_in%diffusion
       MF%unsteady_B0 = MF_in%unsteady_B0
      end subroutine

     subroutine delete_MF(MF)
       implicit none
       type(induction_forces),intent(inout) :: MF
       MF%advection   = .false.
       MF%diffusion   = .false.
       MF%unsteady_B0 = .false.
      end subroutine

     subroutine display_MF(MF,un)
       implicit none
       type(induction_forces),intent(in) :: MF
       integer,intent(in) :: un
       write(un,*) 'advection   = ',MF%advection
       write(un,*) 'diffusion   = ',MF%diffusion
       write(un,*) 'unsteady_B0 = ',MF%unsteady_B0
      end subroutine

     subroutine print_MF(MF)
       implicit none
       type(induction_forces),intent(in) :: MF
       call display(MF,6)
      end subroutine

     subroutine export_MF(MF,un)
       implicit none
       type(induction_forces),intent(in) :: MF
       integer,intent(in) :: un
       write(un,*) MF%advection
       write(un,*) MF%diffusion
       write(un,*) MF%unsteady_B0
      end subroutine

     subroutine import_MF(MF,un)
       implicit none
       type(induction_forces),intent(inout) :: MF
       integer,intent(in) :: un
       write(un,*) MF%advection
       write(un,*) MF%diffusion
       write(un,*) MF%unsteady_B0
      end subroutine

     subroutine export_MF_wrapper(MF,dir,name)
       implicit none
       type(induction_forces),intent(in) :: MF
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(MF,un)
       call close_and_message(un,dir,name)
      end subroutine

     end module