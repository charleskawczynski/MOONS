     module momentum_forces_mod
     use IO_tools_mod
     implicit none

     private
     public :: momentum_forces
     public :: init,delete,display,print,export,import

     interface delete;  module procedure delete_MF;         end interface
     interface init;    module procedure init_MF_copy;      end interface
     interface display; module procedure display_MF;        end interface
     interface print;   module procedure print_MF;          end interface
     interface export;  module procedure export_MF;         end interface
     interface import;  module procedure import_MF;         end interface
     interface export;  module procedure export_MF_wrapper; end interface

     type momentum_forces
       logical :: mean_pressure_grad = .false.
       logical :: JCrossB            = .false.
       logical :: Q2D_JCrossB        = .false.
       logical :: Buoyancy           = .false.
       logical :: Gravity            = .false.
     end type

     contains

     subroutine init_MF_copy(MF,MF_in)
       implicit none
       type(momentum_forces),intent(inout) :: MF
       type(momentum_forces),intent(in) :: MF_in
       MF%mean_pressure_grad = MF_in%mean_pressure_grad
       MF%JCrossB            = MF_in%JCrossB
       MF%Q2D_JCrossB        = MF_in%Q2D_JCrossB
       MF%Buoyancy           = MF_in%Buoyancy
       MF%Gravity            = MF_in%Gravity
      end subroutine

     subroutine delete_MF(MF)
       implicit none
       type(momentum_forces),intent(inout) :: MF
       MF%mean_pressure_grad = .false.
       MF%JCrossB            = .false.
       MF%Q2D_JCrossB        = .false.
       MF%Buoyancy           = .false.
       MF%Gravity            = .false.
      end subroutine

     subroutine display_MF(MF,un)
       implicit none
       type(momentum_forces),intent(in) :: MF
       integer,intent(in) :: un
       write(un,*) 'mean_pressure_grad = ',MF%mean_pressure_grad
       write(un,*) 'JCrossB            = ',MF%JCrossB
       write(un,*) 'Q2D_JCrossB        = ',MF%Q2D_JCrossB
       write(un,*) 'Buoyancy           = ',MF%Buoyancy
       write(un,*) 'Gravity            = ',MF%Gravity
      end subroutine

     subroutine print_MF(MF)
       implicit none
       type(momentum_forces),intent(in) :: MF
       call display(MF,6)
      end subroutine

     subroutine export_MF(MF,un)
       implicit none
       type(momentum_forces),intent(in) :: MF
       integer,intent(in) :: un
       write(un,*) MF%mean_pressure_grad
       write(un,*) MF%JCrossB
       write(un,*) MF%Q2D_JCrossB
       write(un,*) MF%Buoyancy
       write(un,*) MF%Gravity
      end subroutine

     subroutine import_MF(MF,un)
       implicit none
       type(momentum_forces),intent(inout) :: MF
       integer,intent(in) :: un
       write(un,*) MF%mean_pressure_grad
       write(un,*) MF%JCrossB
       write(un,*) MF%Q2D_JCrossB
       write(un,*) MF%Buoyancy
       write(un,*) MF%Gravity
      end subroutine

     subroutine export_MF_wrapper(MF,dir,name)
       implicit none
       type(momentum_forces),intent(in) :: MF
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(MF,un)
       call close_and_message(un,dir,name)
      end subroutine

     end module