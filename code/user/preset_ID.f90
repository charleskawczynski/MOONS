     module preset_ID_mod
     implicit none

     private
     public :: preset_ID
     public :: init,delete,display,print,export,import

     public :: PID

     type preset_ID
       integer :: T   = 0
       integer :: U   = 0
       integer :: p   = 0
       integer :: B   = 0
       integer :: B0  = 0
       integer :: phi = 0
     end type

     interface init;    module procedure init_PID_copy;  end interface
     interface delete;  module procedure delete_PID;     end interface
     interface display; module procedure display_PID;    end interface
     interface print;   module procedure print_PID;      end interface
     interface export;  module procedure export_PID;     end interface
     interface import;  module procedure import_PID;     end interface

     interface PID;      module procedure PID_PID;         end interface

     contains

     function PID_PID(T,U,p,B,B0,phi) result(I)
       implicit none
       integer,intent(in) :: T,U,p,B,B0,phi
       type(preset_ID) :: I
       I%T   = T
       I%U   = U
       I%p   = p
       I%B   = B
       I%B0  = B0
       I%phi = phi
     end function

     subroutine delete_PID(I)
       implicit none
       type(preset_ID),intent(inout) :: I
       I%T   = 0
       I%U   = 0
       I%p   = 0
       I%B   = 0
       I%B0  = 0
       I%phi = 0
     end subroutine

     subroutine display_PID(I,un)
       implicit none
       type(preset_ID),intent(in) :: I
       integer,intent(in) :: un
       write(un,*) 'T = ',  I%T
       write(un,*) 'U = ',  I%U
       write(un,*) 'p = ',  I%p
       write(un,*) 'B = ',  I%B
       write(un,*) 'B0 = ', I%B0
       write(un,*) 'phi = ',I%phi
     end subroutine

     subroutine print_PID(I)
       implicit none
       type(preset_ID),intent(in) :: I
       call display(I,6)
     end subroutine

     subroutine export_PID(I,un)
       implicit none
       type(preset_ID),intent(in) :: I
       integer,intent(in) :: un
       write(un,*) 'T = ';  write(un,*) I%T
       write(un,*) 'U = ';  write(un,*) I%U
       write(un,*) 'p = ';  write(un,*) I%p
       write(un,*) 'B = ';  write(un,*) I%B
       write(un,*) 'B0 = '; write(un,*) I%B0
       write(un,*) 'phi = ';write(un,*) I%phi
     end subroutine

     subroutine import_PID(I,un)
       implicit none
       type(preset_ID),intent(inout) :: I
       integer,intent(in) :: un
       read(un,*); read(un,*) I%T
       read(un,*); read(un,*) I%U
       read(un,*); read(un,*) I%p
       read(un,*); read(un,*) I%B
       read(un,*); read(un,*) I%B0
       read(un,*); read(un,*) I%phi
     end subroutine

     subroutine init_PID_copy(A,B)
       implicit none
       type(preset_ID),intent(inout) :: A
       type(preset_ID),intent(in) :: B
       A%T   = B%T
       A%U   = B%U
       A%p   = B%p
       A%B   = B%B
       A%B0  = B%B0
       A%phi = B%phi
     end subroutine

     end module