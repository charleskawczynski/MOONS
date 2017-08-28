     module restart_file_mod
     use IO_tools_mod
     implicit none

     private
     public :: restart_file
     public :: init,export,import

     interface init;         module procedure init_RF;            end interface
     interface export;       module procedure export_RF;          end interface
     interface export;       module procedure export_RF_wrapper;  end interface
     interface import;       module procedure import_RF;          end interface
     interface import;       module procedure import_RF_wrapper;  end interface

     type restart_file
       logical :: restart_input_file = .false.
       logical :: restart_fields = .false.
     end type

     contains

     subroutine init_RF(RF)
       implicit none
       type(restart_file),intent(inout) :: RF
       RF%restart_fields = .false.
       RF%restart_input_file = .false.
     end subroutine

     subroutine export_RF(RF,un)
       implicit none
       type(restart_file),intent(in) :: RF
       integer,intent(in) :: un
       write(un,*) 'restart_input_file  = '; write(un,*) RF%restart_input_file
       write(un,*) 'restart_fields      = '; write(un,*) RF%restart_fields
     end subroutine

     subroutine import_RF(RF,un)
       implicit none
       type(restart_file),intent(inout) :: RF
       integer,intent(in) :: un
       read(un,*); read(un,*) RF%restart_input_file
       read(un,*); read(un,*) RF%restart_fields
     end subroutine

     subroutine export_RF_wrapper(RF,dir,name)
       implicit none
       type(restart_file),intent(in) :: RF
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(RF,un)
       call close_and_message(un,dir,name)
     end subroutine

     subroutine import_RF_wrapper(RF,dir,name)
       implicit none
       type(restart_file),intent(inout) :: RF
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call import(RF,un)
       call close_and_message(un,dir,name)
     end subroutine

     end module