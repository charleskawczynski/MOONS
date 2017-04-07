     module equation_term_mod
     use current_precision_mod
     use IO_tools_mod
     implicit none

     private
     public :: equation_term
     public :: init,delete,display,print,export,import

     interface delete;  module procedure delete_ET;         end interface
     interface init;    module procedure init_ET_copy;      end interface
     interface display; module procedure display_ET;        end interface
     interface print;   module procedure print_ET;          end interface
     interface export;  module procedure export_ET;         end interface
     interface import;  module procedure import_ET;         end interface
     interface export;  module procedure export_ET_wrapper; end interface

     type equation_term
       logical :: add = .false.
       real(cp) :: scale = 0.0_cp
     end type

     contains

     subroutine init_ET_copy(ET,ET_in)
       implicit none
       type(equation_term),intent(inout) :: ET
       type(equation_term),intent(in) :: ET_in
       ET%add   = ET_in%add
       ET%scale = ET_in%scale
      end subroutine

     subroutine delete_ET(ET)
       implicit none
       type(equation_term),intent(inout) :: ET
       ET%add   = .false.
       ET%scale = 0.0_cp
      end subroutine

     subroutine display_ET(ET,un,name)
       implicit none
       type(equation_term),intent(in) :: ET
       integer,intent(in) :: un
       character(len=*),intent(in) :: name
       write(un,*) name
       write(un,*) 'add   = ',ET%add
       write(un,*) 'scale = ',ET%scale
      end subroutine

     subroutine print_ET(ET,name)
       implicit none
       type(equation_term),intent(in) :: ET
       character(len=*),intent(in) :: name
       call display(ET,6,name)
      end subroutine

     subroutine export_ET(ET,un,name)
       implicit none
       type(equation_term),intent(in) :: ET
       integer,intent(in) :: un
       character(len=*),intent(in) :: name
       write(un,*) 'name  = '; write(un,*) name
       write(un,*) 'add   = '; write(un,*) ET%add
       write(un,*) 'scale = '; write(un,*) ET%scale
      end subroutine

     subroutine import_ET(ET,un)
       implicit none
       type(equation_term),intent(inout) :: ET
       integer,intent(in) :: un
       read(un,*); read(un,*)
       read(un,*); read(un,*) ET%add
       read(un,*); read(un,*) ET%scale
      end subroutine

     subroutine export_ET_wrapper(ET,dir,name)
       implicit none
       type(equation_term),intent(in) :: ET
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(ET,un,name)
       call close_and_message(un,dir,name)
      end subroutine

     end module