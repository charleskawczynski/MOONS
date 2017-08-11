     module induction_terms_mod
     use equation_term_mod
     use IO_tools_mod
     implicit none

     private
     public :: induction_terms
     public :: init,delete,display,print,export,import

     interface delete;  module procedure delete_IT;         end interface
     interface init;    module procedure init_IT_copy;      end interface
     interface display; module procedure display_IT;        end interface
     interface print;   module procedure print_IT;          end interface
     interface export;  module procedure export_IT;         end interface
     interface import;  module procedure import_IT;         end interface
     interface export;  module procedure export_IT_wrapper; end interface

     type induction_terms
       type(equation_term) :: advection
       type(equation_term) :: diffusion
       type(equation_term) :: diffusion_linear
       type(equation_term) :: unsteady_B0
       type(equation_term) :: current
       type(equation_term) :: B_applied
     end type

     contains

     subroutine init_IT_copy(IT,IT_in)
       implicit none
       type(induction_terms),intent(inout) :: IT
       type(induction_terms),intent(in) :: IT_in
       call init(IT%advection,IT_in%advection)
       call init(IT%diffusion,IT_in%diffusion)
       call init(IT%diffusion_linear,IT_in%diffusion_linear)
       call init(IT%unsteady_B0,IT_in%unsteady_B0)
       call init(IT%current,IT_in%current)
       call init(IT%B_applied,IT_in%B_applied)
      end subroutine

     subroutine delete_IT(IT)
       implicit none
       type(induction_terms),intent(inout) :: IT
       call delete(IT%advection)
       call delete(IT%diffusion)
       call delete(IT%diffusion_linear)
       call delete(IT%unsteady_B0)
       call delete(IT%current)
       call delete(IT%B_applied)
      end subroutine

     subroutine display_IT(IT,un)
       implicit none
       type(induction_terms),intent(in) :: IT
       integer,intent(in) :: un
       call display(IT%advection,un,'advection')
       call display(IT%diffusion,un,'diffusion')
       call display(IT%diffusion_linear,un,'diffusion_linear')
       call display(IT%unsteady_B0,un,'unsteady_B0')
       call display(IT%current,un,'current')
       call display(IT%B_applied,un,'B_applied')
      end subroutine

     subroutine print_IT(IT)
       implicit none
       type(induction_terms),intent(in) :: IT
       call display(IT,6)
      end subroutine

     subroutine export_IT(IT,un)
       implicit none
       type(induction_terms),intent(in) :: IT
       integer,intent(in) :: un
       write(un,*) ' --------- induction_terms --------- '
       call export(IT%advection,un,'advection')
       call export(IT%diffusion,un,'diffusion')
       call export(IT%diffusion_linear,un,'diffusion_linear')
       call export(IT%unsteady_B0,un,'unsteady_B0')
       call export(IT%current,un,'current')
       call export(IT%B_applied,un,'B_applied')
       write(un,*) ' ----------------------------------- '
      end subroutine

     subroutine import_IT(IT,un)
       implicit none
       type(induction_terms),intent(inout) :: IT
       integer,intent(in) :: un
       read(un,*);
       call import(IT%advection,un)
       call import(IT%diffusion,un)
       call import(IT%diffusion_linear,un)
       call import(IT%unsteady_B0,un)
       call import(IT%current,un)
       call import(IT%B_applied,un)
       read(un,*);
      end subroutine

     subroutine export_IT_wrapper(IT,dir,name)
       implicit none
       type(induction_terms),intent(in) :: IT
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(IT,un)
       call close_and_message(un,dir,name)
      end subroutine

     end module