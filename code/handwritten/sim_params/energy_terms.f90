     module energy_terms_mod
     use IO_tools_mod
     use equation_term_mod
     implicit none

     private
     public :: energy_terms
     public :: init,delete,display,print,export,import

     interface delete;  module procedure delete_ET;         end interface
     interface init;    module procedure init_ET_copy;      end interface
     interface display; module procedure display_ET;        end interface
     interface print;   module procedure print_ET;          end interface
     interface export;  module procedure export_ET;         end interface
     interface import;  module procedure import_ET;         end interface
     interface export;  module procedure export_ET_wrapper; end interface

     type energy_terms
       type(equation_term) :: advection
       type(equation_term) :: diffusion
       type(equation_term) :: KE_diffusion
       type(equation_term) :: viscous_dissipation
       type(equation_term) :: joule_heating
       type(equation_term) :: volumetric_heating
     end type

     contains

     subroutine init_ET_copy(ET,ET_in)
       implicit none
       type(energy_terms),intent(inout) :: ET
       type(energy_terms),intent(in) :: ET_in
       call init(ET%advection,ET_in%advection)
       call init(ET%diffusion,ET_in%diffusion)
       call init(ET%KE_diffusion,ET_in%KE_diffusion)
       call init(ET%viscous_dissipation,ET_in%viscous_dissipation)
       call init(ET%joule_heating,ET_in%joule_heating)
       call init(ET%volumetric_heating,ET_in%volumetric_heating)
      end subroutine

     subroutine delete_ET(ET)
       implicit none
       type(energy_terms),intent(inout) :: ET
       call delete(ET%advection)
       call delete(ET%diffusion)
       call delete(ET%KE_diffusion)
       call delete(ET%viscous_dissipation)
       call delete(ET%joule_heating)
       call delete(ET%volumetric_heating)
      end subroutine

     subroutine display_ET(ET,un)
       implicit none
       type(energy_terms),intent(in) :: ET
       integer,intent(in) :: un
       call display(ET%advection,un,'advection')
       call display(ET%diffusion,un,'diffusion')
       call display(ET%KE_diffusion,un,'KE_diffusion')
       call display(ET%viscous_dissipation,un,'viscous_dissipation')
       call display(ET%joule_heating,un,'joule_heating')
       call display(ET%volumetric_heating,un,'volumetric_heating')
      end subroutine

     subroutine print_ET(ET)
       implicit none
       type(energy_terms),intent(in) :: ET
       call display(ET,6)
      end subroutine

     subroutine export_ET(ET,un)
       implicit none
       type(energy_terms),intent(in) :: ET
       integer,intent(in) :: un
       write(un,*) ' --------- energy_terms --------- '
       call export(ET%advection,un,'advection')
       call export(ET%diffusion,un,'diffusion')
       call export(ET%KE_diffusion,un,'KE_diffusion')
       call export(ET%viscous_dissipation,un,'viscous_dissipation')
       call export(ET%joule_heating,un,'joule_heating')
       call export(ET%volumetric_heating,un,'volumetric_heating')
       write(un,*) ' -------------------------------- '
      end subroutine

     subroutine import_ET(ET,un)
       implicit none
       type(energy_terms),intent(inout) :: ET
       integer,intent(in) :: un
       read(un,*);
       call import(ET%advection,un)
       call import(ET%diffusion,un)
       call import(ET%KE_diffusion,un)
       call import(ET%viscous_dissipation,un)
       call import(ET%joule_heating,un)
       call import(ET%volumetric_heating,un)
       read(un,*);
      end subroutine

     subroutine export_ET_wrapper(ET,dir,name)
       implicit none
       type(energy_terms),intent(in) :: ET
       character(len=*),intent(in) :: dir,name
       integer :: un
       un = new_and_open(dir,name)
       call export(ET,un)
       call close_and_message(un,dir,name)
      end subroutine

     end module