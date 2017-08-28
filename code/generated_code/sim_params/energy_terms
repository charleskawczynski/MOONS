       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module energy_terms_mod
       use IO_tools_mod
       use equation_term_mod
       implicit none

       private
       public :: energy_terms
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_energy_terms;           end interface
       interface delete; module procedure delete_energy_terms;         end interface
       interface display;module procedure display_energy_terms;        end interface
       interface display;module procedure display_wrapper_energy_terms;end interface
       interface print;  module procedure print_energy_terms;          end interface
       interface export; module procedure export_energy_terms;         end interface
       interface import; module procedure import_energy_terms;         end interface
       interface export; module procedure export_wrapper_energy_terms; end interface
       interface import; module procedure import_wrapper_energy_terms; end interface

       type energy_terms
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: ke_diffusion
         type(equation_term) :: viscous_dissipation
         type(equation_term) :: joule_heating
         type(equation_term) :: volumetric_heating
       end type

       contains

       subroutine init_energy_terms(this,that)
         implicit none
         type(energy_terms),intent(inout) :: this
         type(energy_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%ke_diffusion,that%ke_diffusion)
         call init(this%viscous_dissipation,that%viscous_dissipation)
         call init(this%joule_heating,that%joule_heating)
         call init(this%volumetric_heating,that%volumetric_heating)
       end subroutine

       subroutine delete_energy_terms(this)
         implicit none
         type(energy_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%ke_diffusion)
         call delete(this%viscous_dissipation)
         call delete(this%joule_heating)
         call delete(this%volumetric_heating)
       end subroutine

       subroutine display_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- energy_terms'
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%ke_diffusion,un)
         call display(this%viscous_dissipation,un)
         call display(this%joule_heating,un)
         call display(this%volumetric_heating,un)
       end subroutine

       subroutine print_energy_terms(this)
         implicit none
         type(energy_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%ke_diffusion,un)
         call export(this%viscous_dissipation,un)
         call export(this%joule_heating,un)
         call export(this%volumetric_heating,un)
       end subroutine

       subroutine import_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%advection,un)
         call import(this%diffusion,un)
         call import(this%ke_diffusion,un)
         call import(this%viscous_dissipation,un)
         call import(this%joule_heating,un)
         call import(this%volumetric_heating,un)
       end subroutine

       subroutine display_wrapper_energy_terms(this,dir,name)
         implicit none
         type(energy_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_energy_terms(this,dir,name)
         implicit none
         type(energy_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_energy_terms(this,dir,name)
         implicit none
         type(energy_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module