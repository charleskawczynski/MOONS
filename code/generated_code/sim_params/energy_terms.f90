       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module energy_terms_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use equation_term_mod
       use string_mod
       implicit none

       private
       public :: energy_terms
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_energy_terms;          end interface
       interface delete;           module procedure delete_energy_terms;             end interface
       interface display;          module procedure display_energy_terms;            end interface
       interface display_short;    module procedure display_short_energy_terms;      end interface
       interface display;          module procedure display_wrap_energy_terms;       end interface
       interface print;            module procedure print_energy_terms;              end interface
       interface print_short;      module procedure print_short_energy_terms;        end interface
       interface export;           module procedure export_energy_terms;             end interface
       interface export_primitives;module procedure export_primitives_energy_terms;  end interface
       interface import;           module procedure import_energy_terms;             end interface
       interface export_structured;module procedure export_structured_D_energy_terms;end interface
       interface import_structured;module procedure import_structured_D_energy_terms;end interface
       interface import_primitives;module procedure import_primitives_energy_terms;  end interface
       interface export;           module procedure export_wrap_energy_terms;        end interface
       interface import;           module procedure import_wrap_energy_terms;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_energy_terms;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_energy_terms;        end interface
       interface suppress_warnings;module procedure suppress_warnings_energy_terms;  end interface

       type energy_terms
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: diffusion_linear
         type(equation_term) :: KE_diffusion
         type(equation_term) :: viscous_dissipation
         type(equation_term) :: joule_heating
         type(equation_term) :: volumetric_heating
       end type

       contains

       subroutine init_copy_energy_terms(this,that)
         implicit none
         type(energy_terms),intent(inout) :: this
         type(energy_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%diffusion_linear,that%diffusion_linear)
         call init(this%KE_diffusion,that%KE_diffusion)
         call init(this%viscous_dissipation,that%viscous_dissipation)
         call init(this%joule_heating,that%joule_heating)
         call init(this%volumetric_heating,that%volumetric_heating)
       end subroutine

       subroutine delete_energy_terms(this)
         implicit none
         type(energy_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%diffusion_linear)
         call delete(this%KE_diffusion)
         call delete(this%viscous_dissipation)
         call delete(this%joule_heating)
         call delete(this%volumetric_heating)
       end subroutine

       subroutine display_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%KE_diffusion,un)
         call display(this%viscous_dissipation,un)
         call display(this%joule_heating,un)
         call display(this%volumetric_heating,un)
       end subroutine

       subroutine display_short_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%KE_diffusion,un)
         call display(this%viscous_dissipation,un)
         call display(this%joule_heating,un)
         call display(this%volumetric_heating,un)
       end subroutine

       subroutine display_wrap_energy_terms(this,dir,name)
         implicit none
         type(energy_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_energy_terms(this)
         implicit none
         type(energy_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_energy_terms(this)
         implicit none
         type(energy_terms),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%diffusion_linear,un)
         call export(this%KE_diffusion,un)
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
         call import(this%diffusion_linear,un)
         call import(this%KE_diffusion,un)
         call import(this%viscous_dissipation,un)
         call import(this%joule_heating,un)
         call import(this%volumetric_heating,un)
       end subroutine

       subroutine export_primitives_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_energy_terms(this,un)
         implicit none
         type(energy_terms),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_energy_terms(this,dir,name)
         implicit none
         type(energy_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_energy_terms(this,dir,name)
         implicit none
         type(energy_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_energy_terms(this,dir)
         implicit none
         type(energy_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%advection,dir//'advection'//fortran_PS)
         call set_IO_dir(this%diffusion,dir//'diffusion'//fortran_PS)
         call set_IO_dir(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call set_IO_dir(this%KE_diffusion,dir//'KE_diffusion'//fortran_PS)
         call set_IO_dir(this%viscous_dissipation,&
         dir//'viscous_dissipation'//fortran_PS)
         call set_IO_dir(this%joule_heating,&
         dir//'joule_heating'//fortran_PS)
         call set_IO_dir(this%volumetric_heating,&
         dir//'volumetric_heating'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_energy_terms(this,dir)
         implicit none
         type(energy_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%advection,dir//'advection'//fortran_PS)
         call make_IO_dir(this%diffusion,dir//'diffusion'//fortran_PS)
         call make_IO_dir(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call make_IO_dir(this%KE_diffusion,dir//'KE_diffusion'//fortran_PS)
         call make_IO_dir(this%viscous_dissipation,&
         dir//'viscous_dissipation'//fortran_PS)
         call make_IO_dir(this%joule_heating,&
         dir//'joule_heating'//fortran_PS)
         call make_IO_dir(this%volumetric_heating,&
         dir//'volumetric_heating'//fortran_PS)
       end subroutine

       subroutine export_structured_D_energy_terms(this,dir)
         implicit none
         type(energy_terms),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%advection,dir//'advection'//fortran_PS)
         call export_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call export_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call export_structured(this%KE_diffusion,&
         dir//'KE_diffusion'//fortran_PS)
         call export_structured(this%viscous_dissipation,&
         dir//'viscous_dissipation'//fortran_PS)
         call export_structured(this%joule_heating,&
         dir//'joule_heating'//fortran_PS)
         call export_structured(this%volumetric_heating,&
         dir//'volumetric_heating'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_energy_terms(this,dir)
         implicit none
         type(energy_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call delete(this)
         call import_primitives(this,un)
         call import_structured(this%advection,dir//'advection'//fortran_PS)
         call import_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call import_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call import_structured(this%KE_diffusion,&
         dir//'KE_diffusion'//fortran_PS)
         call import_structured(this%viscous_dissipation,&
         dir//'viscous_dissipation'//fortran_PS)
         call import_structured(this%joule_heating,&
         dir//'joule_heating'//fortran_PS)
         call import_structured(this%volumetric_heating,&
         dir//'volumetric_heating'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_energy_terms(this)
         implicit none
         type(energy_terms),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module