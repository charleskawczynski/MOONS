       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module momentum_terms_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use equation_term_mod
       use string_mod
       implicit none

       private
       public :: momentum_terms
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_momentum_terms;          end interface
       interface delete;           module procedure delete_momentum_terms;             end interface
       interface display;          module procedure display_momentum_terms;            end interface
       interface display_short;    module procedure display_short_momentum_terms;      end interface
       interface display;          module procedure display_wrap_momentum_terms;       end interface
       interface print;            module procedure print_momentum_terms;              end interface
       interface print_short;      module procedure print_short_momentum_terms;        end interface
       interface export;           module procedure export_momentum_terms;             end interface
       interface export_primitives;module procedure export_primitives_momentum_terms;  end interface
       interface import;           module procedure import_momentum_terms;             end interface
       interface export_structured;module procedure export_structured_D_momentum_terms;end interface
       interface import_structured;module procedure import_structured_D_momentum_terms;end interface
       interface import_primitives;module procedure import_primitives_momentum_terms;  end interface
       interface export;           module procedure export_wrap_momentum_terms;        end interface
       interface import;           module procedure import_wrap_momentum_terms;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_momentum_terms;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_momentum_terms;        end interface
       interface suppress_warnings;module procedure suppress_warnings_momentum_terms;  end interface

       type momentum_terms
         type(equation_term) :: pressure_grad
         type(equation_term) :: advection_divergence
         type(equation_term) :: advection_convection
         type(equation_term) :: advection_base_flow
         type(equation_term) :: diffusion
         type(equation_term) :: diffusion_linear
         type(equation_term) :: mean_pressure_grad
         type(equation_term) :: JCrossB
         type(equation_term) :: Q2D_JCrossB
         type(equation_term) :: Buoyancy
         type(equation_term) :: Gravity
       end type

       contains

       subroutine init_copy_momentum_terms(this,that)
         implicit none
         type(momentum_terms),intent(inout) :: this
         type(momentum_terms),intent(in) :: that
         call delete(this)
         call init(this%pressure_grad,that%pressure_grad)
         call init(this%advection_divergence,that%advection_divergence)
         call init(this%advection_convection,that%advection_convection)
         call init(this%advection_base_flow,that%advection_base_flow)
         call init(this%diffusion,that%diffusion)
         call init(this%diffusion_linear,that%diffusion_linear)
         call init(this%mean_pressure_grad,that%mean_pressure_grad)
         call init(this%JCrossB,that%JCrossB)
         call init(this%Q2D_JCrossB,that%Q2D_JCrossB)
         call init(this%Buoyancy,that%Buoyancy)
         call init(this%Gravity,that%Gravity)
       end subroutine

       subroutine delete_momentum_terms(this)
         implicit none
         type(momentum_terms),intent(inout) :: this
         call delete(this%pressure_grad)
         call delete(this%advection_divergence)
         call delete(this%advection_convection)
         call delete(this%advection_base_flow)
         call delete(this%diffusion)
         call delete(this%diffusion_linear)
         call delete(this%mean_pressure_grad)
         call delete(this%JCrossB)
         call delete(this%Q2D_JCrossB)
         call delete(this%Buoyancy)
         call delete(this%Gravity)
       end subroutine

       subroutine display_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%pressure_grad,un)
         call display(this%advection_divergence,un)
         call display(this%advection_convection,un)
         call display(this%advection_base_flow,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%mean_pressure_grad,un)
         call display(this%JCrossB,un)
         call display(this%Q2D_JCrossB,un)
         call display(this%Buoyancy,un)
         call display(this%Gravity,un)
       end subroutine

       subroutine display_short_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%pressure_grad,un)
         call display(this%advection_divergence,un)
         call display(this%advection_convection,un)
         call display(this%advection_base_flow,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%mean_pressure_grad,un)
         call display(this%JCrossB,un)
         call display(this%Q2D_JCrossB,un)
         call display(this%Buoyancy,un)
         call display(this%Gravity,un)
       end subroutine

       subroutine display_wrap_momentum_terms(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_momentum_terms(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_momentum_terms(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%pressure_grad,un)
         call export(this%advection_divergence,un)
         call export(this%advection_convection,un)
         call export(this%advection_base_flow,un)
         call export(this%diffusion,un)
         call export(this%diffusion_linear,un)
         call export(this%mean_pressure_grad,un)
         call export(this%JCrossB,un)
         call export(this%Q2D_JCrossB,un)
         call export(this%Buoyancy,un)
         call export(this%Gravity,un)
       end subroutine

       subroutine import_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%pressure_grad,un)
         call import(this%advection_divergence,un)
         call import(this%advection_convection,un)
         call import(this%advection_base_flow,un)
         call import(this%diffusion,un)
         call import(this%diffusion_linear,un)
         call import(this%mean_pressure_grad,un)
         call import(this%JCrossB,un)
         call import(this%Q2D_JCrossB,un)
         call import(this%Buoyancy,un)
         call import(this%Gravity,un)
       end subroutine

       subroutine export_primitives_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_momentum_terms(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_momentum_terms(this,dir,name)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_momentum_terms(this,dir)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%pressure_grad,dir//'pressure_grad'//fortran_PS)
         call set_IO_dir(this%advection_divergence,&
         dir//'advection_divergence'//fortran_PS)
         call set_IO_dir(this%advection_convection,&
         dir//'advection_convection'//fortran_PS)
         call set_IO_dir(this%advection_base_flow,&
         dir//'advection_base_flow'//fortran_PS)
         call set_IO_dir(this%diffusion,dir//'diffusion'//fortran_PS)
         call set_IO_dir(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call set_IO_dir(this%mean_pressure_grad,&
         dir//'mean_pressure_grad'//fortran_PS)
         call set_IO_dir(this%JCrossB,dir//'JCrossB'//fortran_PS)
         call set_IO_dir(this%Q2D_JCrossB,dir//'Q2D_JCrossB'//fortran_PS)
         call set_IO_dir(this%Buoyancy,dir//'Buoyancy'//fortran_PS)
         call set_IO_dir(this%Gravity,dir//'Gravity'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_momentum_terms(this,dir)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%pressure_grad,dir//'pressure_grad'//fortran_PS)
         call make_IO_dir(this%advection_divergence,&
         dir//'advection_divergence'//fortran_PS)
         call make_IO_dir(this%advection_convection,&
         dir//'advection_convection'//fortran_PS)
         call make_IO_dir(this%advection_base_flow,&
         dir//'advection_base_flow'//fortran_PS)
         call make_IO_dir(this%diffusion,dir//'diffusion'//fortran_PS)
         call make_IO_dir(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call make_IO_dir(this%mean_pressure_grad,&
         dir//'mean_pressure_grad'//fortran_PS)
         call make_IO_dir(this%JCrossB,dir//'JCrossB'//fortran_PS)
         call make_IO_dir(this%Q2D_JCrossB,dir//'Q2D_JCrossB'//fortran_PS)
         call make_IO_dir(this%Buoyancy,dir//'Buoyancy'//fortran_PS)
         call make_IO_dir(this%Gravity,dir//'Gravity'//fortran_PS)
       end subroutine

       subroutine export_structured_D_momentum_terms(this,dir)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%pressure_grad,&
         dir//'pressure_grad'//fortran_PS)
         call export_structured(this%advection_divergence,&
         dir//'advection_divergence'//fortran_PS)
         call export_structured(this%advection_convection,&
         dir//'advection_convection'//fortran_PS)
         call export_structured(this%advection_base_flow,&
         dir//'advection_base_flow'//fortran_PS)
         call export_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call export_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call export_structured(this%mean_pressure_grad,&
         dir//'mean_pressure_grad'//fortran_PS)
         call export_structured(this%JCrossB,dir//'JCrossB'//fortran_PS)
         call export_structured(this%Q2D_JCrossB,&
         dir//'Q2D_JCrossB'//fortran_PS)
         call export_structured(this%Buoyancy,dir//'Buoyancy'//fortran_PS)
         call export_structured(this%Gravity,dir//'Gravity'//fortran_PS)
       end subroutine

       subroutine import_structured_D_momentum_terms(this,dir)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%pressure_grad,&
         dir//'pressure_grad'//fortran_PS)
         call import_structured(this%advection_divergence,&
         dir//'advection_divergence'//fortran_PS)
         call import_structured(this%advection_convection,&
         dir//'advection_convection'//fortran_PS)
         call import_structured(this%advection_base_flow,&
         dir//'advection_base_flow'//fortran_PS)
         call import_structured(this%diffusion,dir//'diffusion'//fortran_PS)
         call import_structured(this%diffusion_linear,&
         dir//'diffusion_linear'//fortran_PS)
         call import_structured(this%mean_pressure_grad,&
         dir//'mean_pressure_grad'//fortran_PS)
         call import_structured(this%JCrossB,dir//'JCrossB'//fortran_PS)
         call import_structured(this%Q2D_JCrossB,&
         dir//'Q2D_JCrossB'//fortran_PS)
         call import_structured(this%Buoyancy,dir//'Buoyancy'//fortran_PS)
         call import_structured(this%Gravity,dir//'Gravity'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_momentum_terms(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module