       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module momentum_terms_mod
       use IO_tools_mod
       use equation_term_mod
       implicit none

       private
       public :: momentum_terms
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_mo;    end interface
       interface delete;       module procedure delete_mo;       end interface
       interface display;      module procedure display_mo;      end interface
       interface display_short;module procedure display_short_mo;end interface
       interface display;      module procedure display_wrap_mo; end interface
       interface print;        module procedure print_mo;        end interface
       interface print_short;  module procedure print_short_mo;  end interface
       interface export;       module procedure export_mo;       end interface
       interface import;       module procedure import_mo;       end interface
       interface export;       module procedure export_wrap_mo;  end interface
       interface import;       module procedure import_wrap_mo;  end interface

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

       subroutine init_copy_mo(this,that)
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

       subroutine delete_mo(this)
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

       subroutine display_mo(this,un)
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

       subroutine display_short_mo(this,un)
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

       subroutine print_mo(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_mo(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_mo(this,un)
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

       subroutine import_mo(this,un)
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

       subroutine display_wrap_mo(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_mo(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_mo(this,dir,name)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module