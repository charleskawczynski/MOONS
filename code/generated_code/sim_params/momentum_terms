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

       interface init;   module procedure init_momentum_terms;           end interface
       interface delete; module procedure delete_momentum_terms;         end interface
       interface display;module procedure display_momentum_terms;        end interface
       interface display;module procedure display_wrapper_momentum_terms;end interface
       interface print;  module procedure print_momentum_terms;          end interface
       interface export; module procedure export_momentum_terms;         end interface
       interface import; module procedure import_momentum_terms;         end interface
       interface export; module procedure export_wrapper_momentum_terms; end interface
       interface import; module procedure import_wrapper_momentum_terms; end interface

       type momentum_terms
         type(equation_term) :: pressure_grad
         type(equation_term) :: advection_divergence
         type(equation_term) :: advection_convection
         type(equation_term) :: advection_base_flow
         type(equation_term) :: diffusion
         type(equation_term) :: mean_pressure_grad
         type(equation_term) :: jcrossb
         type(equation_term) :: q2d_jcrossb
         type(equation_term) :: buoyancy
         type(equation_term) :: gravity
       end type

       contains

       subroutine init_momentum_terms(this,that)
         implicit none
         type(momentum_terms),intent(inout) :: this
         type(momentum_terms),intent(in) :: that
         call delete(this)
         call init(this%pressure_grad,that%pressure_grad)
         call init(this%advection_divergence,that%advection_divergence)
         call init(this%advection_convection,that%advection_convection)
         call init(this%advection_base_flow,that%advection_base_flow)
         call init(this%diffusion,that%diffusion)
         call init(this%mean_pressure_grad,that%mean_pressure_grad)
         call init(this%jcrossb,that%jcrossb)
         call init(this%q2d_jcrossb,that%q2d_jcrossb)
         call init(this%buoyancy,that%buoyancy)
         call init(this%gravity,that%gravity)
       end subroutine

       subroutine delete_momentum_terms(this)
         implicit none
         type(momentum_terms),intent(inout) :: this
         call delete(this%pressure_grad)
         call delete(this%advection_divergence)
         call delete(this%advection_convection)
         call delete(this%advection_base_flow)
         call delete(this%diffusion)
         call delete(this%mean_pressure_grad)
         call delete(this%jcrossb)
         call delete(this%q2d_jcrossb)
         call delete(this%buoyancy)
         call delete(this%gravity)
       end subroutine

       subroutine display_momentum_terms(this,un)
         implicit none
         type(momentum_terms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- momentum_terms'
         call display(this%pressure_grad,un)
         call display(this%advection_divergence,un)
         call display(this%advection_convection,un)
         call display(this%advection_base_flow,un)
         call display(this%diffusion,un)
         call display(this%mean_pressure_grad,un)
         call display(this%jcrossb,un)
         call display(this%q2d_jcrossb,un)
         call display(this%buoyancy,un)
         call display(this%gravity,un)
       end subroutine

       subroutine print_momentum_terms(this)
         implicit none
         type(momentum_terms),intent(in) :: this
         call display(this,6)
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
         call export(this%mean_pressure_grad,un)
         call export(this%jcrossb,un)
         call export(this%q2d_jcrossb,un)
         call export(this%buoyancy,un)
         call export(this%gravity,un)
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
         call import(this%mean_pressure_grad,un)
         call import(this%jcrossb,un)
         call import(this%q2d_jcrossb,un)
         call import(this%buoyancy,un)
         call import(this%gravity,un)
       end subroutine

       subroutine display_wrapper_momentum_terms(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_momentum_terms(this,dir,name)
         implicit none
         type(momentum_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_momentum_terms(this,dir,name)
         implicit none
         type(momentum_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module