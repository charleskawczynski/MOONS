       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module density_mod
       use IO_tools_mod
       use PCG_solver_VF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       use mesh_domain_mod
       implicit none

       private
       public :: density
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_density;           end interface
       interface delete; module procedure delete_density;         end interface
       interface display;module procedure display_density;        end interface
       interface display;module procedure display_wrapper_density;end interface
       interface print;  module procedure print_density;          end interface
       interface export; module procedure export_density;         end interface
       interface import; module procedure import_density;         end interface
       interface export; module procedure export_wrapper_density; end interface
       interface import; module procedure import_wrapper_density; end interface

       type density
         type(mesh) :: m
         type(pcg_solver_vf) :: pcg_rho
         type(sf) :: rho
         type(sf) :: temp_cc1
         type(vf) :: temp_f
         type(vf) :: k
         type(vf) :: u_f
         type(mesh_domain) :: md
       end type

       contains

       subroutine init_density(this,that)
         implicit none
         type(density),intent(inout) :: this
         type(density),intent(in) :: that
         call delete(this)
         call init(this%m,that%m)
         call init(this%pcg_rho,that%pcg_rho)
         call init(this%rho,that%rho)
         call init(this%temp_cc1,that%temp_cc1)
         call init(this%temp_f,that%temp_f)
         call init(this%k,that%k)
         call init(this%u_f,that%u_f)
         call init(this%md,that%md)
       end subroutine

       subroutine delete_density(this)
         implicit none
         type(density),intent(inout) :: this
         call delete(this%m)
         call delete(this%pcg_rho)
         call delete(this%rho)
         call delete(this%temp_cc1)
         call delete(this%temp_f)
         call delete(this%k)
         call delete(this%u_f)
         call delete(this%md)
       end subroutine

       subroutine display_density(this,un)
         implicit none
         type(density),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- density'
         call display(this%m,un)
         call display(this%pcg_rho,un)
         call display(this%rho,un)
         call display(this%temp_cc1,un)
         call display(this%temp_f,un)
         call display(this%k,un)
         call display(this%u_f,un)
         call display(this%md,un)
       end subroutine

       subroutine print_density(this)
         implicit none
         type(density),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_density(this,un)
         implicit none
         type(density),intent(in) :: this
         integer,intent(in) :: un
         call export(this%m,un)
         call export(this%pcg_rho,un)
         call export(this%rho,un)
         call export(this%temp_cc1,un)
         call export(this%temp_f,un)
         call export(this%k,un)
         call export(this%u_f,un)
         call export(this%md,un)
       end subroutine

       subroutine import_density(this,un)
         implicit none
         type(density),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%m,un)
         call import(this%pcg_rho,un)
         call import(this%rho,un)
         call import(this%temp_cc1,un)
         call import(this%temp_f,un)
         call import(this%k,un)
         call import(this%u_f,un)
         call import(this%md,un)
       end subroutine

       subroutine display_wrapper_density(this,dir,name)
         implicit none
         type(density),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_density(this,dir,name)
         implicit none
         type(density),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_density(this,dir,name)
         implicit none
         type(density),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module