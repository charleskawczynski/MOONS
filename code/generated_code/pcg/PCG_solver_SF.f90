       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module pcg_solver_sf_mod
       use IO_tools_mod
       use SF_mod
       use TF_mod
       use iter_solver_params_mod
       use matrix_free_operators_mod
       use matrix_free_params_mod
       use norms_mod
       use preconditioners_mod
       use string_mod
       implicit none

       private
       public :: pcg_solver_sf
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_pcg_solver_sf;           end interface
       interface delete; module procedure delete_pcg_solver_sf;         end interface
       interface display;module procedure display_pcg_solver_sf;        end interface
       interface display;module procedure display_wrapper_pcg_solver_sf;end interface
       interface print;  module procedure print_pcg_solver_sf;          end interface
       interface export; module procedure export_pcg_solver_sf;         end interface
       interface import; module procedure import_pcg_solver_sf;         end interface
       interface export; module procedure export_wrapper_pcg_solver_sf; end interface
       interface import; module procedure import_wrapper_pcg_solver_sf; end interface

       type pcg_solver_sf
         integer :: un = 0
         integer :: un_convergence = 0
         type(matrix_free_params) :: mfp
         type(tf) :: tempk
         type(tf) :: k
         type(sf) :: r
         type(sf) :: p
         type(sf) :: tempx
         type(sf) :: ax
         type(sf) :: x_bc
         type(sf) :: vol
         type(sf) :: z
         type(sf) :: minv
         type(norms) :: norm
         type(iter_solver_params) :: isp
         type(string) :: dir
         type(string) :: name
         procedure(preconditioner_sf),pointer,nopass :: prec
         procedure(op_sf),pointer,nopass :: operator
         procedure(op_sf_explicit),pointer,nopass :: operator_explicit
       end type

       contains

       subroutine init_pcg_solver_sf(this,that)
         implicit none
         type(pcg_solver_sf),intent(inout) :: this
         type(pcg_solver_sf),intent(in) :: that
         call delete(this)
         this%un = that%un
         this%un_convergence = that%un_convergence
         call init(this%mfp,that%mfp)
         call init(this%tempk,that%tempk)
         call init(this%k,that%k)
         call init(this%r,that%r)
         call init(this%p,that%p)
         call init(this%tempx,that%tempx)
         call init(this%ax,that%ax)
         call init(this%x_bc,that%x_bc)
         call init(this%vol,that%vol)
         call init(this%z,that%z)
         call init(this%minv,that%minv)
         call init(this%norm,that%norm)
         call init(this%isp,that%isp)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         this%prec => that%prec
         this%operator => that%operator
         this%operator_explicit => that%operator_explicit
       end subroutine

       subroutine delete_pcg_solver_sf(this)
         implicit none
         type(pcg_solver_sf),intent(inout) :: this
         this%un = 0
         this%un_convergence = 0
         call delete(this%mfp)
         call delete(this%tempk)
         call delete(this%k)
         call delete(this%r)
         call delete(this%p)
         call delete(this%tempx)
         call delete(this%ax)
         call delete(this%x_bc)
         call delete(this%vol)
         call delete(this%z)
         call delete(this%minv)
         call delete(this%norm)
         call delete(this%isp)
         call delete(this%dir)
         call delete(this%name)
         nullify(this%prec)
         nullify(this%operator)
         nullify(this%operator_explicit)
       end subroutine

       subroutine display_pcg_solver_sf(this,un)
         implicit none
         type(pcg_solver_sf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- pcg_solver_sf'
         write(un,*) 'un                = ',this%un
         write(un,*) 'un_convergence    = ',this%un_convergence
         call display(this%mfp,un)
         call display(this%tempk,un)
         call display(this%k,un)
         call display(this%r,un)
         call display(this%p,un)
         call display(this%tempx,un)
         call display(this%ax,un)
         call display(this%x_bc,un)
         call display(this%vol,un)
         call display(this%z,un)
         call display(this%minv,un)
         call display(this%norm,un)
         call display(this%isp,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_pcg_solver_sf(this)
         implicit none
         type(pcg_solver_sf),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_pcg_solver_sf(this,un)
         implicit none
         type(pcg_solver_sf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un                 = ';write(un,*) this%un
         write(un,*) 'un_convergence     = ';write(un,*) this%un_convergence
         call export(this%mfp,un)
         call export(this%tempk,un)
         call export(this%k,un)
         call export(this%r,un)
         call export(this%p,un)
         call export(this%tempx,un)
         call export(this%ax,un)
         call export(this%x_bc,un)
         call export(this%vol,un)
         call export(this%z,un)
         call export(this%minv,un)
         call export(this%norm,un)
         call export(this%isp,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_pcg_solver_sf(this,un)
         implicit none
         type(pcg_solver_sf),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%un_convergence
         call import(this%mfp,un)
         call import(this%tempk,un)
         call import(this%k,un)
         call import(this%r,un)
         call import(this%p,un)
         call import(this%tempx,un)
         call import(this%ax,un)
         call import(this%x_bc,un)
         call import(this%vol,un)
         call import(this%z,un)
         call import(this%minv,un)
         call import(this%norm,un)
         call import(this%isp,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrapper_pcg_solver_sf(this,dir,name)
         implicit none
         type(pcg_solver_sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_pcg_solver_sf(this,dir,name)
         implicit none
         type(pcg_solver_sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_pcg_solver_sf(this,dir,name)
         implicit none
         type(pcg_solver_sf),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module