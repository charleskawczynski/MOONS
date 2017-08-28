       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module momentum_mod
       use IO_tools_mod
       use PCG_solver_SF_mod
       use PCG_solver_VF_mod
       use SF_mod
       use TF_mod
       use VF_mod
       use mesh_mod
       use probe_mod
       use time_statistics_VF_mod
       implicit none

       private
       public :: momentum
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_momentum;           end interface
       interface delete; module procedure delete_momentum;         end interface
       interface display;module procedure display_momentum;        end interface
       interface display;module procedure display_wrapper_momentum;end interface
       interface print;  module procedure print_momentum;          end interface
       interface export; module procedure export_momentum;         end interface
       interface import; module procedure import_momentum;         end interface
       interface export; module procedure export_wrapper_momentum; end interface
       interface import; module procedure import_wrapper_momentum; end interface

       type momentum
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(pcg_solver_sf) :: pcg_p
         type(pcg_solver_vf) :: pcg_u
         type(time_statistics_vf) :: ts
         type(sf) :: p
         type(sf) :: divu
         type(sf) :: temp_cc
         type(vf) :: u
         type(vf) :: ustar
         type(vf) :: unm1
         type(vf) :: u_cc
         type(vf) :: f
         type(vf) :: fnm1
         type(vf) :: l
         type(vf) :: temp_f1
         type(vf) :: temp_f2
         type(vf) :: temp_f3
         type(vf) :: temp_e
         type(vf) :: temp_cc_vf
         type(tf) :: u_e
         type(tf) :: tf_cc
         type(tf) :: tf_cc_edge
         type(probe) :: probe_ke
         type(probe) :: probe_ke_2c
         type(probe) :: probe_divu
         type(probe) :: probe_q
       end type

       contains

       subroutine init_momentum(this,that)
         implicit none
         type(momentum),intent(inout) :: this
         type(momentum),intent(in) :: that
         call delete(this)
         this%suppress_warning = that%suppress_warning
         call init(this%m,that%m)
         call init(this%pcg_p,that%pcg_p)
         call init(this%pcg_u,that%pcg_u)
         call init(this%ts,that%ts)
         call init(this%p,that%p)
         call init(this%divu,that%divu)
         call init(this%temp_cc,that%temp_cc)
         call init(this%u,that%u)
         call init(this%ustar,that%ustar)
         call init(this%unm1,that%unm1)
         call init(this%u_cc,that%u_cc)
         call init(this%f,that%f)
         call init(this%fnm1,that%fnm1)
         call init(this%l,that%l)
         call init(this%temp_f1,that%temp_f1)
         call init(this%temp_f2,that%temp_f2)
         call init(this%temp_f3,that%temp_f3)
         call init(this%temp_e,that%temp_e)
         call init(this%temp_cc_vf,that%temp_cc_vf)
         call init(this%u_e,that%u_e)
         call init(this%tf_cc,that%tf_cc)
         call init(this%tf_cc_edge,that%tf_cc_edge)
         call init(this%probe_ke,that%probe_ke)
         call init(this%probe_ke_2c,that%probe_ke_2c)
         call init(this%probe_divu,that%probe_divu)
         call init(this%probe_q,that%probe_q)
       end subroutine

       subroutine delete_momentum(this)
         implicit none
         type(momentum),intent(inout) :: this
         this%suppress_warning = .false.
         call delete(this%m)
         call delete(this%pcg_p)
         call delete(this%pcg_u)
         call delete(this%ts)
         call delete(this%p)
         call delete(this%divu)
         call delete(this%temp_cc)
         call delete(this%u)
         call delete(this%ustar)
         call delete(this%unm1)
         call delete(this%u_cc)
         call delete(this%f)
         call delete(this%fnm1)
         call delete(this%l)
         call delete(this%temp_f1)
         call delete(this%temp_f2)
         call delete(this%temp_f3)
         call delete(this%temp_e)
         call delete(this%temp_cc_vf)
         call delete(this%u_e)
         call delete(this%tf_cc)
         call delete(this%tf_cc_edge)
         call delete(this%probe_ke)
         call delete(this%probe_ke_2c)
         call delete(this%probe_divu)
         call delete(this%probe_q)
       end subroutine

       subroutine display_momentum(this,un)
         implicit none
         type(momentum),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- momentum'
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%pcg_p,un)
         call display(this%pcg_u,un)
         call display(this%ts,un)
         call display(this%p,un)
         call display(this%divu,un)
         call display(this%temp_cc,un)
         call display(this%u,un)
         call display(this%ustar,un)
         call display(this%unm1,un)
         call display(this%u_cc,un)
         call display(this%f,un)
         call display(this%fnm1,un)
         call display(this%l,un)
         call display(this%temp_f1,un)
         call display(this%temp_f2,un)
         call display(this%temp_f3,un)
         call display(this%temp_e,un)
         call display(this%temp_cc_vf,un)
         call display(this%u_e,un)
         call display(this%tf_cc,un)
         call display(this%tf_cc_edge,un)
         call display(this%probe_ke,un)
         call display(this%probe_ke_2c,un)
         call display(this%probe_divu,un)
         call display(this%probe_q,un)
       end subroutine

       subroutine print_momentum(this)
         implicit none
         type(momentum),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_momentum(this,un)
         implicit none
         type(momentum),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
         call export(this%m,un)
         call export(this%pcg_p,un)
         call export(this%pcg_u,un)
         call export(this%ts,un)
         call export(this%p,un)
         call export(this%divu,un)
         call export(this%temp_cc,un)
         call export(this%u,un)
         call export(this%ustar,un)
         call export(this%unm1,un)
         call export(this%u_cc,un)
         call export(this%f,un)
         call export(this%fnm1,un)
         call export(this%l,un)
         call export(this%temp_f1,un)
         call export(this%temp_f2,un)
         call export(this%temp_f3,un)
         call export(this%temp_e,un)
         call export(this%temp_cc_vf,un)
         call export(this%u_e,un)
         call export(this%tf_cc,un)
         call export(this%tf_cc_edge,un)
         call export(this%probe_ke,un)
         call export(this%probe_ke_2c,un)
         call export(this%probe_divu,un)
         call export(this%probe_q,un)
       end subroutine

       subroutine import_momentum(this,un)
         implicit none
         type(momentum),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%suppress_warning
         call import(this%m,un)
         call import(this%pcg_p,un)
         call import(this%pcg_u,un)
         call import(this%ts,un)
         call import(this%p,un)
         call import(this%divu,un)
         call import(this%temp_cc,un)
         call import(this%u,un)
         call import(this%ustar,un)
         call import(this%unm1,un)
         call import(this%u_cc,un)
         call import(this%f,un)
         call import(this%fnm1,un)
         call import(this%l,un)
         call import(this%temp_f1,un)
         call import(this%temp_f2,un)
         call import(this%temp_f3,un)
         call import(this%temp_e,un)
         call import(this%temp_cc_vf,un)
         call import(this%u_e,un)
         call import(this%tf_cc,un)
         call import(this%tf_cc_edge,un)
         call import(this%probe_ke,un)
         call import(this%probe_ke_2c,un)
         call import(this%probe_divu,un)
         call import(this%probe_q,un)
       end subroutine

       subroutine display_wrapper_momentum(this,dir,name)
         implicit none
         type(momentum),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_momentum(this,dir,name)
         implicit none
         type(momentum),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_momentum(this,dir,name)
         implicit none
         type(momentum),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module