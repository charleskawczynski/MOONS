       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module energy_mod
       use IO_tools_mod
       use PCG_solver_VF_mod
       use SF_mod
       use TF_mod
       use VF_mod
       use mesh_mod
       use mesh_domain_mod
       use probe_mod
       implicit none

       private
       public :: energy
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_energy;           end interface
       interface delete; module procedure delete_energy;         end interface
       interface display;module procedure display_energy;        end interface
       interface display;module procedure display_wrapper_energy;end interface
       interface print;  module procedure print_energy;          end interface
       interface export; module procedure export_energy;         end interface
       interface import; module procedure import_energy;         end interface
       interface export; module procedure export_wrapper_energy; end interface
       interface import; module procedure import_wrapper_energy; end interface

       type energy
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(pcg_solver_vf) :: pcg_t
         type(sf) :: t
         type(sf) :: tnm1
         type(sf) :: temp_cc1
         type(sf) :: temp_cc2
         type(sf) :: f
         type(sf) :: fnm1
         type(sf) :: l
         type(sf) :: divq
         type(sf) :: q_source
         type(vf) :: temp_f
         type(vf) :: k
         type(vf) :: u_f
         type(vf) :: u_cc
         type(vf) :: gravity
         type(vf) :: temp_cc1_vf
         type(vf) :: temp_cc2_vf
         type(tf) :: temp_cc_tf
         type(tf) :: temp_f_tf
         type(probe),dimension(3) :: probe_divq
         type(mesh_domain) :: md
       end type

       contains

       subroutine init_energy(this,that)
         implicit none
         type(energy),intent(inout) :: this
         type(energy),intent(in) :: that
         integer :: i_probe_divq
         integer :: s_probe_divq
         call delete(this)
         this%suppress_warning = that%suppress_warning
         call init(this%m,that%m)
         call init(this%pcg_t,that%pcg_t)
         call init(this%t,that%t)
         call init(this%tnm1,that%tnm1)
         call init(this%temp_cc1,that%temp_cc1)
         call init(this%temp_cc2,that%temp_cc2)
         call init(this%f,that%f)
         call init(this%fnm1,that%fnm1)
         call init(this%l,that%l)
         call init(this%divq,that%divq)
         call init(this%q_source,that%q_source)
         call init(this%temp_f,that%temp_f)
         call init(this%k,that%k)
         call init(this%u_f,that%u_f)
         call init(this%u_cc,that%u_cc)
         call init(this%gravity,that%gravity)
         call init(this%temp_cc1_vf,that%temp_cc1_vf)
         call init(this%temp_cc2_vf,that%temp_cc2_vf)
         call init(this%temp_cc_tf,that%temp_cc_tf)
         call init(this%temp_f_tf,that%temp_f_tf)
         s_probe_divq = size(that%probe_divq)
         do i_probe_divq=1,s_probe_divq
           call init(this%probe_divq(i_probe_divq),that%probe_divq(i_probe_divq))
         enddo
         call init(this%md,that%md)
       end subroutine

       subroutine delete_energy(this)
         implicit none
         type(energy),intent(inout) :: this
         integer :: i_probe_divq
         integer :: s_probe_divq
         this%suppress_warning = .false.
         call delete(this%m)
         call delete(this%pcg_t)
         call delete(this%t)
         call delete(this%tnm1)
         call delete(this%temp_cc1)
         call delete(this%temp_cc2)
         call delete(this%f)
         call delete(this%fnm1)
         call delete(this%l)
         call delete(this%divq)
         call delete(this%q_source)
         call delete(this%temp_f)
         call delete(this%k)
         call delete(this%u_f)
         call delete(this%u_cc)
         call delete(this%gravity)
         call delete(this%temp_cc1_vf)
         call delete(this%temp_cc2_vf)
         call delete(this%temp_cc_tf)
         call delete(this%temp_f_tf)
         s_probe_divq = size(this%probe_divq)
         do i_probe_divq=1,s_probe_divq
           call delete(this%probe_divq(i_probe_divq))
         enddo
         call delete(this%md)
       end subroutine

       subroutine display_energy(this,un)
         implicit none
         type(energy),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- energy'
         integer :: i_probe_divq
         integer :: s_probe_divq
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%pcg_t,un)
         call display(this%t,un)
         call display(this%tnm1,un)
         call display(this%temp_cc1,un)
         call display(this%temp_cc2,un)
         call display(this%f,un)
         call display(this%fnm1,un)
         call display(this%l,un)
         call display(this%divq,un)
         call display(this%q_source,un)
         call display(this%temp_f,un)
         call display(this%k,un)
         call display(this%u_f,un)
         call display(this%u_cc,un)
         call display(this%gravity,un)
         call display(this%temp_cc1_vf,un)
         call display(this%temp_cc2_vf,un)
         call display(this%temp_cc_tf,un)
         call display(this%temp_f_tf,un)
         s_probe_divq = size(this%probe_divq)
         do i_probe_divq=1,s_probe_divq
           call display(this%probe_divq(i_probe_divq),un)
         enddo
         call display(this%md,un)
       end subroutine

       subroutine print_energy(this)
         implicit none
         type(energy),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_energy(this,un)
         implicit none
         type(energy),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_probe_divq
         integer :: s_probe_divq
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
         call export(this%m,un)
         call export(this%pcg_t,un)
         call export(this%t,un)
         call export(this%tnm1,un)
         call export(this%temp_cc1,un)
         call export(this%temp_cc2,un)
         call export(this%f,un)
         call export(this%fnm1,un)
         call export(this%l,un)
         call export(this%divq,un)
         call export(this%q_source,un)
         call export(this%temp_f,un)
         call export(this%k,un)
         call export(this%u_f,un)
         call export(this%u_cc,un)
         call export(this%gravity,un)
         call export(this%temp_cc1_vf,un)
         call export(this%temp_cc2_vf,un)
         call export(this%temp_cc_tf,un)
         call export(this%temp_f_tf,un)
         s_probe_divq = size(this%probe_divq)
         write(un,*) s_probe_divq
         do i_probe_divq=1,s_probe_divq
           call export(this%probe_divq(i_probe_divq),un)
         enddo
         call export(this%md,un)
       end subroutine

       subroutine import_energy(this,un)
         implicit none
         type(energy),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_probe_divq
         integer :: s_probe_divq
         call delete(this)
         read(un,*); read(un,*) this%suppress_warning
         call import(this%m,un)
         call import(this%pcg_t,un)
         call import(this%t,un)
         call import(this%tnm1,un)
         call import(this%temp_cc1,un)
         call import(this%temp_cc2,un)
         call import(this%f,un)
         call import(this%fnm1,un)
         call import(this%l,un)
         call import(this%divq,un)
         call import(this%q_source,un)
         call import(this%temp_f,un)
         call import(this%k,un)
         call import(this%u_f,un)
         call import(this%u_cc,un)
         call import(this%gravity,un)
         call import(this%temp_cc1_vf,un)
         call import(this%temp_cc2_vf,un)
         call import(this%temp_cc_tf,un)
         call import(this%temp_f_tf,un)
         read(un,*) s_probe_divq
         do i_probe_divq=1,s_probe_divq
           call import(this%probe_divq(i_probe_divq),un)
         enddo
         call import(this%md,un)
       end subroutine

       subroutine display_wrapper_energy(this,dir,name)
         implicit none
         type(energy),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_energy(this,dir,name)
         implicit none
         type(energy),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_energy(this,dir,name)
         implicit none
         type(energy),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module