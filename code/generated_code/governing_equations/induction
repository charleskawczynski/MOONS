       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module induction_mod
       use IO_tools_mod
       use PCG_solver_SF_mod
       use PCG_solver_VF_mod
       use SF_mod
       use TF_mod
       use VF_mod
       use mesh_mod
       use mesh_domain_mod
       use probe_mod
       implicit none

       private
       public :: induction
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_induction;           end interface
       interface delete; module procedure delete_induction;         end interface
       interface display;module procedure display_induction;        end interface
       interface display;module procedure display_wrapper_induction;end interface
       interface print;  module procedure print_induction;          end interface
       interface export; module procedure export_induction;         end interface
       interface import; module procedure import_induction;         end interface
       interface export; module procedure export_wrapper_induction; end interface
       interface import; module procedure import_wrapper_induction; end interface

       type induction
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(pcg_solver_vf) :: pcg_b
         type(pcg_solver_sf) :: pcg_cleanb
         type(tf) :: u_e
         type(tf) :: temp_e_tf
         type(tf) :: temp_f1_tf
         type(tf) :: temp_f2_tf
         type(sf) :: sigmainv_cc
         type(sf) :: divb
         type(sf) :: divj
         type(sf) :: phi
         type(sf) :: temp_cc
         type(vf) :: f
         type(vf) :: fnm1
         type(vf) :: l
         type(vf) :: j
         type(vf) :: temp_e
         type(vf) :: b
         type(vf) :: bnm1
         type(vf) :: b0
         type(vf) :: b_interior
         type(vf) :: temp_f1
         type(vf) :: temp_f2
         type(vf) :: bstar
         type(vf) :: db0dt
         type(vf) :: temp_cc_vf
         type(vf) :: sigmainv_edge
         type(vf) :: j_interior
         type(vf) :: curlucrossb
         type(vf) :: cc_vf_fluid
         type(vf) :: cc_vf_sigma
         type(probe) :: probe_divb
         type(probe) :: probe_divj
         type(probe) :: je
         type(probe) :: je_fluid
         type(probe),dimension(3) :: me
         type(probe),dimension(3) :: me_fluid
         type(probe),dimension(3) :: me_conductor
         type(probe),dimension(3) :: probe_db0dt
         type(probe),dimension(3) :: probe_b0
         type(mesh_domain) :: md_fluid
         type(mesh_domain) :: md_sigma
       end type

       contains

       subroutine init_induction(this,that)
         implicit none
         type(induction),intent(inout) :: this
         type(induction),intent(in) :: that
         integer :: i_me
         integer :: i_me_fluid
         integer :: i_me_conductor
         integer :: i_probe_db0dt
         integer :: i_probe_b0
         integer :: s_me
         integer :: s_me_fluid
         integer :: s_me_conductor
         integer :: s_probe_db0dt
         integer :: s_probe_b0
         call delete(this)
         this%suppress_warning = that%suppress_warning
         call init(this%m,that%m)
         call init(this%pcg_b,that%pcg_b)
         call init(this%pcg_cleanb,that%pcg_cleanb)
         call init(this%u_e,that%u_e)
         call init(this%temp_e_tf,that%temp_e_tf)
         call init(this%temp_f1_tf,that%temp_f1_tf)
         call init(this%temp_f2_tf,that%temp_f2_tf)
         call init(this%sigmainv_cc,that%sigmainv_cc)
         call init(this%divb,that%divb)
         call init(this%divj,that%divj)
         call init(this%phi,that%phi)
         call init(this%temp_cc,that%temp_cc)
         call init(this%f,that%f)
         call init(this%fnm1,that%fnm1)
         call init(this%l,that%l)
         call init(this%j,that%j)
         call init(this%temp_e,that%temp_e)
         call init(this%b,that%b)
         call init(this%bnm1,that%bnm1)
         call init(this%b0,that%b0)
         call init(this%b_interior,that%b_interior)
         call init(this%temp_f1,that%temp_f1)
         call init(this%temp_f2,that%temp_f2)
         call init(this%bstar,that%bstar)
         call init(this%db0dt,that%db0dt)
         call init(this%temp_cc_vf,that%temp_cc_vf)
         call init(this%sigmainv_edge,that%sigmainv_edge)
         call init(this%j_interior,that%j_interior)
         call init(this%curlucrossb,that%curlucrossb)
         call init(this%cc_vf_fluid,that%cc_vf_fluid)
         call init(this%cc_vf_sigma,that%cc_vf_sigma)
         call init(this%probe_divb,that%probe_divb)
         call init(this%probe_divj,that%probe_divj)
         call init(this%je,that%je)
         call init(this%je_fluid,that%je_fluid)
         s_me = size(that%me)
         do i_me=1,s_me
           call init(this%me(i_me),that%me(i_me))
         enddo
         s_me_fluid = size(that%me_fluid)
         do i_me_fluid=1,s_me_fluid
           call init(this%me_fluid(i_me_fluid),that%me_fluid(i_me_fluid))
         enddo
         s_me_conductor = size(that%me_conductor)
         do i_me_conductor=1,s_me_conductor
           call init(this%me_conductor(i_me_conductor),that%me_conductor(i_me_conductor))
         enddo
         s_probe_db0dt = size(that%probe_db0dt)
         do i_probe_db0dt=1,s_probe_db0dt
           call init(this%probe_db0dt(i_probe_db0dt),that%probe_db0dt(i_probe_db0dt))
         enddo
         s_probe_b0 = size(that%probe_b0)
         do i_probe_b0=1,s_probe_b0
           call init(this%probe_b0(i_probe_b0),that%probe_b0(i_probe_b0))
         enddo
         call init(this%md_fluid,that%md_fluid)
         call init(this%md_sigma,that%md_sigma)
       end subroutine

       subroutine delete_induction(this)
         implicit none
         type(induction),intent(inout) :: this
         integer :: i_me
         integer :: i_me_fluid
         integer :: i_me_conductor
         integer :: i_probe_db0dt
         integer :: i_probe_b0
         integer :: s_me
         integer :: s_me_fluid
         integer :: s_me_conductor
         integer :: s_probe_db0dt
         integer :: s_probe_b0
         this%suppress_warning = .false.
         call delete(this%m)
         call delete(this%pcg_b)
         call delete(this%pcg_cleanb)
         call delete(this%u_e)
         call delete(this%temp_e_tf)
         call delete(this%temp_f1_tf)
         call delete(this%temp_f2_tf)
         call delete(this%sigmainv_cc)
         call delete(this%divb)
         call delete(this%divj)
         call delete(this%phi)
         call delete(this%temp_cc)
         call delete(this%f)
         call delete(this%fnm1)
         call delete(this%l)
         call delete(this%j)
         call delete(this%temp_e)
         call delete(this%b)
         call delete(this%bnm1)
         call delete(this%b0)
         call delete(this%b_interior)
         call delete(this%temp_f1)
         call delete(this%temp_f2)
         call delete(this%bstar)
         call delete(this%db0dt)
         call delete(this%temp_cc_vf)
         call delete(this%sigmainv_edge)
         call delete(this%j_interior)
         call delete(this%curlucrossb)
         call delete(this%cc_vf_fluid)
         call delete(this%cc_vf_sigma)
         call delete(this%probe_divb)
         call delete(this%probe_divj)
         call delete(this%je)
         call delete(this%je_fluid)
         s_me = size(this%me)
         do i_me=1,s_me
           call delete(this%me(i_me))
         enddo
         s_me_fluid = size(this%me_fluid)
         do i_me_fluid=1,s_me_fluid
           call delete(this%me_fluid(i_me_fluid))
         enddo
         s_me_conductor = size(this%me_conductor)
         do i_me_conductor=1,s_me_conductor
           call delete(this%me_conductor(i_me_conductor))
         enddo
         s_probe_db0dt = size(this%probe_db0dt)
         do i_probe_db0dt=1,s_probe_db0dt
           call delete(this%probe_db0dt(i_probe_db0dt))
         enddo
         s_probe_b0 = size(this%probe_b0)
         do i_probe_b0=1,s_probe_b0
           call delete(this%probe_b0(i_probe_b0))
         enddo
         call delete(this%md_fluid)
         call delete(this%md_sigma)
       end subroutine

       subroutine display_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- induction'
         integer :: i_me
         integer :: i_me_fluid
         integer :: i_me_conductor
         integer :: i_probe_db0dt
         integer :: i_probe_b0
         integer :: s_me
         integer :: s_me_fluid
         integer :: s_me_conductor
         integer :: s_probe_db0dt
         integer :: s_probe_b0
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%pcg_b,un)
         call display(this%pcg_cleanb,un)
         call display(this%u_e,un)
         call display(this%temp_e_tf,un)
         call display(this%temp_f1_tf,un)
         call display(this%temp_f2_tf,un)
         call display(this%sigmainv_cc,un)
         call display(this%divb,un)
         call display(this%divj,un)
         call display(this%phi,un)
         call display(this%temp_cc,un)
         call display(this%f,un)
         call display(this%fnm1,un)
         call display(this%l,un)
         call display(this%j,un)
         call display(this%temp_e,un)
         call display(this%b,un)
         call display(this%bnm1,un)
         call display(this%b0,un)
         call display(this%b_interior,un)
         call display(this%temp_f1,un)
         call display(this%temp_f2,un)
         call display(this%bstar,un)
         call display(this%db0dt,un)
         call display(this%temp_cc_vf,un)
         call display(this%sigmainv_edge,un)
         call display(this%j_interior,un)
         call display(this%curlucrossb,un)
         call display(this%cc_vf_fluid,un)
         call display(this%cc_vf_sigma,un)
         call display(this%probe_divb,un)
         call display(this%probe_divj,un)
         call display(this%je,un)
         call display(this%je_fluid,un)
         s_me = size(this%me)
         do i_me=1,s_me
           call display(this%me(i_me),un)
         enddo
         s_me_fluid = size(this%me_fluid)
         do i_me_fluid=1,s_me_fluid
           call display(this%me_fluid(i_me_fluid),un)
         enddo
         s_me_conductor = size(this%me_conductor)
         do i_me_conductor=1,s_me_conductor
           call display(this%me_conductor(i_me_conductor),un)
         enddo
         s_probe_db0dt = size(this%probe_db0dt)
         do i_probe_db0dt=1,s_probe_db0dt
           call display(this%probe_db0dt(i_probe_db0dt),un)
         enddo
         s_probe_b0 = size(this%probe_b0)
         do i_probe_b0=1,s_probe_b0
           call display(this%probe_b0(i_probe_b0),un)
         enddo
         call display(this%md_fluid,un)
         call display(this%md_sigma,un)
       end subroutine

       subroutine print_induction(this)
         implicit none
         type(induction),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_induction(this,un)
         implicit none
         type(induction),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_me
         integer :: i_me_fluid
         integer :: i_me_conductor
         integer :: i_probe_db0dt
         integer :: i_probe_b0
         integer :: s_me
         integer :: s_me_fluid
         integer :: s_me_conductor
         integer :: s_probe_db0dt
         integer :: s_probe_b0
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
         call export(this%m,un)
         call export(this%pcg_b,un)
         call export(this%pcg_cleanb,un)
         call export(this%u_e,un)
         call export(this%temp_e_tf,un)
         call export(this%temp_f1_tf,un)
         call export(this%temp_f2_tf,un)
         call export(this%sigmainv_cc,un)
         call export(this%divb,un)
         call export(this%divj,un)
         call export(this%phi,un)
         call export(this%temp_cc,un)
         call export(this%f,un)
         call export(this%fnm1,un)
         call export(this%l,un)
         call export(this%j,un)
         call export(this%temp_e,un)
         call export(this%b,un)
         call export(this%bnm1,un)
         call export(this%b0,un)
         call export(this%b_interior,un)
         call export(this%temp_f1,un)
         call export(this%temp_f2,un)
         call export(this%bstar,un)
         call export(this%db0dt,un)
         call export(this%temp_cc_vf,un)
         call export(this%sigmainv_edge,un)
         call export(this%j_interior,un)
         call export(this%curlucrossb,un)
         call export(this%cc_vf_fluid,un)
         call export(this%cc_vf_sigma,un)
         call export(this%probe_divb,un)
         call export(this%probe_divj,un)
         call export(this%je,un)
         call export(this%je_fluid,un)
         s_me = size(this%me)
         write(un,*) s_me
         do i_me=1,s_me
           call export(this%me(i_me),un)
         enddo
         s_me_fluid = size(this%me_fluid)
         write(un,*) s_me_fluid
         do i_me_fluid=1,s_me_fluid
           call export(this%me_fluid(i_me_fluid),un)
         enddo
         s_me_conductor = size(this%me_conductor)
         write(un,*) s_me_conductor
         do i_me_conductor=1,s_me_conductor
           call export(this%me_conductor(i_me_conductor),un)
         enddo
         s_probe_db0dt = size(this%probe_db0dt)
         write(un,*) s_probe_db0dt
         do i_probe_db0dt=1,s_probe_db0dt
           call export(this%probe_db0dt(i_probe_db0dt),un)
         enddo
         s_probe_b0 = size(this%probe_b0)
         write(un,*) s_probe_b0
         do i_probe_b0=1,s_probe_b0
           call export(this%probe_b0(i_probe_b0),un)
         enddo
         call export(this%md_fluid,un)
         call export(this%md_sigma,un)
       end subroutine

       subroutine import_induction(this,un)
         implicit none
         type(induction),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_me
         integer :: i_me_fluid
         integer :: i_me_conductor
         integer :: i_probe_db0dt
         integer :: i_probe_b0
         integer :: s_me
         integer :: s_me_fluid
         integer :: s_me_conductor
         integer :: s_probe_db0dt
         integer :: s_probe_b0
         call delete(this)
         read(un,*); read(un,*) this%suppress_warning
         call import(this%m,un)
         call import(this%pcg_b,un)
         call import(this%pcg_cleanb,un)
         call import(this%u_e,un)
         call import(this%temp_e_tf,un)
         call import(this%temp_f1_tf,un)
         call import(this%temp_f2_tf,un)
         call import(this%sigmainv_cc,un)
         call import(this%divb,un)
         call import(this%divj,un)
         call import(this%phi,un)
         call import(this%temp_cc,un)
         call import(this%f,un)
         call import(this%fnm1,un)
         call import(this%l,un)
         call import(this%j,un)
         call import(this%temp_e,un)
         call import(this%b,un)
         call import(this%bnm1,un)
         call import(this%b0,un)
         call import(this%b_interior,un)
         call import(this%temp_f1,un)
         call import(this%temp_f2,un)
         call import(this%bstar,un)
         call import(this%db0dt,un)
         call import(this%temp_cc_vf,un)
         call import(this%sigmainv_edge,un)
         call import(this%j_interior,un)
         call import(this%curlucrossb,un)
         call import(this%cc_vf_fluid,un)
         call import(this%cc_vf_sigma,un)
         call import(this%probe_divb,un)
         call import(this%probe_divj,un)
         call import(this%je,un)
         call import(this%je_fluid,un)
         read(un,*) s_me
         do i_me=1,s_me
           call import(this%me(i_me),un)
         enddo
         read(un,*) s_me_fluid
         do i_me_fluid=1,s_me_fluid
           call import(this%me_fluid(i_me_fluid),un)
         enddo
         read(un,*) s_me_conductor
         do i_me_conductor=1,s_me_conductor
           call import(this%me_conductor(i_me_conductor),un)
         enddo
         read(un,*) s_probe_db0dt
         do i_probe_db0dt=1,s_probe_db0dt
           call import(this%probe_db0dt(i_probe_db0dt),un)
         enddo
         read(un,*) s_probe_b0
         do i_probe_b0=1,s_probe_b0
           call import(this%probe_b0(i_probe_b0),un)
         enddo
         call import(this%md_fluid,un)
         call import(this%md_sigma,un)
       end subroutine

       subroutine display_wrapper_induction(this,dir,name)
         implicit none
         type(induction),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_induction(this,dir,name)
         implicit none
         type(induction),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_induction(this,dir,name)
         implicit none
         type(induction),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module