       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sim_params_mod
       use IO_tools_mod
       use dimensionless_params_mod
       use energy_terms_mod
       use export_frequency_mod
       use export_logicals_mod
       use flow_control_logicals_mod
       use geometry_props_mod
       use induction_terms_mod
       use mesh_params_mod
       use mesh_quality_params_mod
       use mirror_props_mod
       use momentum_terms_mod
       use sim_config_params_mod
       use time_marching_params_mod
       use time_statistics_params_mod
       use var_set_mod
       implicit none

       private
       public :: sim_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_sim_params;      end interface
       interface delete;       module procedure delete_sim_params;         end interface
       interface display;      module procedure display_sim_params;        end interface
       interface display_short;module procedure display_short_sim_params;  end interface
       interface display;      module procedure display_wrapper_sim_params;end interface
       interface print;        module procedure print_sim_params;          end interface
       interface print_short;  module procedure print_short_sim_params;    end interface
       interface export;       module procedure export_sim_params;         end interface
       interface import;       module procedure import_sim_params;         end interface
       interface export;       module procedure export_wrapper_sim_params; end interface
       interface import;       module procedure import_wrapper_sim_params; end interface

       type sim_params
         type(var_set) :: VS
         type(mesh_params) :: MP_mom
         type(mesh_quality_params) :: MQP
         type(mesh_params) :: MP_ind
         type(mesh_params) :: MP_sigma
         type(dimensionless_params) :: DP
         type(export_logicals) :: EL
         type(export_frequency) :: EF
         type(energy_terms) :: ET
         type(momentum_terms) :: MT
         type(induction_terms) :: IT
         type(geometry_props) :: GP
         type(mirror_props) :: MP
         type(time_marching_params) :: coupled
         type(flow_control_logicals) :: FCL
         type(time_statistics_params) :: TSP
         type(sim_config_params) :: SCP
         logical :: matrix_based = .false.
       end type

       contains

       subroutine init_copy_sim_params(this,that)
         implicit none
         type(sim_params),intent(inout) :: this
         type(sim_params),intent(in) :: that
         call delete(this)
         call init(this%VS,that%VS)
         call init(this%MP_mom,that%MP_mom)
         call init(this%MQP,that%MQP)
         call init(this%MP_ind,that%MP_ind)
         call init(this%MP_sigma,that%MP_sigma)
         call init(this%DP,that%DP)
         call init(this%EL,that%EL)
         call init(this%EF,that%EF)
         call init(this%ET,that%ET)
         call init(this%MT,that%MT)
         call init(this%IT,that%IT)
         call init(this%GP,that%GP)
         call init(this%MP,that%MP)
         call init(this%coupled,that%coupled)
         call init(this%FCL,that%FCL)
         call init(this%TSP,that%TSP)
         call init(this%SCP,that%SCP)
         this%matrix_based = that%matrix_based
       end subroutine

       subroutine delete_sim_params(this)
         implicit none
         type(sim_params),intent(inout) :: this
         call delete(this%VS)
         call delete(this%MP_mom)
         call delete(this%MQP)
         call delete(this%MP_ind)
         call delete(this%MP_sigma)
         call delete(this%DP)
         call delete(this%EL)
         call delete(this%EF)
         call delete(this%ET)
         call delete(this%MT)
         call delete(this%IT)
         call delete(this%GP)
         call delete(this%MP)
         call delete(this%coupled)
         call delete(this%FCL)
         call delete(this%TSP)
         call delete(this%SCP)
         this%matrix_based = .false.
       end subroutine

       subroutine display_sim_params(this,un)
         implicit none
         type(sim_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- sim_params'
         call display(this%VS,un)
         call display(this%MP_mom,un)
         call display(this%MQP,un)
         call display(this%MP_ind,un)
         call display(this%MP_sigma,un)
         call display(this%DP,un)
         call display(this%EL,un)
         call display(this%EF,un)
         call display(this%ET,un)
         call display(this%MT,un)
         call display(this%IT,un)
         call display(this%GP,un)
         call display(this%MP,un)
         call display(this%coupled,un)
         call display(this%FCL,un)
         call display(this%TSP,un)
         call display(this%SCP,un)
         write(un,*) 'matrix_based = ',this%matrix_based
       end subroutine

       subroutine display_short_sim_params(this,un)
         implicit none
         type(sim_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%VS,un)
         call display(this%MP_mom,un)
         call display(this%MQP,un)
         call display(this%MP_ind,un)
         call display(this%MP_sigma,un)
         call display(this%DP,un)
         call display(this%EL,un)
         call display(this%EF,un)
         call display(this%ET,un)
         call display(this%MT,un)
         call display(this%IT,un)
         call display(this%GP,un)
         call display(this%MP,un)
         call display(this%coupled,un)
         call display(this%FCL,un)
         call display(this%TSP,un)
         call display(this%SCP,un)
         write(un,*) 'matrix_based = ',this%matrix_based
       end subroutine

       subroutine print_sim_params(this)
         implicit none
         type(sim_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_sim_params(this)
         implicit none
         type(sim_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_sim_params(this,un)
         implicit none
         type(sim_params),intent(in) :: this
         integer,intent(in) :: un
         call export(this%VS,un)
         call export(this%MP_mom,un)
         call export(this%MQP,un)
         call export(this%MP_ind,un)
         call export(this%MP_sigma,un)
         call export(this%DP,un)
         call export(this%EL,un)
         call export(this%EF,un)
         call export(this%ET,un)
         call export(this%MT,un)
         call export(this%IT,un)
         call export(this%GP,un)
         call export(this%MP,un)
         call export(this%coupled,un)
         call export(this%FCL,un)
         call export(this%TSP,un)
         call export(this%SCP,un)
         write(un,*) 'matrix_based  = ';write(un,*) this%matrix_based
       end subroutine

       subroutine import_sim_params(this,un)
         implicit none
         type(sim_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%VS,un)
         call import(this%MP_mom,un)
         call import(this%MQP,un)
         call import(this%MP_ind,un)
         call import(this%MP_sigma,un)
         call import(this%DP,un)
         call import(this%EL,un)
         call import(this%EF,un)
         call import(this%ET,un)
         call import(this%MT,un)
         call import(this%IT,un)
         call import(this%GP,un)
         call import(this%MP,un)
         call import(this%coupled,un)
         call import(this%FCL,un)
         call import(this%TSP,un)
         call import(this%SCP,un)
         read(un,*); read(un,*) this%matrix_based
       end subroutine

       subroutine display_wrapper_sim_params(this,dir,name)
         implicit none
         type(sim_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_sim_params(this,dir,name)
         implicit none
         type(sim_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_sim_params(this,dir,name)
         implicit none
         type(sim_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module