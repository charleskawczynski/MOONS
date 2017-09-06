       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module flow_control_logicals_mod
       use IO_tools_mod
       implicit none

       private
       public :: flow_control_logicals
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_fl;    end interface
       interface delete;       module procedure delete_fl;       end interface
       interface display;      module procedure display_fl;      end interface
       interface display_short;module procedure display_short_fl;end interface
       interface display;      module procedure display_wrap_fl; end interface
       interface print;        module procedure print_fl;        end interface
       interface print_short;  module procedure print_short_fl;  end interface
       interface export;       module procedure export_fl;       end interface
       interface import;       module procedure import_fl;       end interface
       interface export;       module procedure export_wrap_fl;  end interface
       interface import;       module procedure import_wrap_fl;  end interface

       type flow_control_logicals
         logical :: post_process = .false.
         logical :: skip_solver_loop = .false.
         logical :: stop_before_solve = .false.
         logical :: stop_after_mesh_export = .false.
         logical :: Poisson_test = .false.
         logical :: Taylor_Green_Vortex_test = .false.
         logical :: temporal_convergence_test = .false.
         logical :: export_numerical_flow_rate = .false.
         logical :: export_Shercliff_Hunt_analytic_sol = .false.
         logical :: export_vorticity_streamfunction = .false.
         logical :: compute_export_E_K_Budget = .false.
         logical :: compute_export_E_M_budget = .false.
         logical :: operator_commute_test = .false.
         logical :: export_final_tec = .false.
         logical :: export_final_restart = .false.
         logical :: restart_meshes = .false.
         logical :: export_heavy = .false.
         logical :: print_every_MHD_step = .false.
         logical :: compute_surface_power = .false.
       end type

       contains

       subroutine init_copy_fl(this,that)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         type(flow_control_logicals),intent(in) :: that
         call delete(this)
         this%post_process = that%post_process
         this%skip_solver_loop = that%skip_solver_loop
         this%stop_before_solve = that%stop_before_solve
         this%stop_after_mesh_export = that%stop_after_mesh_export
         this%Poisson_test = that%Poisson_test
         this%Taylor_Green_Vortex_test = that%Taylor_Green_Vortex_test
         this%temporal_convergence_test = that%temporal_convergence_test
         this%export_numerical_flow_rate = that%export_numerical_flow_rate
         this%export_Shercliff_Hunt_analytic_sol = that%export_Shercliff_Hunt_analytic_sol
         this%export_vorticity_streamfunction = that%export_vorticity_streamfunction
         this%compute_export_E_K_Budget = that%compute_export_E_K_Budget
         this%compute_export_E_M_budget = that%compute_export_E_M_budget
         this%operator_commute_test = that%operator_commute_test
         this%export_final_tec = that%export_final_tec
         this%export_final_restart = that%export_final_restart
         this%restart_meshes = that%restart_meshes
         this%export_heavy = that%export_heavy
         this%print_every_MHD_step = that%print_every_MHD_step
         this%compute_surface_power = that%compute_surface_power
       end subroutine

       subroutine delete_fl(this)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         this%post_process = .false.
         this%skip_solver_loop = .false.
         this%stop_before_solve = .false.
         this%stop_after_mesh_export = .false.
         this%Poisson_test = .false.
         this%Taylor_Green_Vortex_test = .false.
         this%temporal_convergence_test = .false.
         this%export_numerical_flow_rate = .false.
         this%export_Shercliff_Hunt_analytic_sol = .false.
         this%export_vorticity_streamfunction = .false.
         this%compute_export_E_K_Budget = .false.
         this%compute_export_E_M_budget = .false.
         this%operator_commute_test = .false.
         this%export_final_tec = .false.
         this%export_final_restart = .false.
         this%restart_meshes = .false.
         this%export_heavy = .false.
         this%print_every_MHD_step = .false.
         this%compute_surface_power = .false.
       end subroutine

       subroutine display_fl(this,un)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         integer,intent(in) :: un
       end subroutine

       subroutine display_short_fl(this,un)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'post_process                       = ',this%post_process
         write(un,*) 'skip_solver_loop                   = ',this%skip_solver_loop
         write(un,*) 'stop_before_solve                  = ',this%stop_before_solve
         write(un,*) 'stop_after_mesh_export             = ',this%stop_after_mesh_export
         write(un,*) 'Poisson_test                       = ',this%Poisson_test
         write(un,*) 'Taylor_Green_Vortex_test           = ',this%Taylor_Green_Vortex_test
         write(un,*) 'temporal_convergence_test          = ',this%temporal_convergence_test
         write(un,*) 'export_numerical_flow_rate         = ',this%export_numerical_flow_rate
         write(un,*) 'export_Shercliff_Hunt_analytic_sol = ',this%export_Shercliff_Hunt_analytic_sol
         write(un,*) 'export_vorticity_streamfunction    = ',this%export_vorticity_streamfunction
         write(un,*) 'compute_export_E_K_Budget          = ',this%compute_export_E_K_Budget
         write(un,*) 'compute_export_E_M_budget          = ',this%compute_export_E_M_budget
         write(un,*) 'operator_commute_test              = ',this%operator_commute_test
         write(un,*) 'export_final_tec                   = ',this%export_final_tec
         write(un,*) 'export_final_restart               = ',this%export_final_restart
         write(un,*) 'restart_meshes                     = ',this%restart_meshes
         write(un,*) 'export_heavy                       = ',this%export_heavy
         write(un,*) 'print_every_MHD_step               = ',this%print_every_MHD_step
         write(un,*) 'compute_surface_power              = ',this%compute_surface_power
       end subroutine

       subroutine print_fl(this)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_fl(this)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_fl(this,un)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'post_process                        = ';write(un,*) this%post_process
         write(un,*) 'skip_solver_loop                    = ';write(un,*) this%skip_solver_loop
         write(un,*) 'stop_before_solve                   = ';write(un,*) this%stop_before_solve
         write(un,*) 'stop_after_mesh_export              = ';write(un,*) this%stop_after_mesh_export
         write(un,*) 'Poisson_test                        = ';write(un,*) this%Poisson_test
         write(un,*) 'Taylor_Green_Vortex_test            = ';write(un,*) this%Taylor_Green_Vortex_test
         write(un,*) 'temporal_convergence_test           = ';write(un,*) this%temporal_convergence_test
         write(un,*) 'export_numerical_flow_rate          = ';write(un,*) this%export_numerical_flow_rate
         write(un,*) 'export_Shercliff_Hunt_analytic_sol  = ';write(un,*) this%export_Shercliff_Hunt_analytic_sol
         write(un,*) 'export_vorticity_streamfunction     = ';write(un,*) this%export_vorticity_streamfunction
         write(un,*) 'compute_export_E_K_Budget           = ';write(un,*) this%compute_export_E_K_Budget
         write(un,*) 'compute_export_E_M_budget           = ';write(un,*) this%compute_export_E_M_budget
         write(un,*) 'operator_commute_test               = ';write(un,*) this%operator_commute_test
         write(un,*) 'export_final_tec                    = ';write(un,*) this%export_final_tec
         write(un,*) 'export_final_restart                = ';write(un,*) this%export_final_restart
         write(un,*) 'restart_meshes                      = ';write(un,*) this%restart_meshes
         write(un,*) 'export_heavy                        = ';write(un,*) this%export_heavy
         write(un,*) 'print_every_MHD_step                = ';write(un,*) this%print_every_MHD_step
         write(un,*) 'compute_surface_power               = ';write(un,*) this%compute_surface_power
       end subroutine

       subroutine import_fl(this,un)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%post_process
         read(un,*); read(un,*) this%skip_solver_loop
         read(un,*); read(un,*) this%stop_before_solve
         read(un,*); read(un,*) this%stop_after_mesh_export
         read(un,*); read(un,*) this%Poisson_test
         read(un,*); read(un,*) this%Taylor_Green_Vortex_test
         read(un,*); read(un,*) this%temporal_convergence_test
         read(un,*); read(un,*) this%export_numerical_flow_rate
         read(un,*); read(un,*) this%export_Shercliff_Hunt_analytic_sol
         read(un,*); read(un,*) this%export_vorticity_streamfunction
         read(un,*); read(un,*) this%compute_export_E_K_Budget
         read(un,*); read(un,*) this%compute_export_E_M_budget
         read(un,*); read(un,*) this%operator_commute_test
         read(un,*); read(un,*) this%export_final_tec
         read(un,*); read(un,*) this%export_final_restart
         read(un,*); read(un,*) this%restart_meshes
         read(un,*); read(un,*) this%export_heavy
         read(un,*); read(un,*) this%print_every_MHD_step
         read(un,*); read(un,*) this%compute_surface_power
       end subroutine

       subroutine display_wrap_fl(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_fl(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_fl(this,dir,name)
         implicit none
         type(flow_control_logicals),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module