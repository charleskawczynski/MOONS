       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module dir_tree_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_group_mod
       use dir_manip_mod
       use path_mod
       use string_mod
       implicit none

       private
       public :: dir_tree
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_dir_tree;          end interface
       interface delete;           module procedure delete_dir_tree;             end interface
       interface display;          module procedure display_dir_tree;            end interface
       interface display_short;    module procedure display_short_dir_tree;      end interface
       interface display;          module procedure display_wrap_dir_tree;       end interface
       interface print;            module procedure print_dir_tree;              end interface
       interface print_short;      module procedure print_short_dir_tree;        end interface
       interface export;           module procedure export_dir_tree;             end interface
       interface export_primitives;module procedure export_primitives_dir_tree;  end interface
       interface import;           module procedure import_dir_tree;             end interface
       interface export_structured;module procedure export_structured_D_dir_tree;end interface
       interface import_structured;module procedure import_structured_D_dir_tree;end interface
       interface import_primitives;module procedure import_primitives_dir_tree;  end interface
       interface export;           module procedure export_wrap_dir_tree;        end interface
       interface import;           module procedure import_wrap_dir_tree;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_dir_tree;         end interface
       interface suppress_warnings;module procedure suppress_warnings_dir_tree;  end interface

       type dir_tree
         type(path) :: tar_p
         type(path) :: out_dir
         type(path) :: LDC
         type(path) :: mat
         type(path) :: config
         type(path) :: meshes
         type(path) :: BEM
         type(path) :: wall_clock
         type(path) :: matrix_visualization
         type(path) :: dimensionless_params
         type(path) :: params
         type(path) :: ISP
         type(path) :: TMP
         type(path) :: EF
         type(path) :: export_now
         type(path) :: refine_mesh
         type(path) :: e_budget
         type(path) :: e_budget_N
         type(path) :: e_budget_C
         type(path) :: restart_sim
         type(path) :: restart1
         type(path) :: restart2
         type(path) :: restart
         type(path) :: mesh_restart
         type(path) :: unknowns
         type(path) :: governing_equations
         type(string) :: PS
         type(string) :: tar
         type(dir_group) :: U
         type(dir_group) :: B
         type(dir_group) :: J
         type(dir_group) :: T
         type(dir_group) :: p
         type(dir_group) :: phi
         type(dir_group) :: rho
         type(dir_group) :: test
       end type

       contains

       subroutine init_copy_dir_tree(this,that)
         implicit none
         type(dir_tree),intent(inout) :: this
         type(dir_tree),intent(in) :: that
         call delete(this)
         call init(this%tar_p,that%tar_p)
         call init(this%out_dir,that%out_dir)
         call init(this%LDC,that%LDC)
         call init(this%mat,that%mat)
         call init(this%config,that%config)
         call init(this%meshes,that%meshes)
         call init(this%BEM,that%BEM)
         call init(this%wall_clock,that%wall_clock)
         call init(this%matrix_visualization,that%matrix_visualization)
         call init(this%dimensionless_params,that%dimensionless_params)
         call init(this%params,that%params)
         call init(this%ISP,that%ISP)
         call init(this%TMP,that%TMP)
         call init(this%EF,that%EF)
         call init(this%export_now,that%export_now)
         call init(this%refine_mesh,that%refine_mesh)
         call init(this%e_budget,that%e_budget)
         call init(this%e_budget_N,that%e_budget_N)
         call init(this%e_budget_C,that%e_budget_C)
         call init(this%restart_sim,that%restart_sim)
         call init(this%restart1,that%restart1)
         call init(this%restart2,that%restart2)
         call init(this%restart,that%restart)
         call init(this%mesh_restart,that%mesh_restart)
         call init(this%unknowns,that%unknowns)
         call init(this%governing_equations,that%governing_equations)
         call init(this%PS,that%PS)
         call init(this%tar,that%tar)
         call init(this%U,that%U)
         call init(this%B,that%B)
         call init(this%J,that%J)
         call init(this%T,that%T)
         call init(this%p,that%p)
         call init(this%phi,that%phi)
         call init(this%rho,that%rho)
         call init(this%test,that%test)
       end subroutine

       subroutine delete_dir_tree(this)
         implicit none
         type(dir_tree),intent(inout) :: this
         call delete(this%tar_p)
         call delete(this%out_dir)
         call delete(this%LDC)
         call delete(this%mat)
         call delete(this%config)
         call delete(this%meshes)
         call delete(this%BEM)
         call delete(this%wall_clock)
         call delete(this%matrix_visualization)
         call delete(this%dimensionless_params)
         call delete(this%params)
         call delete(this%ISP)
         call delete(this%TMP)
         call delete(this%EF)
         call delete(this%export_now)
         call delete(this%refine_mesh)
         call delete(this%e_budget)
         call delete(this%e_budget_N)
         call delete(this%e_budget_C)
         call delete(this%restart_sim)
         call delete(this%restart1)
         call delete(this%restart2)
         call delete(this%restart)
         call delete(this%mesh_restart)
         call delete(this%unknowns)
         call delete(this%governing_equations)
         call delete(this%PS)
         call delete(this%tar)
         call delete(this%U)
         call delete(this%B)
         call delete(this%J)
         call delete(this%T)
         call delete(this%p)
         call delete(this%phi)
         call delete(this%rho)
         call delete(this%test)
       end subroutine

       subroutine display_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(in) :: this
         integer,intent(in) :: un
         call display(this%tar_p,un)
         call display(this%out_dir,un)
         call display(this%LDC,un)
         call display(this%mat,un)
         call display(this%config,un)
         call display(this%meshes,un)
         call display(this%BEM,un)
         call display(this%wall_clock,un)
         call display(this%matrix_visualization,un)
         call display(this%dimensionless_params,un)
         call display(this%params,un)
         call display(this%ISP,un)
         call display(this%TMP,un)
         call display(this%EF,un)
         call display(this%export_now,un)
         call display(this%refine_mesh,un)
         call display(this%e_budget,un)
         call display(this%e_budget_N,un)
         call display(this%e_budget_C,un)
         call display(this%restart_sim,un)
         call display(this%restart1,un)
         call display(this%restart2,un)
         call display(this%restart,un)
         call display(this%mesh_restart,un)
         call display(this%unknowns,un)
         call display(this%governing_equations,un)
         call display(this%PS,un)
         call display(this%tar,un)
         call display(this%U,un)
         call display(this%B,un)
         call display(this%J,un)
         call display(this%T,un)
         call display(this%p,un)
         call display(this%phi,un)
         call display(this%rho,un)
         call display(this%test,un)
       end subroutine

       subroutine display_short_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(in) :: this
         integer,intent(in) :: un
         call display(this%tar_p,un)
         call display(this%out_dir,un)
         call display(this%LDC,un)
         call display(this%mat,un)
         call display(this%config,un)
         call display(this%meshes,un)
         call display(this%BEM,un)
         call display(this%wall_clock,un)
         call display(this%matrix_visualization,un)
         call display(this%dimensionless_params,un)
         call display(this%params,un)
         call display(this%ISP,un)
         call display(this%TMP,un)
         call display(this%EF,un)
         call display(this%export_now,un)
         call display(this%refine_mesh,un)
         call display(this%e_budget,un)
         call display(this%e_budget_N,un)
         call display(this%e_budget_C,un)
         call display(this%restart_sim,un)
         call display(this%restart1,un)
         call display(this%restart2,un)
         call display(this%restart,un)
         call display(this%mesh_restart,un)
         call display(this%unknowns,un)
         call display(this%governing_equations,un)
         call display(this%PS,un)
         call display(this%tar,un)
         call display(this%U,un)
         call display(this%B,un)
         call display(this%J,un)
         call display(this%T,un)
         call display(this%p,un)
         call display(this%phi,un)
         call display(this%rho,un)
         call display(this%test,un)
       end subroutine

       subroutine display_wrap_dir_tree(this,dir,name)
         implicit none
         type(dir_tree),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_dir_tree(this)
         implicit none
         type(dir_tree),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_dir_tree(this)
         implicit none
         type(dir_tree),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(in) :: this
         integer,intent(in) :: un
         call export(this%tar_p,un)
         call export(this%out_dir,un)
         call export(this%LDC,un)
         call export(this%mat,un)
         call export(this%config,un)
         call export(this%meshes,un)
         call export(this%BEM,un)
         call export(this%wall_clock,un)
         call export(this%matrix_visualization,un)
         call export(this%dimensionless_params,un)
         call export(this%params,un)
         call export(this%ISP,un)
         call export(this%TMP,un)
         call export(this%EF,un)
         call export(this%export_now,un)
         call export(this%refine_mesh,un)
         call export(this%e_budget,un)
         call export(this%e_budget_N,un)
         call export(this%e_budget_C,un)
         call export(this%restart_sim,un)
         call export(this%restart1,un)
         call export(this%restart2,un)
         call export(this%restart,un)
         call export(this%mesh_restart,un)
         call export(this%unknowns,un)
         call export(this%governing_equations,un)
         call export(this%PS,un)
         call export(this%tar,un)
         call export(this%U,un)
         call export(this%B,un)
         call export(this%J,un)
         call export(this%T,un)
         call export(this%p,un)
         call export(this%phi,un)
         call export(this%rho,un)
         call export(this%test,un)
       end subroutine

       subroutine import_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%tar_p,un)
         call import(this%out_dir,un)
         call import(this%LDC,un)
         call import(this%mat,un)
         call import(this%config,un)
         call import(this%meshes,un)
         call import(this%BEM,un)
         call import(this%wall_clock,un)
         call import(this%matrix_visualization,un)
         call import(this%dimensionless_params,un)
         call import(this%params,un)
         call import(this%ISP,un)
         call import(this%TMP,un)
         call import(this%EF,un)
         call import(this%export_now,un)
         call import(this%refine_mesh,un)
         call import(this%e_budget,un)
         call import(this%e_budget_N,un)
         call import(this%e_budget_C,un)
         call import(this%restart_sim,un)
         call import(this%restart1,un)
         call import(this%restart2,un)
         call import(this%restart,un)
         call import(this%mesh_restart,un)
         call import(this%unknowns,un)
         call import(this%governing_equations,un)
         call import(this%PS,un)
         call import(this%tar,un)
         call import(this%U,un)
         call import(this%B,un)
         call import(this%J,un)
         call import(this%T,un)
         call import(this%p,un)
         call import(this%phi,un)
         call import(this%rho,un)
         call import(this%test,un)
       end subroutine

       subroutine export_primitives_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_dir_tree(this,dir,name)
         implicit none
         type(dir_tree),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_dir_tree(this,dir,name)
         implicit none
         type(dir_tree),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_dir_tree(this,dir)
         implicit none
         type(dir_tree),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call set_IO_dir(this%tar_p,dir//'tar_p'//fortran_PS)
         call set_IO_dir(this%out_dir,dir//'out_dir'//fortran_PS)
         call set_IO_dir(this%LDC,dir//'LDC'//fortran_PS)
         call set_IO_dir(this%mat,dir//'mat'//fortran_PS)
         call set_IO_dir(this%config,dir//'config'//fortran_PS)
         call set_IO_dir(this%meshes,dir//'meshes'//fortran_PS)
         call set_IO_dir(this%BEM,dir//'BEM'//fortran_PS)
         call set_IO_dir(this%wall_clock,dir//'wall_clock'//fortran_PS)
         call set_IO_dir(this%matrix_visualization,&
         dir//'matrix_visualization'//fortran_PS)
         call set_IO_dir(this%dimensionless_params,&
         dir//'dimensionless_params'//fortran_PS)
         call set_IO_dir(this%params,dir//'params'//fortran_PS)
         call set_IO_dir(this%ISP,dir//'ISP'//fortran_PS)
         call set_IO_dir(this%TMP,dir//'TMP'//fortran_PS)
         call set_IO_dir(this%EF,dir//'EF'//fortran_PS)
         call set_IO_dir(this%export_now,dir//'export_now'//fortran_PS)
         call set_IO_dir(this%refine_mesh,dir//'refine_mesh'//fortran_PS)
         call set_IO_dir(this%e_budget,dir//'e_budget'//fortran_PS)
         call set_IO_dir(this%e_budget_N,dir//'e_budget_N'//fortran_PS)
         call set_IO_dir(this%e_budget_C,dir//'e_budget_C'//fortran_PS)
         call set_IO_dir(this%restart_sim,dir//'restart_sim'//fortran_PS)
         call set_IO_dir(this%restart1,dir//'restart1'//fortran_PS)
         call set_IO_dir(this%restart2,dir//'restart2'//fortran_PS)
         call set_IO_dir(this%restart,dir//'restart'//fortran_PS)
         call set_IO_dir(this%mesh_restart,dir//'mesh_restart'//fortran_PS)
         call set_IO_dir(this%unknowns,dir//'unknowns'//fortran_PS)
         call set_IO_dir(this%governing_equations,&
         dir//'governing_equations'//fortran_PS)
         call set_IO_dir(this%U,dir//'U'//fortran_PS)
         call set_IO_dir(this%B,dir//'B'//fortran_PS)
         call set_IO_dir(this%J,dir//'J'//fortran_PS)
         call set_IO_dir(this%T,dir//'T'//fortran_PS)
         call set_IO_dir(this%p,dir//'p'//fortran_PS)
         call set_IO_dir(this%phi,dir//'phi'//fortran_PS)
         call set_IO_dir(this%rho,dir//'rho'//fortran_PS)
         call set_IO_dir(this%test,dir//'test'//fortran_PS)
       end subroutine

       subroutine export_structured_D_dir_tree(this,dir)
         implicit none
         type(dir_tree),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%tar_p,dir//'tar_p'//fortran_PS)
         call export_structured(this%out_dir,dir//'out_dir'//fortran_PS)
         call export_structured(this%LDC,dir//'LDC'//fortran_PS)
         call export_structured(this%mat,dir//'mat'//fortran_PS)
         call export_structured(this%config,dir//'config'//fortran_PS)
         call export_structured(this%meshes,dir//'meshes'//fortran_PS)
         call export_structured(this%BEM,dir//'BEM'//fortran_PS)
         call export_structured(this%wall_clock,dir//'wall_clock'//fortran_PS)
         call export_structured(this%matrix_visualization,&
         dir//'matrix_visualization'//fortran_PS)
         call export_structured(this%dimensionless_params,&
         dir//'dimensionless_params'//fortran_PS)
         call export_structured(this%params,dir//'params'//fortran_PS)
         call export_structured(this%ISP,dir//'ISP'//fortran_PS)
         call export_structured(this%TMP,dir//'TMP'//fortran_PS)
         call export_structured(this%EF,dir//'EF'//fortran_PS)
         call export_structured(this%export_now,dir//'export_now'//fortran_PS)
         call export_structured(this%refine_mesh,&
         dir//'refine_mesh'//fortran_PS)
         call export_structured(this%e_budget,dir//'e_budget'//fortran_PS)
         call export_structured(this%e_budget_N,dir//'e_budget_N'//fortran_PS)
         call export_structured(this%e_budget_C,dir//'e_budget_C'//fortran_PS)
         call export_structured(this%restart_sim,&
         dir//'restart_sim'//fortran_PS)
         call export_structured(this%restart1,dir//'restart1'//fortran_PS)
         call export_structured(this%restart2,dir//'restart2'//fortran_PS)
         call export_structured(this%restart,dir//'restart'//fortran_PS)
         call export_structured(this%mesh_restart,&
         dir//'mesh_restart'//fortran_PS)
         call export_structured(this%unknowns,dir//'unknowns'//fortran_PS)
         call export_structured(this%governing_equations,&
         dir//'governing_equations'//fortran_PS)
         call export_structured(this%U,dir//'U'//fortran_PS)
         call export_structured(this%B,dir//'B'//fortran_PS)
         call export_structured(this%J,dir//'J'//fortran_PS)
         call export_structured(this%T,dir//'T'//fortran_PS)
         call export_structured(this%p,dir//'p'//fortran_PS)
         call export_structured(this%phi,dir//'phi'//fortran_PS)
         call export_structured(this%rho,dir//'rho'//fortran_PS)
         call export_structured(this%test,dir//'test'//fortran_PS)
       end subroutine

       subroutine import_structured_D_dir_tree(this,dir)
         implicit none
         type(dir_tree),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%tar_p,dir//'tar_p'//fortran_PS)
         call import_structured(this%out_dir,dir//'out_dir'//fortran_PS)
         call import_structured(this%LDC,dir//'LDC'//fortran_PS)
         call import_structured(this%mat,dir//'mat'//fortran_PS)
         call import_structured(this%config,dir//'config'//fortran_PS)
         call import_structured(this%meshes,dir//'meshes'//fortran_PS)
         call import_structured(this%BEM,dir//'BEM'//fortran_PS)
         call import_structured(this%wall_clock,dir//'wall_clock'//fortran_PS)
         call import_structured(this%matrix_visualization,&
         dir//'matrix_visualization'//fortran_PS)
         call import_structured(this%dimensionless_params,&
         dir//'dimensionless_params'//fortran_PS)
         call import_structured(this%params,dir//'params'//fortran_PS)
         call import_structured(this%ISP,dir//'ISP'//fortran_PS)
         call import_structured(this%TMP,dir//'TMP'//fortran_PS)
         call import_structured(this%EF,dir//'EF'//fortran_PS)
         call import_structured(this%export_now,dir//'export_now'//fortran_PS)
         call import_structured(this%refine_mesh,&
         dir//'refine_mesh'//fortran_PS)
         call import_structured(this%e_budget,dir//'e_budget'//fortran_PS)
         call import_structured(this%e_budget_N,dir//'e_budget_N'//fortran_PS)
         call import_structured(this%e_budget_C,dir//'e_budget_C'//fortran_PS)
         call import_structured(this%restart_sim,&
         dir//'restart_sim'//fortran_PS)
         call import_structured(this%restart1,dir//'restart1'//fortran_PS)
         call import_structured(this%restart2,dir//'restart2'//fortran_PS)
         call import_structured(this%restart,dir//'restart'//fortran_PS)
         call import_structured(this%mesh_restart,&
         dir//'mesh_restart'//fortran_PS)
         call import_structured(this%unknowns,dir//'unknowns'//fortran_PS)
         call import_structured(this%governing_equations,&
         dir//'governing_equations'//fortran_PS)
         call import_structured(this%U,dir//'U'//fortran_PS)
         call import_structured(this%B,dir//'B'//fortran_PS)
         call import_structured(this%J,dir//'J'//fortran_PS)
         call import_structured(this%T,dir//'T'//fortran_PS)
         call import_structured(this%p,dir//'p'//fortran_PS)
         call import_structured(this%phi,dir//'phi'//fortran_PS)
         call import_structured(this%rho,dir//'rho'//fortran_PS)
         call import_structured(this%test,dir//'test'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_dir_tree(this)
         implicit none
         type(dir_tree),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module