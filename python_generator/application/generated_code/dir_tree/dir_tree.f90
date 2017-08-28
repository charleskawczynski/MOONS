       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module dir_tree_mod
       use IO_tools_mod
       use dir_group_mod
       use path_mod
       use string_mod
       implicit none

       private
       public :: dir_tree
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_dir_tree;           end interface
       interface delete; module procedure delete_dir_tree;         end interface
       interface display;module procedure display_dir_tree;        end interface
       interface display;module procedure display_wrapper_dir_tree;end interface
       interface print;  module procedure print_dir_tree;          end interface
       interface export; module procedure export_dir_tree;         end interface
       interface import; module procedure import_dir_tree;         end interface
       interface export; module procedure export_wrapper_dir_tree; end interface
       interface import; module procedure import_wrapper_dir_tree; end interface

       type dir_tree
         type(path) :: tar_p
         type(path) :: out_dir
         type(path) :: ldc
         type(path) :: mat
         type(path) :: meshes
         type(path) :: bem
         type(path) :: wall_clock
         type(path) :: matrix_visualization
         type(path) :: dimensionless_params
         type(path) :: params
         type(path) :: isp
         type(path) :: tmp
         type(path) :: ef
         type(path) :: export_now
         type(path) :: refine_mesh
         type(path) :: e_budget
         type(path) :: e_budget_n
         type(path) :: e_budget_c
         type(path) :: restart_sim
         type(path) :: restart1
         type(path) :: restart2
         type(path) :: restart
         type(path) :: mesh_restart
         type(path) :: unknowns
         type(string) :: ps
         type(string) :: tar
         type(dir_group) :: u
         type(dir_group) :: b
         type(dir_group) :: j
         type(dir_group) :: t
         type(dir_group) :: p
         type(dir_group) :: phi
         type(dir_group) :: rho
         type(dir_group) :: test
       end type

       contains

       subroutine init_dir_tree(this,that)
         implicit none
         type(dir_tree),intent(inout) :: this
         type(dir_tree),intent(in) :: that
         call delete(this)
         call init(this%tar_p,that%tar_p)
         call init(this%out_dir,that%out_dir)
         call init(this%ldc,that%ldc)
         call init(this%mat,that%mat)
         call init(this%meshes,that%meshes)
         call init(this%bem,that%bem)
         call init(this%wall_clock,that%wall_clock)
         call init(this%matrix_visualization,that%matrix_visualization)
         call init(this%dimensionless_params,that%dimensionless_params)
         call init(this%params,that%params)
         call init(this%isp,that%isp)
         call init(this%tmp,that%tmp)
         call init(this%ef,that%ef)
         call init(this%export_now,that%export_now)
         call init(this%refine_mesh,that%refine_mesh)
         call init(this%e_budget,that%e_budget)
         call init(this%e_budget_n,that%e_budget_n)
         call init(this%e_budget_c,that%e_budget_c)
         call init(this%restart_sim,that%restart_sim)
         call init(this%restart1,that%restart1)
         call init(this%restart2,that%restart2)
         call init(this%restart,that%restart)
         call init(this%mesh_restart,that%mesh_restart)
         call init(this%unknowns,that%unknowns)
         call init(this%ps,that%ps)
         call init(this%tar,that%tar)
         call init(this%u,that%u)
         call init(this%b,that%b)
         call init(this%j,that%j)
         call init(this%t,that%t)
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
         call delete(this%ldc)
         call delete(this%mat)
         call delete(this%meshes)
         call delete(this%bem)
         call delete(this%wall_clock)
         call delete(this%matrix_visualization)
         call delete(this%dimensionless_params)
         call delete(this%params)
         call delete(this%isp)
         call delete(this%tmp)
         call delete(this%ef)
         call delete(this%export_now)
         call delete(this%refine_mesh)
         call delete(this%e_budget)
         call delete(this%e_budget_n)
         call delete(this%e_budget_c)
         call delete(this%restart_sim)
         call delete(this%restart1)
         call delete(this%restart2)
         call delete(this%restart)
         call delete(this%mesh_restart)
         call delete(this%unknowns)
         call delete(this%ps)
         call delete(this%tar)
         call delete(this%u)
         call delete(this%b)
         call delete(this%j)
         call delete(this%t)
         call delete(this%p)
         call delete(this%phi)
         call delete(this%rho)
         call delete(this%test)
       end subroutine

       subroutine display_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- dir_tree'
         call display(this%tar_p,un)
         call display(this%out_dir,un)
         call display(this%ldc,un)
         call display(this%mat,un)
         call display(this%meshes,un)
         call display(this%bem,un)
         call display(this%wall_clock,un)
         call display(this%matrix_visualization,un)
         call display(this%dimensionless_params,un)
         call display(this%params,un)
         call display(this%isp,un)
         call display(this%tmp,un)
         call display(this%ef,un)
         call display(this%export_now,un)
         call display(this%refine_mesh,un)
         call display(this%e_budget,un)
         call display(this%e_budget_n,un)
         call display(this%e_budget_c,un)
         call display(this%restart_sim,un)
         call display(this%restart1,un)
         call display(this%restart2,un)
         call display(this%restart,un)
         call display(this%mesh_restart,un)
         call display(this%unknowns,un)
         call display(this%ps,un)
         call display(this%tar,un)
         call display(this%u,un)
         call display(this%b,un)
         call display(this%j,un)
         call display(this%t,un)
         call display(this%p,un)
         call display(this%phi,un)
         call display(this%rho,un)
         call display(this%test,un)
       end subroutine

       subroutine print_dir_tree(this)
         implicit none
         type(dir_tree),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_dir_tree(this,un)
         implicit none
         type(dir_tree),intent(in) :: this
         integer,intent(in) :: un
         call export(this%tar_p,un)
         call export(this%out_dir,un)
         call export(this%ldc,un)
         call export(this%mat,un)
         call export(this%meshes,un)
         call export(this%bem,un)
         call export(this%wall_clock,un)
         call export(this%matrix_visualization,un)
         call export(this%dimensionless_params,un)
         call export(this%params,un)
         call export(this%isp,un)
         call export(this%tmp,un)
         call export(this%ef,un)
         call export(this%export_now,un)
         call export(this%refine_mesh,un)
         call export(this%e_budget,un)
         call export(this%e_budget_n,un)
         call export(this%e_budget_c,un)
         call export(this%restart_sim,un)
         call export(this%restart1,un)
         call export(this%restart2,un)
         call export(this%restart,un)
         call export(this%mesh_restart,un)
         call export(this%unknowns,un)
         call export(this%ps,un)
         call export(this%tar,un)
         call export(this%u,un)
         call export(this%b,un)
         call export(this%j,un)
         call export(this%t,un)
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
         call import(this%ldc,un)
         call import(this%mat,un)
         call import(this%meshes,un)
         call import(this%bem,un)
         call import(this%wall_clock,un)
         call import(this%matrix_visualization,un)
         call import(this%dimensionless_params,un)
         call import(this%params,un)
         call import(this%isp,un)
         call import(this%tmp,un)
         call import(this%ef,un)
         call import(this%export_now,un)
         call import(this%refine_mesh,un)
         call import(this%e_budget,un)
         call import(this%e_budget_n,un)
         call import(this%e_budget_c,un)
         call import(this%restart_sim,un)
         call import(this%restart1,un)
         call import(this%restart2,un)
         call import(this%restart,un)
         call import(this%mesh_restart,un)
         call import(this%unknowns,un)
         call import(this%ps,un)
         call import(this%tar,un)
         call import(this%u,un)
         call import(this%b,un)
         call import(this%j,un)
         call import(this%t,un)
         call import(this%p,un)
         call import(this%phi,un)
         call import(this%rho,un)
         call import(this%test,un)
       end subroutine

       subroutine display_wrapper_dir_tree(this,dir,name)
         implicit none
         type(dir_tree),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_dir_tree(this,dir,name)
         implicit none
         type(dir_tree),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_dir_tree(this,dir,name)
         implicit none
         type(dir_tree),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module