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
       public :: display_short,print_short

       interface init;         module procedure init_copy_dir_tree;      end interface
       interface delete;       module procedure delete_dir_tree;         end interface
       interface display;      module procedure display_dir_tree;        end interface
       interface display_short;module procedure display_short_dir_tree;  end interface
       interface display;      module procedure display_wrapper_dir_tree;end interface
       interface print;        module procedure print_dir_tree;          end interface
       interface print_short;  module procedure print_short_dir_tree;    end interface
       interface export;       module procedure export_dir_tree;         end interface
       interface import;       module procedure import_dir_tree;         end interface
       interface export;       module procedure export_wrapper_dir_tree; end interface
       interface import;       module procedure import_wrapper_dir_tree; end interface

       type dir_tree
         type(path) :: tar_p
         type(path) :: out_dir
         type(path) :: LDC
         type(path) :: mat
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
         write(un,*) ' -------------------- dir_tree'
         call display(this%tar_p,un)
         call display(this%out_dir,un)
         call display(this%LDC,un)
         call display(this%mat,un)
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