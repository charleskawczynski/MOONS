     module flow_control_logicals_mod
     implicit none

     private
     public :: flow_control_logicals
     public :: init,delete,display,print,export,import

     type flow_control_logicals
       logical :: post_process = .false.
       logical :: skip_solver_loop = .false.
       logical :: stop_before_solve = .false.
       logical :: stop_after_mesh_export = .false.
     end type

     interface init;    module procedure init_copy_FCL; end interface
     interface delete;  module procedure delete_FCL;    end interface
     interface display; module procedure display_FCL;   end interface
     interface print;   module procedure print_FCL;     end interface
     interface export;  module procedure export_FCL;    end interface
     interface import;  module procedure import_FCL;    end interface

     contains

     subroutine init_copy_FCL(FCL,FCL_in)
       implicit none
       type(flow_control_logicals),intent(inout) :: FCL
       type(flow_control_logicals),intent(in) :: FCL_in
       FCL%post_process           = FCL_in%post_process
       FCL%skip_solver_loop       = FCL_in%skip_solver_loop
       FCL%stop_before_solve      = FCL_in%stop_before_solve
       FCL%stop_after_mesh_export = FCL_in%stop_after_mesh_export
     end subroutine

     subroutine delete_FCL(FCL)
       implicit none
       type(flow_control_logicals),intent(inout) :: FCL
       FCL%post_process           = .false.
       FCL%skip_solver_loop       = .false.
       FCL%stop_before_solve      = .false.
       FCL%stop_after_mesh_export = .false.
     end subroutine

     subroutine display_FCL(FCL,un)
       implicit none
       type(flow_control_logicals),intent(in) :: FCL
       integer,intent(in) :: un
       write(un,*) 'post_process           = ',FCL%post_process
       write(un,*) 'skip_solver_loop       = ',FCL%skip_solver_loop
       write(un,*) 'stop_before_solve      = ',FCL%stop_before_solve
       write(un,*) 'stop_after_mesh_export = ',FCL%stop_after_mesh_export
     end subroutine

     subroutine print_FCL(FCL)
       implicit none
       type(flow_control_logicals),intent(in) :: FCL
       call display(FCL,6)
     end subroutine

     subroutine export_FCL(FCL,un)
       implicit none
       type(flow_control_logicals),intent(in) :: FCL
       integer,intent(in) :: un
       write(un,*) FCL%post_process
       write(un,*) FCL%skip_solver_loop
       write(un,*) FCL%stop_before_solve
       write(un,*) FCL%stop_after_mesh_export
     end subroutine

     subroutine import_FCL(FCL,un)
       implicit none
       type(flow_control_logicals),intent(inout) :: FCL
       integer,intent(in) :: un
       read(un,*) FCL%post_process
       read(un,*) FCL%skip_solver_loop
       read(un,*) FCL%stop_before_solve
       read(un,*) FCL%stop_after_mesh_export
     end subroutine

     end module