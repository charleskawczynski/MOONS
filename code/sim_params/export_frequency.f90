       module export_frequency_mod
       use current_precision_mod
       use string_mod
       use IO_tools_mod
       use export_frequency_params_mod
       implicit none
       private
       public :: export_frequency
       public :: init,delete,display,print,export,import
       public :: update

       interface init;    module procedure init_EF_copy;      end interface
       interface delete;  module procedure delete_EF;         end interface
       interface display; module procedure display_EF;        end interface
       interface print;   module procedure print_EF;          end interface
       interface export;  module procedure export_EF;         end interface
       interface export;  module procedure export_EF_wrapper; end interface
       interface import;  module procedure import_EF;         end interface
       interface import;  module procedure import_EF_wrapper; end interface

       interface update;  module procedure update_EF;         end interface

       type export_frequency
         type(export_frequency_params) :: info
         type(export_frequency_params) :: unsteady_0D
         type(export_frequency_params) :: unsteady_1D
         type(export_frequency_params) :: unsteady_2D
         type(export_frequency_params) :: unsteady_3D
         type(export_frequency_params) :: final_solution
         type(export_frequency_params) :: restart_files
         type(string) :: dir,name
       end type

       contains

       subroutine init_EF_copy(EF,EF_in)
         implicit none
         type(export_frequency),intent(inout) :: EF
         type(export_frequency),intent(in) :: EF_in
         call init(EF%info,EF_in%info)
         call init(EF%unsteady_0D,EF_in%unsteady_0D)
         call init(EF%unsteady_1D,EF_in%unsteady_1D)
         call init(EF%unsteady_2D,EF_in%unsteady_2D)
         call init(EF%unsteady_3D,EF_in%unsteady_3D)
         call init(EF%final_solution,EF_in%final_solution)
         call init(EF%restart_files,EF_in%restart_files)
         call init(EF%dir,EF_in%dir)
         call init(EF%name,EF_in%name)
       end subroutine

       subroutine delete_EF(EF)
         implicit none
         type(export_frequency),intent(inout) :: EF
         call delete(EF%info)
         call delete(EF%unsteady_0D)
         call delete(EF%unsteady_1D)
         call delete(EF%unsteady_2D)
         call delete(EF%unsteady_3D)
         call delete(EF%final_solution)
         call delete(EF%restart_files)
         call delete(EF%dir)
         call delete(EF%name)
       end subroutine

       subroutine display_EF(EF,un)
         implicit none
         type(export_frequency),intent(in) :: EF
         integer,intent(in) :: un
         call display(EF%info,un)
         call display(EF%unsteady_0D,un)
         call display(EF%unsteady_1D,un)
         call display(EF%unsteady_2D,un)
         call display(EF%unsteady_3D,un)
         call display(EF%final_solution,un)
         call display(EF%restart_files,un)
       end subroutine

       subroutine print_EF(EF)
         implicit none
         type(export_frequency),intent(in) :: EF
         call display(EF,6)
       end subroutine

       subroutine export_EF(EF,un)
         implicit none
         type(export_frequency),intent(in) :: EF
         integer,intent(in) :: un
         write(un,*) ' ------------ export_frequency ------------ '
         call export(EF%info,un)
         call export(EF%unsteady_0D,un)
         call export(EF%unsteady_1D,un)
         call export(EF%unsteady_2D,un)
         call export(EF%unsteady_3D,un)
         call export(EF%final_solution,un)
         call export(EF%restart_files,un)
         call export(EF%dir,un)
         call export(EF%name,un)
         write(un,*) ' ------------------------------------------ '
       end subroutine

       subroutine import_EF(EF,un)
         implicit none
         type(export_frequency),intent(inout) :: EF
         integer,intent(in) :: un
         read(un,*);
         call import(EF%info,un)
         call import(EF%unsteady_0D,un)
         call import(EF%unsteady_1D,un)
         call import(EF%unsteady_2D,un)
         call import(EF%unsteady_3D,un)
         call import(EF%final_solution,un)
         call import(EF%restart_files,un)
         call import(EF%dir,un)
         call import(EF%name,un)
         read(un,*);
       end subroutine

       subroutine export_EF_wrapper(EF)
         implicit none
         type(export_frequency),intent(in) :: EF
         integer :: un
         un = new_and_open(str(EF%dir),str(EF%name))
         call export(EF,un)
         close(un)
       end subroutine

       subroutine import_EF_wrapper(EF)
         implicit none
         type(export_frequency),intent(inout) :: EF
         integer :: un
         un = open_to_read(str(EF%dir),str(EF%name))
         call import(EF,un)
         close(un)
       end subroutine

       subroutine update_EF(EF,n_step,substep)
         implicit none
         type(export_frequency),intent(inout) :: EF
         integer(li),intent(in) :: n_step
         logical,intent(in) :: substep
         call update(EF%info,n_step,substep)
         call update(EF%unsteady_0D,n_step,substep)
         call update(EF%unsteady_1D,n_step,substep)
         call update(EF%unsteady_2D,n_step,substep)
         call update(EF%unsteady_3D,n_step,substep)
         call update(EF%final_solution,n_step,substep)
         call update(EF%restart_files,n_step,substep)
       end subroutine

       end module