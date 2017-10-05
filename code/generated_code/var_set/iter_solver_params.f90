       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module iter_solver_params_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use exit_criteria_mod
       use string_mod
       implicit none

       private
       public :: iter_solver_params
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings,export,import,export_structured,import_structured

       interface init;             module procedure init_copy_iter_solver_params;           end interface
       interface delete;           module procedure delete_iter_solver_params;              end interface
       interface display;          module procedure display_iter_solver_params;             end interface
       interface display_short;    module procedure display_short_iter_solver_params;       end interface
       interface display;          module procedure display_wrap_iter_solver_params;        end interface
       interface print;            module procedure print_iter_solver_params;               end interface
       interface print_short;      module procedure print_short_iter_solver_params;         end interface
       interface export;           module procedure export_iter_solver_params;              end interface
       interface export_primitives;module procedure export_primitives_iter_solver_params;   end interface
       interface import;           module procedure import_iter_solver_params;              end interface
       interface export_structured;module procedure export_structured_D_iter_solver_params; end interface
       interface import_structured;module procedure import_structured_D_iter_solver_params; end interface
       interface import_primitives;module procedure import_primitives_iter_solver_params;   end interface
       interface export;           module procedure export_wrap_iter_solver_params;         end interface
       interface import;           module procedure import_wrap_iter_solver_params;         end interface
       interface set_IO_dir;       module procedure set_IO_dir_iter_solver_params;          end interface
       interface make_IO_dir;      module procedure make_IO_dir_iter_solver_params;         end interface
       interface suppress_warnings;module procedure suppress_warnings_iter_solver_params;   end interface
       interface export;           module procedure export_DN_iter_solver_params;           end interface
       interface import;           module procedure import_DN_iter_solver_params;           end interface
       interface export_structured;module procedure export_structured_DN_iter_solver_params;end interface
       interface import_structured;module procedure import_structured_DN_iter_solver_params;end interface

       type iter_solver_params
         type(exit_criteria) :: EC
         logical,dimension(3) :: exit_loop = .false.
         integer :: un = 0
         type(string) :: dir
         type(string) :: name
         integer :: iter_total = 0
         integer :: iter_per_call = 0
         integer :: n_skip_check_res = 0
         logical :: export_convergence = .false.
         logical :: export_heavy = .false.
       end type

       contains

       subroutine init_copy_iter_solver_params(this,that)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         type(iter_solver_params),intent(in) :: that
         call delete(this)
         call init(this%EC,that%EC)
         this%exit_loop = that%exit_loop
         this%un = that%un
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         this%iter_total = that%iter_total
         this%iter_per_call = that%iter_per_call
         this%n_skip_check_res = that%n_skip_check_res
         this%export_convergence = that%export_convergence
         this%export_heavy = that%export_heavy
       end subroutine

       subroutine delete_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         call delete(this%EC)
         this%exit_loop = .false.
         this%un = 0
         call delete(this%dir)
         call delete(this%name)
         this%iter_total = 0
         this%iter_per_call = 0
         this%n_skip_check_res = 0
         this%export_convergence = .false.
         this%export_heavy = .false.
       end subroutine

       subroutine display_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%EC,un)
         write(un,*) 'exit_loop          = ',this%exit_loop
         write(un,*) 'un                 = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_total         = ',this%iter_total
         write(un,*) 'iter_per_call      = ',this%iter_per_call
         write(un,*) 'n_skip_check_res   = ',this%n_skip_check_res
         write(un,*) 'export_convergence = ',this%export_convergence
         write(un,*) 'export_heavy       = ',this%export_heavy
       end subroutine

       subroutine display_short_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         call display(this%EC,un)
         write(un,*) 'exit_loop          = ',this%exit_loop
         write(un,*) 'un                 = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'iter_total         = ',this%iter_total
         write(un,*) 'iter_per_call      = ',this%iter_per_call
         write(un,*) 'n_skip_check_res   = ',this%n_skip_check_res
         write(un,*) 'export_convergence = ',this%export_convergence
         write(un,*) 'export_heavy       = ',this%export_heavy
       end subroutine

       subroutine display_wrap_iter_solver_params(this,dir,name)
         implicit none
         type(iter_solver_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%EC,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%EC,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_primitives_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'exit_loop           = ';write(un,*) this%exit_loop
         write(un,*) 'un                  = ';write(un,*) this%un
         write(un,*) 'iter_total          = ';write(un,*) this%iter_total
         write(un,*) 'iter_per_call       = ';write(un,*) this%iter_per_call
         write(un,*) 'n_skip_check_res    = ';write(un,*) this%n_skip_check_res
         write(un,*) 'export_convergence  = ';write(un,*) this%export_convergence
         write(un,*) 'export_heavy        = ';write(un,*) this%export_heavy
       end subroutine

       subroutine import_primitives_iter_solver_params(this,un)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%exit_loop
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%iter_total
         read(un,*); read(un,*) this%iter_per_call
         read(un,*); read(un,*) this%n_skip_check_res
         read(un,*); read(un,*) this%export_convergence
         read(un,*); read(un,*) this%export_heavy
       end subroutine

       subroutine export_wrap_iter_solver_params(this,dir,name)
         implicit none
         type(iter_solver_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_iter_solver_params(this,dir,name)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       subroutine export_structured_DN_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         call export_structured(this%EC,str(this%dir)//'EC'//fortran_PS)
         call export_structured(this%dir,str(this%dir)//'dir'//fortran_PS)
         call export_structured(this%name,str(this%dir)//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_DN_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         call import_structured(this%EC,str(this%dir)//'EC'//fortran_PS)
         call import_structured(this%dir,str(this%dir)//'dir'//fortran_PS)
         call import_structured(this%name,str(this%dir)//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine set_IO_dir_iter_solver_params(this,dir)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call set_IO_dir(this%EC,dir//'EC'//fortran_PS)
         call set_IO_dir(this%dir,dir//'dir'//fortran_PS)
         call set_IO_dir(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_iter_solver_params(this,dir)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call make_IO_dir(this%EC,dir//'EC'//fortran_PS)
         call make_IO_dir(this%dir,dir//'dir'//fortran_PS)
         call make_IO_dir(this%name,dir//'name'//fortran_PS)
       end subroutine

       subroutine export_structured_D_iter_solver_params(this,dir)
         implicit none
         type(iter_solver_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%EC,dir//'EC'//fortran_PS)
         call export_structured(this%dir,dir//'dir'//fortran_PS)
         call export_structured(this%name,dir//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_iter_solver_params(this,dir)
         implicit none
         type(iter_solver_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call delete(this)
         call import_primitives(this,un)
         call import_structured(this%EC,dir//'EC'//fortran_PS)
         call import_structured(this%dir,dir//'dir'//fortran_PS)
         call import_structured(this%name,dir//'name'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_iter_solver_params(this)
         implicit none
         type(iter_solver_params),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module