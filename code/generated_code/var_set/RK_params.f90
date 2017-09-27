       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module RK_params_mod
       use IO_tools_mod
       use array_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: RK_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_RK_params;        end interface
       interface delete;           module procedure delete_RK_params;           end interface
       interface display;          module procedure display_RK_params;          end interface
       interface display_short;    module procedure display_short_RK_params;    end interface
       interface display;          module procedure display_wrap_RK_params;     end interface
       interface print;            module procedure print_RK_params;            end interface
       interface print_short;      module procedure print_short_RK_params;      end interface
       interface export;           module procedure export_RK_params;           end interface
       interface export_primitives;module procedure export_primitives_RK_params;end interface
       interface export_restart;   module procedure export_restart_RK_params;   end interface
       interface import;           module procedure import_RK_params;           end interface
       interface import_restart;   module procedure import_restart_RK_params;   end interface
       interface import_primitives;module procedure import_primitives_RK_params;end interface
       interface export;           module procedure export_wrap_RK_params;      end interface
       interface import;           module procedure import_wrap_RK_params;      end interface
       interface make_restart_dir; module procedure make_restart_dir_RK_params; end interface
       interface suppress_warnings;module procedure suppress_warnings_RK_params;end interface

       type RK_params
         integer :: n_stages = 0
         integer :: n = 0
         logical :: RK_active = .false.
         type(array) :: gamma
         type(array) :: zeta
         type(array) :: alpha
         type(array) :: beta
       end type

       contains

       subroutine init_copy_RK_params(this,that)
         implicit none
         type(RK_params),intent(inout) :: this
         type(RK_params),intent(in) :: that
         call delete(this)
         this%n_stages = that%n_stages
         this%n = that%n
         this%RK_active = that%RK_active
         call init(this%gamma,that%gamma)
         call init(this%zeta,that%zeta)
         call init(this%alpha,that%alpha)
         call init(this%beta,that%beta)
       end subroutine

       subroutine delete_RK_params(this)
         implicit none
         type(RK_params),intent(inout) :: this
         this%n_stages = 0
         this%n = 0
         this%RK_active = .false.
         call delete(this%gamma)
         call delete(this%zeta)
         call delete(this%alpha)
         call delete(this%beta)
       end subroutine

       subroutine display_RK_params(this,un)
         implicit none
         type(RK_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_stages  = ',this%n_stages
         write(un,*) 'n         = ',this%n
         write(un,*) 'RK_active = ',this%RK_active
         call display(this%gamma,un)
         call display(this%zeta,un)
         call display(this%alpha,un)
         call display(this%beta,un)
       end subroutine

       subroutine display_short_RK_params(this,un)
         implicit none
         type(RK_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_stages  = ',this%n_stages
         write(un,*) 'n         = ',this%n
         write(un,*) 'RK_active = ',this%RK_active
         call display(this%gamma,un)
         call display(this%zeta,un)
         call display(this%alpha,un)
         call display(this%beta,un)
       end subroutine

       subroutine display_wrap_RK_params(this,dir,name)
         implicit none
         type(RK_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_RK_params(this)
         implicit none
         type(RK_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_RK_params(this)
         implicit none
         type(RK_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_RK_params(this,un)
         implicit none
         type(RK_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_stages   = ';write(un,*) this%n_stages
         write(un,*) 'n          = ';write(un,*) this%n
         write(un,*) 'RK_active  = ';write(un,*) this%RK_active
       end subroutine

       subroutine export_RK_params(this,un)
         implicit none
         type(RK_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_stages   = ';write(un,*) this%n_stages
         write(un,*) 'n          = ';write(un,*) this%n
         write(un,*) 'RK_active  = ';write(un,*) this%RK_active
         call export(this%gamma,un)
         call export(this%zeta,un)
         call export(this%alpha,un)
         call export(this%beta,un)
       end subroutine

       subroutine import_primitives_RK_params(this,un)
         implicit none
         type(RK_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%n_stages
         read(un,*); read(un,*) this%n
         read(un,*); read(un,*) this%RK_active
       end subroutine

       subroutine import_RK_params(this,un)
         implicit none
         type(RK_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%n_stages
         read(un,*); read(un,*) this%n
         read(un,*); read(un,*) this%RK_active
         call import(this%gamma,un)
         call import(this%zeta,un)
         call import(this%alpha,un)
         call import(this%beta,un)
       end subroutine

       subroutine export_wrap_RK_params(this,dir,name)
         implicit none
         type(RK_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_RK_params(this,dir,name)
         implicit none
         type(RK_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_RK_params(this,dir)
         implicit none
         type(RK_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%gamma,dir//'gamma'//fortran_PS)
         call make_restart_dir(this%zeta,dir//'zeta'//fortran_PS)
         call make_restart_dir(this%alpha,dir//'alpha'//fortran_PS)
         call make_restart_dir(this%beta,dir//'beta'//fortran_PS)
       end subroutine

       subroutine export_restart_RK_params(this,dir)
         implicit none
         type(RK_params),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%gamma,dir//'gamma'//fortran_PS)
         call export_restart(this%zeta,dir//'zeta'//fortran_PS)
         call export_restart(this%alpha,dir//'alpha'//fortran_PS)
         call export_restart(this%beta,dir//'beta'//fortran_PS)
       end subroutine

       subroutine import_restart_RK_params(this,dir)
         implicit none
         type(RK_params),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%gamma,dir//'gamma'//fortran_PS)
         call import_restart(this%zeta,dir//'zeta'//fortran_PS)
         call import_restart(this%alpha,dir//'alpha'//fortran_PS)
         call import_restart(this%beta,dir//'beta'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_RK_params(this)
         implicit none
         type(RK_params),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module