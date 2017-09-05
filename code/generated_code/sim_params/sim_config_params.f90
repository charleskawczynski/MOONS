       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module sim_config_params_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: sim_config_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_si;    end interface
       interface delete;       module procedure delete_si;       end interface
       interface display;      module procedure display_si;      end interface
       interface display_short;module procedure display_short_si;end interface
       interface display;      module procedure display_wrap_si; end interface
       interface print;        module procedure print_si;        end interface
       interface print_short;  module procedure print_short_si;  end interface
       interface export;       module procedure export_si;       end interface
       interface import;       module procedure import_si;       end interface
       interface export;       module procedure export_wrap_si;  end interface
       interface import;       module procedure import_wrap_si;  end interface

       type sim_config_params
         real(cp) :: export_safe_period = 0.0_cp
         logical :: embed_b_interior = .false.
         logical :: couple_time_steps = .false.
         logical :: finite_rem = .false.
         logical :: include_vacuum = .false.
         integer :: mpg_dir = 0
         integer :: uniform_b0_dir = 0
         integer :: uniform_gravity_dir = 0
       end type

       contains

       subroutine init_copy_si(this,that)
         implicit none
         type(sim_config_params),intent(inout) :: this
         type(sim_config_params),intent(in) :: that
         call delete(this)
         this%export_safe_period = that%export_safe_period
         this%embed_b_interior = that%embed_b_interior
         this%couple_time_steps = that%couple_time_steps
         this%finite_rem = that%finite_rem
         this%include_vacuum = that%include_vacuum
         this%mpg_dir = that%mpg_dir
         this%uniform_b0_dir = that%uniform_b0_dir
         this%uniform_gravity_dir = that%uniform_gravity_dir
       end subroutine

       subroutine delete_si(this)
         implicit none
         type(sim_config_params),intent(inout) :: this
         this%export_safe_period = 0.0_cp
         this%embed_b_interior = .false.
         this%couple_time_steps = .false.
         this%finite_rem = .false.
         this%include_vacuum = .false.
         this%mpg_dir = 0
         this%uniform_b0_dir = 0
         this%uniform_gravity_dir = 0
       end subroutine

       subroutine display_si(this,un)
         implicit none
         type(sim_config_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- sim_config_params'
         write(un,*) 'export_safe_period  = ',this%export_safe_period
         write(un,*) 'embed_b_interior    = ',this%embed_b_interior
         write(un,*) 'couple_time_steps   = ',this%couple_time_steps
         write(un,*) 'finite_rem          = ',this%finite_rem
         write(un,*) 'include_vacuum      = ',this%include_vacuum
         write(un,*) 'mpg_dir             = ',this%mpg_dir
         write(un,*) 'uniform_b0_dir      = ',this%uniform_b0_dir
         write(un,*) 'uniform_gravity_dir = ',this%uniform_gravity_dir
       end subroutine

       subroutine display_short_si(this,un)
         implicit none
         type(sim_config_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_safe_period  = ',this%export_safe_period
         write(un,*) 'embed_b_interior    = ',this%embed_b_interior
         write(un,*) 'couple_time_steps   = ',this%couple_time_steps
         write(un,*) 'finite_rem          = ',this%finite_rem
         write(un,*) 'include_vacuum      = ',this%include_vacuum
         write(un,*) 'mpg_dir             = ',this%mpg_dir
         write(un,*) 'uniform_b0_dir      = ',this%uniform_b0_dir
         write(un,*) 'uniform_gravity_dir = ',this%uniform_gravity_dir
       end subroutine

       subroutine print_si(this)
         implicit none
         type(sim_config_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_si(this)
         implicit none
         type(sim_config_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_si(this,un)
         implicit none
         type(sim_config_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_safe_period   = ';write(un,*) this%export_safe_period
         write(un,*) 'embed_b_interior     = ';write(un,*) this%embed_b_interior
         write(un,*) 'couple_time_steps    = ';write(un,*) this%couple_time_steps
         write(un,*) 'finite_rem           = ';write(un,*) this%finite_rem
         write(un,*) 'include_vacuum       = ';write(un,*) this%include_vacuum
         write(un,*) 'mpg_dir              = ';write(un,*) this%mpg_dir
         write(un,*) 'uniform_b0_dir       = ';write(un,*) this%uniform_b0_dir
         write(un,*) 'uniform_gravity_dir  = ';write(un,*) this%uniform_gravity_dir
       end subroutine

       subroutine import_si(this,un)
         implicit none
         type(sim_config_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_safe_period
         read(un,*); read(un,*) this%embed_b_interior
         read(un,*); read(un,*) this%couple_time_steps
         read(un,*); read(un,*) this%finite_rem
         read(un,*); read(un,*) this%include_vacuum
         read(un,*); read(un,*) this%mpg_dir
         read(un,*); read(un,*) this%uniform_b0_dir
         read(un,*); read(un,*) this%uniform_gravity_dir
       end subroutine

       subroutine display_wrap_si(this,dir,name)
         implicit none
         type(sim_config_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_si(this,dir,name)
         implicit none
         type(sim_config_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_si(this,dir,name)
         implicit none
         type(sim_config_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module