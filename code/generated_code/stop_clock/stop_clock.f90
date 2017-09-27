       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module stop_clock_mod
       use current_precision_mod
       use IO_tools_mod
       use clock_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       use unit_conversion_mod
       implicit none

       private
       public :: stop_clock
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_stop_clock;        end interface
       interface delete;           module procedure delete_stop_clock;           end interface
       interface display;          module procedure display_stop_clock;          end interface
       interface display_short;    module procedure display_short_stop_clock;    end interface
       interface display;          module procedure display_wrap_stop_clock;     end interface
       interface print;            module procedure print_stop_clock;            end interface
       interface print_short;      module procedure print_short_stop_clock;      end interface
       interface export;           module procedure export_stop_clock;           end interface
       interface export_primitives;module procedure export_primitives_stop_clock;end interface
       interface export_restart;   module procedure export_restart_stop_clock;   end interface
       interface import;           module procedure import_stop_clock;           end interface
       interface import_restart;   module procedure import_restart_stop_clock;   end interface
       interface import_primitives;module procedure import_primitives_stop_clock;end interface
       interface export;           module procedure export_wrap_stop_clock;      end interface
       interface import;           module procedure import_wrap_stop_clock;      end interface
       interface make_restart_dir; module procedure make_restart_dir_stop_clock; end interface
       interface suppress_warnings;module procedure suppress_warnings_stop_clock;end interface
       interface export;           module procedure export_DN_stop_clock;        end interface
       interface import;           module procedure import_DN_stop_clock;        end interface

       type stop_clock
         type(string) :: dir
         type(string) :: name
         type(clock) :: c
         type(unit_conversion) :: uc
         real(cp) :: percentage_complete_RB = 0.0_cp
         real(cp) :: percentage_complete_SB = 0.0_cp
         real(cp) :: seconds_per_step = 0.0_cp
         real(cp) :: sim_time_per_sec = 0.0_cp
         real(cp) :: t_passed = 0.0_cp
         real(cp) :: estimated_total = 0.0_cp
         real(cp) :: estimated_remaining = 0.0_cp
         real(cp) :: percentage_complete = 0.0_cp
         real(cp) :: percentage_complete_wc = 0.0_cp
         real(cp) :: t_elapsed = 0.0_cp
         logical :: frozen_elapsed = .false.
         integer :: un_plot = 0
       end type

       contains

       subroutine init_copy_stop_clock(this,that)
         implicit none
         type(stop_clock),intent(inout) :: this
         type(stop_clock),intent(in) :: that
         call delete(this)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         call init(this%c,that%c)
         call init(this%uc,that%uc)
         this%percentage_complete_RB = that%percentage_complete_RB
         this%percentage_complete_SB = that%percentage_complete_SB
         this%seconds_per_step = that%seconds_per_step
         this%sim_time_per_sec = that%sim_time_per_sec
         this%t_passed = that%t_passed
         this%estimated_total = that%estimated_total
         this%estimated_remaining = that%estimated_remaining
         this%percentage_complete = that%percentage_complete
         this%percentage_complete_wc = that%percentage_complete_wc
         this%t_elapsed = that%t_elapsed
         this%frozen_elapsed = that%frozen_elapsed
         this%un_plot = that%un_plot
       end subroutine

       subroutine delete_stop_clock(this)
         implicit none
         type(stop_clock),intent(inout) :: this
         call delete(this%dir)
         call delete(this%name)
         call delete(this%c)
         call delete(this%uc)
         this%percentage_complete_RB = 0.0_cp
         this%percentage_complete_SB = 0.0_cp
         this%seconds_per_step = 0.0_cp
         this%sim_time_per_sec = 0.0_cp
         this%t_passed = 0.0_cp
         this%estimated_total = 0.0_cp
         this%estimated_remaining = 0.0_cp
         this%percentage_complete = 0.0_cp
         this%percentage_complete_wc = 0.0_cp
         this%t_elapsed = 0.0_cp
         this%frozen_elapsed = .false.
         this%un_plot = 0
       end subroutine

       subroutine display_stop_clock(this,un)
         implicit none
         type(stop_clock),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%c,un)
         call display(this%uc,un)
         write(un,*) 'percentage_complete_RB = ',this%percentage_complete_RB
         write(un,*) 'percentage_complete_SB = ',this%percentage_complete_SB
         write(un,*) 'seconds_per_step       = ',this%seconds_per_step
         write(un,*) 'sim_time_per_sec       = ',this%sim_time_per_sec
         write(un,*) 't_passed               = ',this%t_passed
         write(un,*) 'estimated_total        = ',this%estimated_total
         write(un,*) 'estimated_remaining    = ',this%estimated_remaining
         write(un,*) 'percentage_complete    = ',this%percentage_complete
         write(un,*) 'percentage_complete_wc = ',this%percentage_complete_wc
         write(un,*) 't_elapsed              = ',this%t_elapsed
         write(un,*) 'frozen_elapsed         = ',this%frozen_elapsed
         write(un,*) 'un_plot                = ',this%un_plot
       end subroutine

       subroutine display_short_stop_clock(this,un)
         implicit none
         type(stop_clock),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%c,un)
         call display(this%uc,un)
         write(un,*) 'percentage_complete_RB = ',this%percentage_complete_RB
         write(un,*) 'percentage_complete_SB = ',this%percentage_complete_SB
         write(un,*) 'seconds_per_step       = ',this%seconds_per_step
         write(un,*) 'sim_time_per_sec       = ',this%sim_time_per_sec
         write(un,*) 't_passed               = ',this%t_passed
         write(un,*) 'estimated_total        = ',this%estimated_total
         write(un,*) 'estimated_remaining    = ',this%estimated_remaining
         write(un,*) 'percentage_complete    = ',this%percentage_complete
         write(un,*) 'percentage_complete_wc = ',this%percentage_complete_wc
         write(un,*) 't_elapsed              = ',this%t_elapsed
         write(un,*) 'frozen_elapsed         = ',this%frozen_elapsed
         write(un,*) 'un_plot                = ',this%un_plot
       end subroutine

       subroutine display_wrap_stop_clock(this,dir,name)
         implicit none
         type(stop_clock),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_stop_clock(this)
         implicit none
         type(stop_clock),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_stop_clock(this)
         implicit none
         type(stop_clock),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_stop_clock(this,un)
         implicit none
         type(stop_clock),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'percentage_complete_RB  = ';write(un,*) this%percentage_complete_RB
         write(un,*) 'percentage_complete_SB  = ';write(un,*) this%percentage_complete_SB
         write(un,*) 'seconds_per_step        = ';write(un,*) this%seconds_per_step
         write(un,*) 'sim_time_per_sec        = ';write(un,*) this%sim_time_per_sec
         write(un,*) 't_passed                = ';write(un,*) this%t_passed
         write(un,*) 'estimated_total         = ';write(un,*) this%estimated_total
         write(un,*) 'estimated_remaining     = ';write(un,*) this%estimated_remaining
         write(un,*) 'percentage_complete     = ';write(un,*) this%percentage_complete
         write(un,*) 'percentage_complete_wc  = ';write(un,*) this%percentage_complete_wc
         write(un,*) 't_elapsed               = ';write(un,*) this%t_elapsed
         write(un,*) 'frozen_elapsed          = ';write(un,*) this%frozen_elapsed
         write(un,*) 'un_plot                 = ';write(un,*) this%un_plot
       end subroutine

       subroutine export_stop_clock(this,un)
         implicit none
         type(stop_clock),intent(in) :: this
         integer,intent(in) :: un
         call export(this%dir,un)
         call export(this%name,un)
         call export(this%c,un)
         call export(this%uc,un)
         write(un,*) 'percentage_complete_RB  = ';write(un,*) this%percentage_complete_RB
         write(un,*) 'percentage_complete_SB  = ';write(un,*) this%percentage_complete_SB
         write(un,*) 'seconds_per_step        = ';write(un,*) this%seconds_per_step
         write(un,*) 'sim_time_per_sec        = ';write(un,*) this%sim_time_per_sec
         write(un,*) 't_passed                = ';write(un,*) this%t_passed
         write(un,*) 'estimated_total         = ';write(un,*) this%estimated_total
         write(un,*) 'estimated_remaining     = ';write(un,*) this%estimated_remaining
         write(un,*) 'percentage_complete     = ';write(un,*) this%percentage_complete
         write(un,*) 'percentage_complete_wc  = ';write(un,*) this%percentage_complete_wc
         write(un,*) 't_elapsed               = ';write(un,*) this%t_elapsed
         write(un,*) 'frozen_elapsed          = ';write(un,*) this%frozen_elapsed
         write(un,*) 'un_plot                 = ';write(un,*) this%un_plot
       end subroutine

       subroutine import_primitives_stop_clock(this,un)
         implicit none
         type(stop_clock),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%percentage_complete_RB
         read(un,*); read(un,*) this%percentage_complete_SB
         read(un,*); read(un,*) this%seconds_per_step
         read(un,*); read(un,*) this%sim_time_per_sec
         read(un,*); read(un,*) this%t_passed
         read(un,*); read(un,*) this%estimated_total
         read(un,*); read(un,*) this%estimated_remaining
         read(un,*); read(un,*) this%percentage_complete
         read(un,*); read(un,*) this%percentage_complete_wc
         read(un,*); read(un,*) this%t_elapsed
         read(un,*); read(un,*) this%frozen_elapsed
         read(un,*); read(un,*) this%un_plot
       end subroutine

       subroutine import_stop_clock(this,un)
         implicit none
         type(stop_clock),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%dir,un)
         call import(this%name,un)
         call import(this%c,un)
         call import(this%uc,un)
         read(un,*); read(un,*) this%percentage_complete_RB
         read(un,*); read(un,*) this%percentage_complete_SB
         read(un,*); read(un,*) this%seconds_per_step
         read(un,*); read(un,*) this%sim_time_per_sec
         read(un,*); read(un,*) this%t_passed
         read(un,*); read(un,*) this%estimated_total
         read(un,*); read(un,*) this%estimated_remaining
         read(un,*); read(un,*) this%percentage_complete
         read(un,*); read(un,*) this%percentage_complete_wc
         read(un,*); read(un,*) this%t_elapsed
         read(un,*); read(un,*) this%frozen_elapsed
         read(un,*); read(un,*) this%un_plot
       end subroutine

       subroutine export_wrap_stop_clock(this,dir,name)
         implicit none
         type(stop_clock),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_stop_clock(this,dir,name)
         implicit none
         type(stop_clock),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_stop_clock(this)
         implicit none
         type(stop_clock),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_stop_clock(this)
         implicit none
         type(stop_clock),intent(inout) :: this
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

       subroutine make_restart_dir_stop_clock(this,dir)
         implicit none
         type(stop_clock),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%c,dir//fortran_PS//'c')
         call make_restart_dir(this%uc,dir//fortran_PS//'uc')
       end subroutine

       subroutine export_restart_stop_clock(this,dir)
         implicit none
         type(stop_clock),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%c,dir//fortran_PS//'c')
         call export_restart(this%uc,dir//fortran_PS//'uc')
       end subroutine

       subroutine import_restart_stop_clock(this,dir)
         implicit none
         type(stop_clock),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%c,dir//fortran_PS//'c')
         call import_restart(this%uc,dir//fortran_PS//'uc')
       end subroutine

       subroutine suppress_warnings_stop_clock(this)
         implicit none
         type(stop_clock),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module