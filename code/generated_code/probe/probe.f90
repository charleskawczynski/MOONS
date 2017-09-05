       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module probe_mod
       use current_precision_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: probe
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_pr;    end interface
       interface delete;       module procedure delete_pr;       end interface
       interface display;      module procedure display_pr;      end interface
       interface display_short;module procedure display_short_pr;end interface
       interface display;      module procedure display_wrap_pr; end interface
       interface print;        module procedure print_pr;        end interface
       interface print_short;  module procedure print_short_pr;  end interface
       interface export;       module procedure export_pr;       end interface
       interface import;       module procedure import_pr;       end interface
       interface export;       module procedure export_wrap_pr;  end interface
       interface import;       module procedure import_wrap_pr;  end interface
       interface export;       module procedure export_DN_pr;    end interface
       interface import;       module procedure import_DN_pr;    end interface

       type probe
         type(string) :: dir
         type(string) :: name
         real(cp) :: d = 0.0_cp
         real(cp) :: d_data_dt = 0.0_cp
         real(cp) :: d_amax = 0.0_cp
         real(cp) :: t = 0.0_cp
         integer :: un = 0
         integer :: cols = 0
         integer(li) :: n_step = 0
         logical :: restart = .false.
         logical :: simple = .false.
       end type

       contains

       subroutine init_copy_pr(this,that)
         implicit none
         type(probe),intent(inout) :: this
         type(probe),intent(in) :: that
         call delete(this)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         this%d = that%d
         this%d_data_dt = that%d_data_dt
         this%d_amax = that%d_amax
         this%t = that%t
         this%un = that%un
         this%cols = that%cols
         this%n_step = that%n_step
         this%restart = that%restart
         this%simple = that%simple
       end subroutine

       subroutine delete_pr(this)
         implicit none
         type(probe),intent(inout) :: this
         call delete(this%dir)
         call delete(this%name)
         this%d = 0.0_cp
         this%d_data_dt = 0.0_cp
         this%d_amax = 0.0_cp
         this%t = 0.0_cp
         this%un = 0
         this%cols = 0
         this%n_step = 0
         this%restart = .false.
         this%simple = .false.
       end subroutine

       subroutine display_pr(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- probe'
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'd         = ',this%d
         write(un,*) 'd_data_dt = ',this%d_data_dt
         write(un,*) 'd_amax    = ',this%d_amax
         write(un,*) 't         = ',this%t
         write(un,*) 'un        = ',this%un
         write(un,*) 'cols      = ',this%cols
         write(un,*) 'n_step    = ',this%n_step
         write(un,*) 'restart   = ',this%restart
         write(un,*) 'simple    = ',this%simple
       end subroutine

       subroutine display_short_pr(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         write(un,*) 'd         = ',this%d
         write(un,*) 'd_data_dt = ',this%d_data_dt
         write(un,*) 'd_amax    = ',this%d_amax
         write(un,*) 't         = ',this%t
         write(un,*) 'un        = ',this%un
         write(un,*) 'cols      = ',this%cols
         write(un,*) 'n_step    = ',this%n_step
         write(un,*) 'restart   = ',this%restart
         write(un,*) 'simple    = ',this%simple
       end subroutine

       subroutine print_pr(this)
         implicit none
         type(probe),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_pr(this)
         implicit none
         type(probe),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_pr(this,un)
         implicit none
         type(probe),intent(in) :: this
         integer,intent(in) :: un
         call export(this%dir,un)
         call export(this%name,un)
         write(un,*) 'd          = ';write(un,*) this%d
         write(un,*) 'd_data_dt  = ';write(un,*) this%d_data_dt
         write(un,*) 'd_amax     = ';write(un,*) this%d_amax
         write(un,*) 't          = ';write(un,*) this%t
         write(un,*) 'un         = ';write(un,*) this%un
         write(un,*) 'cols       = ';write(un,*) this%cols
         write(un,*) 'n_step     = ';write(un,*) this%n_step
         write(un,*) 'restart    = ';write(un,*) this%restart
         write(un,*) 'simple     = ';write(un,*) this%simple
       end subroutine

       subroutine import_pr(this,un)
         implicit none
         type(probe),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%dir,un)
         call import(this%name,un)
         read(un,*); read(un,*) this%d
         read(un,*); read(un,*) this%d_data_dt
         read(un,*); read(un,*) this%d_amax
         read(un,*); read(un,*) this%t
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%cols
         read(un,*); read(un,*) this%n_step
         read(un,*); read(un,*) this%restart
         read(un,*); read(un,*) this%simple
       end subroutine

       subroutine display_wrap_pr(this,dir,name)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_pr(this,dir,name)
         implicit none
         type(probe),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_pr(this,dir,name)
         implicit none
         type(probe),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_pr(this)
         implicit none
         type(probe),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_pr(this)
         implicit none
         type(probe),intent(inout) :: this
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

       end module