       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_now_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use step_mod
       use string_mod
       implicit none

       private
       public :: export_now
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_now;        end interface
       interface delete;           module procedure delete_export_now;           end interface
       interface display;          module procedure display_export_now;          end interface
       interface display_short;    module procedure display_short_export_now;    end interface
       interface display;          module procedure display_wrap_export_now;     end interface
       interface print;            module procedure print_export_now;            end interface
       interface print_short;      module procedure print_short_export_now;      end interface
       interface export;           module procedure export_export_now;           end interface
       interface export_primitives;module procedure export_primitives_export_now;end interface
       interface export_restart;   module procedure export_restart_export_now;   end interface
       interface import;           module procedure import_export_now;           end interface
       interface import_restart;   module procedure import_restart_export_now;   end interface
       interface import_primitives;module procedure import_primitives_export_now;end interface
       interface export;           module procedure export_wrap_export_now;      end interface
       interface import;           module procedure import_wrap_export_now;      end interface
       interface make_restart_dir; module procedure make_restart_dir_export_now; end interface
       interface suppress_warnings;module procedure suppress_warnings_export_now;end interface
       interface export;           module procedure export_DN_export_now;        end interface
       interface import;           module procedure import_DN_export_now;        end interface

       type export_now
         type(step) :: U
         type(step) :: B
         type(step) :: T
         type(step) :: rho
         type(step) :: all
         logical :: any_next = .false.
         logical :: any_now = .false.
         integer :: un = 0
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_export_now(this,that)
         implicit none
         type(export_now),intent(inout) :: this
         type(export_now),intent(in) :: that
         call delete(this)
         call init(this%U,that%U)
         call init(this%B,that%B)
         call init(this%T,that%T)
         call init(this%rho,that%rho)
         call init(this%all,that%all)
         this%any_next = that%any_next
         this%any_now = that%any_now
         this%un = that%un
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_export_now(this)
         implicit none
         type(export_now),intent(inout) :: this
         call delete(this%U)
         call delete(this%B)
         call delete(this%T)
         call delete(this%rho)
         call delete(this%all)
         this%any_next = .false.
         this%any_now = .false.
         this%un = 0
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_export_now(this,un)
         implicit none
         type(export_now),intent(in) :: this
         integer,intent(in) :: un
         call display(this%U,un)
         call display(this%B,un)
         call display(this%T,un)
         call display(this%rho,un)
         call display(this%all,un)
         write(un,*) 'any_next = ',this%any_next
         write(un,*) 'any_now  = ',this%any_now
         write(un,*) 'un       = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_export_now(this,un)
         implicit none
         type(export_now),intent(in) :: this
         integer,intent(in) :: un
         call display(this%U,un)
         call display(this%B,un)
         call display(this%T,un)
         call display(this%rho,un)
         call display(this%all,un)
         write(un,*) 'any_next = ',this%any_next
         write(un,*) 'any_now  = ',this%any_now
         write(un,*) 'un       = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_export_now(this,dir,name)
         implicit none
         type(export_now),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_now(this)
         implicit none
         type(export_now),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_now(this)
         implicit none
         type(export_now),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_export_now(this,un)
         implicit none
         type(export_now),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'any_next  = ';write(un,*) this%any_next
         write(un,*) 'any_now   = ';write(un,*) this%any_now
         write(un,*) 'un        = ';write(un,*) this%un
       end subroutine

       subroutine export_export_now(this,un)
         implicit none
         type(export_now),intent(in) :: this
         integer,intent(in) :: un
         call export(this%U,un)
         call export(this%B,un)
         call export(this%T,un)
         call export(this%rho,un)
         call export(this%all,un)
         write(un,*) 'any_next  = ';write(un,*) this%any_next
         write(un,*) 'any_now   = ';write(un,*) this%any_now
         write(un,*) 'un        = ';write(un,*) this%un
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_primitives_export_now(this,un)
         implicit none
         type(export_now),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%any_next
         read(un,*); read(un,*) this%any_now
         read(un,*); read(un,*) this%un
       end subroutine

       subroutine import_export_now(this,un)
         implicit none
         type(export_now),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%U,un)
         call import(this%B,un)
         call import(this%T,un)
         call import(this%rho,un)
         call import(this%all,un)
         read(un,*); read(un,*) this%any_next
         read(un,*); read(un,*) this%any_now
         read(un,*); read(un,*) this%un
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_wrap_export_now(this,dir,name)
         implicit none
         type(export_now),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_now(this,dir,name)
         implicit none
         type(export_now),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_export_now(this)
         implicit none
         type(export_now),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_export_now(this)
         implicit none
         type(export_now),intent(inout) :: this
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

       subroutine make_restart_dir_export_now(this,dir)
         implicit none
         type(export_now),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%U,dir//fortran_PS//'U')
         call make_restart_dir(this%B,dir//fortran_PS//'B')
         call make_restart_dir(this%T,dir//fortran_PS//'T')
         call make_restart_dir(this%rho,dir//fortran_PS//'rho')
         call make_restart_dir(this%all,dir//fortran_PS//'all')
       end subroutine

       subroutine export_restart_export_now(this,dir)
         implicit none
         type(export_now),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%U,dir//fortran_PS//'U')
         call export_restart(this%B,dir//fortran_PS//'B')
         call export_restart(this%T,dir//fortran_PS//'T')
         call export_restart(this%rho,dir//fortran_PS//'rho')
         call export_restart(this%all,dir//fortran_PS//'all')
       end subroutine

       subroutine import_restart_export_now(this,dir)
         implicit none
         type(export_now),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%U,dir//fortran_PS//'U')
         call import_restart(this%B,dir//fortran_PS//'B')
         call import_restart(this%T,dir//fortran_PS//'T')
         call import_restart(this%rho,dir//fortran_PS//'rho')
         call import_restart(this%all,dir//fortran_PS//'all')
       end subroutine

       subroutine suppress_warnings_export_now(this)
         implicit none
         type(export_now),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module