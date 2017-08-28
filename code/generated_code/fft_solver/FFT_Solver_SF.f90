       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module fft_solver_sf_mod
       use current_precision_mod
       use IO_tools_mod
       use SF_mod
       implicit none

       private
       public :: fft_solver_sf
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_fft_solver_sf;           end interface
       interface delete; module procedure delete_fft_solver_sf;         end interface
       interface display;module procedure display_fft_solver_sf;        end interface
       interface display;module procedure display_wrapper_fft_solver_sf;end interface
       interface print;  module procedure print_fft_solver_sf;          end interface
       interface export; module procedure export_fft_solver_sf;         end interface
       interface import; module procedure import_fft_solver_sf;         end interface
       interface export; module procedure export_wrapper_fft_solver_sf; end interface
       interface import; module procedure import_wrapper_fft_solver_sf; end interface

       type fft_solver_sf
         type(sf) :: f
         type(sf) :: res
         type(sf) :: coeff_x
         type(sf) :: coeff_y
         type(sf) :: coeff_z
         integer,dimension(3) :: s = 0
         real(cp),dimension(3) :: dh2 = 0.0_cp
         integer :: nx = 0
         integer :: ny = 0
         integer :: nz = 0
       end type

       contains

       subroutine init_fft_solver_sf(this,that)
         implicit none
         type(fft_solver_sf),intent(inout) :: this
         type(fft_solver_sf),intent(in) :: that
         call delete(this)
         call init(this%f,that%f)
         call init(this%res,that%res)
         call init(this%coeff_x,that%coeff_x)
         call init(this%coeff_y,that%coeff_y)
         call init(this%coeff_z,that%coeff_z)
         this%s = that%s
         this%dh2 = that%dh2
         this%nx = that%nx
         this%ny = that%ny
         this%nz = that%nz
       end subroutine

       subroutine delete_fft_solver_sf(this)
         implicit none
         type(fft_solver_sf),intent(inout) :: this
         call delete(this%f)
         call delete(this%res)
         call delete(this%coeff_x)
         call delete(this%coeff_y)
         call delete(this%coeff_z)
         this%s = 0
         this%dh2 = 0.0_cp
         this%nx = 0
         this%ny = 0
         this%nz = 0
       end subroutine

       subroutine display_fft_solver_sf(this,un)
         implicit none
         type(fft_solver_sf),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- fft_solver_sf'
         call display(this%f,un)
         call display(this%res,un)
         call display(this%coeff_x,un)
         call display(this%coeff_y,un)
         call display(this%coeff_z,un)
         write(un,*) 's       = ',this%s
         write(un,*) 'dh2     = ',this%dh2
         write(un,*) 'nx      = ',this%nx
         write(un,*) 'ny      = ',this%ny
         write(un,*) 'nz      = ',this%nz
       end subroutine

       subroutine print_fft_solver_sf(this)
         implicit none
         type(fft_solver_sf),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_fft_solver_sf(this,un)
         implicit none
         type(fft_solver_sf),intent(in) :: this
         integer,intent(in) :: un
         call export(this%f,un)
         call export(this%res,un)
         call export(this%coeff_x,un)
         call export(this%coeff_y,un)
         call export(this%coeff_z,un)
         write(un,*) 's        = ';write(un,*) this%s
         write(un,*) 'dh2      = ';write(un,*) this%dh2
         write(un,*) 'nx       = ';write(un,*) this%nx
         write(un,*) 'ny       = ';write(un,*) this%ny
         write(un,*) 'nz       = ';write(un,*) this%nz
       end subroutine

       subroutine import_fft_solver_sf(this,un)
         implicit none
         type(fft_solver_sf),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%f,un)
         call import(this%res,un)
         call import(this%coeff_x,un)
         call import(this%coeff_y,un)
         call import(this%coeff_z,un)
         read(un,*); read(un,*) this%s
         read(un,*); read(un,*) this%dh2
         read(un,*); read(un,*) this%nx
         read(un,*); read(un,*) this%ny
         read(un,*); read(un,*) this%nz
       end subroutine

       subroutine display_wrapper_fft_solver_sf(this,dir,name)
         implicit none
         type(fft_solver_sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_fft_solver_sf(this,dir,name)
         implicit none
         type(fft_solver_sf),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_fft_solver_sf(this,dir,name)
         implicit none
         type(fft_solver_sf),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module