       module dimensionless_params_mod
       use current_precision_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: dimensionless_params
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_dimensionless_params;          end interface
       interface delete; module procedure delete_dimensionless_params;        end interface
       interface display;module procedure display_dimensionless_params;       end interface
       interface print;  module procedure print_dimensionless_params;         end interface
       interface export; module procedure export_dimensionless_params;        end interface
       interface import; module procedure import_dimensionless_params;        end interface
       interface export; module procedure export_wrapper_dimensionless_params;end interface
       interface import; module procedure import_wrapper_dimensionless_params;end interface

       type dimensionless_params
         real(cp) :: re = 0.0_cp
         real(cp) :: al = 0.0_cp
         real(cp) :: n = 0.0_cp
         real(cp) :: ha = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: gr = 0.0_cp
         real(cp) :: fr = 0.0_cp
         real(cp) :: pr = 0.0_cp
         real(cp) :: pe = 0.0_cp
         real(cp) :: ec = 0.0_cp
         real(cp) :: rem = 0.0_cp
         real(cp),dimension(6) :: c_w = 0.0_cp
         real(cp),dimension(6) :: robin_coeff = 0.0_cp
         real(cp) :: q = 0.0_cp
         real(cp) :: sig_local_over_sig_f = 0.0_cp
         real(cp) :: ke_scale = 0.0_cp
         real(cp) :: me_scale = 0.0_cp
         real(cp) :: je_scale = 0.0_cp
         real(cp) :: l_eta = 0.0_cp
         real(cp) :: u_eta = 0.0_cp
         real(cp) :: t_eta = 0.0_cp
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_dimensionless_params(this,that)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         type(dimensionless_params),intent(in) :: that
         call delete(this)
         this%re = that%re
         this%al = that%al
         this%n = that%n
         this%ha = that%ha
         this%tau = that%tau
         this%gr = that%gr
         this%fr = that%fr
         this%pr = that%pr
         this%pe = that%pe
         this%ec = that%ec
         this%rem = that%rem
         this%c_w = that%c_w
         this%robin_coeff = that%robin_coeff
         this%q = that%q
         this%sig_local_over_sig_f = that%sig_local_over_sig_f
         this%ke_scale = that%ke_scale
         this%me_scale = that%me_scale
         this%je_scale = that%je_scale
         this%l_eta = that%l_eta
         this%u_eta = that%u_eta
         this%t_eta = that%t_eta
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_dimensionless_params(this)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         this%re = 0.0_cp
         this%al = 0.0_cp
         this%n = 0.0_cp
         this%ha = 0.0_cp
         this%tau = 0.0_cp
         this%gr = 0.0_cp
         this%fr = 0.0_cp
         this%pr = 0.0_cp
         this%pe = 0.0_cp
         this%ec = 0.0_cp
         this%rem = 0.0_cp
         this%c_w = 0.0_cp
         this%robin_coeff = 0.0_cp
         this%q = 0.0_cp
         this%sig_local_over_sig_f = 0.0_cp
         this%ke_scale = 0.0_cp
         this%me_scale = 0.0_cp
         this%je_scale = 0.0_cp
         this%l_eta = 0.0_cp
         this%u_eta = 0.0_cp
         this%t_eta = 0.0_cp
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_dimensionless_params(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- dimensionless_params'
         write(un,*) 're                   = ',this%re
         write(un,*) 'al                   = ',this%al
         write(un,*) 'n                    = ',this%n
         write(un,*) 'ha                   = ',this%ha
         write(un,*) 'tau                  = ',this%tau
         write(un,*) 'gr                   = ',this%gr
         write(un,*) 'fr                   = ',this%fr
         write(un,*) 'pr                   = ',this%pr
         write(un,*) 'pe                   = ',this%pe
         write(un,*) 'ec                   = ',this%ec
         write(un,*) 'rem                  = ',this%rem
         write(un,*) 'c_w                  = ',this%c_w
         write(un,*) 'robin_coeff          = ',this%robin_coeff
         write(un,*) 'q                    = ',this%q
         write(un,*) 'sig_local_over_sig_f = ',this%sig_local_over_sig_f
         write(un,*) 'ke_scale             = ',this%ke_scale
         write(un,*) 'me_scale             = ',this%me_scale
         write(un,*) 'je_scale             = ',this%je_scale
         write(un,*) 'l_eta                = ',this%l_eta
         write(un,*) 'u_eta                = ',this%u_eta
         write(un,*) 't_eta                = ',this%t_eta
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_dimensionless_params(this)
         implicit none
         type(dimensionless_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_dimensionless_params(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 're                    = ';write(un,*) this%re
         write(un,*) 'al                    = ';write(un,*) this%al
         write(un,*) 'n                     = ';write(un,*) this%n
         write(un,*) 'ha                    = ';write(un,*) this%ha
         write(un,*) 'tau                   = ';write(un,*) this%tau
         write(un,*) 'gr                    = ';write(un,*) this%gr
         write(un,*) 'fr                    = ';write(un,*) this%fr
         write(un,*) 'pr                    = ';write(un,*) this%pr
         write(un,*) 'pe                    = ';write(un,*) this%pe
         write(un,*) 'ec                    = ';write(un,*) this%ec
         write(un,*) 'rem                   = ';write(un,*) this%rem
         write(un,*) 'c_w                   = ';write(un,*) this%c_w
         write(un,*) 'robin_coeff           = ';write(un,*) this%robin_coeff
         write(un,*) 'q                     = ';write(un,*) this%q
         write(un,*) 'sig_local_over_sig_f  = ';write(un,*) this%sig_local_over_sig_f
         write(un,*) 'ke_scale              = ';write(un,*) this%ke_scale
         write(un,*) 'me_scale              = ';write(un,*) this%me_scale
         write(un,*) 'je_scale              = ';write(un,*) this%je_scale
         write(un,*) 'l_eta                 = ';write(un,*) this%l_eta
         write(un,*) 'u_eta                 = ';write(un,*) this%u_eta
         write(un,*) 't_eta                 = ';write(un,*) this%t_eta
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_dimensionless_params(this,un)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%re
         read(un,*); read(un,*) this%al
         read(un,*); read(un,*) this%n
         read(un,*); read(un,*) this%ha
         read(un,*); read(un,*) this%tau
         read(un,*); read(un,*) this%gr
         read(un,*); read(un,*) this%fr
         read(un,*); read(un,*) this%pr
         read(un,*); read(un,*) this%pe
         read(un,*); read(un,*) this%ec
         read(un,*); read(un,*) this%rem
         read(un,*); read(un,*) this%c_w
         read(un,*); read(un,*) this%robin_coeff
         read(un,*); read(un,*) this%q
         read(un,*); read(un,*) this%sig_local_over_sig_f
         read(un,*); read(un,*) this%ke_scale
         read(un,*); read(un,*) this%me_scale
         read(un,*); read(un,*) this%je_scale
         read(un,*); read(un,*) this%l_eta
         read(un,*); read(un,*) this%u_eta
         read(un,*); read(un,*) this%t_eta
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_wrapper_dimensionless_params(this,dir,name)
         implicit none
         type(dimensionless_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_dimensionless_params(this,dir,name)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module