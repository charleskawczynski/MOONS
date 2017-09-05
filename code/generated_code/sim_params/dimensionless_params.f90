       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module dimensionless_params_mod
       use current_precision_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: dimensionless_params
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_di;    end interface
       interface delete;       module procedure delete_di;       end interface
       interface display;      module procedure display_di;      end interface
       interface display_short;module procedure display_short_di;end interface
       interface display;      module procedure display_wrap_di; end interface
       interface print;        module procedure print_di;        end interface
       interface print_short;  module procedure print_short_di;  end interface
       interface export;       module procedure export_di;       end interface
       interface import;       module procedure import_di;       end interface
       interface export;       module procedure export_wrap_di;  end interface
       interface import;       module procedure import_wrap_di;  end interface
       interface export;       module procedure export_DN_di;    end interface
       interface import;       module procedure import_DN_di;    end interface

       type dimensionless_params
         real(cp) :: Re = 0.0_cp
         real(cp) :: Al = 0.0_cp
         real(cp) :: N = 0.0_cp
         real(cp) :: Ha = 0.0_cp
         real(cp) :: tau = 0.0_cp
         real(cp) :: Gr = 0.0_cp
         real(cp) :: Fr = 0.0_cp
         real(cp) :: Pr = 0.0_cp
         real(cp) :: Pe = 0.0_cp
         real(cp) :: Ec = 0.0_cp
         real(cp) :: Rem = 0.0_cp
         real(cp),dimension(6) :: c_w = 0.0_cp
         real(cp),dimension(6) :: Robin_coeff = 0.0_cp
         real(cp) :: Q = 0.0_cp
         real(cp) :: sig_local_over_sig_f = 0.0_cp
         real(cp) :: KE_scale = 0.0_cp
         real(cp) :: ME_scale = 0.0_cp
         real(cp) :: JE_scale = 0.0_cp
         real(cp) :: L_eta = 0.0_cp
         real(cp) :: U_eta = 0.0_cp
         real(cp) :: t_eta = 0.0_cp
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_di(this,that)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         type(dimensionless_params),intent(in) :: that
         call delete(this)
         this%Re = that%Re
         this%Al = that%Al
         this%N = that%N
         this%Ha = that%Ha
         this%tau = that%tau
         this%Gr = that%Gr
         this%Fr = that%Fr
         this%Pr = that%Pr
         this%Pe = that%Pe
         this%Ec = that%Ec
         this%Rem = that%Rem
         this%c_w = that%c_w
         this%Robin_coeff = that%Robin_coeff
         this%Q = that%Q
         this%sig_local_over_sig_f = that%sig_local_over_sig_f
         this%KE_scale = that%KE_scale
         this%ME_scale = that%ME_scale
         this%JE_scale = that%JE_scale
         this%L_eta = that%L_eta
         this%U_eta = that%U_eta
         this%t_eta = that%t_eta
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_di(this)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         this%Re = 0.0_cp
         this%Al = 0.0_cp
         this%N = 0.0_cp
         this%Ha = 0.0_cp
         this%tau = 0.0_cp
         this%Gr = 0.0_cp
         this%Fr = 0.0_cp
         this%Pr = 0.0_cp
         this%Pe = 0.0_cp
         this%Ec = 0.0_cp
         this%Rem = 0.0_cp
         this%c_w = 0.0_cp
         this%Robin_coeff = 0.0_cp
         this%Q = 0.0_cp
         this%sig_local_over_sig_f = 0.0_cp
         this%KE_scale = 0.0_cp
         this%ME_scale = 0.0_cp
         this%JE_scale = 0.0_cp
         this%L_eta = 0.0_cp
         this%U_eta = 0.0_cp
         this%t_eta = 0.0_cp
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_di(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- dimensionless_params'
         write(un,*) 'Re                   = ',this%Re
         write(un,*) 'Al                   = ',this%Al
         write(un,*) 'N                    = ',this%N
         write(un,*) 'Ha                   = ',this%Ha
         write(un,*) 'tau                  = ',this%tau
         write(un,*) 'Gr                   = ',this%Gr
         write(un,*) 'Fr                   = ',this%Fr
         write(un,*) 'Pr                   = ',this%Pr
         write(un,*) 'Pe                   = ',this%Pe
         write(un,*) 'Ec                   = ',this%Ec
         write(un,*) 'Rem                  = ',this%Rem
         write(un,*) 'c_w                  = ',this%c_w
         write(un,*) 'Robin_coeff          = ',this%Robin_coeff
         write(un,*) 'Q                    = ',this%Q
         write(un,*) 'sig_local_over_sig_f = ',this%sig_local_over_sig_f
         write(un,*) 'KE_scale             = ',this%KE_scale
         write(un,*) 'ME_scale             = ',this%ME_scale
         write(un,*) 'JE_scale             = ',this%JE_scale
         write(un,*) 'L_eta                = ',this%L_eta
         write(un,*) 'U_eta                = ',this%U_eta
         write(un,*) 't_eta                = ',this%t_eta
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_di(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'Re                   = ',this%Re
         write(un,*) 'Al                   = ',this%Al
         write(un,*) 'N                    = ',this%N
         write(un,*) 'Ha                   = ',this%Ha
         write(un,*) 'tau                  = ',this%tau
         write(un,*) 'Gr                   = ',this%Gr
         write(un,*) 'Fr                   = ',this%Fr
         write(un,*) 'Pr                   = ',this%Pr
         write(un,*) 'Pe                   = ',this%Pe
         write(un,*) 'Ec                   = ',this%Ec
         write(un,*) 'Rem                  = ',this%Rem
         write(un,*) 'c_w                  = ',this%c_w
         write(un,*) 'Robin_coeff          = ',this%Robin_coeff
         write(un,*) 'Q                    = ',this%Q
         write(un,*) 'sig_local_over_sig_f = ',this%sig_local_over_sig_f
         write(un,*) 'KE_scale             = ',this%KE_scale
         write(un,*) 'ME_scale             = ',this%ME_scale
         write(un,*) 'JE_scale             = ',this%JE_scale
         write(un,*) 'L_eta                = ',this%L_eta
         write(un,*) 'U_eta                = ',this%U_eta
         write(un,*) 't_eta                = ',this%t_eta
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_di(this)
         implicit none
         type(dimensionless_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_di(this)
         implicit none
         type(dimensionless_params),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_di(this,un)
         implicit none
         type(dimensionless_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'Re                    = ';write(un,*) this%Re
         write(un,*) 'Al                    = ';write(un,*) this%Al
         write(un,*) 'N                     = ';write(un,*) this%N
         write(un,*) 'Ha                    = ';write(un,*) this%Ha
         write(un,*) 'tau                   = ';write(un,*) this%tau
         write(un,*) 'Gr                    = ';write(un,*) this%Gr
         write(un,*) 'Fr                    = ';write(un,*) this%Fr
         write(un,*) 'Pr                    = ';write(un,*) this%Pr
         write(un,*) 'Pe                    = ';write(un,*) this%Pe
         write(un,*) 'Ec                    = ';write(un,*) this%Ec
         write(un,*) 'Rem                   = ';write(un,*) this%Rem
         write(un,*) 'c_w                   = ';write(un,*) this%c_w
         write(un,*) 'Robin_coeff           = ';write(un,*) this%Robin_coeff
         write(un,*) 'Q                     = ';write(un,*) this%Q
         write(un,*) 'sig_local_over_sig_f  = ';write(un,*) this%sig_local_over_sig_f
         write(un,*) 'KE_scale              = ';write(un,*) this%KE_scale
         write(un,*) 'ME_scale              = ';write(un,*) this%ME_scale
         write(un,*) 'JE_scale              = ';write(un,*) this%JE_scale
         write(un,*) 'L_eta                 = ';write(un,*) this%L_eta
         write(un,*) 'U_eta                 = ';write(un,*) this%U_eta
         write(un,*) 't_eta                 = ';write(un,*) this%t_eta
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_di(this,un)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%Re
         read(un,*); read(un,*) this%Al
         read(un,*); read(un,*) this%N
         read(un,*); read(un,*) this%Ha
         read(un,*); read(un,*) this%tau
         read(un,*); read(un,*) this%Gr
         read(un,*); read(un,*) this%Fr
         read(un,*); read(un,*) this%Pr
         read(un,*); read(un,*) this%Pe
         read(un,*); read(un,*) this%Ec
         read(un,*); read(un,*) this%Rem
         read(un,*); read(un,*) this%c_w
         read(un,*); read(un,*) this%Robin_coeff
         read(un,*); read(un,*) this%Q
         read(un,*); read(un,*) this%sig_local_over_sig_f
         read(un,*); read(un,*) this%KE_scale
         read(un,*); read(un,*) this%ME_scale
         read(un,*); read(un,*) this%JE_scale
         read(un,*); read(un,*) this%L_eta
         read(un,*); read(un,*) this%U_eta
         read(un,*); read(un,*) this%t_eta
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrap_di(this,dir,name)
         implicit none
         type(dimensionless_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_di(this,dir,name)
         implicit none
         type(dimensionless_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_di(this,dir,name)
         implicit none
         type(dimensionless_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_di(this)
         implicit none
         type(dimensionless_params),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_di(this)
         implicit none
         type(dimensionless_params),intent(inout) :: this
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