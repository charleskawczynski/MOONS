       module dimensionless_params_mod
       use current_precision_mod
       implicit none
       private
       public :: dimensionless_params
       public :: init,delete,display,print,export,import

       interface init;    module procedure init_copy_DP; end interface
       interface delete;  module procedure delete_DP;    end interface
       interface display; module procedure display_DP;   end interface
       interface print;   module procedure print_DP;     end interface
       interface export;  module procedure export_DP;    end interface
       interface import;  module procedure import_DP;    end interface

       type dimensionless_params
         real(cp) :: Re = 0.0_cp
         real(cp) :: Al = 0.0_cp
         real(cp) :: N  = 0.0_cp
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
       end type

       contains

       subroutine init_copy_DP(DP,DP_in)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         type(dimensionless_params),intent(in) :: DP_in
         DP%Re         = DP_in%Re
         DP%Al         = DP_in%Al
         DP%N          = DP_in%N
         DP%Ha         = DP_in%Ha
         DP%tau        = DP_in%tau
         DP%Rem        = DP_in%Rem
         DP%c_w        = DP_in%c_w
         DP%Robin_coeff= DP_in%Robin_coeff
         DP%Q          = DP_in%Q
         DP%sig_local_over_sig_f = DP_in%sig_local_over_sig_f
         DP%Gr         = DP_in%Gr
         DP%Pr         = DP_in%Pr
         DP%Pe         = DP_in%Pe
         DP%Fr         = DP_in%Fr
         DP%Ec         = DP_in%Ec
         DP%KE_scale   = DP_in%KE_scale
         DP%ME_scale   = DP_in%ME_scale
         DP%JE_scale   = DP_in%JE_scale
         DP%L_eta      = DP_in%L_eta
         DP%U_eta      = DP_in%U_eta
         DP%t_eta      = DP_in%t_eta
       end subroutine

       subroutine delete_DP(DP)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         DP%Re         = 0.0_cp
         DP%Al         = 0.0_cp
         DP%N          = 0.0_cp
         DP%Ha         = 0.0_cp
         DP%tau        = 0.0_cp
         DP%Rem        = 0.0_cp
         DP%c_w        = 0.0_cp
         DP%Robin_coeff= 0.0_cp
         DP%Q          = 0.0_cp
         DP%sig_local_over_sig_f = 0.0_cp
         DP%Gr         = 0.0_cp
         DP%Pr         = 0.0_cp
         DP%Pe         = 0.0_cp
         DP%Fr         = 0.0_cp
         DP%Ec         = 0.0_cp
         DP%KE_scale   = 0.0_cp
         DP%ME_scale   = 0.0_cp
         DP%JE_scale   = 0.0_cp
         DP%L_eta      = 0.0_cp
         DP%U_eta      = 0.0_cp
         DP%t_eta      = 0.0_cp
       end subroutine

       subroutine display_DP(DP,un)
         implicit none
         type(dimensionless_params),intent(in) :: DP
         integer,intent(in) :: un
         write(un,*) 'Re                   = ',DP%Re
         write(un,*) 'Al                   = ',DP%Al
         write(un,*) 'N                    = ',DP%N
         write(un,*) 'Ha                   = ',DP%Ha
         write(un,*) 'tau                  = ',DP%tau
         write(un,*) 'Rem                  = ',DP%Rem
         write(un,*) 'c_w                  = ',DP%c_w
         write(un,*) 'Robin_coeff          = ',DP%Robin_coeff
         write(un,*) 'Q                    = ',DP%Q
         write(un,*) 'sig_local_over_sig_f = ',DP%sig_local_over_sig_f
         write(un,*) 'Gr                   = ',DP%Gr
         write(un,*) 'Pr                   = ',DP%Pr
         write(un,*) 'Pe                   = ',DP%Pe
         write(un,*) 'Fr                   = ',DP%Fr
         write(un,*) 'Ec                   = ',DP%Ec
         write(un,*) 'KE_scale             = ',DP%KE_scale
         write(un,*) 'ME_scale             = ',DP%ME_scale
         write(un,*) 'JE_scale             = ',DP%JE_scale
         write(un,*) 'L_eta                = ',DP%L_eta
         write(un,*) 'U_eta                = ',DP%U_eta
         write(un,*) 't_eta                = ',DP%t_eta
       end subroutine

       subroutine print_DP(DP)
         implicit none
         type(dimensionless_params),intent(in) :: DP
         call display(DP,6)
       end subroutine

       subroutine export_DP(DP,un)
         implicit none
         type(dimensionless_params),intent(in) :: DP
         integer,intent(in) :: un
         write(un,*) 'Re                   = '; write(un,*) DP%Re
         write(un,*) 'Al                   = '; write(un,*) DP%Al
         write(un,*) 'N                    = '; write(un,*) DP%N
         write(un,*) 'Ha                   = '; write(un,*) DP%Ha
         write(un,*) 'tau                  = '; write(un,*) DP%tau
         write(un,*) 'Rem                  = '; write(un,*) DP%Rem
         write(un,*) 'c_w                  = '; write(un,*) DP%c_w
         write(un,*) 'Robin_coeff          = '; write(un,*) DP%Robin_coeff
         write(un,*) 'Q                    = '; write(un,*) DP%Q
         write(un,*) 'sig_local_over_sig_f = '; write(un,*) DP%sig_local_over_sig_f
         write(un,*) 'Gr                   = '; write(un,*) DP%Gr
         write(un,*) 'Pr                   = '; write(un,*) DP%Pr
         write(un,*) 'Pe                   = '; write(un,*) DP%Pe
         write(un,*) 'Fr                   = '; write(un,*) DP%Fr
         write(un,*) 'Ec                   = '; write(un,*) DP%Ec
         write(un,*) 'KE_scale             = '; write(un,*) DP%KE_scale
         write(un,*) 'ME_scale             = '; write(un,*) DP%ME_scale
         write(un,*) 'JE_scale             = '; write(un,*) DP%JE_scale
         write(un,*) 'L_eta                = '; write(un,*) DP%L_eta
         write(un,*) 'U_eta                = '; write(un,*) DP%U_eta
         write(un,*) 't_eta                = '; write(un,*) DP%t_eta
       end subroutine

       subroutine import_DP(DP,un)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         integer,intent(in) :: un
         read(un,*); read(un,*) DP%Re
         read(un,*); read(un,*) DP%Al
         read(un,*); read(un,*) DP%N
         read(un,*); read(un,*) DP%Ha
         read(un,*); read(un,*) DP%tau
         read(un,*); read(un,*) DP%Rem
         read(un,*); read(un,*) DP%c_w
         read(un,*); read(un,*) DP%Robin_coeff
         read(un,*); read(un,*) DP%Q
         read(un,*); read(un,*) DP%sig_local_over_sig_f
         read(un,*); read(un,*) DP%Gr
         read(un,*); read(un,*) DP%Pr
         read(un,*); read(un,*) DP%Pe
         read(un,*); read(un,*) DP%Fr
         read(un,*); read(un,*) DP%Ec
         read(un,*); read(un,*) DP%KE_scale
         read(un,*); read(un,*) DP%ME_scale
         read(un,*); read(un,*) DP%JE_scale
         read(un,*); read(un,*) DP%L_eta
         read(un,*); read(un,*) DP%U_eta
         read(un,*); read(un,*) DP%t_eta
       end subroutine

       end module