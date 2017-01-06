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
         real(cp) :: Ha = 0.0_cp
         real(cp) :: Gr = 0.0_cp
         real(cp) :: Fr = 0.0_cp
         real(cp) :: Pr = 0.0_cp
         real(cp) :: Ec = 0.0_cp
         real(cp) :: Rem = 0.0_cp
         real(cp) :: cw = 0.0_cp
         real(cp) :: sig_local_over_sig_f = 0.0_cp
       end type

       contains

       subroutine init_copy_DP(DP,DP_in)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         type(dimensionless_params),intent(in) :: DP_in
         DP%Re         = DP_in%Re
         DP%Ha         = DP_in%Ha
         DP%Rem        = DP_in%Rem
         DP%cw         = DP_in%cw
         DP%sig_local_over_sig_f = DP_in%sig_local_over_sig_f
         DP%Gr         = DP_in%Gr
         DP%Pr         = DP_in%Pr
         DP%Fr         = DP_in%Fr
         DP%Ec         = DP_in%Ec
       end subroutine

       subroutine delete_DP(DP)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         DP%Re         = 0.0_cp
         DP%Ha         = 0.0_cp
         DP%Rem        = 0.0_cp
         DP%cw         = 0.0_cp
         DP%sig_local_over_sig_f = 0.0_cp
         DP%Gr         = 0.0_cp
         DP%Pr         = 0.0_cp
         DP%Fr         = 0.0_cp
         DP%Ec         = 0.0_cp
       end subroutine

       subroutine display_DP(DP,un)
         implicit none
         type(dimensionless_params),intent(in) :: DP
         integer,intent(in) :: un
         write(un,*) 'Re = ',DP%Re
         write(un,*) 'Ha = ',DP%Ha
         write(un,*) 'Rem = ',DP%Rem
         write(un,*) 'cw = ',DP%cw
         write(un,*) 'sig_local_over_sig_f = ',DP%sig_local_over_sig_f
         write(un,*) 'Gr = ',DP%Gr
         write(un,*) 'Pr = ',DP%Pr
         write(un,*) 'Fr = ',DP%Fr
         write(un,*) 'Ec = ',DP%Ec
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
         write(un,*) DP%Re
         write(un,*) DP%Ha
         write(un,*) DP%Rem
         write(un,*) DP%cw
         write(un,*) DP%sig_local_over_sig_f
         write(un,*) DP%Gr
         write(un,*) DP%Pr
         write(un,*) DP%Fr
         write(un,*) DP%Ec
       end subroutine

       subroutine import_DP(DP,un)
         implicit none
         type(dimensionless_params),intent(inout) :: DP
         integer,intent(in) :: un
         read(un,*) DP%Re
         read(un,*) DP%Ha
         read(un,*) DP%Rem
         read(un,*) DP%cw
         read(un,*) DP%sig_local_over_sig_f
         read(un,*) DP%Gr
         read(un,*) DP%Pr
         read(un,*) DP%Fr
         read(un,*) DP%Ec
       end subroutine

       end module