       module norms_extend_mod
       use norms_mod
       ! Computes norms
       !     L(1)
       !     L(2)
       !     L(∞)
       !
       ! Interfaces:
       ! compute(e,u,vol)  = L(n) = volume⁻¹ ( ∫∫∫ | u(i,j,k)ⁿ | dx dy dz )ᵝ, β=1/n
       ! compute(e,u)      = L(n) = ( ΣΣΣ | u(i,j,k)ⁿ | )ᵝ, β=1/n
       ! compute(e,u)      = L(∞) = max(abs(u))
       use current_precision_mod
       use IO_tools_mod
       use grid_mod
       use mesh_extend_mod
       use ops_norms_mod
       use GF_mod
       use SF_extend_mod
       use VF_extend_mod
       implicit none

       private
       public :: norms
       public :: init,delete,display,print,export,import ! Essentials
       public :: compute

       interface init;            module procedure init_norms;             end interface
       interface display;         module procedure display_norms2;         end interface
       interface display;         module procedure display_norms1_name;    end interface
       interface display;         module procedure display_norms2_name;    end interface
       interface print;           module procedure print_norms2;           end interface
       interface print;           module procedure print_norms1_name;      end interface
       interface print;           module procedure print_norms2_name;      end interface

       interface compute;         module procedure compute_norms_vol_SF;   end interface
       interface compute;         module procedure compute_norms_SF;       end interface
       interface compute;         module procedure compute_norms_vol_VF;   end interface
       interface compute;         module procedure compute_norms_VF;       end interface

       contains

       ! **************************************************************
       ! ************************** ESSENTIALS ************************
       ! **************************************************************

       subroutine init_norms(e)
         implicit none
         type(norms),intent(inout) :: e
         e%L1 = 0.0_cp
         e%L2 = 0.0_cp
         e%Linf = 0.0_cp
       end subroutine

       subroutine display_norms1_name(norm,un,name)
         implicit none
         type(norms),intent(in) :: norm
         integer,intent(in) :: un
         character(len=*),intent(in) :: name
         write(un,*) ' -------------- '//name//' -------------- '
         write(un,*) 'L1 = ',norm%L1
         write(un,*) 'L2 = ',norm%L2
         write(un,*) 'Linf = ',norm%Linf
       end subroutine

       subroutine display_norms2_name(n1,n2,un,name)
         implicit none
         type(norms),intent(in) :: n1,n2
         integer,intent(in) :: un
         character(len=*),intent(in) :: name
         write(un,*) ' -------------- '//name//' -------------- '
         write(un,*) 'L1 = ',n1%L1,n2%L1
         write(un,*) 'L2 = ',n1%L2,n2%L2
         write(un,*) 'Linf = ',n1%Linf,n2%Linf
       end subroutine

       subroutine display_norms2(n1,n2,un)
         implicit none
         type(norms),intent(in) :: n1,n2
         integer,intent(in) :: un
         write(un,*) 'L1 = ',n1%L1,n2%L1
         write(un,*) 'L2 = ',n1%L2,n2%L2
         write(un,*) 'Linf = ',n1%Linf,n2%Linf
       end subroutine

       subroutine print_norms1_name(norm,name)
         implicit none
         type(norms),intent(in) :: norm
         character(len=*),intent(in) :: name
         call display(norm,6,name)
       end subroutine

       subroutine print_norms2_name(n1,n2,name)
         implicit none
         type(norms),intent(in) :: n1,n2
         character(len=*),intent(in) :: name
         call display(n1,n2,6,name)
       end subroutine

       subroutine print_norms2(n1,n2)
         implicit none
         type(norms),intent(in) :: n1,n2
         call display(n1,n2,6)
       end subroutine

       ! **************************************************************
       ! ************************ COMPUTATIONS ************************
       ! **************************************************************

       subroutine compute_norms_vol_SF(e,u,vol,tot_vol)
         implicit none
         type(norms),intent(inout) :: e
         type(SF),intent(in) :: u,vol
         real(cp),intent(in) :: tot_vol
         call compute_Ln(e%L1,u,1.0_cp,vol); e%L1 = e%L1/tot_vol
         call compute_Ln(e%L2,u,2.0_cp,vol); e%L2 = (e%L2**0.5_cp)/tot_vol
         e%Linf = amax(u)
       end subroutine

       subroutine compute_norms_vol_VF(e,u,vol,tot_vol)
         implicit none
         type(norms),intent(inout) :: e
         type(VF),intent(in) :: u,vol
         real(cp),intent(in) :: tot_vol
         call compute_Ln(e%L1,u,1.0_cp,vol)
         call compute_Ln(e%L2,u,2.0_cp,vol)
         e%L1 = e%L1/tot_vol
         e%L2 = (e%L2**0.5_cp)/tot_vol
         e%Linf = amax(u)
       end subroutine

       subroutine compute_norms_SF(e,u)
         implicit none
         type(norms),intent(inout) :: e
         type(SF),intent(in) :: u
         call compute_Ln(e%L1,u,1.0_cp)
         call compute_Ln(e%L2,u,2.0_cp)
         e%L2 = e%L2**0.5_cp
         e%Linf = amax(u)
       end subroutine

       subroutine compute_norms_VF(e,u)
         implicit none
         type(norms),intent(inout) :: e
         type(VF),intent(in) :: u
         call compute_Ln(e%L1,u,1.0_cp)
         call compute_Ln(e%L2,u,2.0_cp)
         e%L2 = e%L2**0.5_cp
         e%Linf = amax(u)
       end subroutine

       end module