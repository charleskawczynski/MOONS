       module norms_mod
       ! Computes norms
       !     L(1)
       !     L(2)
       !     L(∞)
       ! 
       ! Interfaces:
       ! compute(e,u,vol)  = L(n) = volume⁻¹ ( ∫∫∫ | u(i,j,k)ⁿ | dx dy dz )ᵝ, β=1/n
       ! compute(e,u)      = L(n) = ( ΣΣΣ | u(i,j,k)ⁿ | )ᵝ, β=1/n
       ! compute(e,u)      = L(∞) = max(abs(u))

       use IO_tools_mod
       use grid_mod
       use mesh_mod
       use ops_norms_mod
       use RF_mod
       use SF_mod
       use VF_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: norms,init
       public :: print,export
       public :: compute

       type norms
         real(cp) :: L1,L2,Linf
       end type

       interface init;            module procedure init_norms;             end interface
       interface init;            module procedure print_copy;             end interface
       interface export;          module procedure export_norms;           end interface
       interface export;          module procedure export_norms_dir;       end interface
       interface print;           module procedure print_norms;            end interface

       interface compute;         module procedure compute_norms_vol_SF;   end interface
       interface compute;         module procedure compute_norms_SF;       end interface
       interface compute;         module procedure compute_norms_vol_VF;   end interface
       interface compute;         module procedure compute_norms_VF;       end interface

       contains

       ! **************************************************************
       ! **************************** INIT ****************************
       ! **************************************************************

       subroutine init_norms(e)
         implicit none
         type(norms),intent(inout) :: e
         e%L1 = 0.0_cp; e%L2 = 0.0_cp; e%Linf = 0.0_cp
       end subroutine

       subroutine print_copy(eCopy,e)
         implicit none
         type(norms),intent(inout) :: eCopy
         type(norms),intent(in) :: e
         eCopy%L1 = e%L1; eCopy%L2 = e%L2; eCopy%Linf = e%Linf
       end subroutine

       ! **************************************************************
       ! *********************** PRINT / EXPORT ***********************
       ! **************************************************************

       subroutine print_norms(norm,name)
         implicit none
         type(norms),intent(in) :: norm
         character(len=*),intent(in) :: name
         call export_norms(norm,name,6)
       end subroutine

       subroutine export_norms_dir(norm,dir,name)
         implicit none
         type(norms),intent(in) :: norm
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = newAndOpen(dir,'norms_'//trim(adjustl(name)))
         call export_norms(norm,name,un)
         call closeAndMessage(un,trim(adjustl(name)),dir)
       end subroutine

       subroutine export_norms(norm,name,un)
         implicit none
         type(norms),intent(in) :: norm
         integer,intent(in) :: un
         character(len=*),intent(in) :: name
         write(un,*) '++++++++++++++ '//trim(adjustl(name))//' +++++++++++++++'
         write(un,*) 'L1 = ',norm%L1
         write(un,*) 'L2 = ',norm%L2
         write(un,*) 'Linf = ',norm%Linf
         write(un,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
       end subroutine

       ! **************************************************************
       ! ************************ COMPUTATIONS ************************
       ! **************************************************************

       subroutine compute_norms_vol_SF(e,u,vol)
         implicit none
         type(norms),intent(inout) :: e
         type(SF),intent(in) :: u,vol
         call Ln(e%L1,u,1.0_cp,vol); e%L1 = e%L1/vol%vol
         call Ln(e%L2,u,2.0_cp,vol); e%L2 = (e%L2**0.5_cp)/vol%vol
         e%Linf = maxabs(u)
       end subroutine

       subroutine compute_norms_vol_VF(e,u,vol)
         implicit none
         type(norms),intent(inout) :: e
         type(VF),intent(in) :: u,vol
         call Ln(e%L1,u,1.0_cp,vol); e%L1 = e%L1/vol%x%vol
         call Ln(e%L2,u,2.0_cp,vol); e%L2 = (e%L2**0.5_cp)/vol%x%vol
         e%Linf = maxabs(u)
       end subroutine

       subroutine compute_norms_SF(e,u)
         implicit none
         type(norms),intent(inout) :: e
         type(SF),intent(in) :: u
         call Ln(e%L1,u,1.0_cp)
         call Ln(e%L2,u,2.0_cp)
         e%L2 = e%L2**0.5_cp
         e%Linf = maxabs(u)
       end subroutine

       subroutine compute_norms_VF(e,u)
         implicit none
         type(norms),intent(inout) :: e
         type(VF),intent(in) :: u
         call Ln(e%L1,u,1.0_cp)
         call Ln(e%L2,u,2.0_cp)
         e%L2 = e%L2**0.5_cp
         e%Linf = maxabs(u)
       end subroutine

       end module