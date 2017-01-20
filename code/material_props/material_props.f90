       module material_props_mod
       ! Refs:
       !
       ! 1) https://en.wikipedia.org/wiki/Permeability_(electromagnetism)
       ! 2) L8 INTRO TO MHD 1 2014 (Sergey's PPT for fusion class)
       ! 3) https://www.microwaves101.com/encyclopedias/magnetic-materials
       !
       !
       !
       ! Diamagnetic: Magnetic Materials K_m<1
       ! Nonmagnetic: Magnetic Materials K_m=1 (air is a good example,
       !              but most metals are very close to nonmagnetic,
       !              close enough so you can round off Magnetic MaterialsR to 1.)
       ! Paramagnetic: Magnetic Materials  K_m>1
       ! Ferromagnetic: Magnetic Materials  K_m>>1
       use current_precision_mod
       use constants_mod
       implicit none

       private
       public :: init_PbLi
       interface init_PbLi;    module procedure init_PbLi_MP;    end interface

       type material_props
         ! ν = μ/ρ
         ! Χ_m = μ/μ₀-1
         ! Χ_m = K_m-1
         ! K_m = μ/μ₀
         real(cp) :: rho = 0.0_cp
         real(cp) :: sigma = 0.0_cp
         real(cp) :: nu = 0.0_cp
         real(cp) :: mu = 0.0_cp
         real(cp) :: mu_m = 0.0_cp
         real(cp) :: K_m = 0.0_cp  ! relative magnetic permeability
         real(cp) :: Xi_m = 0.0_cp ! Magnetic susceptibility
       end type

       contains

       subroutine init_PbLi_MP(MP)
         implicit none
         type(material_props),intent(inout) :: MP
         MP%rho = 485.0_cp                  ! [kg/m^3]
         MP%sigma = 2.8_cp*pow(6)           ! [Ohm^-1 m^-1]
         MP%nu = 8.5_cp*pow(-7)             ! [m^2/s]
         MP%mu = MP%nu*MP%rho               ! [kg/(s m^1)]
         MP%Xi_m = 1.4_cp*pow(-5)           ! [1]
         MP%K_m = MP%Xi_m+1.0_cp            ! [1]
         MP%mu_m = MP%K_m*mu_m0             ! [NA^-2]
       end subroutine

       end module