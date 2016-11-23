       module material_props_mod
       use current_precision_mod
       implicit none

       private
       public :: init_Li
       interface init_Li;    module procedure init_Li_MP;    end interface
       interface init_Li;    module procedure init_Li_MP;    end interface

       type material_props
         real(cp) :: rho = 0.0_cp
         real(cp) :: sigma = 0.0_cp
         real(cp) :: nu = 0.0_cp
         real(cp) :: mu = 0.0_cp
       end type

       contains

       subroutine init_Li_MP(MP)
         implicit none
         type(material_props),intent(inout) :: MP
         MP%rho = 485.0_cp        ! [kg/m^3]
         MP%sigma = 2.8_cp*pow(6) ! [Ohm^-1 m^-1]
         MP%nu = 8.5_cp*pow(-7)   ! [m^2/s]
         MP%mu =                  !
       end subroutine

       function pow(i) result(p)
         implicit none
         integer,intent(in) :: i
         real(cp) :: p
         p = 10.0_cp**(real(i,cp))
       end function

       end module