       module constants_mod
       use current_precision_mod
       implicit none
       private
       public :: PI
       real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp
       ! real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

       public :: mu_m0 ! Permeability of free space:
       real(cp),parameter :: mu_m0 = 4.0_cp*PI**10.0_cp**(-7.0_cp) ! [NA^-2]
       end module