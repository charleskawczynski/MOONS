       module PI_mod
       use current_precision_mod
       implicit none
       private
       public :: PI
       real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp
       end module