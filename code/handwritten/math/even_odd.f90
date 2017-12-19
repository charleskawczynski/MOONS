       module even_odd_mod
       implicit none
       private
       public :: is_odd
       public :: is_even

       contains

       pure function is_odd(n) result(L)
         implicit none
         integer,intent(in) :: n
         logical :: L
         L = .not.is_even(n)
       end function

       pure function is_even(n) result(L)
         implicit none
         integer,intent(in) :: n
         logical :: L
         L = MOD(n,2).eq.0
       end function

       end module