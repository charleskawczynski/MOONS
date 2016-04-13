      module RF_aux_mod
      use RF_mod
      use RF_sum_mod
      implicit none
      private

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      public :: min
      public :: max
      public :: maxabs
      public :: maxabsdiff
      public :: mean
      public :: size

      interface min;        module procedure min_RF;         end interface
      interface min;        module procedure min_pad_RF;     end interface
      interface max;        module procedure max_pad_RF;     end interface
      interface max;        module procedure max_RF;         end interface
      interface maxabs;     module procedure maxabs_RF;      end interface
      interface maxabsdiff; module procedure maxabsdiff_RF;  end interface
      interface mean;       module procedure mean_RF;        end interface
      interface size;       module procedure size_RF;        end interface

      contains

      function min_RF(a) result(m)
        implicit none
        type(realField),intent(in) :: a
        real(cp) :: m
        m = minval(a%f)
      end function

      function min_pad_RF(a,pad) result(m)
        implicit none
        type(realField),intent(in) :: a
        integer,intent(in) :: pad
        real(cp) :: m
        m = minval(a%f(1+pad:a%s(1)-pad,1+pad:a%s(2)-pad,1+pad:a%s(3)-pad))
      end function

      function max_RF(a) result(m)
        implicit none
        type(realField),intent(in) :: a
        real(cp) :: m
        m = maxval(a%f)
      end function

      function max_pad_RF(a,pad) result(m)
        implicit none
        type(realField),intent(in) :: a
        integer,intent(in) :: pad
        real(cp) :: m
        m = maxval(a%f(1+pad:a%s(1)-pad,1+pad:a%s(2)-pad,1+pad:a%s(3)-pad))
      end function

      function maxabs_RF(a) result(m)
        implicit none
        type(realField),intent(in) :: a
        real(cp) :: m
        m = maxval(abs(a%f))
      end function

      function maxabsdiff_RF(a,b) result(m)
        implicit none
        type(realField),intent(in) :: a,b
        real(cp) :: m
        m = maxval(abs(a%f-b%f))
      end function

      function mean_RF(a,s1,s2) result(m)
        implicit none
        type(realField),intent(in) :: a
        integer,dimension(3),intent(in) :: s1,s2
        real(cp) :: m
        m = sum(a,s1,s2)/(max(1,size(a%f)))
      end function

      function size_RF(a) result(s)
        implicit none
        type(realField),intent(in) :: a
        integer :: s
        s = a%s(1)*a%s(2)*a%s(3)
      end function

      end module