      module SF_mod
      use mesh_mod
      use RF_mod
      
      use RF_assign_negative_mod
      use RF_assign_mod

      use RF_add_mod
      use RF_subtract_mod
      use RF_multiply_mod
      use RF_divide_mod

      use RF_square_mod
      use RF_sum_mod

      use RF_aux_mod
      implicit none
      private

      ! Operators
      public :: assign,assign_negative
      public :: add,subtract
      public :: multiply,divide
      public :: invert
      ! Auxiliary
      public :: dot_product
      public :: square,min,max,maxabs
      public :: maxabsdiff,mean,sum

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      interface dot_product;      module procedure dot_product_SF;         end interface

      interface assign;           module procedure assign_SF_S;            end interface
      interface assign;           module procedure assign_SF_SF;           end interface
      interface assign_negative;  module procedure assign_negative_SF_SF;  end interface

      interface add;              module procedure add_SF_SF;              end interface
      interface add;              module procedure add_SF_SF_SF;           end interface
      interface add;              module procedure add_SF_S;               end interface
      interface add;              module procedure add_S_SF;               end interface

      interface multiply;         module procedure multiply_SF_SF;         end interface
      interface multiply;         module procedure multiply_SF_SF_SF;      end interface
      interface multiply;         module procedure multiply_SF_S;          end interface
      interface multiply;         module procedure multiply_S_SF;          end interface

      interface subtract;         module procedure subtract_SF_SF;         end interface
      interface subtract;         module procedure subtract_SF_SF_SF;      end interface
      interface subtract;         module procedure subtract_SF_S;          end interface
      interface subtract;         module procedure subtract_S_SF;          end interface

      interface divide;           module procedure divide_SF_SF;           end interface
      interface divide;           module procedure divide_SF_SF_SF;        end interface
      interface divide;           module procedure divide_SF_S_SF;         end interface
      interface divide;           module procedure divide_SF_S;            end interface
      interface divide;           module procedure divide_S_SF;            end interface

      interface invert;           module procedure invert_SF;              end interface

      interface square;           module procedure square_SF;              end interface
      interface min;              module procedure min_SF;                 end interface
      interface max;              module procedure max_SF;                 end interface
      interface min;              module procedure min_pad_SF;             end interface
      interface max;              module procedure max_pad_SF;             end interface
      interface maxabs;           module procedure maxabs_SF;              end interface
      interface maxabsdiff;       module procedure maxabsdiff_SF;          end interface
      interface mean;             module procedure mean_SF;                end interface
      interface sum;              module procedure sum_SF;                 end interface

      contains

      subroutine assign_SF_SF(f,g)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        do i=1,f%s; call assign(f%RF(i),g%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine assign_SF_S(f,g)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        do i=1,f%s; call assign(f%RF(i),g,(/1,1,1/),f%RF(i)%s); enddo
      end subroutine
      
      subroutine assign_negative_SF_SF(f,g)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        do i=1,f%s; call assign_negative(f%RF(i),g%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

    ! ------------------- ADD ------------------------

      subroutine add_SF_SF(f,g)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        do i=1,f%s; call add(f%RF(i),g%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine add_SF_SF_SF(f,g,r)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g,r
        integer :: i
        do i=1,f%s; call add(f%RF(i),g%RF(i),r%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine add_SF_S(f,g)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        do i=1,f%s; call add(f%RF(i),g,(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine add_S_SF(g2,f)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g2
        integer :: i
        do i=1,f%s; call add(g2,f%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

    ! ------------------- SUBTRACT ------------------------

      subroutine subtract_SF_SF(f,g)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        do i=1,f%s; call subtract(f%RF(i),g%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine subtract_SF_SF_SF(f,g,q)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g,q
        integer :: i
        do i=1,f%s; call subtract(f%RF(i),g%RF(i),q%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine subtract_SF_S(f,g)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        do i=1,f%s; call subtract(f%RF(i),g,(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine subtract_S_SF(g2,f)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g2
        integer :: i
        do i=1,f%s; call subtract(g2,f%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

    ! ------------------- MULTIPLY ------------------------

      subroutine multiply_SF_SF(f,g)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        do i=1,f%s; call multiply(f%RF(i),g%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine multiply_SF_SF_SF(f,g,q)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g,q
        integer :: i
        do i=1,f%s; call multiply(f%RF(i),g%RF(i),q%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine multiply_SF_S(f,g)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        do i=1,f%s; call multiply(f%RF(i),g,(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine multiply_S_SF(g2,f)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g2
        integer :: i
        do i=1,f%s; call multiply(f%RF(i),g2,(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

    ! ------------------- DIVIDE ------------------------

      subroutine divide_SF_SF(f,g)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g
        integer :: i
        do i=1,f%s; call divide(f%RF(i),g%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine divide_SF_SF_SF(f,g,q)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: g,q
        integer :: i
        do i=1,f%s; call divide(f%RF(i),g%RF(i),q%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine divide_SF_S_SF(f,g,q)
        implicit none
        type(SF),intent(inout) :: f
        type(SF),intent(in) :: q
        real(cp),intent(in) :: g
        integer :: i
        do i=1,f%s; call divide(f%RF(i),g,q%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine divide_SF_S(f,g)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g
        integer :: i
        do i=1,f%s; call divide(f%RF(i),g,(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine divide_S_SF(g2,f)
        implicit none
        type(SF),intent(inout) :: f
        real(cp),intent(in) :: g2
        integer :: i
        do i=1,f%s; call divide(g2,f%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

    ! ------------------- OTHER ------------------------

      subroutine invert_SF(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        do i=1,f%s; call divide(1.0_cp,f%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      subroutine square_SF(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        do i=1,f%s; call square(f%RF(i),(/1,1,1/),f%RF(i)%s); enddo
      end subroutine

      function min_SF(f) result(m)
        implicit none
        type(SF),intent(in) :: f
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,f%s
          m = minval((/m,min(f%RF(i))/))
        enddo
      end function

      function max_SF(f) result(m)
        implicit none
        type(SF),intent(in) :: f
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,f%s
          m = maxval((/m,max(f%RF(i))/))
        enddo
      end function

      function min_pad_SF(f,pad) result(m)
        implicit none
        type(SF),intent(in) :: f
        integer,intent(in) :: pad
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,f%s
          m = minval((/m,min(f%RF(i),pad)/))
        enddo
      end function

      function max_pad_SF(f,pad) result(m)
        implicit none
        type(SF),intent(in) :: f
        integer,intent(in) :: pad
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,f%s
          m = maxval((/m,max(f%RF(i),pad)/))
        enddo
      end function

      function maxabs_SF(f) result(m)
        implicit none
        type(SF),intent(in) :: f
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,f%s
          m = maxval(abs((/m,maxabs(f%RF(i))/)))
        enddo
      end function

      function maxabsdiff_SF(a,b) result(m)
        implicit none
        type(SF),intent(in) :: a,b
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,a%s
          m = maxval((/m,maxabsdiff(a%RF(i),b%RF(i))/))
        enddo
      end function

      function mean_SF(a) result(m)
        implicit none
        type(SF),intent(in) :: a
        real(cp) :: m
        integer :: i,s
        m = sum(a)
        s = 0
        do i=1,a%s
          s = s + size(a%RF(i))
        enddo
        m = m/real(s,cp)
      end function

      function sum_SF(f) result(m)
        implicit none
        type(SF),intent(in) :: f
        real(cp) :: m
        integer :: i
        m = 0.0_cp
        do i=1,f%s
          m = m + sum(f%RF(i),(/1,1,1/),f%RF(i)%s)
        enddo
      end function

      function dot_product_SF(A,B,temp) result(dot)
        implicit none
        type(SF),intent(in) :: A,B
        type(SF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        dot = sum(temp)
      end function

      end module