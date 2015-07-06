module fft_mod
  ! References:
  ! http://www.maths.cam.ac.uk/undergrad/course/na/ii/poisson_equation/poisson_equation.php
  ! 
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

  integer,parameter :: dp = selected_real_kind(14)
  real(cp),parameter :: pi=3.141592653589793238460_cp

contains
 
  ! In place Cooley-Tukey FFT
  recursive subroutine fft(x)
    complex(cp), dimension(:), intent(inout)  :: x
    complex(cp)                               :: t
    integer                                   :: N
    integer                                   :: i
    complex(cp), dimension(:), allocatable    :: even, odd
 
    N=size(x)
    if(N .le. 1) return
 
    allocate(odd((N+1)/2))
    allocate(even(N/2))
 
    ! divide
    odd  = x(1:N:2)
    even = x(2:N:2)
 
    ! conquer
    call fft(odd)
    call fft(even)
 
    ! combine
    do i=1,N/2
       t=exp(cmplx(0.0_dp,-2.0_dp*pi*real(i-1,dp)/real(N,dp),dp))*even(i)
       x(i)     = odd(i) + t
       x(i+N/2) = odd(i) - t
    end do
 
    deallocate(odd)
    deallocate(even)
 
  end subroutine fft
 
end module fft_mod

program test
  use fft_mod
  implicit none
  complex(cp), dimension(8) :: data = (/1.0, 1.0, 1.0, 1.0, 0.0,0.0, 0.0, 0.0/)
  integer :: i
 
  call fft(data)
 
  do i=1,8
     write(*,'("(", F20.15, ",", F20.15, "i )")') data(i)
  end do
 
end program test
