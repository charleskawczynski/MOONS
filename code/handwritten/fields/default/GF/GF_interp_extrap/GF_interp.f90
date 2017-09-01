       module GF_interp_mod
       ! Compiler flags: (_DEBUG_INTERP_,_PARALLELIZE_INTERP_)
       use current_precision_mod
       use grid_field_mod
       use grid_field_extend_mod
       use GF_extrap_mod
       use grid_mod
       implicit none

       private
       public :: interp_C2N
       public :: interp_N2C
       interface interp_C2N;      module procedure interp_C2N_GF;    end interface
       interface interp_N2C;      module procedure interp_N2C_GF;    end interface

       contains

       subroutine interp_C2N_GF(N,C,g,dir,x,y,z)
         !         N  C  N  C  N  C  N  C  N
         !         |--o--|--o--|--o--|--o--|      --> dir
         !               *     *     *
         implicit none
         type(grid_field),intent(inout) :: N
         type(grid_field),intent(in) :: C
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,x,y,z
         integer :: i,j,k,t
#ifdef _DEBUG_INTERP_
         call insist_shape_match(C,N,dir,'interp_C2N_GF')
         call insist_shape_staggered(C,N,dir,'interp_C2N_GF')
#endif

#ifdef _PARALLELIZE_INTERP_
           !$OMP PARALLEL DO PRIVATE(t)

#endif
           do k=1,C%s(3)-z; do j=1,C%s(2)-y; do i=1,C%s(1)-x
           t = i*x + j*y + k*z
           N%f(i+x,j+y,k+z) = C%f(i+x,j+y,k+z)*g%c(dir)%theta%D%f(t) + &
                              C%f( i , j , k )*g%c(dir)%theta%U%f(t)
           enddo; enddo; enddo
#ifdef _PARALLELIZE_INTERP_
           !$OMP END PARALLEL DO

#endif
           call extrap(N,C,dir)
       end subroutine

       subroutine interp_N2C_GF(C,N,dir,x,y,z)
         !         N  C  N  C  N  C  N  C  N
         !         |--o--|--o--|--o--|--o--|   --> dir
         !            *     *     *     *
         implicit none
         type(grid_field),intent(inout) :: C
         type(grid_field),intent(in) :: N
         integer,intent(in) :: dir,x,y,z
         integer :: i,j,k
#ifdef _DEBUG_INTERP_
         call insist_shape_match(C,N,dir,'interp_N2C_GF')
         call insist_shape_staggered(C,N,dir,'interp_N2C_GF')
#endif

#ifdef _PARALLELIZE_INTERP_
           !$OMP PARALLEL DO

#endif
           do k=1,N%s(3)-z; do j=1,N%s(2)-y; do i=1,N%s(1)-x
           C%f(i,j,k) = 0.5_cp*(N%f( i , j , k )+&
                                N%f(i+x,j+y,k+z))
           enddo; enddo; enddo
#ifdef _PARALLELIZE_INTERP_
           !$OMP END PARALLEL DO

#endif
       end subroutine

       end module