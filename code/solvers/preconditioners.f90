      module preconditioners_mod
      use mesh_mod
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
      public :: laplacian

      contains

      subroutine laplacian(Minv,x,m)
        ! Minv = L D U
        implicit none
        type(SF),intent(inout) :: Minv
        type(SF),intent(in) :: x
        type(mesh),intent(in) :: m
        integer :: i,j,k,t
        if (x%is_CC) then
          !$OMP PARALLEL DO
          do t=1,m%s; do k=1,x%RF(t)%s(3); do j=1,x%RF(t)%s(2); do i=1,x%RF(t)%s(1)
          Minv%RF(t)%f(i,j,k) = m%g(t)%c(1)%stagCC2N%D(i)*m%g(t)%c(1)%stagN2CC%U(i) + &
                                m%g(t)%c(1)%stagCC2N%U(i)*m%g(t)%c(1)%stagN2CC%D(i) + &
                                m%g(t)%c(2)%stagCC2N%D(j)*m%g(t)%c(2)%stagN2CC%U(j) + &
                                m%g(t)%c(2)%stagCC2N%U(j)*m%g(t)%c(2)%stagN2CC%D(j) + &
                                m%g(t)%c(3)%stagCC2N%D(k)*m%g(t)%c(3)%stagN2CC%U(k) + &
                                m%g(t)%c(3)%stagCC2N%U(k)*m%g(t)%c(3)%stagN2CC%D(k)
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO

        elseif (x%is_Face) then
        elseif (x%is_Node) then
        elseif (x%is_Edge) then
        else
        stop 'Error: bad input to laplacian in preconditions.f90'
        endif
      end subroutine

      end module