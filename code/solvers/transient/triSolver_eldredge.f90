      module triSolver_eldredge_mod
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

      contains

      subroutine triSolveEldredge(u,f,a,b,c,M)
        implicit none
        integer,intent(in) :: M
        real(cp),dimension(M),intent(inout) :: u,f
        real(cp),dimension(M-1),intent(inout) :: c
        real(cp),dimension(M-1),intent(in) :: a
        real(cp),dimension(M),intent(in) :: b
        real(cp) :: temp,ctemp,ftemp
        integer j
        c(1) = c(1) / b(1)
        f(1) = f(1) / b(1)
        ctemp = c(1)
        ftemp = f(1)
        do j = 2, M-1
            temp = b(j) - a(j-1)*ctemp
            ctemp = c(j)/temp
            ftemp = (f(j) - a(j-1)*ftemp)/temp
            c(j) = ctemp
            f(j) = ftemp
        enddo
        f(M) = (f(M)-a(M-1)*f(M-1))/(b(M)-a(M-1)*c(M-1))
        temp = f(M)
        u(M) = temp
        do j = M-1, 2, -1
            temp = f(j) - c(j)*temp
            u(j) = temp
        enddo
        u(1) = f(1) - c(1)*temp
      end subroutine

      subroutine triSolveEldredge_preserve(u,f,a,b,c,M)
        implicit none
        integer,intent(in) :: M
        real(cp),dimension(M),intent(inout) :: u
        real(cp),dimension(M),intent(in) :: b,f
        real(cp),dimension(M-1),intent(in) :: a,c
        real(cp),dimension(M-1) :: ctemp
        real(cp),dimension(M) :: ftemp
        ctemp = c; ftemp = f
        call triSolveEldredge(u,ftemp,a,b,ctemp,M)
      end subroutine

      end module
