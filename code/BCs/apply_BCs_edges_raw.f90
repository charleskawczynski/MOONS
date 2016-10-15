       module apply_BCs_edges_raw_mod
       ! Compiler flags: (_PARALLELIZE_APPLY_BCS_EDGES_RAW_)
       ! Applicable cases:
       !                           ^
       !                           |
       !                           -------------------------
       !                           |     |     |     |     |
       !                           |-----|-----|-----|-----|
       ! CC   data or              |     |     |     |     |
       ! Face data (into page):    |-----|-----|-----|-----|
       !                           |  L  |  i  |     |     |
       !                           |-----b-----|-----|-----|
       !                           |  g  |  R  |     |     |
       !                           ---------------------------->
       ! 
       !                           ^
       !                           |
       !                           -------------------------
       !                           |     |     |     |     |
       !                           |-----|-----|-----|-----|
       ! Node data or              |     |     |     |     |
       ! Edge data (into page)     |-----|-----i-----|-----|
       !                           |     |     |     |     |
       !                           |-----b-----|-----|-----|
       !                           |     |     |     |     |
       !                           g--------------------------->
       ! 
       !                           ^
       !                           |
       !                           -------------------------
       !                           |     |     |     |     |
       !                           |-----|-----|-----|-----|
       ! Face data (in page) or    |     |     |     |     |
       ! Edge data (in page)       |-----|-----|-----|-----|
       !                           |     |     |     |     |
       !                           |     i     |     |     |
       !                           |     |     |     |     |
       !                           |-----b-----|-----|-----|
       !                           |     |     |     |     |
       !                           |     g     |     |     |
       !                           |     |     |     |     |
       !                           ---------------------------->
       ! 
       use current_precision_mod
       implicit none

       private
       public :: apply_Dirichlet_C
       public :: apply_Neumann_C
       public :: apply_Periodic_C

       public :: apply_Dirichlet_N
       public :: apply_Neumann_N
       public :: apply_Periodic_N

       public :: apply_Dirichlet_F
       public :: apply_Neumann_F
       public :: apply_Periodic_F

       real(cp),parameter :: root2 = sqrt(2.0_cp)

       contains

       subroutine apply_Dirichlet_C(g,b,i,L,R,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: b,i,L,R
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = 4.0_cp*b(j) - (i(j) + L(j) + R(j))
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_C(g,b,i,dh,nhat,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: b,i
         integer,intent(in) :: s,p
         real(cp),intent(in) :: nhat,dh
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = i(j) + b(j)*dh*root2*nhat
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Periodic_C(g,ui_opp,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: ui_opp
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = ui_opp(j)
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Dirichlet_N(ub,b,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: ub
         real(cp),dimension(s),intent(in) :: b
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         ub(j) = b(j)
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_N(g,b,i,dh,nhat,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: b,i
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = i(j) + 2.0_cp*dh*b(j)*root2*nhat
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Periodic_N(g,ui_opp,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: ui_opp
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = ui_opp(j)
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Dirichlet_F(g,b,i,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: b,i
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
           g(j) = 2.0_cp*b(j) - i(j)
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_F(g,b,i,dh,nhat,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: b,i
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = i(j) + dh*b(j)*nhat
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Periodic_F(g,ui_opp,s,p)
         implicit none
         real(cp),dimension(s),intent(inout) :: g
         real(cp),dimension(s),intent(in) :: ui_opp
         integer,intent(in) :: s,p
         integer :: j
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,s-p
         g(j) = ui_opp(j)
         enddo
#ifdef _PARALLELIZE_APPLY_BCS_EDGES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       end module