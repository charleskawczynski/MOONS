       module apply_BCs_faces_raw_mod
       ! Compiler flags: (_PARALLELIZE_APPLY_BCS_FACES_RAW_)
       use current_precision_mod
       implicit none

       private
       public :: apply_Dirichlet_C
       public :: apply_Dirichlet_N

       public :: apply_Neumann_C
       public :: apply_Neumann_N

       public :: apply_Periodic_C
       public :: apply_Periodic_N

       public :: apply_Symmetric_C
       public :: apply_Symmetric_N

       public :: apply_antisymmetric_C
       public :: apply_antisymmetric_N

       public :: apply_Robin_C
       public :: apply_Robin_N

       contains

       subroutine apply_Dirichlet_C(ug,ui,bvals,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = 2.0_cp*bvals(i,j) - ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug,ub
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ub(i,j) = bvals(i,j); ug(i,j) = 2.0_cp*ub(i,j) - ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_C(ug,ui,bvals,dh_nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh_nhat
         integer :: i,j

#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + bvals(i,j)*dh_nhat
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Neumann_N(ug,ui,bvals,dh_nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh_nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + 2.0_cp*bvals(i,j)*dh_nhat
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Periodic_C(ug,ui_opp,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui_opp
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui_opp(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Periodic_N(ug,ui_opp,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui_opp
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui_opp(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Symmetric_C(ug,ui,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Symmetric_N(ug,ui,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_antisymmetric_C(ug,ui,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = -ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_antisymmetric_N(ug,ui,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = -ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Robin_C(ug,ui,bvals,dh_nhat,x,y,p)
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals ! c = bvals
         real(cp),intent(in) :: dh_nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j)*(2.0_cp*bvals(i,j)/dh_nhat-1.0_cp)/(2.0_cp*bvals(i,j)/dh_nhat+1.0_cp)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Robin_N(ug,ui,ub,bvals,dh_nhat,x,y,p)
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,ub,bvals ! c = bvals
         real(cp),intent(in) :: dh_nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + ub(i,j)*(2.0_cp*dh_nhat/bvals(i,j))
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       end module