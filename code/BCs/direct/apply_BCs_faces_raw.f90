       module apply_BCs_faces_raw_mod
       ! Compiler flags: (_PARALLELIZE_APPLY_BCS_FACES_RAW_)
       use current_precision_mod
       implicit none

       private
       public :: apply_assign

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

       subroutine apply_assign(u,bvals,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: u
         real(cp),dimension(x,y),intent(in) :: bvals
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         u(i,j) = bvals(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

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

       subroutine apply_Robin_C(ug,ui,bvals,dh,nhat,c_w,x,y,p)
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals ! c = bvals
         real(cp),intent(in) :: dh,nhat,c_w
         real(cp) :: coeff_1,coeff_2
         integer :: i,j
         coeff_1 = (2.0_cp*c_w*nhat/dh-1.0_cp)/(2.0_cp*c_w*nhat/dh+1.0_cp)
         coeff_2 =  2.0_cp/(2.0_cp*c_w*nhat/dh+1.0_cp)
         coeff_2 =  0.0_cp
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j)*coeff_1 + bvals(i,j)*coeff_2
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Robin_N(ug,ui,ub,bvals,dh,nhat,c_w,x,y,p)
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,ub,bvals ! c = bvals
         real(cp),intent(in) :: dh,nhat,c_w
         real(cp) :: coeff_1,coeff_2
         integer :: i,j
         coeff_1 = -2.0_cp*dh/c_w*nhat
         coeff_2 =  2.0_cp*dh/c_w*nhat
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + ub(i,j)*coeff_1 + bvals(i,j)*coeff_2
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       end module