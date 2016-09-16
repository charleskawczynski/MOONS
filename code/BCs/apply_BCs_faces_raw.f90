       module apply_BCs_faces_raw_mod
       use current_precision_mod
       use bctype_mod
       implicit none

       private
       public :: apply_Dirichlet_C, apply_Dirichlet_N
       public :: apply_Neumann_C,   apply_Neumann_N
       public :: apply_Periodic_C,  apply_Periodic_N
       public :: apply_Robin_C

       contains

       subroutine apply_Dirichlet_C(ug,ui,bvals,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = 2.0_cp*bvals(i,j) - ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh,nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + nhat*bvals(i,j)*dh
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Periodic_C(ug,ui_opp,x,y,p) ! interpolated - (wall incoincident)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui_opp
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui_opp(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Robin_C(ug,ui,bvals,dh,nhat,x,y,p) ! interpolated - (wall incoincident)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh,nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j)*(2.0_cp*bvals(i,j)/dh*nhat - 1.0_cp)/(2.0_cp*bvals(i,j)/dh*nhat + 1.0_cp)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug,ub
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ub(i,j) = bvals(i,j); ug(i,j) = 2.0_cp*ub(i,j) - ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_N(ug,ui,bvals,dh,nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh,nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + 2.0_cp*bvals(i,j)*dh*nhat
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Periodic_N(ug,ui_opp,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui_opp
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui_opp(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       end module