       module apply_BCs_faces_raw_implicit_mod
       ! Compiler flags: (_PARALLELIZE_APPLY_BCS_FACES_RAW_)
       use current_precision_mod
       implicit none

       private
       public :: apply_Dirichlet_C_implicit
       public :: apply_Dirichlet_N_implicit

       public :: apply_Neumann_C_implicit
       public :: apply_Neumann_N_implicit

       public :: apply_Periodic_C_implicit
       public :: apply_Periodic_N_implicit

       public :: apply_Symmetric_C_implicit
       public :: apply_Symmetric_N_implicit

       public :: apply_antisymmetric_C_implicit
       public :: apply_antisymmetric_N_implicit

       public :: apply_Robin_C_implicit
       public :: apply_Robin_N_implicit

       contains

       subroutine apply_Dirichlet_C_implicit(ug,ui,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = - ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Dirichlet_N_implicit(ug,ub,ui,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug,ub
         real(cp),dimension(x,y),intent(in) :: ui
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ub(i,j) = 0.0_cp; ug(i,j) = - ui(i,j)
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       subroutine apply_Neumann_C_implicit(ug,ui,x,y,p)
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
       subroutine apply_Neumann_N_implicit(ug,ui,x,y,p)
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

       subroutine apply_Periodic_C_implicit(ug,ui_opp,x,y,p)
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
       subroutine apply_Periodic_N_implicit(ug,ui_opp,x,y,p)
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

       subroutine apply_Symmetric_C_implicit(ug,ui,x,y,p)
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
       subroutine apply_Symmetric_N_implicit(ug,ui,x,y,p)
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

       subroutine apply_antisymmetric_C_implicit(ug,ui,x,y,p)
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
       subroutine apply_antisymmetric_N_implicit(ug,ui,x,y,p)
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

       subroutine apply_Robin_C_implicit(ug,ui,dh,nhat,c_w,x,y,p) ! not yet tested
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui ! c = bvals
         real(cp),intent(in) :: dh,nhat,c_w
         real(cp) :: coeff_1
         integer :: i,j
         coeff_1 = (2.0_cp*c_w*nhat/dh-1.0_cp)/(2.0_cp*c_w*nhat/dh+1.0_cp)
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j)*coeff_1
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Robin_N_implicit(ug,ub,ui,dh,nhat,c_w,x,y,p) ! not yet tested
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ub,ui ! c = bvals
         real(cp),intent(in) :: dh,nhat,c_w
         real(cp) :: coeff_1
         integer :: i,j
         coeff_1 = -2.0_cp*dh*nhat/c_w
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + ub(i,j)*coeff_1
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine

       end module