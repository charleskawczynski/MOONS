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

       public :: apply_Antisymmetric_C_implicit
       public :: apply_Antisymmetric_N_implicit

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

       subroutine apply_Neumann_C_implicit(ug,ui,bvals,dh,nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh,nhat
         integer :: i,j

#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + nhat*bvals(i,j)*dh
         enddo; enddo
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP END PARALLEL DO

#endif
       end subroutine
       subroutine apply_Neumann_N_implicit(ug,ui,bvals,dh,nhat,x,y,p)
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh,nhat
         integer :: i,j
#ifdef _PARALLELIZE_APPLY_BCS_FACES_RAW_
        !$OMP PARALLEL DO

#endif
         do j=1+p,y-p; do i=1+p,x-p
         ug(i,j) = ui(i,j) + 2.0_cp*bvals(i,j)*dh*nhat
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

       subroutine apply_Antisymmetric_C_implicit(ug,ui,x,y,p)
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
       subroutine apply_Antisymmetric_N_implicit(ug,ui,x,y,p)
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

       subroutine apply_Robin_C_implicit(ug,ui,bvals,dh,nhat,x,y,p) ! not yet tested
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,bvals ! c = bvals
         real(cp),intent(in) :: dh,nhat
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
       subroutine apply_Robin_N_implicit(ug,ui,ub,bvals,dh,nhat,x,y,p) ! not yet tested
         ! u + c du/dh = 0
         implicit none
         integer,intent(in) :: x,y,p
         real(cp),dimension(x,y),intent(inout) :: ug
         real(cp),dimension(x,y),intent(in) :: ui,ub,bvals ! c = bvals
         real(cp),intent(in) :: dh,nhat
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

       end module