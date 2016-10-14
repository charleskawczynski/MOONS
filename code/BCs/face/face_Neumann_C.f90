       module face_Neumann_C_mod
       use current_precision_mod
       use apply_BCs_faces_raw_mod
       use grid_mod
       use GF_mod
       implicit none

       ! From apply_BCs_faces_raw.f90:
       !       call apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)

       private
       public :: apply_face_BC_op
       public :: F_N_C1 ! Face Dirichlet Cell centered data
       public :: F_N_C2 ! Face Dirichlet Cell centered data
       public :: F_N_C3 ! Face Dirichlet Cell centered data
       public :: F_N_C4 ! Face Dirichlet Cell centered data
       public :: F_N_C5 ! Face Dirichlet Cell centered data
       public :: F_N_C6 ! Face Dirichlet Cell centered data

       abstract interface
         subroutine apply_face_BC_op(GF,g,bvals,pad)
           import grid,grid_field
           implicit none
           type(grid_field),intent(inout) :: GF
           type(grid),intent(in) :: g
           type(grid_field),intent(in) :: bvals
           integer,intent(in) :: pad
         end subroutine
       end interface

       contains

       subroutine F_N_C1(GF,g,bvals,pad)
        implicit none
        type(grid_field),intent(inout) :: GF
        type(grid),intent(in) :: g
        type(grid_field),intent(in) :: bvals
        integer,intent(in) :: pad
        call apply_Neumann_C(&
        GF%f(1,:,:),&
        GF%f(2,:,:),&
        bvals%f,&
        g%c(1)%dhc(1),&
        -1.0_cp,&
        GF%s(2),&
        GF%s(3),&
        pad)
       end subroutine
       subroutine F_N_C2(GF,g,bvals,pad)
        implicit none
        type(grid_field),intent(inout) :: GF
        type(grid),intent(in) :: g
        type(grid_field),intent(in) :: bvals
        integer,intent(in) :: pad
        call apply_Neumann_C(&
        GF%f(GF%s(1),:,:),&
        GF%f(GF%s(1)-1,:,:),&
        bvals%f,&
        g%c(1)%dhc_e,&
        1.0_cp,&
        GF%s(2),&
        GF%s(3),&
        pad)
       end subroutine
       subroutine F_N_C3(GF,g,bvals,pad)
        implicit none
        type(grid_field),intent(inout) :: GF
        type(grid),intent(in) :: g
        type(grid_field),intent(in) :: bvals
        integer,intent(in) :: pad
        call apply_Neumann_C(&
        GF%f(:,1,:),&
        GF%f(:,2,:),&
        bvals%f,&
        g%c(2)%dhc(1),&
        -1.0_cp,&
        GF%s(1),&
        GF%s(3),&
        pad)
       end subroutine
       subroutine F_N_C4(GF,g,bvals,pad)
        implicit none
        type(grid_field),intent(inout) :: GF
        type(grid),intent(in) :: g
        type(grid_field),intent(in) :: bvals
        integer,intent(in) :: pad
        call apply_Neumann_C(&
        GF%f(:,GF%s(2),:),&
        GF%f(:,GF%s(2)-1,:),&
        bvals%f,&
        g%c(2)%dhc_e,&
        1.0_cp,&
        GF%s(1),&
        GF%s(3),&
        pad)
       end subroutine
       subroutine F_N_C5(GF,g,bvals,pad)
        implicit none
        type(grid_field),intent(inout) :: GF
        type(grid),intent(in) :: g
        type(grid_field),intent(in) :: bvals
        integer,intent(in) :: pad
        call apply_Neumann_C(&
        GF%f(:,:,1),&
        GF%f(:,:,2),&
        bvals%f,&
        g%c(3)%dhc(1),&
        -1.0_cp,&
        GF%s(1),&
        GF%s(2),&
        pad)
       end subroutine
       subroutine F_N_C6(GF,g,bvals,pad)
        implicit none
        type(grid_field),intent(inout) :: GF
        type(grid),intent(in) :: g
        type(grid_field),intent(in) :: bvals
        integer,intent(in) :: pad
        call apply_Neumann_C(&
        GF%f(:,:,GF%s(3)),&
        GF%f(:,:,GF%s(3)-1),&
        bvals%f,&
        g%c(3)%dhc_e,&
        1.0_cp,&
        GF%s(1),&
        GF%s(2),&
        pad)
       end subroutine

       end module