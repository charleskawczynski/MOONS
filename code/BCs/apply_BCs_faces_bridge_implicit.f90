       module apply_BCs_faces_bridge_implicit_mod
       use current_precision_mod
       use apply_BCs_faces_raw_implicit_mod
       use GF_mod
       use face_SD_mod

       ! From apply_BCs_faces_raw.f90:
       !       call apply_Dirichlet_C_implicit(ug,ui,bvals,x,y,p)
       !       call apply_Dirichlet_N_implicit(ug,ub,ui,bvals,x,y,p)
       !       call apply_Neumann_C_implicit(ug,ui,bvals,dh,nhat,x,y,p)
       !       call apply_Neumann_N_implicit(ug,ui,bvals,dh,nhat,x,y,p)
       !       call apply_Periodic_C_implicit(ug,ui_opp,x,y,p)
       !       call apply_Periodic_N_implicit(ug,ui_opp,x,y,p)
       !       call apply_Robin_C_implicit(ug,ui,bvals,dh,nhat,x,y,p)

       ! From here:
       !       call Dirichlet_C(BF,i_sd)
       !       call Dirichlet_N(BF,i_sd)
       !       call Neumann_C(BF,B,i_sd)
       !       call Neumann_N(BF,B,i_sd)
       !       call Periodic_C(BF,i_sd)
       !       call Periodic_N(BF,i_sd)
       !       call Robin_C(BF,B,i_sd)

       implicit none
       private
       public :: apply_face_BC_op

       public :: Dirichlet_C_implicit
       public :: Dirichlet_N_implicit
       public :: Neumann_C_implicit
       public :: Neumann_N_implicit
       public :: Periodic_C_implicit
       public :: Periodic_N_implicit
       public :: Robin_C_implicit
       public :: Robin_N_implicit

       abstract interface
         subroutine apply_face_BC_op(GF,surf,FSD,face)
           import grid_field,face_SD
           implicit none
           type(grid_field),intent(inout) :: GF
           type(grid_field),intent(in) :: surf
           type(face_SD),intent(in) :: FSD
           integer,intent(in) :: face
         end subroutine
       end interface

       contains

       ! *********************************************************************************
       ! ********************************* DIRICHLET *************************************
       ! *********************************************************************************

       subroutine Dirichlet_C_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Dirichlet_C_GF(GF,surf,&
                               FSD%G(face)%M(1:3)%i2(1),&
                               FSD%G(face)%M(1:3)%i2(2),&
                               FSD%I(face)%M(1:3)%i2(1),&
                               FSD%I(face)%M(1:3)%i2(2),&
                               FSD%i_2D(face)%i,&
                               0)
       end subroutine
       subroutine F_Dirichlet_C_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Dirichlet_C_implicit(ug,ui,bvals,x,y,p)
         call apply_Dirichlet_C_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Dirichlet_N_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Dirichlet_N_GF(GF,surf,&
                               FSD%G(face)%M(1:3)%i2(1),&
                               FSD%G(face)%M(1:3)%i2(2),&
                               FSD%B(face)%M(1:3)%i2(1),&
                               FSD%B(face)%M(1:3)%i2(2),&
                               FSD%I(face)%M(1:3)%i2(1),&
                               FSD%I(face)%M(1:3)%i2(2),&
                               FSD%i_2D(face)%i,&
                               0)
       end subroutine
       subroutine F_Dirichlet_N_GF(bulk,surf,G1,G2,B1,B2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,B1,B2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Dirichlet_N_implicit(ug,ub,ui,bvals,x,y,p)
         call apply_Dirichlet_N_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! *********************************** NEUMANN *************************************
       ! *********************************************************************************

       subroutine Neumann_C_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Neumann_C_GF(GF,surf,&
                             FSD%G(face)%M(1:3)%i2(1),&
                             FSD%G(face)%M(1:3)%i2(2),&
                             FSD%I(face)%M(1:3)%i2(1),&
                             FSD%I(face)%M(1:3)%i2(2),&
                             FSD%i_2D(face)%i,&
                             FSD%dh(face),&
                             FSD%nhat(face),&
                             0)
       end subroutine
       subroutine F_Neumann_C_GF(bulk,surf,G1,G2,I1,I2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Neumann_C_implicit(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Neumann_C_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f,&
                              dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Neumann_N_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Neumann_N_GF(GF,surf,&
                             FSD%G(face)%M(1:3)%i2(1),&
                             FSD%G(face)%M(1:3)%i2(2),&
                             FSD%I(face)%M(1:3)%i2(1),&
                             FSD%I(face)%M(1:3)%i2(2),&
                             FSD%i_2D(face)%i,&
                             FSD%dh(face),&
                             FSD%nhat(face),&
                             0)
       end subroutine
       subroutine F_Neumann_N_GF(bulk,surf,G1,G2,I1,I2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         real(cp),intent(in) :: dh,nhat
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Neumann_N_implicit(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Neumann_N_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f,&
                              dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ********************************** PERIODIC *************************************
       ! *********************************************************************************

       subroutine Periodic_C_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Periodic_C_GF(GF,surf,&
                              FSD%G(face)%M(1:3)%i2(1),&
                              FSD%G(face)%M(1:3)%i2(2),&
                              FSD%I_OPP(face)%M(1:3)%i2(1),&
                              FSD%I_OPP(face)%M(1:3)%i2(2),&
                              FSD%i_2D(face)%i,&
                              0)
       end subroutine
       subroutine F_Periodic_C_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! apply_Periodic_C(ug,ui_opp,x,y,p)
         call apply_Periodic_C_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Periodic_N_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Periodic_N_GF(GF,surf,&
                              FSD%G(face)%M(1:3)%i2(1),&
                              FSD%G(face)%M(1:3)%i2(2),&
                              FSD%I_OPP(face)%M(1:3)%i2(1),&
                              FSD%I_OPP(face)%M(1:3)%i2(2),&
                              FSD%i_2D(face)%i,&
                              0)
       end subroutine
       subroutine F_Periodic_N_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Periodic_N_implicit(ug,ui_opp,x,y,p)
         call apply_Periodic_N_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ************************************ ROBIN **************************************
       ! *********************************************************************************

       subroutine Robin_C_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Robin_C_GF(GF,surf,&
                           FSD%G(face)%M(1:3)%i2(1),&
                           FSD%G(face)%M(1:3)%i2(2),&
                           FSD%I(face)%M(1:3)%i2(1),&
                           FSD%I(face)%M(1:3)%i2(2),&
                           FSD%i_2D(face)%i,&
                           FSD%dh(face),&
                           FSD%nhat(face),&
                           0)
       end subroutine
       subroutine F_Robin_C_GF(bulk,surf,G1,G2,I1,I2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Robin_C_implicit(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Robin_C_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                            bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                            surf%f,&
                            dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Robin_N_implicit(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Robin_N_GF(GF,surf,&
                           FSD%G(face)%M(1:3)%i2(1),&
                           FSD%G(face)%M(1:3)%i2(2),&
                           FSD%I(face)%M(1:3)%i2(1),&
                           FSD%I(face)%M(1:3)%i2(2),&
                           FSD%B(face)%M(1:3)%i2(1),&
                           FSD%B(face)%M(1:3)%i2(2),&
                           FSD%i_2D(face)%i,&
                           FSD%dh(face),&
                           FSD%nhat(face),&
                           0)
       end subroutine
       subroutine F_Robin_N_GF(bulk,surf,G1,G2,I1,I2,B1,B2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,B1,B2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Robin_N_implicit(ug,ui,ub,bvals,dh,nhat,x,y,p)
         call apply_Robin_N_implicit(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                            bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                            bulk%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3)),&
                            surf%f,&
                            dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       end module