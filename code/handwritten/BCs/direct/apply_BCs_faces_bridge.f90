       module apply_BCs_faces_bridge_mod
       use current_precision_mod
       use apply_BCs_faces_raw_mod
       use apply_face_BC_op_mod
       use GF_mod
       use face_SD_mod
       use face_SD_extend_mod

       implicit none
       private
       public :: apply_face_BC_op

       public :: Dirichlet_C
       public :: Dirichlet_N
       public :: Neumann_C
       public :: Neumann_N
       public :: Periodic_C
       public :: Periodic_C_prescribed
       public :: Periodic_N
       public :: Periodic_N_prescribed
       public :: Robin_C
       public :: Robin_C_prescribed
       public :: Robin_N
       public :: Symmetric_C
       public :: Symmetric_N
       public :: antisymmetric_C
       public :: antisymmetric_N

       contains

       ! *********************************************************************************
       ! ********************************* DIRICHLET *************************************
       ! *********************************************************************************

       subroutine Dirichlet_C(GF,surf,FSD,face)
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
         call apply_Dirichlet_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%f,&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Dirichlet_N(GF,surf,FSD,face)
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
         call apply_Dirichlet_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%f,&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! *********************************** NEUMANN *************************************
       ! *********************************************************************************

       subroutine Neumann_C(GF,surf,FSD,face)
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
         call apply_Neumann_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f,&
                              dh*nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Neumann_N(GF,surf,FSD,face)
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
         call apply_Neumann_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f,&
                              dh*nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ********************************** PERIODIC *************************************
       ! *********************************************************************************

       subroutine Periodic_C(GF,surf,FSD,face)
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
         call apply_Periodic_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Periodic_C_prescribed(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Periodic_C_prescribed_GF(GF,surf,&
                                         FSD%G(face)%M(1:3)%i2(1),&
                                         FSD%G(face)%M(1:3)%i2(2),&
                                         FSD%i_2D(face)%i,&
                                         0)
       end subroutine
       subroutine F_Periodic_C_prescribed_GF(bulk,surf,G1,G2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! apply_Periodic_C(ug,ui_opp,x,y,p)
         call apply_Periodic_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               surf%f,&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Periodic_N(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Periodic_N_GF(GF,surf,&
                              FSD%G_periodic_N(face)%M(1:3)%i2(1),&
                              FSD%G_periodic_N(face)%M(1:3)%i2(2),&
                              FSD%I_OPP_periodic_N(face)%M(1:3)%i2(1),&
                              FSD%I_OPP_periodic_N(face)%M(1:3)%i2(2),&
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
         call apply_Periodic_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Periodic_N_prescribed(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Periodic_N_prescribed_GF(GF,surf,&
                                         FSD%G(face)%M(1:3)%i2(1),&
                                         FSD%G(face)%M(1:3)%i2(2),&
                                         FSD%i_2D(face)%i,&
                                         0)
       end subroutine
       subroutine F_Periodic_N_prescribed_GF(bulk,surf,G1,G2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         call apply_Periodic_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               surf%f,&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ************************************ ROBIN **************************************
       ! *********************************************************************************

       subroutine Robin_C(GF,surf,FSD,face)
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
                           FSD%c_w(face),&
                           0)
       end subroutine
       subroutine F_Robin_C_GF(bulk,surf,G1,G2,I1,I2,iR,dh,nhat,c_w,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat,c_w
         integer,intent(in) :: p
         call apply_Robin_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                            bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                            surf%f,&
                            dh,nhat,c_w,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Robin_C_prescribed(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Robin_C_prescribed_GF(GF,surf,&
                           FSD%G(face)%M(1:3)%i2(1),&
                           FSD%G(face)%M(1:3)%i2(2),&
                           FSD%i_2D(face)%i,&
                           0)
       end subroutine
       subroutine F_Robin_C_prescribed_GF(bulk,surf,G1,G2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         call apply_assign(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                           surf%f,&
                           surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Robin_N(GF,surf,FSD,face)
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
                           FSD%c_w(face),&
                           0)
       end subroutine
       subroutine F_Robin_N_GF(bulk,surf,G1,G2,I1,I2,B1,B2,iR,dh,nhat,c_w,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,B1,B2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat,c_w
         integer,intent(in) :: p
         call apply_Robin_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                            bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                            bulk%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3)),&
                            surf%f,&
                            dh,nhat,c_w,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ************************************ SYMMETRIC **********************************
       ! *********************************************************************************

       subroutine Symmetric_C(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Symmetric_C_GF(GF,surf,&
                               FSD%G(face)%M(1:3)%i2(1),&
                               FSD%G(face)%M(1:3)%i2(2),&
                               FSD%I(face)%M(1:3)%i2(1),&
                               FSD%I(face)%M(1:3)%i2(2),&
                               FSD%i_2D(face)%i,&
                               0)
       end subroutine
       subroutine F_Symmetric_C_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         call apply_Symmetric_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Symmetric_N(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_Symmetric_N_GF(GF,surf,&
                               FSD%G(face)%M(1:3)%i2(1),&
                               FSD%G(face)%M(1:3)%i2(2),&
                               FSD%I(face)%M(1:3)%i2(1),&
                               FSD%I(face)%M(1:3)%i2(2),&
                               FSD%i_2D(face)%i,&
                               0)
       end subroutine
       subroutine F_Symmetric_N_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         call apply_Symmetric_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ******************************* ANTI-SYMMETRIC **********************************
       ! *********************************************************************************

       subroutine antisymmetric_C(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_antisymmetric_C_GF(GF,surf,&
                                    FSD%G(face)%M(1:3)%i2(1),&
                                    FSD%G(face)%M(1:3)%i2(2),&
                                    FSD%I(face)%M(1:3)%i2(1),&
                                    FSD%I(face)%M(1:3)%i2(2),&
                                    FSD%i_2D(face)%i,&
                                    0)
       end subroutine
       subroutine F_antisymmetric_C_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         call apply_antisymmetric_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                     bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                     surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine antisymmetric_N(GF,surf,FSD,face)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_SD),intent(in) :: FSD
         integer,intent(in) :: face
         call F_antisymmetric_N_GF(GF,surf,&
                                    FSD%G(face)%M(1:3)%i2(1),&
                                    FSD%G(face)%M(1:3)%i2(2),&
                                    FSD%I(face)%M(1:3)%i2(1),&
                                    FSD%I(face)%M(1:3)%i2(2),&
                                    FSD%i_2D(face)%i,&
                                    0)
       end subroutine
       subroutine F_antisymmetric_N_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         call apply_antisymmetric_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                     bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                     surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       end module