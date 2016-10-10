       module apply_BCs_faces_bridge_mod
       use current_precision_mod
       use overlap_mod
       use face_edge_corner_indexing_mod
       use apply_BCs_faces_raw_mod
       use domain_mod
       use data_location_mod
       use GF_mod
       use face_domain_mod

       ! From apply_BCs_faces_raw.f90:
       !       call apply_Dirichlet_C(ug,ui,bvals,x,y,p)
       !       call apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
       !       call apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)
       !       call apply_Neumann_N(ug,ui,bvals,dh,nhat,x,y,p)
       !       call apply_Periodic_C(ug,ui_opp,x,y,p)
       !       call apply_Periodic_N(ug,ui_opp,x,y,p)
       !       call apply_Robin_C(ug,ui,bvals,dh,nhat,x,y,p)

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
       public :: apply_BC_op

       public :: Dirichlet_C
       public :: Dirichlet_N
       public :: Neumann_C
       public :: Neumann_N
       public :: Periodic_C
       public :: Periodic_N
       public :: Robin_C

       abstract interface
         subroutine apply_BC_op(GF,surf,FD,i_sd)
           import grid_field,face_domain
           implicit none
           type(grid_field),intent(inout) :: GF
           type(grid_field),intent(in) :: surf
           type(face_domain),intent(in) :: FD
           integer,intent(in) :: i_sd
         end subroutine
       end interface

       contains

       ! *********************************************************************************
       ! ********************************* DIRICHLET *************************************
       ! *********************************************************************************

       subroutine Dirichlet_C(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Dirichlet_C_GF(GF,surf,&
                               FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                               FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                               FD%i%sd(i_sd)%OL_DL(1:3)%i2(1),&
                               FD%i%sd(i_sd)%OL_DL(1:3)%i2(2),&
                               FD%b%sd(i_sd)%OL_DL(1:3)%i1(1),&
                               FD%b%sd(i_sd)%OL_DL(1:3)%i1(2),&
                               FD%b%sd(i_sd)%i_2D,&
                               1)
       end subroutine
       subroutine F_Dirichlet_C_GF(bulk,surf,G1,G2,I1,I2,S1,S2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Dirichlet_C(ug,ui,bvals,x,y,p)
         call apply_Dirichlet_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Dirichlet_N(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Dirichlet_N_GF(GF,surf,&
                               FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                               FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                               FD%b%sd(i_sd)%OL_DL(1:3)%i2(1),&
                               FD%b%sd(i_sd)%OL_DL(1:3)%i2(2),&
                               FD%i%sd(i_sd)%OL_DL(1:3)%i2(1),&
                               FD%i%sd(i_sd)%OL_DL(1:3)%i2(2),&
                               FD%b%sd(i_sd)%OL_DL(1:3)%i1(1),&
                               FD%b%sd(i_sd)%OL_DL(1:3)%i1(2),&
                               FD%b%sd(i_sd)%i_2D,&
                               1)
       end subroutine
       subroutine F_Dirichlet_N_GF(bulk,surf,G1,G2,B1,B2,I1,I2,S1,S2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,B1,B2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
         call apply_Dirichlet_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                                surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! *********************************** NEUMANN *************************************
       ! *********************************************************************************

       subroutine Neumann_C(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Neumann_C_GF(GF,surf,&
                             FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                             FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                             FD%i%sd(i_sd)%OL_DL(1:3)%i2(1),&
                             FD%i%sd(i_sd)%OL_DL(1:3)%i2(2),&
                             FD%b%sd(i_sd)%OL_DL(1:3)%i1(1),&
                             FD%b%sd(i_sd)%OL_DL(1:3)%i1(2),&
                             FD%b%sd(i_sd)%i_2D,&
                             FD%dh(FD%g%sd(i_sd)%g_R1_id),&
                             FD%nhat(FD%g%sd(i_sd)%g_R1_id),&
                             1)
       end subroutine
       subroutine F_Neumann_C_GF(bulk,surf,G1,G2,I1,I2,S1,S2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Neumann_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                              dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       subroutine Neumann_N(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Neumann_N_GF(GF,surf,&
                             FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                             FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                             FD%i%sd(i_sd)%OL_DL(1:3)%i2(1),&
                             FD%i%sd(i_sd)%OL_DL(1:3)%i2(2),&
                             FD%b%sd(i_sd)%OL_DL(1:3)%i1(1),&
                             FD%b%sd(i_sd)%OL_DL(1:3)%i1(2),&
                             FD%b%sd(i_sd)%i_2D,&
                             FD%dh(FD%g%sd(i_sd)%g_R1_id),&
                             FD%nhat(FD%g%sd(i_sd)%g_R1_id),&
                             1)
       end subroutine
       subroutine F_Neumann_N_GF(bulk,surf,G1,G2,I1,I2,S1,S2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         real(cp),intent(in) :: dh,nhat
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Neumann_N(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Neumann_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                              dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ********************************** PERIODIC *************************************
       ! *********************************************************************************

       subroutine Periodic_C(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Periodic_C_GF(GF,surf,&
                              FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                              FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                              FD%i_opp%sd(i_sd)%OL_DL(1:3)%i2(1),&
                              FD%i_opp%sd(i_sd)%OL_DL(1:3)%i2(2),&
                              FD%b%sd(i_sd)%i_2D,&
                              1)
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

       subroutine Periodic_N(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Periodic_N_GF(GF,surf,&
                              FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                              FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                              FD%i_opp%sd(i_sd)%OL_DL(1:3)%i2(1),&
                              FD%i_opp%sd(i_sd)%OL_DL(1:3)%i2(2),&
                              FD%b%sd(i_sd)%i_2D,&
                              1)
       end subroutine
       subroutine F_Periodic_N_GF(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Periodic_N(ug,ui_opp,x,y,p)
         call apply_Periodic_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       ! *********************************************************************************
       ! ************************************ ROBIN **************************************
       ! *********************************************************************************

       subroutine Robin_C(GF,surf,FD,i_sd)
         implicit none
         type(grid_field),intent(inout) :: GF
         type(grid_field),intent(in) :: surf
         type(face_domain),intent(in) :: FD
         integer,intent(in) :: i_sd
         call F_Robin_C_GF(GF,surf,&
                           FD%g%sd(i_sd)%OL_DL(1:3)%i2(1),&
                           FD%g%sd(i_sd)%OL_DL(1:3)%i2(2),&
                           FD%i%sd(i_sd)%OL_DL(1:3)%i2(1),&
                           FD%i%sd(i_sd)%OL_DL(1:3)%i2(2),&
                           FD%b%sd(i_sd)%OL_DL(1:3)%i1(1),&
                           FD%b%sd(i_sd)%OL_DL(1:3)%i1(2),&
                           FD%b%sd(i_sd)%i_2D,&
                           FD%dh(FD%g%sd(i_sd)%g_R1_id),&
                           FD%nhat(FD%g%sd(i_sd)%g_R1_id),&
                           1)
       end subroutine
       subroutine F_Robin_C_GF(bulk,surf,G1,G2,I1,I2,S1,S2,iR,dh,nhat,p)
         implicit none
         type(grid_field),intent(inout) :: bulk
         type(grid_field),intent(in) :: surf
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Robin_C(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Robin_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                            bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                            surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                            dh,nhat,surf%s(iR(1)),surf%s(iR(2)),p)
       end subroutine

       end module