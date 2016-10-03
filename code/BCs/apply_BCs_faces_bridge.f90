       module apply_BCs_faces_bridge_mod
       use current_precision_mod
       use overlap_mod
       use RF_mod

       implicit none

       ! From apply_BCs_faces_raw.f90:
       !       call apply_Dirichlet_C(ug,ui,bvals,x,y,p)
       !       call apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
       !       call apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)
       !       call apply_Neumann_N(ug,ui,bvals,dh,nhat,x,y,p)
       !       call apply_Periodic_C(ug,ui_opp,x,y,p)
       !       call apply_Periodic_N(ug,ui_opp,x,y,p)
       !       call apply_Robin_C(ug,ui,bvals,dh,nhat,x,y,p)

       ! From here:
       !       call Dirichlet_C(bulk,surf,G,I,S,p)
       !       call Dirichlet_N(bulk,surf,G,B,I,S,p)
       !       call Neumann_C(bulk,surf,G,I,S,dh,nhat,p)
       !       call Neumann_N(bulk,surf,G,I,S,dh,nhat,p)
       !       call Periodic_C(bulk,surf,G,S,I_opp,p)
       !       call Periodic_N(bulk,surf,G,I_opp,S,p)
       !       call Robin_C(bulk,surf,G,I,S,dh,nhat,p)


       private
       public :: Dirichlet_C, Dirichlet_N
       public :: Neumann_C,   Neumann_N
       public :: Periodic_C,  Periodic_N
       public :: Robin_C

       interface Dirichlet_C;    module procedure F_Dirichlet_C;     end interface
       interface Dirichlet_N;    module procedure F_Dirichlet_N;     end interface
       interface Neumann_C;      module procedure F_Neumann_C;       end interface
       interface Neumann_N;      module procedure F_Neumann_N;       end interface
       interface Periodic_C;     module procedure F_Periodic_C;      end interface
       interface Periodic_N;     module procedure F_Periodic_N;      end interface
       interface Robin_C;        module procedure F_Robin_C;         end interface

       contains

       ! *********************************************************************************
       ! ********************************* DIRICHLET *************************************
       ! *********************************************************************************

       subroutine F_Dirichlet_C(bulk,surf,G,I,S,p) ! checked
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,I,S
         integer,intent(in) :: p
         call F_Dirichlet_C_I(bulk,surf,G(1:3)%i2(1),&
                                        G(1:3)%i2(2),&
                                        I(1:3)%i2(1),&
                                        I(1:3)%i2(2),&
                                        S(1:3)%i1(1),&
                                        S(1:3)%i1(2),(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Dirichlet_C_I(bulk,surf,G1,G2,I1,I2,S1,S2,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Dirichlet_C(ug,ui,bvals,x,y,p)
         call apply_Dirichlet_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                                iR(1),iR(2),p)
       end subroutine

       subroutine F_Dirichlet_N(bulk,surf,G,B,I,S,p) ! checked
         implicit none
         type(realField),intent(inout) :: B ! Bulk field
         type(realField),intent(in) :: S ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,B,I,S
         integer,intent(in) :: p
         call F_Dirichlet_N_I(bulk,surf,G(1:3)%i2(1),&
                                        G(1:3)%i2(2),&
                                        B(1:3)%i2(1),&
                                        B(1:3)%i2(2),&
                                        I(1:3)%i2(1),&
                                        I(1:3)%i2(2),&
                                        S(1:3)%i1(1),&
                                        S(1:3)%i1(2),(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Dirichlet_N_I(bulk,surf,G1,G2,B1,B2,I1,I2,S1,S2,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,B1,B2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Dirichlet_N(ug,ub,ui,bvals,x,y,p)
         call apply_Dirichlet_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                                bulk%f(B1(1):B2(1),B1(2):B2(2),B1(3):B2(3)),&
                                bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                                surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                                iR(1),iR(2),p)
       end subroutine

       ! *********************************************************************************
       ! *********************************** NEUMANN *************************************
       ! *********************************************************************************

       subroutine F_Neumann_C(bulk,surf,G,I,S,dh,nhat,p) ! checked
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,I,S
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         call F_Neumann_C_I(bulk,surf,G(1:3)%i2(1),&
                                      G(1:3)%i2(2),&
                                      I(1:3)%i2(1),&
                                      I(1:3)%i2(2),&
                                      S(1:3)%i1(1),&
                                      S(1:3)%i1(2),&
                                      dh,nhat,(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Neumann_C_I(bulk,surf,G1,G2,I1,I2,S1,S2,dh,nhat,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Neumann_C(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Neumann_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                              dh,nhat,iR(1),iR(2),p)
       end subroutine

       subroutine F_Neumann_N(bulk,surf,G,I,S,dh,nhat,p) ! checked
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,I,S
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         call F_Neumann_N_I(bulk,surf,G(1:3)%i2(1),&
                                      G(1:3)%i2(2),&
                                      I(1:3)%i2(1),&
                                      I(1:3)%i2(2),&
                                      S(1:3)%i1(1),&
                                      S(1:3)%i1(2),&
                                      dh,nhat,(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Neumann_N_I(bulk,surf,G1,G2,I1,I2,S1,S2,dh,nhat,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         real(cp),intent(in) :: dh,nhat
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Neumann_N(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Neumann_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                              bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                              surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                              dh,nhat,iR(1),iR(2),p)
       end subroutine

       ! *********************************************************************************
       ! ********************************** PERIODIC *************************************
       ! *********************************************************************************

       subroutine F_Periodic_C(bulk,surf,G,S,I_opp,p) ! checked
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,I_opp,S
         integer,intent(in) :: p
         call F_Periodic_C_I(bulk,surf,G(1:3)%i2(1),&
                                       G(1:3)%i2(2),&
                                   I_opp(1:3)%i2(1),&
                                   I_opp(1:3)%i2(2),(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Periodic_C_I(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! apply_Periodic_C(ug,ui_opp,x,y,p)
         call apply_Periodic_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               iR(1),iR(2),p)
       end subroutine

       subroutine F_Periodic_N(bulk,surf,G,I_opp,S,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,I_opp,S
         integer,intent(in) :: p
         call F_Periodic_N_I(bulk,surf,G(1:3)%i2(1),&
                                       G(1:3)%i2(2),&
                                   I_opp(1:3)%i2(1),&
                                   I_opp(1:3)%i2(2),(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Periodic_N_I(bulk,surf,G1,G2,I1,I2,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,I1,I2
         integer,dimension(2),intent(in) :: iR
         integer,intent(in) :: p
         ! call apply_Periodic_N(ug,ui_opp,x,y,p)
         call apply_Periodic_N(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                               bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                               iR(1),iR(2),p)
       end subroutine

       ! *********************************************************************************
       ! ************************************ ROBIN **************************************
       ! *********************************************************************************

       subroutine F_Robin_C(bulk,surf,G,I,S,dh,nhat,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         type(overlap),dimension(3),intent(in) :: G,I,S
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         call F_Robin_C_I(bulk,surf,G(1:3)%i2(1),&
                                    G(1:3)%i2(2),&
                                    I(1:3)%i2(1),&
                                    I(1:3)%i2(2),&
                                    S(1:3)%i1(1),&
                                    S(1:3)%i1(2),&
                                    dh,nhat,(/S%iR_2D(1),S%iR_2D(2)/),p)
       end subroutine
       subroutine F_Robin_C_I(bulk,surf,G1,G2,I1,I2,S1,S2,dh,nhat,iR,p)
         implicit none
         type(realField),intent(inout) :: bulk ! Bulk field
         type(realField),intent(in) :: surf ! Surface field (BCs)
         integer,dimension(3),intent(in) :: G1,G2,I1,I2,S1,S2
         integer,dimension(2),intent(in) :: iR
         real(cp),intent(in) :: dh,nhat
         integer,intent(in) :: p
         ! call apply_Robin_C(ug,ui,bvals,dh,nhat,x,y,p)
         call apply_Robin_C(bulk%f(G1(1):G2(1),G1(2):G2(2),G1(3):G2(3)),&
                            bulk%f(I1(1):I2(1),I1(2):I2(2),I1(3):I2(3)),&
                            surf%f(S1(1):S2(1),S1(2):S2(2),S1(3):S2(3)),&
                            dh,nhat,iR(1),iR(2),p)
       end subroutine

       end module