       module ops_embedExtract_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_)

       use current_precision_mod
       use mesh_mod
       use domain_mod
       use RF_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: extractFace,extractEdge,extractCC
       public :: embedFace
       public :: embedEdge
       public :: embedCC

       public :: embed_N_surface
       public :: extract_N_surface

       public :: embed_F_surface
       public :: extract_F_surface

       interface embedCC;    module procedure embedCC_SF;        end interface
       interface embedCC;    module procedure embedCC_VF;        end interface
       interface EE;         module procedure embedExtract_RF;   end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINE *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine embedExtract_RF(RF_out,RF_in,out1,out2,in1,in2)
         ! This is the embed/extract (EE) routine.
         implicit none
         type(realField),intent(in) :: RF_in
         type(realField),intent(inout) :: RF_out
         integer,dimension(3),intent(in) :: out1,out2,in1,in2
#ifdef _PARALLELIZE_EMBEDEXTRACT_
         integer :: i,j,k
         integer,dimension(3) :: suppress_warning
         suppress_warning = in2 ! in2 is not needed for parallel computations
         !$OMP PARALLEL DO
         do k=out1(3),out2(3);do j=out1(2),out2(2);do i=out1(1),out2(1)
         RF_out%f(i,j,k) = &
         RF_in %f(in1(1)+(i-out1(1)),in1(2)+(j-out1(2)),in1(3)+(k-out1(3)))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
#else
         RF_out%f(out1(1):out2(1),out1(2):out2(2),out1(3):out2(3)) = &
         RF_in %f( in1(1): in2(1), in1(2): in2(2), in1(3): in2(3))
#endif
       end subroutine

       ! *********************************************************************************
       ! *************************** CASE SPECIFIC ROUTINES ******************************
       ! *********************************************************************************

       subroutine extractFace(face_i,face_t,D) ! Extracts Lorentz force from induction to momentum
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(face_i%x%RF(D%sd(i)%g_in_id),face_t%x%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TNI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TNI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% NI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% NI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/))
           call EE(face_i%y%RF(D%sd(i)%g_in_id),face_t%y%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% CI1(1),D%sd(i)% NB1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% NB2(2),D%sd(i)% CE2(3)/))
           call EE(face_i%z%RF(D%sd(i)%g_in_id),face_t%z%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% NB2(3)/))
         enddo
       end subroutine

       subroutine extractEdge(edge_i,edge_t,D) ! Auxiliary (energy budget)
         implicit none
         type(VF),intent(inout) :: edge_i
         type(VF),intent(in) :: edge_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(edge_i%x%RF(D%sd(i)%g_in_id),edge_t%x%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)% CI1(1),D%sd(i)% NB1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% NB2(2),D%sd(i)% NB2(3)/))
           call EE(edge_i%y%RF(D%sd(i)%g_in_id),edge_t%y%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TNI1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TNI2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)% NI1(1),D%sd(i)% CE1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% NI2(1),D%sd(i)% CE2(2),D%sd(i)% NB2(3)/))
           call EE(edge_i%z%RF(D%sd(i)%g_in_id),edge_t%z%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TNI1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TNI2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% NI1(1),D%sd(i)% NB1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% NI2(1),D%sd(i)% NB2(2),D%sd(i)% CE2(3)/))
         enddo
       end subroutine

       subroutine embedEdge(Edge_t,Edge_i,D) ! Embeds velocity from momentum into induction
         implicit none
         type(VF),intent(inout) :: Edge_t
         type(VF),intent(in) :: Edge_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(Edge_t%x%RF(D%sd(i)%g_tot_id),Edge_i%x%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% NB1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% NB2(2),D%sd(i)% NB2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/))
           call EE(Edge_t%y%RF(D%sd(i)%g_tot_id),Edge_i%y%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% NI1(1),D%sd(i)% CE1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% NI2(1),D%sd(i)% CE2(2),D%sd(i)% NB2(3)/),&
            (/D%sd(i)%TNI1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TNI2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/))
           call EE(Edge_t%z%RF(D%sd(i)%g_tot_id),Edge_i%z%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% NI1(1),D%sd(i)% NB1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% NI2(1),D%sd(i)% NB2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TNI1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TNI2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/))
         enddo
       end subroutine

       subroutine embedCC_SF(CC_t,CC_i,D) ! For material properties
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(CC_t%RF(D%sd(i)%g_tot_id),CC_i%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/))
         enddo
       end subroutine

       subroutine embedFace(Face_t,Face_i,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(Face_t%x%RF(D%sd(i)%g_tot_id),Face_i%x%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% NI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% NI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TNI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TNI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/))
           call EE(Face_t%y%RF(D%sd(i)%g_tot_id),Face_i%y%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% NB1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% NB2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/))
           call EE(Face_t%z%RF(D%sd(i)%g_tot_id),Face_i%z%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% NB2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/))
         enddo
       end subroutine

       subroutine extractCC(CC_i,CC_t,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: CC_i
         type(VF),intent(in) :: CC_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(CC_i%x%RF(D%sd(i)%g_in_id),CC_t%x%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/))
           call EE(CC_i%y%RF(D%sd(i)%g_in_id),CC_t%y%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/))
           call EE(CC_i%z%RF(D%sd(i)%g_in_id),CC_t%z%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/))
         enddo
       end subroutine

       subroutine embedCC_VF(CC_t,CC_i,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(CC_t%x%RF(D%sd(i)%g_tot_id),CC_i%x%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/))
           call EE(CC_t%y%RF(D%sd(i)%g_tot_id),CC_i%y%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/))
           call EE(CC_t%z%RF(D%sd(i)%g_tot_id),CC_i%z%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CI1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CI2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TCI1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCI2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/))
         enddo
       end subroutine

       subroutine embed_N_surface(N_t,N_i,D) ! For surface mesh testing
         implicit none
         type(SF),intent(inout) :: N_t
         type(SF),intent(in) :: N_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(N_t%RF(D%sd(i)%g_tot_id),N_i%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% NB1(1),D%sd(i)% NB1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% NB2(1),D%sd(i)% NB2(2),D%sd(i)% NB2(3)/),&
            (/D%sd(i)%TNB1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TNB2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/))
         enddo
       end subroutine

       subroutine extract_N_surface(N_i,N_t,D) ! For surface mesh testing
         implicit none
         type(SF),intent(inout) :: N_i
         type(SF),intent(in) :: N_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(N_i%RF(D%sd(i)%g_in_id),N_t%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TNB1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TNB2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)% NB1(1),D%sd(i)% NB1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% NB2(1),D%sd(i)% NB2(2),D%sd(i)% NB2(3)/))
         enddo
       end subroutine

       subroutine embed_F_surface(Face_t,Face_i,D)
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(Face_t%x%RF(D%sd(i)%g_tot_id),Face_i%x%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% NB1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% NB2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TNB1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TNB2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/))
           call EE(Face_t%y%RF(D%sd(i)%g_tot_id),Face_i%y%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CE1(1),D%sd(i)% NB1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CE2(1),D%sd(i)% NB2(2),D%sd(i)% CE2(3)/),&
            (/D%sd(i)%TCE1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCE2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/))
           call EE(Face_t%z%RF(D%sd(i)%g_tot_id),Face_i%z%RF(D%sd(i)%g_in_id),&
            (/D%sd(i)% CE1(1),D%sd(i)% CE1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% CE2(1),D%sd(i)% CE2(2),D%sd(i)% NB2(3)/),&
            (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/))
         enddo
       end subroutine

       subroutine extract_F_surface(face_i,face_t,D)
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,D%s
           call EE(face_i%x%RF(D%sd(i)%g_in_id),face_t%x%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TNB1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TNB2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% NB1(1),D%sd(i)% CE1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% NB2(1),D%sd(i)% CE2(2),D%sd(i)% CE2(3)/))
           call EE(face_i%y%RF(D%sd(i)%g_in_id),face_t%y%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCE1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
            (/D%sd(i)%TCE2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)% CE1(1),D%sd(i)% NB1(2),D%sd(i)% CE1(3)/),&
            (/D%sd(i)% CE2(1),D%sd(i)% NB2(2),D%sd(i)% CE2(3)/))
           call EE(face_i%z%RF(D%sd(i)%g_in_id),face_t%z%RF(D%sd(i)%g_tot_id),&
            (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
            (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)% CE1(1),D%sd(i)% CE1(2),D%sd(i)% NB1(3)/),&
            (/D%sd(i)% CE2(1),D%sd(i)% CE2(2),D%sd(i)% NB2(3)/))
         enddo
       end subroutine


       end module