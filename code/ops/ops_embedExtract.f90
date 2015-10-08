       module ops_embedExtract_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_)

       use mesh_mod
       use domain_mod
       use RF_mod
       use SF_mod
       use VF_mod

       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: extractFace
       public :: embedFace
       public :: embedEdge
       public :: embedCC
       ! blic :: embedN

       logical,parameter :: includeTF = .false.
       integer,parameter :: includeDir = 0 ! (does nothing if includeTF = .false.)
                                       ! 0 (include all directions)
                                       ! 
                                       ! 1 (include x direction)
                                       ! 2 (include y direction)
                                       ! 3 (include z direction)
                                       ! 
                                       ! -1 (include all but x direction), not yet implemented
                                       ! -2 (include all but y direction), not yet implemented
                                       ! -3 (include all but z direction), not yet implemented

       ! integer,dimension(3),parameter :: includeDir = (/1,1,1/) ! include directions (x,y,z)

       interface embedCC;            module procedure embedCC_SF;            end interface
       interface embedCC;            module procedure embedCC_VF;            end interface

       interface EE;                 module procedure embedExtract_RF;       end interface


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
         integer,dimension(3) :: out_temp
         out_temp = out1-1
         !$OMP PARALLEL DO
         do k=out1(3),out2(3);do j=out1(2),out2(2);do j=out1(1),out2(1)
         RF_out%f(i,j,k) = &
         RF_in %f(i-out_temp(1),j-out_temp(2),k-out_temp(3))
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

       subroutine extractFace(face_i,face_t,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(domain),intent(in) :: D
         integer :: i
         do i=1,face_i%x%s
           call EE(face_i%x%RF(i),face_t%x%RF(i),&
            (/D%sd(i)%NB1(1), D%sd(i)%CI1(2), D%sd(i)%CI1(3)/),&
            (/D%sd(i)%NB2(1), D%sd(i)%CI2(2), D%sd(i)%CI2(3)/),&
           (/D%sd(i)%TNB1(1),D%sd(i)%TCI1(2),D%sd(i)%TCI1(3)/),&
           (/D%sd(i)%TNB2(1),D%sd(i)%TCI2(2),D%sd(i)%TCI2(3)/))
           call EE(face_i%y%RF(i),face_t%y%RF(i),&
            (/D%sd(i)%CI1(1) ,D%sd(i)%NB1(2), D%sd(i)%CI1(3)/),&
            (/D%sd(i)%CI2(1) ,D%sd(i)%NB2(2), D%sd(i)%CI2(3)/),&
           (/D%sd(i)%TCI1(1),D%sd(i)%TNB1(2),D%sd(i)%TCI1(3)/),&
           (/D%sd(i)%TCI2(1),D%sd(i)%TNB2(2),D%sd(i)%TCI2(3)/))
           call EE(face_i%z%RF(i),face_t%z%RF(i),&
            (/D%sd(i)%CI1(1) ,D%sd(i)%CI1(2) ,D%sd(i)%NB1(3)/),&
            (/D%sd(i)%CI2(1) ,D%sd(i)%CI2(2) ,D%sd(i)%NB2(3)/),&
           (/D%sd(i)%TCI1(1),D%sd(i)%TCI1(2),D%sd(i)%TNB1(3)/),&
           (/D%sd(i)%TCI2(1),D%sd(i)%TCI2(2),D%sd(i)%TNB2(3)/))
         enddo
       end subroutine

       subroutine embedEdge(Edge_t,Edge_i,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: Edge_t
         type(VF),intent(in) :: Edge_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,Edge_t%x%s
           call EE(Edge_t%x%RF(i),Edge_i%x%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TNB1(2),D%sd(i)%TNB1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TNB2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%NB1(2), D%sd(i)%NB1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%NB2(2), D%sd(i)%NB2(3)/))
           call EE(Edge_t%y%RF(i),Edge_i%y%RF(i),&
           (/D%sd(i)%TNB1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
           (/D%sd(i)%TNB2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)%NB1(1), D%sd(i)%CE1(2), D%sd(i)%NB1(3)/),&
            (/D%sd(i)%NB2(1), D%sd(i)%CE2(2), D%sd(i)%NB2(3)/))
           call EE(Edge_t%z%RF(i),Edge_i%z%RF(i),&
           (/D%sd(i)%TNB1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TNB2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%NB1(1), D%sd(i)%NB1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%NB2(1), D%sd(i)%NB2(2), D%sd(i)%CE2(3)/))
         enddo
       end subroutine

       subroutine embedFace(Face_t,Face_i,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: Face_t
         type(VF),intent(in) :: Face_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,Face_t%x%s
           call EE(Face_t%x%RF(i),Face_i%x%RF(i),&
           (/D%sd(i)%TNB1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TNB2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%NB1(1), D%sd(i)%CE1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%NB2(1), D%sd(i)%CE2(2), D%sd(i)%CE2(3)/))
           call EE(Face_t%y%RF(i),Face_i%y%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TNB1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TNB2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%NB1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%NB2(2), D%sd(i)%CE2(3)/))
           call EE(Face_t%z%RF(i),Face_i%z%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TNB1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TNB2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%CE1(2), D%sd(i)%NB1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%CE2(2), D%sd(i)%NB2(3)/))
         enddo
       end subroutine

       subroutine embedCC_VF(CC_t,CC_i,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,CC_t%x%s
           call EE(CC_t%x%RF(i),CC_i%x%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%CE1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%CE2(2), D%sd(i)%CE2(3)/))
           call EE(CC_t%y%RF(i),CC_i%y%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%CE1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%CE2(2), D%sd(i)%CE2(3)/))
           call EE(CC_t%z%RF(i),CC_i%z%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%CE1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%CE2(2), D%sd(i)%CE2(3)/))
         enddo
       end subroutine

       subroutine embedCC_SF(CC_t,CC_i,D)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(domain),intent(in) :: D
         integer :: i
         do i=1,CC_t%s
           call EE(CC_t%RF(i),CC_i%RF(i),&
           (/D%sd(i)%TCE1(1),D%sd(i)%TCE1(2),D%sd(i)%TCE1(3)/),&
           (/D%sd(i)%TCE2(1),D%sd(i)%TCE2(2),D%sd(i)%TCE2(3)/),&
            (/D%sd(i)%CE1(1), D%sd(i)%CE1(2), D%sd(i)%CE1(3)/),&
            (/D%sd(i)%CE2(1), D%sd(i)%CE2(2), D%sd(i)%CE2(3)/))
         enddo
       end subroutine


       end module