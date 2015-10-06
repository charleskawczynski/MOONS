       module subdomain_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_)

       use grid_mod
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

       public :: subdomain
       public :: init,delete

       public :: extractFace
       public :: embedFace
       public :: embedEdge
       public :: embedCC
       public :: embedN

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

       interface init;               module procedure init_subdomain;        end interface
       interface init;               module procedure init_subdomain_copy;   end interface
       interface delete;             module procedure delete_subdomain;      end interface

       interface EE;                 module procedure embedExtract_RF;       end interface
       interface EE;                 module procedure embedExtract_SF;       end interface

       interface embedCC;            module procedure embedCC_VF;            end interface
       interface embedCC;            module procedure embedCC_SF;            end interface

       interface embedCCInclude;     module procedure embedCCInclude_VF;     end interface
       interface embedCCInclude;     module procedure embedCCInclude_SF;     end interface
       interface embedCCExclude;     module procedure embedCCExclude_VF;     end interface
       interface embedCCExclude;     module procedure embedCCExclude_SF;     end interface
       interface embedCCIncludeDir;  module procedure embedCCIncludeDir_VF;  end interface
       interface embedCCIncludeDir;  module procedure embedCCIncludeDir_SF;  end interface

       type subdomain
         ! Legend:
         !        C = Cell Center
         !              E = exclude first exterior point
         !              I = include first exterior point
         !        N = Node
         !              B = include boundary point (for node data)
         !              I = include first exterior point
         !              E = exclude boundary point
         !        T = total domain, follow above legend following T
         ! 
         integer,dimension(3) :: CE1,CE2
         integer,dimension(3) :: CI1,CI2
         integer,dimension(3) :: NB1,NB2
         integer,dimension(3) :: NI1,NI2
         integer,dimension(3) :: NE1,NE2

         integer,dimension(3) :: TCE1,TCE2
         integer,dimension(3) :: TCI1,TCI2
         integer,dimension(3) :: TNB1,TNB2
         integer,dimension(3) :: TNI1,TNI2
         integer,dimension(3) :: TNE1,TNE2 ! Excludes wall normal values (momentum eq)
         type(grid) :: g_in,g_tot
       end type

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************** INIT / DELETE **********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine init_subdomain(SD,g_in,g_tot)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(grid),intent(in) :: g_in,g_tot
         call define_CE(SD,g_in%c(1),g_tot%c(1),1)
         call define_CE(SD,g_in%c(2),g_tot%c(2),2)
         call define_CE(SD,g_in%c(3),g_tot%c(3),3)
         call define_CI(SD,1)
         call define_CI(SD,2)
         call define_CI(SD,3)
         call define_T(SD,1)
         call define_T(SD,2)
         call define_T(SD,3)
         call define_N(SD)
         call init(SD%g_in,g_in)
         call init(SD%g_tot,g_tot)
       end subroutine

       subroutine init_subdomain_copy(SD_out,SD_in)
         implicit none
         type(subdomain),intent(inout) :: SD_out
         type(subdomain),intent(in) :: SD_in
         SD_out%CE1 = SD_in%CE1; SD_out%CE2 = SD_in%CE2
         SD_out%CI1 = SD_in%CI1; SD_out%CI2 = SD_in%CI2
         SD_out%NB1 = SD_in%NB1; SD_out%NB2 = SD_in%NB2
         SD_out%NI1 = SD_in%NI1; SD_out%NI2 = SD_in%NI2
         SD_out%NE1 = SD_in%NE1; SD_out%NE2 = SD_in%NE2
         SD_out%TCE1 = SD_in%TCE1; SD_out%TCE2 = SD_in%TCE2
         SD_out%TCI1 = SD_in%TCI1; SD_out%TCI2 = SD_in%TCI2
         SD_out%TNB1 = SD_in%TNB1; SD_out%TNB2 = SD_in%TNB2
         SD_out%TNI1 = SD_in%TNI1; SD_out%TNI2 = SD_in%TNI2
         SD_out%TNE1 = SD_in%TNE1; SD_out%TNE2 = SD_in%TNE2
         call init(SD_out%g_in,SD_in%g_in)
         call init(SD_out%g_tot,SD_in%g_tot)
       end subroutine

       subroutine delete_subdomain(SD)
         implicit none
         type(subdomain),intent(inout) :: SD
         SD%CE1 = 0; SD%CE2 = 0
         SD%CI1 = 0; SD%CI2 = 0
         SD%NB1 = 0; SD%NB2 = 0
         SD%NI1 = 0; SD%NI2 = 0
         SD%NE1 = 0; SD%NE2 = 0
         SD%TCE1 = 0; SD%TCE2 = 0
         SD%TCI1 = 0; SD%TCI2 = 0
         SD%TNB1 = 0; SD%TNB2 = 0
         SD%TNI1 = 0; SD%TNI2 = 0
         SD%TNE1 = 0; SD%TNE2 = 0
         call delete(SD%g_in)
         call delete(SD%g_tot)
       end subroutine

       subroutine define_CE(SD,c_in,c_tot,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         type(grid),intent(in) :: c_in,c_tot
         integer,intent(in) :: dir
         logical :: before,after
         integer :: i
         SD%CE1(dir) = 0
         SD%CE2(dir) = 0
         do i=1,c_tot%sc-1
           before = c_tot%hc( i ).lt.c_in%hmin
           after  = c_tot%hc(i+1).gt.c_in%hmin
           if (before.and.after) SD%CE1(dir) = i+1
         enddo
         do i=1,c_tot%sc-1
           before = c_tot%hc( i ).lt.c_in%hmax
           after  = c_tot%hc(i+1).gt.c_in%hmax
           if (before.and.after) SD%CE2(dir) = i
         enddo
         if (SD%CE1(dir).eq.0) stop 'Error: CE1 not defined in subdomain.f90'
         if (SD%CE2(dir).eq.0) stop 'Error: CE2 not defined in subdomain.f90'
       end subroutine

       subroutine define_CI(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%CI1(dir) = SD%CE1(dir)-1
         SD%CI2(dir) = SD%CE2(dir)+1
       end subroutine

       subroutine define_N(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%N1(dir) = SD%CE1(dir)-1
         SD%N2(dir) = SD%CE2(dir)+1
         SD%NE1(dir) = SD%N1(dir)+1
         SD%NE2(dir) = SD%N2(dir)-1
       end subroutine

       subroutine define_T(SD,dir)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,intent(in) :: dir
         SD%TN1(dir) = 2
         SD%TNE1(dir) = SD%TN1(dir)+1
         SD%CE1(dir) = 3
         SD%CI1(dir) = 2
         SD%TN2(dir)  = SD%g_tot%c(dir)sn-1
         SD%TNE2(dir) = SD%TN2(dir)-1
         SD%TCI2(dir) = SD%g_tot%c(dir)sc-1
         SD%TCE2(dir) = SD%g_tot%c(dir)sc-2
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** LOW-LEVEL ROUTINES ********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine embedExtract_SF(SF_out,SF_in,out1,out2,in1,in2)
         implicit none
         type(SF),intent(inout) :: SF_out
         type(SF),intent(in) :: SF_in
         integer,dimension(3),intent(in) :: out1,out2,in1,in2
         integer :: i
         do i=1,SF_out%s
          call EE(SF_out%RF(i),SF_in%RF(i),out1,out2,in1,in2)
         enddo
       end subroutine

       subroutine embedExtract_RF(RF_out,RF_in,out1,out2,in1,in2)
         ! This is the embed/extract (EE) routine.
         implicit none
         type(RF),intent(in) :: RF_in
         type(RF),intent(inout) :: RF_out
         integer,dimension(3),intent(in) :: out1,out2,in1,in2
         integer :: i,j,k
#ifdef _PARALLELIZE_EMBEDEXTRACT_
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
       ! *********************************************************************************
       ! *************************** CASE SPECIFIC ROUTINES ******************************
       ! *********************************************************************************
       ! *********************************************************************************

       ! *********************************************************************************
       ! ****************************** EXTRACT ROUTINES *********************************
       ! *********************************************************************************

       subroutine extractFace(face_i,face_t,SD,g)
         ! Although extractType = 2 is the most physical, some models may require
         ! other types, e.g. 2D flows with neumann / periodic BCs along a given
         ! direction. This warrents including fictitious cells to ensure no variations
         ! exist along a particular direction.
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         if (includeTF) then
           select case (includeDir)
           case (0); call extractFaceInclude(face_i,face_t,SD)
           case (1:3); call extractFaceIncludeDir(face_i,face_t,SD,g)
           case default
           stop 'Error: includeDir must = 1,2,3 in extractFace in ops_embedExtract.f90'
           end select
         else
           call extractFaceExclude(face_i,face_t,SD,g)
         endif
       end subroutine

       ! *********************************************************************************
       ! ******************************* EMBED ROUTINES **********************************
       ! *********************************************************************************

       subroutine embedEdge(edge_t,edge_i,SD,g)
         implicit none
         type(VF),intent(inout) :: edge_t
         type(VF),intent(in) :: edge_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         if (includeTF) then
           select case (includeDir)
           case (0); call embedEdgeInclude(edge_t,edge_i,SD,g)
           case (1:3); call embedEdgeIncludeDir(edge_t,edge_i,SD,g,includeDir)
           case default
           stop 'Error: includeDir must = 1,2,3 in embedEdge in ops_embedExtract.f90'
           end select
         else
           call embedEdgeExclude(edge_t,edge_i,SD,g)
         endif
       end subroutine

       subroutine embedFace(face_t,face_i,SD,g)
         implicit none
         type(VF),intent(inout) :: face_t
         type(VF),intent(in) :: face_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         if (includeTF) then
           select case (includeDir)
           case (0); call embedFaceInclude(face_t,face_i,SD,g)
           case (1:3); call embedFaceIncludeDir(face_t,face_i,SD,g,includeDir)
           case default
           stop 'Error: includeDir must = 1,2,3 in embedFace in ops_embedExtract.f90'
           end select
         else
           call embedFaceExclude(face_t,face_i,SD,g)
         endif
       end subroutine

       subroutine embedCC_VF(CC_t,CC_i,SD,g)
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         if (includeTF) then
           select case (includeDir)
           case (0); call embedCCInclude(CC_t,CC_i,SD)
           case (1:3); call embedCCIncludeDir(CC_t,CC_i,SD,g,includeDir)
           case default
           stop 'Error: includeDir must = 1,2,3 in embedCC in ops_embedExtract.f90'
           end select
         else
           call embedCCExclude(CC_t,CC_i,SD,g)
         endif
       end subroutine

       subroutine embedCC_SF(CC_t,CC_i,SD,g)
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         if (includeTF) then
           select case (includeDir)
           case (0); call embedCCInclude(CC_t,CC_i,SD)
           case (1:3); call embedCCIncludeDir(CC_t,CC_i,SD,g,includeDir)
           case default
           stop 'Error: includeDir must = 1,2,3 in embedCC in ops_embedExtract.f90'
           end select
         else
           call embedCCExclude(CC_t,CC_i,SD)
         endif
       end subroutine

       ! *********************************************************************************
       ! ****************************** EXTRACT ROUTINES *********************************
       ! *********************************************************************************

       subroutine extractFaceInclude(face_i,face_t,SD)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         call EE(face_i%x,face_t%x,&
         (/SD%TN1(1),SD%TCI1(2),SD%TCI1(3)/),&
         (/SD%TN2(1),SD%TCI2(2),SD%TCI2(3)/),&
          (/SD%N1(1), SD%CI1(2), SD%CI1(3)/),&
          (/SD%N2(1), SD%CI2(2), SD%CI2(3)/))
         call EE(face_i%y,face_t%y,&
         (/SD%TCI1(1),SD%TN1(2),SD%TCI1(3)/),&
         (/SD%TCI2(1),SD%TN2(2),SD%TCI2(3)/),&
          (/SD%CI1(1) ,SD%N1(2), SD%CI1(3)/),&
          (/SD%CI2(1) ,SD%N2(2), SD%CI2(3)/))
         call EE(face_i%z,face_t%z,&
         (/SD%TCI1(1),SD%TCI1(2),SD%TN1(3)/),&
         (/SD%TCI2(1),SD%TCI2(2),SD%TN2(3)/),&
          (/SD%CI1(1) ,SD%CI1(2) ,SD%N1(3)/),&
          (/SD%CI2(1) ,SD%CI2(2) ,SD%N2(3)/))
       end subroutine

       subroutine extractFaceExclude(face_i,face_t,SD)
         ! Excluding ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         call EE(face_i%x,face_t%x,&
         out1(/SD%TNE1(1),SD%TCE1(2),SD%TCE1(3)/),&
         out2(/SD%TNE2(1),SD%TCE2(2),SD%TCE2(3)/),&
         in1(/SD%NE1(1),SD%CE1(2),SD%CE1(3)/),&
         in2(/SD%NE2(1),SD%CE2(2),SD%CE2(3)/))
         call EE(face_i%y,face_t%y,&
         out1(/SD%TCE1(1),SD%TNE1(2),SD%TCE1(3)/),&
         out2(/SD%TCE2(1),SD%TNE2(2),SD%TCE2(3)/),&
         in1(/SD%CE1(1),SD%NE1(2),SD%CE1(3)/),&
         in2(/SD%CE2(1),SD%NE2(2),SD%CE2(3)/))
         call EE(face_i%z,face_t%z,&
         out1(/SD%TCE1(1),SD%TCE1(2),SD%TNE1(3)/),&
         out2(/SD%TCE2(1),SD%TCE2(2),SD%TNE2(3)/),&
         in1(/SD%CE1(1),SD%CE1(2),SD%NE1(3)/),&
         in2(/SD%CE2(1),SD%CE2(2),SD%NE2(3)/))
       end subroutine

       subroutine extractFaceIncludeDir(face_i,face_t,SD)
         ! Excluding ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         call EE(face_i%x,face_t%x,&
         out1(/SD%TN1(1),SD%TCE1(2),SD%TCE1(3)/),&
         out2(/SD%TN2(1),SD%TCE2(2),SD%TCE2(3)/),&
         in1(/SD%N1(1),SD%CE1(2),SD%CE1(3)/),&
         in2(/SD%N2(1),SD%CE2(2),SD%CE2(3)/))
         call EE(face_i%y,face_t%y,&
         out1(/SD%TCE1(1),SD%TN1(2),SD%TCE1(3)/),&
         out2(/SD%TCE2(1),SD%TN2(2),SD%TCE2(3)/),&
         in1(/SD%CE1(1),SD%N1(2),SD%CE1(3)/),&
         in2(/SD%CE2(1),SD%N2(2),SD%CE2(3)/))
         call EE(face_i%z,face_t%z,&
         out1(/SD%TCE1(1),SD%TCE1(2),SD%TN1(3)/),&
         out2(/SD%TCE2(1),SD%TCE2(2),SD%TN2(3)/),&
         in1(/SD%CE1(1),SD%CE1(2),SD%N1(3)/),&
         in2(/SD%CE2(1),SD%CE2(2),SD%N2(3)/))
       end subroutine

       subroutine extractFaceIncludeDir(face_i,face_t,SD,g)
         ! ** Probably the most physically correct one **
         ! Excluding ghost nodes / ghost cells
         ! Including boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,n2,ce1,ce2
         integer :: i
         n1  = SD%n1; n2  = SD%n2; ce1 = SD%ce1; ce2 = SD%ce2
         do i=1,face_i%x%s
           face_i%x%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1) = &
           face_t%x%RF(i)%f( n1(1)-1: n2(1)+1,ce1(2):ce2(2),ce1(3):ce2(3))
           face_i%y%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1) = &
           face_t%y%RF(i)%f(ce1(1):ce2(1), n1(2)-1: n2(2)+1,ce1(3):ce2(3))
           face_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:) = &
           face_t%z%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2), n1(3)-1: n2(3)+1)
         enddo
       end subroutine

       ! *********************************************************************************
       ! ******************************* EMBED ROUTINES **********************************
       ! *********************************************************************************

       subroutine embedFaceInclude(face_t,face_i,SD,g) ! Not yet tested
         ! Include ghost cells (possibly for periodic BCs)
         implicit none
         type(VF),intent(inout) :: face_t
         type(VF),intent(in) :: face_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,n2,ci1,ci2
         integer :: i
         n1  = SD%n1; n2  = SD%n2; ci1 = SD%ci1; ci2 = SD%ci2
         do i=1,face_t%x%s
         face_t%x%RF(i)%f( n1(1): n2(1),ci1(2):ci2(2),ci1(3):ci2(3)) = &
         face_i%x%RF(i)%f(2:g%c(1)%sn-1,:,:)
         face_t%y%RF(i)%f(ci1(1):ci2(1), n1(2): n2(2),ci1(3):ci2(3)) = &
         face_i%y%RF(i)%f(:,2:g%c(2)%sn-1,:)
         face_t%z%RF(i)%f(ci1(1):ci2(1),ci1(2):ci2(2), n1(3): n2(3)) = &
         face_i%z%RF(i)%f(:,:,2:g%c(3)%sn-1)
         enddo
       end subroutine

       subroutine embedFaceExclude(face_t,face_i,SD,g) ! Not yet tested
         ! Exclude wall boundary (the most physically likely case)
         implicit none
         type(VF),intent(inout) :: face_t
         type(VF),intent(in) :: face_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,n2,ce1,ce2
         integer :: i
         n1  = SD%n1; n2  = SD%n2; ce1 = SD%ce1; ce2 = SD%ce2
         do i=1,face_t%x%s
         face_t%x%RF(i)%f(n1(1):n2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
         face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         face_t%y%RF(i)%f(ce1(1):ce2(1),n1(2):n2(2),ce1(3):ce2(3)) = &
         face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         face_t%z%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),n1(3):n2(3)) = &
         face_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         enddo
       end subroutine

       subroutine embedFaceIncludeDir(face_t,face_i,SD,g,dir) ! Not yet tested
         ! Exclude wall boundary (the most physically likely case)
         implicit none
         type(VF),intent(inout) :: face_t
         type(VF),intent(in) :: face_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         integer,dimension(3) :: n1,n2,ce1,ce2,ci1,ci2
         integer :: i
         n1  = SD%n1; n2  = SD%n2; ce1 = SD%ce1
         ce2 = SD%ce2; ci1 = SD%ci1; ci2 = SD%ci2
         do i=1,face_t%x%s
         select case (dir)
         case (1); face_t%x%RF(i)%f(n1(1):n2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
                   face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   face_t%y%RF(i)%f(ci1(1):ci2(1),n1(2):n2(2),ce1(3):ce2(3)) = &
                   face_i%y%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   face_t%z%RF(i)%f(ci1(1):ci2(1),ce1(2):ce2(2),n1(3):n2(3)) = &
                   face_i%z%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         case (2); face_t%x%RF(i)%f(n1(1):n2(1),ci1(2):ci2(2),ce1(3):ce2(3)) = &
                   face_i%x%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sc-1)
                   face_t%y%RF(i)%f(ce1(1):ce2(1),n1(2):n2(2),ce1(3):ce2(3)) = &
                   face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
                   face_t%z%RF(i)%f(ce1(1):ce2(1),ci1(2):ci2(2),n1(3):n2(3)) = &
                   face_i%z%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sn-1)
         case (3); face_t%x%RF(i)%f(n1(1):n2(1),ce1(2):ce2(2),ci1(3):ci2(3)) = &
                   face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,:)
                   face_t%y%RF(i)%f(ce1(1):ce2(1),n1(2):n2(2),ci1(3):ci2(3)) = &
                   face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,:)
                   face_t%z%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),n1(3):n2(3)) = &
                   face_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         case default
         stop 'Error: dir must = 1,2,3 in embedFaceIncludeDir in ops_embedExtract.f90'
         end select
         enddo
       end subroutine

#ifdef _PARALLELIZE_EMBEDEXTRACT_
       subroutine embedEdgeInclude(edge_t,edge_i,SD,g)
         implicit none
         type(VF),intent(inout) :: edge_t
         type(VF),intent(in) :: edge_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,ci1
         integer :: i,j,k,t
         n1  = SD%n1; ci1 = SD%ci1
         do t=1,edge_t%x%s
         !$OMP PARALLEL DO
         do k=2,edge_i%x%RF(t)%s(3)-1; do j=2,edge_i%x%RF(t)%s(2)-1; do i=1,edge_i%x%RF(t)%s(1)
           edge_t%x%RF(t)%f(i-1+ci1(1),j-2+n1(2),k-2+n1(3)) = &
           edge_i%x%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%y%RF(t)%s(3)-1; do j=2,edge_i%y%RF(t)%s(2)-1; do i=1,edge_i%y%RF(t)%s(1)
           edge_t%y%RF(t)%f(i-2+n1(1),j-1+ci1(2),k-2+n1(3)) = &
           edge_i%y%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%z%RF(t)%s(3)-1; do j=2,edge_i%z%RF(t)%s(2)-1; do i=1,edge_i%z%RF(t)%s(1)
           edge_t%z%RF(t)%f(i-1+n1(1),j-2+n1(2),k-1+ci1(3)) = &
           edge_i%z%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         enddo
       end subroutine
#else
       subroutine embedEdgeInclude(edge_t,edge_i,SD,g)
         implicit none
         type(VF),intent(inout) :: edge_t
         type(VF),intent(in) :: edge_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,n2,ci1,ci2
         integer :: i
         n1  = SD%n1; n2  = SD%n2; ci1 = SD%ci1; ci2 = SD%ci2
         do i=1,edge_t%x%s
         edge_t%x%RF(i)%f(ci1(1):ci2(1),n1(2):n2(2),n1(3):n2(3)) = &
         edge_i%x%RF(i)%f(:,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         edge_t%y%RF(i)%f(n1(1):n2(1),ci1(2):ci2(2),n1(3):n2(3)) = &
         edge_i%y%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sn-1)
         edge_t%z%RF(i)%f(n1(1):n2(1),n1(2):n2(2),ci1(3):ci2(3)) = &
         edge_i%z%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sn-1,:)
         enddo
       end subroutine
#endif

#ifdef _PARALLELIZE_EMBEDEXTRACT_
       subroutine embedEdgeExclude(edge_t,edge_i,SD,g)
         implicit none
         type(VF),intent(inout) :: edge_t
         type(VF),intent(in) :: edge_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,ce1
         integer :: i,j,k,t
         n1  = SD%n1; ce1 = SD%ce1
         do t=1,edge_t%x%s
         !$OMP PARALLEL DO
         do k=2,edge_i%x%RF(t)%s(3)-1; do j=2,edge_i%x%RF(t)%s(2)-1; do i=2,edge_i%x%RF(t)%s(1)-1
           edge_t%x%RF(t)%f(i-2+ce1(1),j-2+n1(2),k-2+n1(3)) = &
           edge_i%x%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%y%RF(t)%s(3)-1; do j=2,edge_i%y%RF(t)%s(2)-1; do i=2,edge_i%y%RF(t)%s(1)-1
           edge_t%y%RF(t)%f(i-2+n1(1),j-2+ce1(2),k-2+n1(3)) = &
           edge_i%y%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%z%RF(t)%s(3)-1; do j=2,edge_i%z%RF(t)%s(2)-1; do i=2,edge_i%z%RF(t)%s(1)-1
           edge_t%z%RF(t)%f(i-1+n1(1),j-2+n1(2),k-2+ce1(3)) = &
           edge_i%z%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         enddo
       end subroutine
#else
       subroutine embedEdgeExclude(edge_t,edge_i,SD,g)
         implicit none
         type(VF),intent(inout) :: edge_t
         type(VF),intent(in) :: edge_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: n1,n2,ce1,ce2
         integer :: i
         n1  = SD%n1; n2  = SD%n2; ce1 = SD%ce1; ce2 = SD%ce2
         do i=1,edge_t%x%s
         edge_t%x%RF(i)%f(ce1(1):ce2(1),n1(2):n2(2),n1(3):n2(3)) = &
         edge_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         edge_t%y%RF(i)%f(n1(1):n2(1),ce1(2):ce2(2),n1(3):n2(3)) = &
         edge_i%y%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         edge_t%z%RF(i)%f(n1(1):n2(1),n1(2):n2(2),ce1(3):ce2(3)) = &
         edge_i%z%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         enddo
       end subroutine
#endif

       subroutine embedEdgeIncludeDir(edge_t,edge_i,SD,g,dir)
         implicit none
         type(VF),intent(inout) :: edge_t
         type(VF),intent(in) :: edge_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         integer,dimension(3) :: n1,n2,ce1,ce2,ci1,ci2
         integer :: i
         do i=1,edge_t%x%s
         n1  = SD%n1; n2  = SD%n2; ce1 = SD%ce1
         ce2 = SD%ce2; ci1 = SD%ci1; ci2 = SD%ci2
         select case(dir)
         case (1);edge_t%x%RF(i)%f(ci1(1):ci2(1),n1(2):n2(2),n1(3):n2(3)) = &
                  edge_i%x%RF(i)%f(:,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
                  edge_t%y%RF(i)%f(n1(1)-1:n2(1)+1,ce1(2):ce2(2),n1(3):n2(3)) = &
                  edge_i%y%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
                  edge_t%z%RF(i)%f(n1(1)-1:n2(1)+1,n1(2):n2(2),ce1(3):ce2(3)) = &
                  edge_i%z%RF(i)%f(:,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         case (2);edge_t%x%RF(i)%f(ce1(1):ce2(1),n1(2)-1:n2(2)+1,n1(3):n2(3)) = &
                  edge_i%x%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sn-1)
                  edge_t%y%RF(i)%f(n1(1):n2(1),ci1(2):ci2(2),n1(3):n2(3)) = &
                  edge_i%y%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sn-1)
                  edge_t%z%RF(i)%f(n1(1):n2(1),n1(2)-1:n2(2)+1,ce1(3):ce2(3)) = &
                  edge_i%z%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sc-1)
         case (3);edge_t%x%RF(i)%f(ce1(1):ce2(1),n1(2):n2(2),n1(3)-1:n2(3)+1) = &
                  edge_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,:)
                  edge_t%y%RF(i)%f(n1(1):n2(1),ce1(2):ce2(2),n1(3)-1:n2(3)+1) = &
                  edge_i%y%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,:)
                  edge_t%z%RF(i)%f(n1(1):n2(1),n1(2):n2(2),ci1(3):ci2(3)) = &
                  edge_i%z%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sn-1,:)
         case default
         stop 'Error: dir must = 1,2,3 in embedEdgeDir in ops_embedExtract.f90'
         end select
         enddo
       end subroutine

       subroutine embedCCInclude_VF(CC_t,CC_i,SD)
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         integer,dimension(3) :: ci1,ci2
         integer :: i
         do i=1,CC_t%x%s
         ci1 = SD%ci1; ci2 = SD%ci2
         CC_t%x%RF(i)%f(ci1(1):ci2(1),ci1(2):ci2(2),ci1(3):ci2(3)) = &
         CC_i%x%RF(i)%f
         CC_t%y%RF(i)%f(ci1(1):ci2(1),ci1(2):ci2(2),ci1(3):ci2(3)) = &
         CC_i%y%RF(i)%f
         CC_t%z%RF(i)%f(ci1(1):ci2(1),ci1(2):ci2(2),ci1(3):ci2(3)) = &
         CC_i%z%RF(i)%f
         enddo
       end subroutine

       subroutine embedCCExclude_VF(CC_t,CC_i,SD,g)
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: ce1,ce2
         integer :: i
         do i=1,CC_t%x%s
         ce1 = SD%ce1; ce2 = SD%ce2
         CC_t%x%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
         CC_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         CC_t%y%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
         CC_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         CC_t%z%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
         CC_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         enddo
       end subroutine

       subroutine embedCCIncludeDir_VF(CC_t,CC_i,SD,g,dir)
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         integer,dimension(3) :: ce1,ce2,ci1,ci2
         integer :: i
         do i=1,CC_t%x%s
         ce1 = SD%ce1; ce2 = SD%ce2; ci1 = SD%ci1; ci2 = SD%ci2
         select case (dir)
         case (1); CC_t%x%RF(i)%f(ci1(1):ci2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
                   CC_i%x%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   CC_t%y%RF(i)%f(ci1(1):ci2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
                   CC_i%y%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   CC_t%z%RF(i)%f(ci1(1):ci2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
                   CC_i%z%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         case (2); CC_t%x%RF(i)%f(ce1(1):ce2(1),ci1(2):ci2(2),ce1(3):ce2(3)) = &
                   CC_i%x%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
                   CC_t%y%RF(i)%f(ce1(1):ce2(1),ci1(2):ci2(2),ce1(3):ce2(3)) = &
                   CC_i%y%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
                   CC_t%z%RF(i)%f(ce1(1):ce2(1),ci1(2):ci2(2),ce1(3):ce2(3)) = &
                   CC_i%z%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
         case (3); CC_t%x%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ci1(3):ci2(3)) = &
                   CC_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
                   CC_t%y%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ci1(3):ci2(3)) = &
                   CC_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
                   CC_t%z%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ci1(3):ci2(3)) = &
                   CC_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
         case default
         stop 'Error: dir must = 1,2,3 in embedCCIncludeDir_VF in ops_embedExtract.f90'
         end select
         enddo
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! *************************** SCALAR FIELD ROUTINES *******************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine embedCCInclude_SF(CC_t,CC_i,SD)
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         integer,dimension(3) :: ci1,ci2
         integer :: i
         do i=1,CC_t%s
         ci1 = SD%ci1; ci2 = SD%ci2
         CC_t%RF(i)%f(ci1(1):ci2(1),ci1(2):ci2(2),ci1(3):ci2(3)) = &
         CC_i%RF(i)%f
         enddo
       end subroutine

       subroutine embedCCExclude_SF(CC_t,CC_i,SD)
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         integer,dimension(3) :: ce1,ce2
         integer :: i
         do i=1,CC_t%s
         ce1 = SD%ce1; ce2 = SD%ce2
         CC_t%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
         CC_i%RF(i)%f(2:SD%s(1)-1,2:SD%s(2)-1,2:SD%s(3)-1)
         enddo
       end subroutine

       subroutine embedCCIncludeDir_SF(CC_t,CC_i,SD,g,dir)
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         integer,dimension(3) :: ce1,ce2,ci1,ci2
         integer :: i
         do i=1,CC_t%s
         ce1 = SD%ce1; ce2 = SD%ce2; ci1 = SD%ci1; ci2 = SD%ci2
         select case (dir)
         case (1); CC_t%RF(i)%f(ci1(1):ci2(1),ce1(2):ce2(2),ce1(3):ce2(3)) = &
                   CC_i%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         case (2); CC_t%RF(i)%f(ce1(1):ce2(1),ci1(2):ci2(2),ce1(3):ce2(3)) = &
                   CC_i%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
         case (3); CC_t%RF(i)%f(ce1(1):ce2(1),ce1(2):ce2(2),ci1(3):ci2(3)) = &
                   CC_i%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
         case default
         stop 'Error: dir must = 1,2,3 in embedCCIncludeDir_SF in ops_embedExtract.f90'
         end select
         enddo
       end subroutine

       end module