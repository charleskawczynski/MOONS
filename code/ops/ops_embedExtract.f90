       module ops_embedExtract_mod
       ! Not all embed / extract routines are used for each method. For example,
       ! the CT method only uses embedEdge, and not embedCC or embedFace.
       ! 
       ! Pre-processor directives: (_PARALLELIZE_EMBEDEXTRACT_)

       use grid_mod
       use SF_mod
       use VF_mod
       use ops_aux_mod

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

       public :: init,subdomain

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

       interface init;               module procedure initSubdomain;         end interface

       interface embedCC;            module procedure embedCC_VF;            end interface
       interface embedCC;            module procedure embedCC_SF;            end interface

       interface embedCCInclude;     module procedure embedCCInclude_VF;     end interface
       interface embedCCInclude;     module procedure embedCCInclude_SF;     end interface
       interface embedCCExclude;     module procedure embedCCExclude_VF;     end interface
       interface embedCCExclude;     module procedure embedCCExclude_SF;     end interface
       interface embedCCIncludeDir;  module procedure embedCCIncludeDir_VF;  end interface
       interface embedCCIncludeDir;  module procedure embedCCIncludeDir_SF;  end interface

       type subdomain
         integer,dimension(3) :: s ! shape of interior grid cc-field
         integer,dimension(3) :: N
         integer,dimension(3) :: Nin1
         integer,dimension(3) :: Nin2
         integer,dimension(3) :: Nice1
         integer,dimension(3) :: Nice2
         integer,dimension(3) :: Nici1
         integer,dimension(3) :: Nici2
       end type

       contains

       subroutine initSubdomain(SD,Ni,Nwtop,Nwbot,g)
         implicit none
         type(subdomain),intent(inout) :: SD
         integer,dimension(3),intent(in) :: Ni,Nwtop,Nwbot
         type(grid),intent(in) :: g ! Internal grid
         integer :: i
         ! The INDEX OF CELLS section should not be modified.
         SD%N     = Ni+Nwtop+Nwbot
         SD%s     = (/(g%c(i)%sc,i=1,3)/)
         SD%Nin1  = Nwbot+2
         SD%Nice1 = Nwbot+2
         SD%Nici1 = Nwbot+1
         ! SD%Nin2  = SD%N-Nwtop+2 ! or Ni+Nwbot+2
         ! SD%Nice2 = SD%N-Nwtop+1 ! or Ni+Nwbot+1
         ! SD%Nici2 = SD%N-Nwtop+2 ! or Ni+Nwbot+2
         SD%Nin2  = Ni+Nwbot+2
         SD%Nice2 = Ni+Nwbot+1
         SD%Nici2 = Ni+Nwbot+2
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** INTERFACE ROUTINES ********************************
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

       subroutine embedN(N_t,N_i,SD,g)
         implicit none
         type(VF),intent(inout) :: N_t
         type(VF),intent(in) :: N_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: Nin1,Nin2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2
         do i=1,N_t%x%s
           N_t%x%RF(i)%f(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = &
           N_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
           N_t%y%RF(i)%f(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = &
           N_i%y%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
           N_t%z%RF(i)%f(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = &
           N_i%z%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         enddo
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! *************************** CASE SPECIFIC ROUTINES ******************************
       ! *********************************************************************************
       ! *********************************************************************************

       ! *********************************************************************************
       ! ****************************** EXTRACT ROUTINES *********************************
       ! *********************************************************************************

       subroutine extractFaceInclude(face_i,face_t,SD)
         ! Including ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         integer,dimension(3) :: Nin1,Nin2,Nici1,Nici2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         do i=1,face_i%x%s
           face_i%x%RF(i)%f = face_t%x%RF(i)%f(Nin1(1)-1:Nin2(1)+1,Nici1(2)  :Nici2(2)  ,Nici1(3)  :Nici2(3)  )
           face_i%y%RF(i)%f = face_t%y%RF(i)%f(Nici1(1)  :Nici2(1)  ,Nin1(2)-1:Nin2(2)+1,Nici1(3)  :Nici2(3)  )
           face_i%z%RF(i)%f = face_t%z%RF(i)%f(Nici1(1)  :Nici2(1)  ,Nici1(2)  :Nici2(2)  ,Nin1(3)-1:Nin2(3)+1)
         enddo
       end subroutine

#ifdef _PARALLELIZE_EMBEDEXTRACT_
       subroutine extractFaceExclude(face_i,face_t,SD,g)
         ! Excluding ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: Nin1,Nice1
         integer :: i,j,k,t
         Nin1  = SD%Nin1; Nice1 = SD%Nice1
         call zeroGhostPoints(face_i)
         do t=1,face_i%x%s
           !$OMP PARALLEL DO
           do k=2,face_i%x%RF(t)%s(3)-1; do j=2,face_i%x%RF(t)%s(2)-1; do i=2,face_i%x%RF(t)%s(1)-1
             face_i%x%RF(t)%f(i,j,k) = &
             face_t%x%RF(t)%f(i-2+Nin1(1),j-2+Nice1(2),k-2+Nice1(3))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
           !$OMP PARALLEL DO
           do k=2,face_i%x%RF(t)%s(3)-1; do j=2,face_i%x%RF(t)%s(2)-1; do i=2,face_i%x%RF(t)%s(1)-1
             face_i%y%RF(t)%f(i,j,k) = &
             face_t%y%RF(t)%f(i-2+Nice1(1),j-2+Nin1(2),k-2+Nice1(3))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
           !$OMP PARALLEL DO
           do k=2,face_i%x%RF(t)%s(3)-1; do j=2,face_i%x%RF(t)%s(2)-1; do i=2,face_i%x%RF(t)%s(1)-1
             face_i%z%RF(t)%f(i,j,k) = &
             face_t%z%RF(t)%f(i-2+Nice1(1),j-2+Nice1(2),k-2+Nin1(3))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         enddo
       end subroutine
#else
       subroutine extractFaceExclude(face_i,face_t,SD,g)
         ! Excluding ghost nodes / ghost cells / boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: Nin1,Nin2,Nice1,Nice2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nice1 = SD%Nice1; Nice2 = SD%Nice2
         call zeroGhostPoints(face_i)
         do i=1,face_i%x%s
           face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1) = &
           face_t%x%RF(i)%f( Nin1(1): Nin2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3))
           face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1) = &
           face_t%y%RF(i)%f(Nice1(1):Nice2(1), Nin1(2): Nin2(2),Nice1(3):Nice2(3))
           face_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1) = &
           face_t%z%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2), Nin1(3): Nin2(3))
         enddo
       end subroutine
#endif

       subroutine extractFaceIncludeDir(face_i,face_t,SD,g)
         ! ** Probably the most physically correct one **
         ! Excluding ghost nodes / ghost cells
         ! Including boundary values
         implicit none
         type(VF),intent(inout) :: face_i
         type(VF),intent(in) :: face_t
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: Nin1,Nin2,Nice1,Nice2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nice1 = SD%Nice1; Nice2 = SD%Nice2
         call zeroGhostPoints(face_i)
         do i=1,face_i%x%s
           face_i%x%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1) = &
           face_t%x%RF(i)%f( Nin1(1)-1: Nin2(1)+1,Nice1(2):Nice2(2),Nice1(3):Nice2(3))
           face_i%y%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1) = &
           face_t%y%RF(i)%f(Nice1(1):Nice2(1), Nin1(2)-1: Nin2(2)+1,Nice1(3):Nice2(3))
           face_i%z%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:) = &
           face_t%z%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2), Nin1(3)-1: Nin2(3)+1)
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
         integer,dimension(3) :: Nin1,Nin2,Nici1,Nici2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         do i=1,face_t%x%s
         face_t%x%RF(i)%f( Nin1(1): Nin2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)) = &
         face_i%x%RF(i)%f(2:g%c(1)%sn-1,:,:)
         face_t%y%RF(i)%f(Nici1(1):Nici2(1), Nin1(2): Nin2(2),Nici1(3):Nici2(3)) = &
         face_i%y%RF(i)%f(:,2:g%c(2)%sn-1,:)
         face_t%z%RF(i)%f(Nici1(1):Nici2(1),Nici1(2):Nici2(2), Nin1(3): Nin2(3)) = &
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
         integer,dimension(3) :: Nin1,Nin2,Nice1,Nice2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nice1 = SD%Nice1; Nice2 = SD%Nice2
         do i=1,face_t%x%s
         face_t%x%RF(i)%f(Nin1(1):Nin2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         face_t%y%RF(i)%f(Nice1(1):Nice2(1),Nin1(2):Nin2(2),Nice1(3):Nice2(3)) = &
         face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         face_t%z%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nin1(3):Nin2(3)) = &
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
         integer,dimension(3) :: Nin1,Nin2,Nice1,Nice2,Nici1,Nici2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nice1 = SD%Nice1
         Nice2 = SD%Nice2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         do i=1,face_t%x%s
         select case (dir)
         case (1); face_t%x%RF(i)%f(Nin1(1):Nin2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
                   face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   face_t%y%RF(i)%f(Nici1(1):Nici2(1),Nin1(2):Nin2(2),Nice1(3):Nice2(3)) = &
                   face_i%y%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   face_t%z%RF(i)%f(Nici1(1):Nici2(1),Nice1(2):Nice2(2),Nin1(3):Nin2(3)) = &
                   face_i%z%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         case (2); face_t%x%RF(i)%f(Nin1(1):Nin2(1),Nici1(2):Nici2(2),Nice1(3):Nice2(3)) = &
                   face_i%x%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sc-1)
                   face_t%y%RF(i)%f(Nice1(1):Nice2(1),Nin1(2):Nin2(2),Nice1(3):Nice2(3)) = &
                   face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
                   face_t%z%RF(i)%f(Nice1(1):Nice2(1),Nici1(2):Nici2(2),Nin1(3):Nin2(3)) = &
                   face_i%z%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sn-1)
         case (3); face_t%x%RF(i)%f(Nin1(1):Nin2(1),Nice1(2):Nice2(2),Nici1(3):Nici2(3)) = &
                   face_i%x%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,:)
                   face_t%y%RF(i)%f(Nice1(1):Nice2(1),Nin1(2):Nin2(2),Nici1(3):Nici2(3)) = &
                   face_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,:)
                   face_t%z%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nin1(3):Nin2(3)) = &
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
         integer,dimension(3) :: Nin1,Nici1
         integer :: i,j,k,t
         Nin1  = SD%Nin1; Nici1 = SD%Nici1
         do t=1,edge_t%x%s
         !$OMP PARALLEL DO
         do k=2,edge_i%x%RF(t)%s(3)-1; do j=2,edge_i%x%RF(t)%s(2)-1; do i=1,edge_i%x%RF(t)%s(1)
           edge_t%x%RF(t)%f(i-1+Nici1(1),j-2+Nin1(2),k-2+Nin1(3)) = &
           edge_i%x%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%y%RF(t)%s(3)-1; do j=2,edge_i%y%RF(t)%s(2)-1; do i=1,edge_i%y%RF(t)%s(1)
           edge_t%y%RF(t)%f(i-2+Nin1(1),j-1+Nici1(2),k-2+Nin1(3)) = &
           edge_i%y%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%z%RF(t)%s(3)-1; do j=2,edge_i%z%RF(t)%s(2)-1; do i=1,edge_i%z%RF(t)%s(1)
           edge_t%z%RF(t)%f(i-1+Nin1(1),j-2+Nin1(2),k-1+Nici1(3)) = &
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
         integer,dimension(3) :: Nin1,Nin2,Nici1,Nici2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         do i=1,edge_t%x%s
         edge_t%x%RF(i)%f(Nici1(1):Nici2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = &
         edge_i%x%RF(i)%f(:,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         edge_t%y%RF(i)%f(Nin1(1):Nin2(1),Nici1(2):Nici2(2),Nin1(3):Nin2(3)) = &
         edge_i%y%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sn-1)
         edge_t%z%RF(i)%f(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nici1(3):Nici2(3)) = &
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
         integer,dimension(3) :: Nin1,Nice1
         integer :: i,j,k,t
         Nin1  = SD%Nin1; Nice1 = SD%Nice1
         do t=1,edge_t%x%s
         !$OMP PARALLEL DO
         do k=2,edge_i%x%RF(t)%s(3)-1; do j=2,edge_i%x%RF(t)%s(2)-1; do i=2,edge_i%x%RF(t)%s(1)-1
           edge_t%x%RF(t)%f(i-2+Nice1(1),j-2+Nin1(2),k-2+Nin1(3)) = &
           edge_i%x%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%y%RF(t)%s(3)-1; do j=2,edge_i%y%RF(t)%s(2)-1; do i=2,edge_i%y%RF(t)%s(1)-1
           edge_t%y%RF(t)%f(i-2+Nin1(1),j-2+Nice1(2),k-2+Nin1(3)) = &
           edge_i%y%RF(t)%f(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         !$OMP PARALLEL DO
         do k=2,edge_i%z%RF(t)%s(3)-1; do j=2,edge_i%z%RF(t)%s(2)-1; do i=2,edge_i%z%RF(t)%s(1)-1
           edge_t%z%RF(t)%f(i-1+Nin1(1),j-2+Nin1(2),k-2+Nice1(3)) = &
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
         integer,dimension(3) :: Nin1,Nin2,Nice1,Nice2
         integer :: i
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nice1 = SD%Nice1; Nice2 = SD%Nice2
         do i=1,edge_t%x%s
         edge_t%x%RF(i)%f(Nice1(1):Nice2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = &
         edge_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
         edge_t%y%RF(i)%f(Nin1(1):Nin2(1),Nice1(2):Nice2(2),Nin1(3):Nin2(3)) = &
         edge_i%y%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
         edge_t%z%RF(i)%f(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nice1(3):Nice2(3)) = &
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
         integer,dimension(3) :: Nin1,Nin2,Nice1,Nice2,Nici1,Nici2
         integer :: i
         do i=1,edge_t%x%s
         Nin1  = SD%Nin1; Nin2  = SD%Nin2; Nice1 = SD%Nice1
         Nice2 = SD%Nice2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         select case(dir)
         case (1);edge_t%x%RF(i)%f(Nici1(1):Nici2(1),Nin1(2):Nin2(2),Nin1(3):Nin2(3)) = &
                  edge_i%x%RF(i)%f(:,2:g%c(2)%sn-1,2:g%c(3)%sn-1)
                  edge_t%y%RF(i)%f(Nin1(1)-1:Nin2(1)+1,Nice1(2):Nice2(2),Nin1(3):Nin2(3)) = &
                  edge_i%y%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sn-1)
                  edge_t%z%RF(i)%f(Nin1(1)-1:Nin2(1)+1,Nin1(2):Nin2(2),Nice1(3):Nice2(3)) = &
                  edge_i%z%RF(i)%f(:,2:g%c(2)%sn-1,2:g%c(3)%sc-1)
         case (2);edge_t%x%RF(i)%f(Nice1(1):Nice2(1),Nin1(2)-1:Nin2(2)+1,Nin1(3):Nin2(3)) = &
                  edge_i%x%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sn-1)
                  edge_t%y%RF(i)%f(Nin1(1):Nin2(1),Nici1(2):Nici2(2),Nin1(3):Nin2(3)) = &
                  edge_i%y%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sn-1)
                  edge_t%z%RF(i)%f(Nin1(1):Nin2(1),Nin1(2)-1:Nin2(2)+1,Nice1(3):Nice2(3)) = &
                  edge_i%z%RF(i)%f(2:g%c(1)%sn-1,:,2:g%c(3)%sc-1)
         case (3);edge_t%x%RF(i)%f(Nice1(1):Nice2(1),Nin1(2):Nin2(2),Nin1(3)-1:Nin2(3)+1) = &
                  edge_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sn-1,:)
                  edge_t%y%RF(i)%f(Nin1(1):Nin2(1),Nice1(2):Nice2(2),Nin1(3)-1:Nin2(3)+1) = &
                  edge_i%y%RF(i)%f(2:g%c(1)%sn-1,2:g%c(2)%sc-1,:)
                  edge_t%z%RF(i)%f(Nin1(1):Nin2(1),Nin1(2):Nin2(2),Nici1(3):Nici2(3)) = &
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
         integer,dimension(3) :: Nici1,Nici2
         integer :: i
         do i=1,CC_t%x%s
         Nici1 = SD%Nici1; Nici2 = SD%Nici2
         CC_t%x%RF(i)%f(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)) = &
         CC_i%x%RF(i)%f
         CC_t%y%RF(i)%f(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)) = &
         CC_i%y%RF(i)%f
         CC_t%z%RF(i)%f(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)) = &
         CC_i%z%RF(i)%f
         enddo
       end subroutine

       subroutine embedCCExclude_VF(CC_t,CC_i,SD,g)
         implicit none
         type(VF),intent(inout) :: CC_t
         type(VF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         integer,dimension(3) :: Nice1,Nice2
         integer :: i
         do i=1,CC_t%x%s
         Nice1 = SD%Nice1; Nice2 = SD%Nice2
         CC_t%x%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         CC_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         CC_t%y%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         CC_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         CC_t%z%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
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
         integer,dimension(3) :: Nice1,Nice2,Nici1,Nici2
         integer :: i
         do i=1,CC_t%x%s
         Nice1 = SD%Nice1; Nice2 = SD%Nice2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         select case (dir)
         case (1); CC_t%x%RF(i)%f(Nici1(1):Nici2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
                   CC_i%x%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   CC_t%y%RF(i)%f(Nici1(1):Nici2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
                   CC_i%y%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
                   CC_t%z%RF(i)%f(Nici1(1):Nici2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
                   CC_i%z%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         case (2); CC_t%x%RF(i)%f(Nice1(1):Nice2(1),Nici1(2):Nici2(2),Nice1(3):Nice2(3)) = &
                   CC_i%x%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
                   CC_t%y%RF(i)%f(Nice1(1):Nice2(1),Nici1(2):Nici2(2),Nice1(3):Nice2(3)) = &
                   CC_i%y%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
                   CC_t%z%RF(i)%f(Nice1(1):Nice2(1),Nici1(2):Nici2(2),Nice1(3):Nice2(3)) = &
                   CC_i%z%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
         case (3); CC_t%x%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nici1(3):Nici2(3)) = &
                   CC_i%x%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
                   CC_t%y%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nici1(3):Nici2(3)) = &
                   CC_i%y%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
                   CC_t%z%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nici1(3):Nici2(3)) = &
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
         integer,dimension(3) :: Nici1,Nici2
         integer :: i
         do i=1,CC_t%s
         Nici1 = SD%Nici1; Nici2 = SD%Nici2
         CC_t%RF(i)%f(Nici1(1):Nici2(1),Nici1(2):Nici2(2),Nici1(3):Nici2(3)) = &
         CC_i%RF(i)%f
         enddo
       end subroutine

       subroutine embedCCExclude_SF(CC_t,CC_i,SD)
         implicit none
         type(SF),intent(inout) :: CC_t
         type(SF),intent(in) :: CC_i
         type(subdomain),intent(in) :: SD
         integer,dimension(3) :: Nice1,Nice2
         integer :: i
         do i=1,CC_t%s
         Nice1 = SD%Nice1; Nice2 = SD%Nice2
         CC_t%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
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
         integer,dimension(3) :: Nice1,Nice2,Nici1,Nici2
         integer :: i
         do i=1,CC_t%s
         Nice1 = SD%Nice1; Nice2 = SD%Nice2; Nici1 = SD%Nici1; Nici2 = SD%Nici2
         select case (dir)
         case (1); CC_t%RF(i)%f(Nici1(1):Nici2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
                   CC_i%RF(i)%f(:,2:g%c(2)%sc-1,2:g%c(3)%sc-1)
         case (2); CC_t%RF(i)%f(Nice1(1):Nice2(1),Nici1(2):Nici2(2),Nice1(3):Nice2(3)) = &
                   CC_i%RF(i)%f(2:g%c(1)%sc-1,:,2:g%c(3)%sc-1)
         case (3); CC_t%RF(i)%f(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nici1(3):Nici2(3)) = &
                   CC_i%RF(i)%f(2:g%c(1)%sc-1,2:g%c(2)%sc-1,:)
         case default
         stop 'Error: dir must = 1,2,3 in embedCCIncludeDir_SF in ops_embedExtract.f90'
         end select
         enddo
       end subroutine



       end module