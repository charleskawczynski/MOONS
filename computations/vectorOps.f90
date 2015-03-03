       module vectorOps_mod
       ! This module uses myDel frequently. For a reference,
       ! the derivative type is helpful to know:
       ! 
       ! 
       !  select case (diffType)
       !  case (1); call diff(dfdh,f,dhc,dhn,n,s)      ! Collocated CellCenter derivative
       !  case (2); call diff(dfdh,f,dhn,dhc,n,s)      ! Collocated Node derivative
       !  case (3); call upwind(dfdh,f,dhc,s)          ! Cell centered upwind derivative
       !  case (4); call upwind(dfdh,f,dhn,s)          ! Node centered upwind derivative
       !  end select
       ! 
       ! 
       ! Directions are frequently used in this code. 
       ! For clarity, some diagrams here show how the 
       ! directions are defined.
       ! 
       ! faceDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !      faceDir --->  \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       ! 
       ! 
       ! 
       ! edgeDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !                    \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       !                        -------> edgeDir
       ! 
       ! 
       use scalarField_mod
       use vectorField_mod
       use myDel_mod
       use solverSettings_mod
       use simParams_mod
       use myIO_mod
       use grid_mod
       use griddata_mod
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

       ! ------------------------------- INTERPOLATION ROUTINES ---------------------------------
       public :: myFaceAverage        ! call myFaceAverage(faceAve,f,gd,dir,aveLoc)
       public :: myFace2Node          ! call myFace2Node(nodeAverage,face,gd,dir)
       public :: myFace2CellCenter    ! call myFace2CellCenter(cellCenter,face,gd,faceDir)
       public :: myFace2Edge          ! call myFace2Edge(edge,gd,face,edgeDir)
       public :: myCellCenter2Face    ! call myCellCenter2Face(face,cellCenter,gd,faceDir)
       public :: myCellCenter2Edge    ! call myCellCenter2Edge(edge,cellCenter,gd,edgeDir)
       public :: myCellCenter2Node    ! call myCellCenter2Node(nodeAverage,cellCenter,dir,gd)
       public :: myNode2Face          ! call myNode2Face(face,node,gd,faceDir)
       public :: myNode2Edge          ! call myNode2Edge(edge,node,gd,edgeDir)

       ! ----------------------------------- OTHER ROUTINES ------------------------------------
       public :: myNodeMagnitude      ! call myNodeMagnitude(mag,u,v,w)
       public :: myCollocatedCross    ! call myCollocatedCross(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir)

       ! --------------------------------- DERIVATIVE ROUTINES ---------------------------------
       ! Face based derivatives
       public :: myFaceDiv            ! call myFaceDiv(divU,u,v,w,gd)
       public :: myFaceLap            ! call myFaceLap(lapF,f,gd,dir)
       public :: myFaceAdvect         ! call myFaceAdvect(psi,u,v,w,phi,gd)
       public :: myFaceAdvectDonor    ! call myFaceAdvectDonor(psi,u,v,w,phi,gd)
       public :: myFaceAdvectHybrid   ! call myFaceAdvectHybrid(psi,u,v,w,phi,gd)
       public :: myFaceCurl           ! call myFaceCurl(curlU,u,v,w,gd,dir)

       ! Cell-center based derivatives
       public :: myCC2CCDiv           ! call myCC2CCDiv(divU,u,v,w,gd)
       public :: CC2CCLap             ! call myCC2CCLap(lapU,u,gd)
       public :: myCC2FaceGrad        ! call myCC2FaceGrad(gradx,grady,gradz,p,gd)
       public :: myCC2CCDel           ! call myCC2CCDel(gradp,p,gd,dir)
       public :: myCCCurl             ! call myCCCurl(curl,u,v,w,gd,dir)
       public :: myCC2EdgeCurl        ! call myCC2EdgeCurl(curl,u,v,w,gd,dir)
       public :: myCCVaryDel          ! call myCCVaryDel(ddf,u,v,w,k,gd,dir1,dir2)

       ! Node-center based derivatives
       public :: myNodeGrad           ! call myNodeGrad(gradx,grady,gradz,f,gd)
       public :: myNode2NodeDel       ! call myNode2NodeDel(df,f,gd,dir)
       public :: myNodeDiv            ! call myNodeDiv(divU,u,v,w,gd)
       public :: myNodeLap            ! call myNodeLap(lapU,u,gd)
       public :: myNodeAdvect         ! call myNodeAdvect(psi,u,v,w,phi,gd)
       public :: myNodeCurl           ! call myNodeCurl(curlU,u,v,w,gd[,n])

       ! Edge-centered derivatives
       public :: myEdge2FaceCurl      ! call myEdge2FaceCurl(curl,u,v,w,gd,curlDir)
       
       ! ----------------------------- SPECIAL TERMS -------------------------------------------
       public :: myCCBfieldAdvect            ! call myCCBfieldAdvect(div,u,v,w,Bx,By,Bz,gd,dir)
       public :: myCCBfieldDiffuse           ! call myCCBfieldDiffuse(div,Bx,By,Bz,sigma,mu,gd,dir)
       public :: myCCJCrossB                 ! call myCCJCrossB(jCrossB,Bx,By,Bz,mu,gd,dir)

       public :: computeCurrent
       public :: embedVelocity

       interface interp;     module procedure interpO2;            end interface
       interface extrap;     module procedure extrapO2;            end interface
       interface CC2CCLap;   module procedure CC2CCLapVariCoeff;   end interface
       interface CC2CCLap;   module procedure CC2CCLapUnifCoeff;   end interface


       contains

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* INTERPOLATION ROUTINES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       ! ******************************* INTERPOLATION ROUTINES *********************************

       subroutine interpO2(f,g,gd,dir,gridType) ! Seems to be working.
         ! interpO2 interpolates g from the primary grid to the
         ! dual grid using a 2nd order accurate stencil for non-uniform 
         ! grids. f lives on the dual grid. It is expected that
         ! f lives between g. That is:
         ! 
         ! gridType = 1   :   f(cc grid), g(node/face grid)
         !                extrapolation required
         ! 
         !            f  g  f  g  f  g  f
         !            o--|--o--|--o--|--o   --> dir
         !                  *     *
         ! 
         ! gridType = 2   :   f(node/face grid), g(cc grid)
         !                no extrapolation required
         ! 
         !            g  f  g  f  g  f  g
         !            o--|--o--|--o--|--o      --> dir
         !               *     *     *
         ! 
         ! * = assigned in this routine. This way, the entire
         ! array of f and g may be passed, without having to 
         ! index.
         ! 
         ! Therefore, shape(g) = shape(f) + 1 along dir
         ! Otherwise, shape(g) = shape(f)
         ! 
         ! When gridType = 1, f is assumed to be located at the cell center.
         ! 
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: g
         real(dpn),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir,gridType
         integer :: i,j,k,t,x,y,z
         real(dpn) :: alpha
         real(dpn),dimension(:),allocatable :: hn,hc
         integer,dimension(3) :: sg,sf
         sg = shape(g); sf = shape(f)

         select case (dir)
         case (1); x=1;y=0;z=0
         if (gridType.eq.2) then
           allocate(hn(sg(1)-1)); hn = gd%c(dir)%hn
           allocate(hc(sg(1)));   hc = gd%c(dir)%hc
         endif
         case (2); x=0;y=1;z=0
         if (gridType.eq.2) then
           allocate(hn(sg(2)-1)); hn = gd%c(dir)%hn
           allocate(hc(sg(2)));   hc = gd%c(dir)%hc
         endif
         case (3); x=0;y=0;z=1
         if (gridType.eq.2) then
           allocate(hn(sg(3)-1)); hn = gd%c(dir)%hn
           allocate(hc(sg(3)));   hc = gd%c(dir)%hc
         endif
         case default
           write(*,*) 'Error: dir must = 1,2,3 in interpO2.';stop
         end select

         select case (gridType)
         case (1) ! f(cc grid), g(node/face grid)
         !            f  g  f  g  f  g  f
         !            o--|--o--|--o--|--o   --> dir
         !                  *     *
         
         !$OMP PARALLEL DO
         do k=1,sg(3)-z
           do j=1,sg(2)-y
             do i=1,sg(1)-x
               f(i+x,j+y,k+z) = (g(i,j,k)+g(i+x,j+y,k+z))/real(2.0,cp)
             enddo
           enddo
         enddo
         !$OMP END PARALLEL DO
         case (2) ! f(node/face grid), g(cc grid)
         !            g  f  g  f  g  f  g
         !            o--|--o--|--o--|--o      --> dir
         !               *     *     *

         !$OMP PARALLEL PRIVATE(alpha,t)
         !$OMP DO
         do k=1,sg(3)-z
           do j=1,sg(2)-y
             do i=1,sg(1)-x
               t = i*x + j*y + k*z
               alpha = (hn(t) - hc(t+1))/(hc(t) - hc(t+1))
               f(i,j,k) = g(i,j,k)*alpha +&
                             g(i+x,j+y,k+z)*(real(1.0,cp)-alpha)
             enddo
           enddo
         enddo
         !$OMP END DO
         !$OMP END PARALLEL
         case default
           write(*,*) 'gridType must be 1 or 2. Terminating.';stop
         end select

         if (gridType.eq.2) then
           deallocate(hn,hc)
         endif
       end subroutine

       subroutine extrapO2(f,g,gd,dir) ! Seems to be working.
         ! extrapO2 extrapolates g from the primary grid to the
         ! dual grid using a 2nd order accurate stencil for 
         ! non-uniform grids. f lives on the dual grid. 
         ! It is expected that f lives between g. That is:
         ! 
         ! For interpO2, this corresponds to grid = 1
         ! f(cc grid), g(node/face grid)
         ! 
         !            f  g  f  g  f  g  f
         !            o--|--o--|--o--|--o   --> dir
         !            *                 *
         ! 
         ! * = assigned in this routine. This way, the entire
         ! array of f and g may be passed, without having to 
         ! index.
         ! 
         ! Therefore, size(g) = size(f) + 1 along dir
         ! Otherwise, size(g) = size(f)
         ! 
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: g
         real(dpn),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn) :: a1,a2 ! differences in Taylor expansion
         real(dpn) :: c1,c2 ! Coefficients of function values
         real(dpn),dimension(:),allocatable :: hn,hc
         integer,dimension(3) :: sg,sf
         sg = shape(g); sf = shape(f)
         select case (dir)
         case (1); allocate(hn(sf(1)-1)); hn = gd%c(1)%hn
                   allocate(hc(sf(1)));   hc = gd%c(1)%hc
         case (2); allocate(hn(sf(2)-1)); hn = gd%c(2)%hn
                   allocate(hc(sf(2)));   hc = gd%c(2)%hc
         case (3); allocate(hn(sf(3)-1)); hn = gd%c(3)%hn
                   allocate(hc(sf(3)));   hc = gd%c(3)%hc
         case default
           write(*,*) 'Error: dir must = 1,2,3 in extrapO2.';stop
         end select
         ! BACKWARD EXTRAPOLATION (* = assigned)
         !            f  g  f  g  f  g  f
         !            o--|--o--|--o--|--o   --> dir
         !            *
         a1 = hc(1) - hn(1)
         a2 = hn(1) - hn(2)
         c1 = real(1.0,cp) + a1/a2
         c2 = -a1/a2
         select case (dir)
         case (1); f(1,:,:) = c1*g(1,:,:) + c2*g(2,:,:)
         case (2); f(:,1,:) = c1*g(:,1,:) + c2*g(:,2,:)
         case (3); f(:,:,1) = c1*g(:,:,1) + c2*g(:,:,2)
         end select
         ! FORWARD EXTRAPOLATION (* = assigned)
         !            f  g  f  g  f  g  f
         !            o--|--o--|--o--|--o   --> dir
         !                              *
         a1 = hc(sf(dir)) - hn(sf(dir)-1)
         a2 = hn(sg(dir)) - hn(sg(dir)-1)
         c1 = real(1.0,cp) + a1/a2
         c2 = -a1/a2
         select case (dir)
         case (1); f(sf(1),:,:)=c1*g(sg(1),:,:)+c2*g(sg(1)-1,:,:)
         case (2); f(:,sf(2),:)=c1*g(:,sg(2),:)+c2*g(:,sg(2)-1,:)
         case (3); f(:,:,sf(3))=c1*g(:,:,sg(3))+c2*g(:,:,sg(3)-1)
         end select
         deallocate(hn,hc)
       end subroutine

       ! ******************************* FACE INTERPOLATION ROUTINES *********************************

       subroutine myFace2CellCenter(cellCenter,face,gd,faceDir) ! Needs checking
         ! 1 interpolation routine (no allocation required)
         implicit none
         real(dpn),dimension(:,:,:),intent(in)    :: face        ! size = (Nn+1,Nt+2)
         real(dpn),dimension(:,:,:),intent(inout) :: cellCenter  ! size = (Nn+2,Nt+2)
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         cellCenter = real(0.0,cp)
         call interp(cellCenter,face,gd,faceDir,1)
         call extrap(cellCenter,face,gd,faceDir)
       end subroutine

       subroutine myFace2Edge(edge,face,gd,faceDir,edgeDir) ! Needs checking
         ! Case dependent multiple-interpolation routine (requires 1 or 3 interpolations)
         ! 
         ! This routine moves 
         !       face data along direction faceDir
         !    to edge data along direction edgeDir
         ! 
         ! edgeDir is defined as the direction along which the dimension is of size N+2
         ! faceDir is defined as the direction along which the dimension is of size N+1
         ! 
         ! Where N is the number of cells.
         ! 
         ! This means that there are 2 possible cases:
         !    edgeDir == faceDir       (requires 3 interpolations, 1 to cell center, 2 to edge)
         !    edgeDir ≠ faceDir        (requires 1 interpolation)
         ! 
         implicit none
         real(dpn),dimension(:,:,:),intent(in)    :: face ! size = (Nn+1,Nt+2)
         real(dpn),dimension(:,:,:),intent(inout) :: edge ! size = (Nn+1,Nt+1,Nt+2)
         type(grid),intent(in) :: gd
         integer,intent(in) :: edgeDir,faceDir
         real(dpn),dimension(:,:,:),allocatable :: tempCC
         integer :: orthDir
         if (edgeDir.ne.faceDir) then ! requires 1 interpolation (no allocations)
           orthDir = orthogonalDirection(edgeDir,faceDir)
           call interp(edge,face,gd,orthDir,2)
         else ! Requires 3 interpolations ()
          call myFace2CellCenter(tempCC,face,gd,faceDir)
          call myCellCenter2Edge(edge,tempCC,gd,edgeDir)
         endif
       end subroutine

       subroutine myFaceAverage(faceAve,face,gd,faceDir,aveLoc) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: face
         real(dpn),dimension(:,:,:),intent(inout) :: faceAve
         real(dpn),dimension(:,:,:),allocatable :: cellCenter
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir,aveLoc
         integer :: x,y,z
         integer,dimension(3) :: s
         s = shape(face)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must = 1,2,3 in myFaceAverage.';stop
         end select
         allocate(cellCenter(s(1)+x,s(2)+y,s(3)+z))
         call myFace2CellCenter(cellCenter,face,gd,faceDir)
         call myCellCenter2Face(faceAve,cellCenter,gd,aveLoc)
         deallocate(cellCenter)
       end subroutine

       subroutine myFace2Node(nodeAverage,face,gd,faceDir) ! Needs improvement, big time
         ! This needs to be fixed becuase only 2 interpolations are required.
         ! right now, there are 4, this introduces significant errors.
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: face
         real(dpn),dimension(:,:,:),intent(inout) :: nodeAverage
         real(dpn),dimension(:,:,:),allocatable :: tempcc
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         integer,dimension(3) :: s
         integer :: x,y,z
         s = shape(face)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must = 1,2,3 in myFace2Node.';stop
         end select
         allocate(tempcc(s(1)+x,s(2)+y,s(3)+z))
         call myFace2CellCenter(tempcc,face,gd,faceDir)
         call myCellCenter2Node(nodeAverage,tempcc,gd)
         deallocate(tempcc)
       end subroutine

       ! ******************************* CC INTERPOLATION ROUTINES *********************************

       subroutine myCellCenter2Face(face,cellCenter,gd,faceDir) ! Needs checking
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: cellCenter
         real(dpn),dimension(:,:,:),intent(inout) :: face
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         call interp(face,cellCenter,gd,faceDir,2)
       end subroutine

       subroutine myCellCenter2Node(nodeAverage,cellCenter,gd) ! Finished, needs improvement
         ! Try to allocate less
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: cellCenter
         real(dpn),dimension(:,:,:),intent(inout) :: nodeAverage
         type(grid),intent(in) :: gd
         real(dpn),dimension(:,:,:),allocatable :: temp,tempcc
         integer,dimension(3) :: sc
         sc = shape(cellCenter)

         nodeAverage = real(0.0,cp)
         allocate(tempcc(sc(1),sc(2),sc(3)))
         tempcc = cellCenter

         allocate(temp(sc(1)-1,sc(2),sc(3)))
         call myCellCenter2Face(temp,tempcc,gd,1)
         tempcc(1:sc(1)-1,:,:) = temp; tempcc(sc(1),:,:) = real(0.0,cp)
         deallocate(temp)

         allocate(temp(sc(1),sc(2)-1,sc(3)))
         call myCellCenter2Face(temp,tempcc,gd,2)
         tempcc(:,1:sc(2)-1,:) = temp; tempcc(:,sc(2),:) = real(0.0,cp)
         deallocate(temp)

         allocate(temp(sc(1),sc(2),sc(3)-1))
         call myCellCenter2Face(temp,tempcc,gd,3)
         tempcc(:,:,1:sc(3)-1) = temp; tempcc(:,:,sc(3)) = real(0.0,cp)
         deallocate(temp)
         
         nodeAverage = tempcc(1:sc(1)-1,1:sc(2)-1,1:sc(3)-1)
         deallocate(tempcc)
       end subroutine

       subroutine myCellCenter2Edge(edge,cellCenter,gd,edgeDir) ! Needs checking
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: cellCenter
         real(dpn),dimension(:,:,:),intent(inout) :: edge
         type(grid),intent(in) :: gd
         integer,intent(in) :: edgeDir
         real(dpn),dimension(:,:,:),allocatable :: faceTemp
         integer,dimension(3) :: s
         s = shape(cellCenter)

         select case (edgeDir)
         case(1); allocate(faceTemp(s(1),s(2)-1,s(3)))
         call myCellCenter2Face(faceTemp,cellCenter,gd,2)
         call myFace2Edge(edge,faceTemp,gd,2,1)
         case(2); allocate(faceTemp(s(1)-1,s(2),s(3)))
         call myCellCenter2Face(faceTemp,cellCenter,gd,1)
         call myFace2Edge(edge,faceTemp,gd,1,2)
         case(3); allocate(faceTemp(s(1)-1,s(2),s(3)))
         call myCellCenter2Face(faceTemp,cellCenter,gd,1)
         call myFace2Edge(edge,faceTemp,gd,1,3)
         case default
           write(*,*) 'Error: edgeDir must = 1,2,3 in myCellCenter2Edge.';stop
         end select
         deallocate(faceTemp)
       end subroutine

       ! ******************************* NODE INTERPOLATION ROUTINES *********************************

       subroutine myNode2Edge(edge,node,gd,edgeDir) ! Needs checking
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: node       ! size = Nn+1,Nt+1
         real(dpn),dimension(:,:,:),intent(inout) :: edge    ! size = Nn+2,Nt+1
         type(grid),intent(in) :: gd
         integer,intent(in) :: edgeDir
         call interp(edge,node,gd,edgeDir,1)
         call extrap(edge,node,gd,edgeDir)
       end subroutine

       subroutine myNode2Face(face,node,gd,faceDir) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: face
         real(dpn),dimension(:,:,:),intent(in) :: node
         real(dpn),dimension(:,:,:),allocatable :: tempe
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         integer,dimension(3) :: s
         s = shape(node)
         select case (faceDir)
         case (1)
           allocate(tempe(s(1),s(2)+1,s(3)))
           call myNode2Edge(tempe,node,gd,2)
           call myEdge2Face(face,tempe,gd,2,faceDir)
           deallocate(tempe)
         case (2)
           allocate(tempe(s(1)+1,s(2),s(3)))
           call myNode2Edge(tempe,node,gd,1)
           call myEdge2Face(face,tempe,gd,1,faceDir)
           deallocate(tempe)
         case (3)
           allocate(tempe(s(1)+1,s(2),s(3)))
           call myNode2Edge(tempe,node,gd,1)
           call myEdge2Face(face,tempe,gd,1,faceDir)
           deallocate(tempe)
         case default
           write(*,*) 'Error: faceDir must = 1,2,3 in myNode2Face.';stop
         end select
       end subroutine

       ! ******************************* EDGE INTERPOLATION ROUTINES *********************************

       subroutine myEdge2Face(face,edge,gd,edgeDir,faceDir) ! Needs checking
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: edge    ! size = Ne+2,N+1
         real(dpn),dimension(:,:,:),intent(inout) :: face ! size = Nf+1,Nt+2
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir,edgeDir
         integer :: orthDir
         orthDir = orthogonalDirection(edgeDir,faceDir)
         call interp(face,edge,gd,orthDir,1)
         call extrap(face,edge,gd,orthDir)
       end subroutine

       ! ******************************* OTHER ROUTINES *********************************

       subroutine myNodeMagnitude(mag,u,v,w) ! Finished
         ! This routine was made in order to compare norm(B) with
         ! results from Salah
         implicit none
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         real(dpn),dimension(:,:,:),intent(inout) :: mag
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(u)
         !$OMP PARALLEL DO
         do k=1,s(3)
          do j=1,s(2)
           do i=1,s(1)
            mag(i,j,k) = sqrt(u(i,j,k)**real(2.0,cp) +&
             v(i,j,k)**real(2.0,cp) + w(i,j,k)**real(2.0,cp))
           enddo
          enddo
         enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine myCollocatedCross(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir) ! Finished
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: AcrossB
         real(dpn),dimension(:,:,:),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,intent(in) :: dir
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(Ax)
         select case (dir)
         case (1)
         !$OMP PARALLEL DO
          do k=1,s(3)
            do j=1,s(2)
             do i=1,s(1)
                AcrossB(i,j,k) = Ay(i,j,k)*Bz(i,j,k) - Az(i,j,k)*By(i,j,k)
              enddo
            enddo
           enddo
         !$OMP END PARALLEL DO
         case (2)
         !$OMP PARALLEL DO
          do k=1,s(3)
            do j=1,s(2)
             do i=1,s(1)
                AcrossB(i,j,k) = -(Ax(i,j,k)*Bz(i,j,k) - Az(i,j,k)*Bx(i,j,k))
              enddo
            enddo
           enddo
         !$OMP END PARALLEL DO
         case (3)
         !$OMP PARALLEL DO
          do k=1,s(3)
            do j=1,s(2)
             do i=1,s(1)
                AcrossB(i,j,k) = Ax(i,j,k)*By(i,j,k) - Ay(i,j,k)*Bx(i,j,k)
              enddo
            enddo
           enddo
         !$OMP END PARALLEL DO
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myCollocatedCross.';stop
         end select
       end subroutine

       function orthogonalDirection(dir1,dir2) result(orthDir)
         implicit none
         integer,intent(in) :: dir1,dir2
         integer :: orthDir
         select case (dir1)
           case (1);
             select case (dir2)
             case (2); orthDir = 3
             case (3); orthDir = 2
             case default
               write(*,*) 'Error: bad input to orthogonalDirection'; stop
             end select
           case (2);
             select case (dir2)
             case (1); orthDir = 3
             case (3); orthDir = 1
             case default
               write(*,*) 'Error: bad input to orthogonalDirection'; stop
             end select
           case (3);
             select case (dir2)
             case (1); orthDir = 2
             case (2); orthDir = 1
             case default
               write(*,*) 'Error: bad input to orthogonalDirection'; stop
             end select
           case default
             write(*,*) 'Error: bad input to orthogonalDirection'; stop
         end select
#ifdef _DEBUG_VECTOROPS_
         if (dir1.eq.dir2) then
           write(*,*) 'There are no orthogonal directions';
           stop
         endif
#endif
       end function

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* FACE BASED DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myFaceDiv(divF,u,v,w,gd) ! Finished, needs improvements
         ! Try removing allocatables, consider passing in
         ! temporary cc field
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: divF
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         real(dpn),dimension(:,:,:),allocatable :: temp
         type(grid),intent(in) :: gd
         integer,dimension(3) :: s,sd
         divF = real(0.0,cp)
         sd = shape(divF)

         s = shape(u)
         allocate(temp(s(1),s(2),s(3)))
         call myDel(temp,u,gd,1,1,4,1) ! Padding avoids calcs on fictive cells
         divF(2:sd(1)-1,:,:) = divF(2:sd(1)-1,:,:) + temp(1:s(1)-1,:,:)
         deallocate(temp)

         s = shape(v)
         allocate(temp(s(1),s(2),s(3)))
         call myDel(temp,v,gd,1,2,4,1) ! Padding avoids calcs on fictive cells
         divF(:,2:sd(2)-1,:) = divF(:,2:sd(2)-1,:) + temp(:,1:s(2)-1,:)
         deallocate(temp)

         s = shape(w)
         allocate(temp(s(1),s(2),s(3)))
         call myDel(temp,w,gd,1,3,4,1) ! Padding avoids calcs on fictive cells
         divF(:,:,2:sd(3)-1) = divF(:,:,2:sd(3)-1) + temp(:,:,1:s(3)-1)
         deallocate(temp)
       end subroutine

       subroutine myFaceLap(lapF,f,gd,dir) ! Finished
         ! Consider migrating to momentum solver
         ! try to zero boundaries instead of the whole 3D volume
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: lapF
         real(dpn),dimension(:,:,:),intent(in) :: f
         real(dpn),dimension(:,:,:),allocatable :: temp
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         integer,dimension(3) :: s
         integer :: x,y,z
         s = shape(f)
         lapF = real(0.0,cp)
         select case (dir)
         case (1); x=2;y=1;z=1
         case (2); x=1;y=2;z=1
         case (3); x=1;y=1;z=2
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myFaceLap.'; stop
         end select
         allocate(temp(s(1),s(2),s(3)))
         ! Padding is necessary here!!!
         call myDel(temp,f,gd,2,1,x,1) ! Padding avoids calcs on fictive cells
         lapF = lapF + temp
         call myDel(temp,f,gd,2,2,y,1) ! Padding avoids calcs on fictive cells
         lapF = lapF + temp
         call myDel(temp,f,gd,2,3,z,1) ! Padding avoids calcs on fictive cells
         lapF = lapF + temp
         deallocate(temp)
       end subroutine

       subroutine myFaceAdvect(psi,u,v,w,phi,gd,dir) ! Finished
         ! Input shapes:
         ! 
         ! shape(uvw) = (Nn+1,Nt+1)
         ! shape(phi) = 
         !               if dir==1 -> shape(phi) = shape(u)
         !               if dir==2 -> shape(phi) = shape(v)
         !               if dir==3 -> shape(phi) = shape(w)
         ! 
         ! Auxiliary shapes:
         ! shape(dphi) = shape(phi)
         ! shape(faceAve) = shape(phi)
         ! 
         ! Output shapes:
         ! shape(psi) = shape(phi)
         !
         ! IMPORTANT NOTE
         !         The shape of the derivatives are the same size as the 
         !         functions, however, ONLY the wall facing normal
         !         components should be used when calculating the advection
         !         term. That is, the wall tangent values need to be excluded.
         !
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: psi
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w,phi
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp,tempAve
         integer,dimension(3) :: s

         s = shape(phi)

         allocate(temp(s(1),s(2),s(3)))
         allocate(tempAve(s(1),s(2),s(3)))

         psi = real(0.0,cp)
         ! -------------------------- u d/dx --------------------------
         call myFaceAverage(tempAve,u,gd,1,dir)
         if (dir.eq.1) then
               call myDel(temp,phi,gd,1,1,2,1) ! Padding avoids calcs on fictive cells
         else; call myDel(temp,phi,gd,1,1,1,1)
         endif
         psi = tempAve*temp

         ! -------------------------- v d/dy --------------------------
         call myFaceAverage(tempAve,v,gd,2,dir)

         if (dir.eq.2) then
               call myDel(temp,phi,gd,1,2,2,1) ! Padding avoids calcs on fictive cells
         else; call myDel(temp,phi,gd,1,2,1,1)
         endif
         psi = psi + tempAve*temp

         ! -------------------------- w d/dz --------------------------
         call myFaceAverage(tempAve,w,gd,3,dir)
         if (dir.eq.3) then
               call myDel(temp,phi,gd,1,3,2,1) ! Padding avoids calcs on fictive cells
         else; call myDel(temp,phi,gd,1,3,1,1)
         endif
         psi = psi + tempAve*temp

         deallocate(tempAve)
         deallocate(temp)
       end subroutine

       subroutine myFaceAdvectDonor(psi,u,v,w,ui,gd,faceDir) ! Finished, maybe improvements
         ! 
         ! This routine returns the ith component of
         ! 
         !                 d/dxj (uj ui)
         ! or
         !       d/dx (u ui) + d/dy (v ui) + d/dz (w ui)
         ! 
         ! Where faceDir is defined by ui.
         ! 
         ! It would be nice to reduce the number of allocated temps
         ! This routine does not seem to work when using the total velocity
         ! (including the walls)
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: psi
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w,ui
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         real(dpn),dimension(:,:,:),allocatable :: tempCC,tempAveCC
         real(dpn),dimension(:,:,:),allocatable :: tempAveE1,tempAveE2
         integer,dimension(3) :: s
         integer :: x,y,z,orthDir

         s = shape(ui)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must =1,2,3 in myFaceAdvectDonor.'; stop
         end select

         allocate(tempCC(   s(1)+x,s(2)+y,s(3)+z))
         allocate(tempAveCC(s(1)+x,s(2)+y,s(3)+z))

         psi = real(0.0,cp)
         ! -------------------------- d/dx (u u_i) --------------------------
         if (faceDir.eq.1) then
           call myFace2CellCenter(tempAveCC,u,gd,1)
           tempAveCC = tempAveCC*tempAveCC
           call myDel(tempCC,tempAveCC,gd,1,1,3,1)
           psi = psi + tempCC(1:s(1),:,:)
         else
           allocate(tempAveE1(s(1)-1,s(2),s(3)))
           allocate(tempAveE2(s(1)-1,s(2),s(3)))

           orthDir = orthogonalDirection(faceDir,1)
           call myFace2Edge(tempAveE1,u,gd,1,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call myDel(tempAveE2,tempAveE1,gd,1,1,4,1)
           psi(2:s(1)-1,:,:) = psi(2:s(1)-1,:,:) + tempAveE2(1:s(1)-2,:,:)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dy (v u_i) --------------------------
         if (faceDir.eq.2) then
           call myFace2CellCenter(tempAveCC,v,gd,2)
           tempAveCC = tempAveCC*tempAveCC
           call myDel(tempCC,tempAveCC,gd,1,2,3,1)
           psi = psi + tempCC(:,1:s(2),:)
         else
           allocate(tempAveE1(s(1),s(2)-1,s(3)))
           allocate(tempAveE2(s(1),s(2)-1,s(3)))
           orthDir = orthogonalDirection(faceDir,2)
           call myFace2Edge(tempAveE1,v,gd,2,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call myDel(tempAveE2,tempAveE1,gd,1,2,4,1) ! upwind from edge to cc

           psi(:,2:s(2)-1,:) = psi(:,2:s(2)-1,:) + tempAveE2(:,1:s(2)-2,:)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dz (w u_i) --------------------------
         if (faceDir.eq.3) then
           call myFace2CellCenter(tempAveCC,w,gd,3)
           tempAveCC = tempAveCC*tempAveCC
           call myDel(tempCC,tempAveCC,gd,1,3,3,1)
           psi = psi + tempCC(:,:,1:s(3))
         else
           allocate(tempAveE1(s(1),s(2),s(3)-1))
           allocate(tempAveE2(s(1),s(2),s(3)-1))

           orthDir = orthogonalDirection(faceDir,3)
           call myFace2Edge(tempAveE1,w,gd,3,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call myDel(tempAveE2,tempAveE1,gd,1,3,4,1)

           psi(:,:,2:s(3)-1) = psi(:,:,2:s(3)-1) + tempAveE2(:,:,1:s(3)-2)
           deallocate(tempAveE1,tempAveE2)
         endif

         deallocate(tempAveCC,tempCC)
       end subroutine

       subroutine myFaceAdvectHybrid(psi,u,v,w,ui,gd,faceDir) ! Finished, maybe improvements
         ! 
         ! This routine returns the ith component of
         ! 
         !                 d/dxj (uj ui)
         ! or
         !       d/dx (u ui) + d/dy (v ui) + d/dz (w ui)
         ! 
         ! Where faceDir is defined by ui.
         ! 
         ! It would be nice to reduce the number of allocated temps
         ! This routine does not seem to work when using the total velocity
         ! (including the walls)
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: psi
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w,ui
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         real(dpn),dimension(:,:,:),allocatable :: tempCC,tempAveCC
         real(dpn),dimension(:,:,:),allocatable :: tempAveE1,tempAveE2
         integer,dimension(3) :: s
         integer :: x,y,z,orthDir

         s = shape(ui)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must = 1,2,3 in myFaceAdvectHybrid.'; stop
         end select

         allocate(tempCC(   s(1)+x,s(2)+y,s(3)+z))
         allocate(tempAveCC(s(1)+x,s(2)+y,s(3)+z))

         psi = real(0.0,cp)
         ! -------------------------- d/dx (u u_i) --------------------------
         if (faceDir.eq.1) then
           call myFace2CellCenter(tempAveCC,u,gd,1)
           tempAveCC = tempAveCC*tempAveCC
           call myDel(tempCC,tempAveCC,gd,1,1,3,1)
           psi = psi + tempCC(1:s(1),:,:)
         else
           allocate(tempAveE1(s(1)-1,s(2),s(3)))
           allocate(tempAveE2(s(1)-1,s(2),s(3)))

           orthDir = orthogonalDirection(faceDir,1)
           call myFace2Edge(tempAveE1,u,gd,1,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call myDel(tempAveE2,tempAveE1,gd,1,1,4,1)
           psi(2:s(1)-1,:,:) = psi(2:s(1)-1,:,:) + tempAveE2(1:s(1)-2,:,:)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dy (v u_i) --------------------------
         if (faceDir.eq.2) then
           call myFace2CellCenter(tempAveCC,v,gd,2)
           tempAveCC = tempAveCC*tempAveCC
           call myDel(tempCC,tempAveCC,gd,1,2,3,1)
           psi = psi + tempCC(:,1:s(2),:)
         else
           allocate(tempAveE1(s(1),s(2)-1,s(3)))
           allocate(tempAveE2(s(1),s(2)-1,s(3)))
           orthDir = orthogonalDirection(faceDir,2)
           call myFace2Edge(tempAveE1,v,gd,2,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call myDel(tempAveE2,tempAveE1,gd,1,2,4,1) ! upwind from edge to cc

           psi(:,2:s(2)-1,:) = psi(:,2:s(2)-1,:) + tempAveE2(:,1:s(2)-2,:)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dz (w u_i) --------------------------
         if (faceDir.eq.3) then
           call myFace2CellCenter(tempAveCC,w,gd,3)
           tempAveCC = tempAveCC*tempAveCC
           call myDel(tempCC,tempAveCC,gd,1,3,3,1)
           psi = psi + tempCC(:,:,1:s(3))
         else
           allocate(tempAveE1(s(1),s(2),s(3)-1))
           allocate(tempAveE2(s(1),s(2),s(3)-1))

           orthDir = orthogonalDirection(faceDir,3)
           call myFace2Edge(tempAveE1,w,gd,3,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call myDel(tempAveE2,tempAveE1,gd,1,3,4,1)

           psi(:,:,2:s(3)-1) = psi(:,:,2:s(3)-1) + tempAveE2(:,:,1:s(3)-2)
           deallocate(tempAveE1,tempAveE2)
         endif

         deallocate(tempAveCC,tempCC)
       end subroutine

       subroutine myFaceCurl(curlU,u,v,w,gd,dir) ! Finished? Needs improvement, big time
         ! Broke when introducing walls. Need to replace Nx,Ny,Nz with shape
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: curlU
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: dwdy,dvdz
         real(dpn),dimension(:,:,:),allocatable :: dwdx,dudz
         real(dpn),dimension(:,:,:),allocatable :: dvdx,dudy
         integer,dimension(3) :: s
         integer :: Nx,Ny,Nz

         Nx = gd%c(1)%N; Ny = gd%c(2)%N; Nz = gd%c(3)%N

         select case (dir)
         case (1)
           ! x component
           s = shape(w)
           allocate(dwdy(s(1),s(2),s(3)))
           call myDel(dwdy,w,gd,1,2,4,0)

           s = shape(v)
           allocate(dvdz(s(1),s(2),s(3)))
           call myDel(dvdz,v,gd,1,3,4,0)

           curlU(:,2:Ny,2:Nz) = dwdy(:,2:Ny,2:Nz) - dvdz(:,2:Ny,2:Nz)
           deallocate(dwdy,dvdz)
         case (2)
           ! y component
           s = shape(w)
           allocate(dwdx(s(1),s(2),s(3)))
           call myDel(dwdx,w,gd,1,1,4,0)

           s = shape(u)
           allocate(dudz(s(1),s(2),s(3)))
           call myDel(dudz,u,gd,1,3,4,0)

           curlU(2:Nx,:,2:Nz) = -( dwdx(2:Nx,:,2:Nz) - dudz(2:Nx,:,2:Nz) )
           deallocate(dwdx,dudz)
         case (3)
           ! z component
           s = shape(v)
           allocate(dvdx(s(1),s(2),s(3)))
           call myDel(dvdx,v,gd,1,1,4,0)

           s = shape(u)
           allocate(dudy(s(1),s(2),s(3)))
           call myDel(dudy,u,gd,1,2,4,0)

           curlU(2:Nx,2:Ny,:) = dvdx(2:Nx,2:Ny,:) - dudy(2:Nx,2:Ny,:)
           deallocate(dvdx,dudy)
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myFaceCurl.'; stop
         end select
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* CELL BASED DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myCC2CCDiv(divU,u,v,w,gd) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: divU
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         real(dpn),dimension(:,:,:),allocatable :: temp
         type(grid),intent(in) :: gd
         integer,dimension(3) :: s
         s = shape(u)
         divU = real(0.0,cp)
         allocate(temp(s(1),s(2),s(3)))
         call myDel(temp,u,gd,1,1,1,0)
         divU = divU + temp
         call myDel(temp,v,gd,1,2,1,0)
         divU = divU + temp
         call myDel(temp,w,gd,1,3,1,0)
         divU = divU + temp
         deallocate(temp)
       end subroutine

       subroutine CC2CCLapUnifCoeff(lapU,u,gd) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: lapU
         real(dpn),dimension(:,:,:),intent(in) :: u
         real(dpn),dimension(:,:,:),allocatable :: temp
         type(grid),intent(in) :: gd
         integer,dimension(3) :: s
         s = shape(u)
         lapU = real(0.0,cp)

         allocate(temp(s(1),s(2),s(3)))

         call myDel(temp,u,gd,2,1,1,1) ! Padding avoids calcs on fictive cells
         lapU = lapU + temp

         call myDel(temp,u,gd,2,2,1,1) ! Padding avoids calcs on fictive cells
         lapU = lapU + temp
         
         call myDel(temp,u,gd,2,3,1,1) ! Padding avoids calcs on fictive cells
         lapU = lapU + temp

         deallocate(temp)
       end subroutine

       subroutine CC2CCLapVariCoeff(df,f,k,gd,dir) ! Finished, needs to be checked
         ! myCCVaryDel computes
         ! 
         !  df = ∇(k∇f)
         ! 
         ! Where f and k live at the cell center.
         ! Note that k may vary in space.
         ! 
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: df
         real(dpn),dimension(:,:,:),intent(in) :: f,k
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         integer :: x,y,z
         s = shape(f)
         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myCCVaryDel.'; stop
         end select

         allocate(temp1(s(1)-x,s(2)-y,s(3)-z))
         allocate(temp2(s(1)-x,s(2)-y,s(3)-z))
         call myDel(temp1,f,gd,1,dir,3,0)
         call interp(temp2,k,gd,dir,2)
         temp1 = temp1*temp2
         call myDel(temp2,temp1,gd,1,dir,4,0)
         df(1+x:s(1)-x,1+y:s(2)-y,1+z:s(3)-z) = temp2(1:s(1)-2*x,1:s(2)-2*y,1:s(3)-2*z)
         deallocate(temp1,temp2)
       end subroutine

       subroutine myCC2FaceGrad(gradx,grady,gradz,p,gd) ! Finsihed
         ! This cell center grad operator takes cell centered data,
         ! takes the gradient, and the result lands on the cell faces (approximately).
         ! Ask Eldgredge if using the collocated first derivative formula
         ! is more appropriate here.
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(dpn),dimension(:,:,:),intent(in) :: p
         type(grid),intent(in) :: gd
         call myDel(gradx,p,gd,1,1,3,1) ! Padding avoids calcs on fictive cells
         call myDel(grady,p,gd,1,2,3,1) ! Padding avoids calcs on fictive cells
         call myDel(gradz,p,gd,1,3,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine myCC2CCDel(gradp,p,gd,dir) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: gradp
         real(dpn),dimension(:,:,:),intent(in) :: p
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         call myDel(gradp,p,gd,1,dir,1,0)
       end subroutine

       subroutine myCCCurl(curl,u,v,w,gd,dir) ! Finished, needs to be checked
         ! myCCCurl computes the component dir of the cell center-based
         ! curl using a collocated derivative. 
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: curl
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         s = shape(u)

         allocate(temp1(s(1),s(2),s(3)))
         allocate(temp2(s(1),s(2),s(3)))

         select case (dir)
         case (1)
           call myDel(temp1,w,gd,1,2,1,0)
           call myDel(temp2,v,gd,1,3,1,0)
           curl = temp1 - temp2
         case (2)
           call myDel(temp1,w,gd,1,1,1,0)
           call myDel(temp2,u,gd,1,3,1,0)
           curl = -( temp1 - temp2 )
         case (3)
           call myDel(temp1,v,gd,1,1,1,0)
           call myDel(temp2,u,gd,1,2,1,0)
           curl = temp1 - temp2
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myCCCurl.'; stop
         end select

         deallocate(temp1,temp2)
       end subroutine

       subroutine myCC2EdgeCurl(curl,u,v,w,gd,dir) ! Finished, needs to be checked
         ! myCCCurl computes the component dir of the curl of (u,v,w). The
         ! result lives on the edge. 
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: curl
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: tempfx,tempfy,tempfz
         real(dpn),dimension(:,:,:),allocatable :: tempe
         integer,dimension(3) :: s
         integer :: x,y,z
         s = shape(u)

         select case (dir)
         case(1); x=0;y=1;z=1
         case(2); x=1;y=0;z=1
         case(3); x=1;y=1;z=0
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myCC2EdgeCurl.'; stop
         end select
         allocate(tempe(s(1)-x,s(2)-y,s(3)-z))
         select case (dir)
         case (1)
           allocate(tempfy(s(1),s(2)-1,s(3)))
           allocate(tempfz(s(1),s(2),s(3)-1))
           call myCellCenter2Face(tempfz,w,gd,3)
           call myCellCenter2Face(tempfy,v,gd,2)
           call myDel(tempe,tempfz,gd,1,2,3,0) ! dw/dy
           curl = tempe(1:s(1),1:s(2)-1,1:s(3)-1)
           call myDel(tempe,tempfy,gd,1,3,3,0) ! dv/dz
           curl = curl - tempe(1:s(1),1:s(2)-1,1:s(3)-1)
           deallocate(tempfy,tempfz)
         case (2)
           allocate(tempfx(s(1)-1,s(2),s(3)))
           allocate(tempfz(s(1),s(2),s(3)-1))
           call myCellCenter2Face(tempfz,w,gd,3)
           call myCellCenter2Face(tempfx,u,gd,1)
           call myDel(tempe,tempfz,gd,1,1,3,0) ! dw/dx
           curl = tempe(1:s(1)-1,1:s(2),1:s(3)-1)
           call myDel(tempe,tempfx,gd,1,3,3,0) ! du/dz
           curl = -(curl - tempe(1:s(1)-1,1:s(2),1:s(3)-1))
           deallocate(tempfx,tempfz)
         case (3)
           allocate(tempfx(s(1)-1,s(2),s(3)))
           allocate(tempfy(s(1),s(2)-1,s(3)))
           call myCellCenter2Face(tempfy,v,gd,2)
           call myCellCenter2Face(tempfx,u,gd,1)
           call myDel(tempe,tempfy,gd,1,1,3,0) ! dv/dx
           curl = tempe(1:s(1)-1,1:s(2)-1,1:s(3))
           call myDel(tempe,tempfx,gd,1,2,3,0) ! du/dy
           curl = curl - tempe(1:s(1)-1,1:s(2)-1,1:s(3))
           deallocate(tempfx,tempfy)
         end select
         deallocate(tempe)
       end subroutine

       subroutine myCCVaryDel(df,f,k,gd,dir1,dir2) ! Finished, needs to be checked
         ! myCCVaryDel computes
         ! 
         !     d/dxj (k df/dxi)
         !        ^         ^
         !        |         |
         !       dir2      dir1
         ! 
         ! f and df live at the cell center.
         !
         !     dir1 == dir2
         !            upwind -> interpolate coefficient -> upwind
         !     dir1 ≠ dir2
         !            CD2 -> CD2
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: df
         real(dpn),dimension(:,:,:),intent(in) :: f,k
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir1,dir2
         real(dpn),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         integer :: x,y,z
         s = shape(f)
         select case (dir1)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir1 must = 1,2,3 in myCCVaryDel.'; stop
         end select

         if (dir1.eq.dir2) then
           allocate(temp1(s(1)-x,s(2)-y,s(3)-z))
           allocate(temp2(s(1)-x,s(2)-y,s(3)-z))
           call myDel(temp1,f,gd,1,dir1,3,0)
           call interp(temp2,k,gd,dir1,2)
           temp1 = temp1*temp2
           call myDel(temp2,temp1,gd,1,dir2,4,0)
           df(1+x:s(1)-x,1+y:s(2)-y,1+z:s(3)-z) = temp2(1:s(1)-2*x,1:s(2)-2*y,1:s(3)-2*z)
           deallocate(temp1,temp2)
         else
           allocate(temp1(s(1),s(2),s(3)))
           allocate(temp2(s(1),s(2),s(3)))
           call myDel(temp1,f,gd,1,dir1,1,0)
           temp1 = temp1*k
           call myDel(temp2,temp1,gd,1,dir2,1,0)
           df = temp2
           deallocate(temp1,temp2)
         endif
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* NODE BASED DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myNodeGrad(gradx,grady,gradz,f,gd) ! Not finished being developed...
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(dpn),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: gd
         ! call myNode2CellCenter(cellCenter,node,1,gd)
         ! call myCellGrad(gradx,grady,gradz,f,gd)
         call myDel(gradx,f,gd,1,1,2,0)
         call myDel(grady,f,gd,1,2,2,0)
         call myDel(gradz,f,gd,1,3,2,0)
       end subroutine

       subroutine myNode2NodeDel(df,f,gd,dir) ! Finished but maybe not necessary?
         ! This is the correct approach to taking the div(B).
         ! it results in VERY low div(B) for the low Rem
         ! approximation in both the source term AND
         ! in the result.
         ! node->myDel->node (collocated derivative)
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: df
         real(dpn),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         ! Padding is necessary for the divergence.
         call myDel(df,f,gd,1,dir,2,0)
       end subroutine

       subroutine myNode2NodeDelUpwind(df,f,gd,dir) ! Finished but maybe not necessary?
         ! This is the correct approach to taking the div(B).
         ! it results in VERY low div(B) for the low Rem
         ! approximation in both the source term AND
         ! in the result.
         ! node->myDel->node (collocated derivative)
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: df
         real(dpn),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         integer,dimension(3) :: s
         integer :: x,y,z
         real(dpn),dimension(:,:,:),allocatable :: tempe
         ! Padding is necessary for the divergence.
         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myNode2NodeDelUpwind.'; stop
         end select
         s = shape(df)
         allocate(tempe(s(1)+x,s(2)+y,s(3)+z))
         call myNode2edge(tempe,f,gd,dir)
         call myDel(df,tempe,gd,1,dir,3,0)
         deallocate(tempe)
       end subroutine

       subroutine myNodeDiv(divU,u,v,w,gd) ! Finished, but needs improvement
         ! Consider removing dependence on myNode2NodeDel, 
         ! the derivative type is simple
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: divU
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         real(dpn),dimension(:,:,:),allocatable :: temp
         type(grid),intent(in) :: gd
         integer,dimension(3) :: s
         s = shape(u)
         divU = real(0.0,cp)

         allocate(temp(s(1),s(2),s(3)))
         call myNode2NodeDelUpwind(temp,u,gd,1)
         divU = temp
         call myNode2NodeDelUpwind(temp,v,gd,2)
         divU = divU + temp
         call myNode2NodeDelUpwind(temp,w,gd,3)
         divU = divU + temp
         deallocate(temp)
       end subroutine

       subroutine myNodeCurl(curl,u,v,w,gd,dir) ! Finished
         ! myNodeCurl computes the component dir of the node-based
         ! curl using a collocated derivative. 
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: curl
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         s = shape(u)

         allocate(temp1(s(1),s(2),s(3)))
         allocate(temp2(s(1),s(2),s(3)))

         select case (dir)
         case (1)
           call myDel(temp1,w,gd,1,2,2,0)
           call myDel(temp2,v,gd,1,3,2,0)
           curl = temp1 - temp2
         case (2)
           call myDel(temp1,w,gd,1,1,2,0)
           call myDel(temp2,u,gd,1,3,2,0)
           curl = -( temp1 - temp2 )
         case (3)
           call myDel(temp1,v,gd,1,1,2,0)
           call myDel(temp2,u,gd,1,2,2,0)
           curl = temp1 - temp2
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myNodeCurl.'; stop
         end select

         deallocate(temp1,temp2)
       end subroutine

       subroutine myNodeLap(lapU,u,gd) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: lapU
         real(dpn),dimension(:,:,:),intent(in) :: u
         real(dpn),dimension(:,:,:),allocatable :: temp
         type(grid),intent(in) :: gd
         integer,dimension(3) :: s
         s = shape(u)
         lapU = real(0.0,cp)
         allocate(temp(s(1),s(2),s(3)))

         call myDel(temp,u,gd,2,1,2,1)
         lapU = temp
         call myDel(temp,u,gd,2,2,2,1)
         lapU = lapU + temp
         call myDel(temp,u,gd,2,3,2,1)
         lapU = lapU + temp

         deallocate(temp)
       end subroutine

       subroutine myNodeAdvect(psi,u,v,w,phi,gd) ! Finished
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: psi
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w,phi
         type(grid),intent(in) :: gd
         real(dpn),dimension(:,:,:),allocatable :: temp
         real(dpn),dimension(:,:,:),allocatable :: xi
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(u)

         psi = real(0.0,cp)
         allocate(temp(s(1),s(2),s(3)))
         allocate(xi(s(1),s(2),s(3)))

         call myDel(temp,phi,gd,1,1,2,0)
         !$OMP PARALLEL DO
         do j=1,s(2)
           do k=1,s(3)
             do i=1,s(1)
               xi(i,j,k) = u(i,j,k)*temp(i,j,k)
             enddo
           enddo
         enddo
         !$OMP END PARALLEL DO
         psi = psi + xi

         call myDel(temp,phi,gd,1,2,2,0)
         !$OMP PARALLEL DO
         do i=1,s(1)
           do k=1,s(3)
             do j=1,s(2)
               xi(i,j,k) = v(i,j,k)*temp(i,j,k)
             enddo
           enddo
         enddo
         !$OMP END PARALLEL DO
         psi = psi + xi

         call myDel(temp,phi,gd,1,3,2,0)
         !$OMP PARALLEL DO
         do i=1,s(1)
           do j=1,s(2)
             do k=1,s(3)
               xi(i,j,k) = w(i,j,k)*temp(i,j,k)
             enddo
           enddo
         enddo
         !$OMP END PARALLEL DO
         psi = psi + xi

         deallocate(xi)
         deallocate(temp)
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* EDGE BASED DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myEdge2FaceCurl(curl,u,v,w,gd,curlDir) ! Needs checking
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: curl
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: curlDir
         real(dpn),dimension(:,:,:),allocatable :: tempU,tempV,tempW
         integer,dimension(3) :: su,sv,sw,s
         s = shape(curl)
         curl = real(0.0,cp)
         select case (curlDir)
         case (1)
           sv = shape(v); sw = shape(w)
           allocate(tempV(sv(1),sv(2),sv(3)))
           allocate(tempW(sw(1),sw(2),sw(3)))
           call myDel(tempW,w,gd,1,2,3,0)
           call myDel(tempV,v,gd,1,3,3,0)
           ! shape = (Nx+1,Ny+1-1,Nz+2) - (Nx+1,Ny+2,Nz+1-1)
           ! removing the pad from this, we have the interior of the face data:
           ! shape = (Nx+1,Ny,Nz) - (Nx+1,Ny,Nz)
           curl(:,2:s(2)-1,2:s(3)-1) =    tempW(:,1:sw(2)-1,2:sw(3)-1) - tempV(:,2:sv(2)-1,1:sv(3)-1)
           deallocate(tempW,tempV)
         case (2)
           su = shape(u); sw = shape(w)
           allocate(tempU(su(1),su(2),su(3)))
           allocate(tempW(sw(1),sw(2),sw(3)))
           call myDel(tempW,w,gd,1,1,3,0)
           call myDel(tempU,u,gd,1,3,3,0)
           curl(2:s(1)-1,:,2:s(3)-1) = -( tempW(1:sw(1)-1,:,2:sw(3)-1) - tempU(2:su(1)-1,:,1:su(3)-1) )
           deallocate(tempW,tempU)
         case (3)
           su = shape(u); sv = shape(v)
           allocate(tempU(su(1),su(2),su(3)))
           allocate(tempV(sv(1),sv(2),sv(3)))
           call myDel(tempV,v,gd,1,1,3,0)
           call myDel(tempU,u,gd,1,2,3,0)
           curl(2:s(1)-1,2:s(2)-1,:) =    tempV(1:sv(1)-1,2:sv(2)-1,:) - tempU(2:su(1)-1,1:su(2)-1,:)
         case default
           write(*,*) 'Error: curlDir must = 1,2,3 in myEdge2FaceCurl.'; stop
         deallocate(tempV,tempU)
         end select
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ************************************ SPECIAL TERMS *************************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       ! ************************************ CELL CENTER *************************************

       subroutine myCCBfieldAdvect(div,u,v,w,Bx,By,Bz,gd,dir)
         ! Returns the ith component of the advective term
         ! in the induction equation:
         !   d/dxj (uj Bi - ui Bj)
         ! or
         !   d/dx (u Bi - ui Bx) + 
         !   d/dy (v Bi - ui By) + 
         !   d/dz (w Bi - ui Bz)
         ! All variables are expected to be located at the cell centers.
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: div
         real(dpn),dimension(:,:,:),intent(in) :: u,v,w,Bx,By,Bz
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         s = shape(div)
         allocate(temp1(s(1),s(2),s(3)))
         allocate(temp2(s(1),s(2),s(3)))
         div = real(0.0,cp)

         ! * = needs interpolation --         *    *
         ! -------------------------- d/dx (u Bi - ui Bx)
         select case (dir)
         case (2)
         temp2 = u*By - v*Bx
         call myDel(temp1,temp2,gd,1,1,1,0)
         div = div + temp1
         case (3)
         temp2 = u*Bz - w*Bx
         call myDel(temp1,temp2,gd,1,1,1,0)
         div = div + temp1
         end select
         ! * = needs interpolation --         *    *
         ! -------------------------- d/dy (v Bi - ui By)
         select case (dir)
         case (1)
         temp2 = v*Bx - u*By
         call myDel(temp1,temp2,gd,1,2,1,0)
         div = div + temp1
         case (3)
         temp2 = v*Bz - w*By
         call myDel(temp1,temp2,gd,1,2,1,0)
         div = div + temp1
         end select

         ! * = needs interpolation --         *    *
         ! -------------------------- d/dz (w Bi - ui Bz)
         select case (dir)
         case (1)
         temp2 = w*Bx - u*Bz
         call myDel(temp1,temp2,gd,1,3,1,0)
         div = div + temp1
         case (2)
         temp2 = w*By - v*Bz
         call myDel(temp1,temp2,gd,1,3,1,0)
         div = div + temp1
         end select

         deallocate(temp1,temp2)
       end subroutine

       subroutine myCCBfieldDiffuse(div,Bx,By,Bz,sigmaInv,gd,dir) ! Finished, needs testing
         ! There are a lot of local allocatables
         ! 
         ! Returns the ith component of the diffusion term 
         ! in the induction equation:
         ! 
         !   d/dxj [ sigmaInv ( d/dxi (Bj) - d/dxj (Bi) ) ]
         ! 
         ! or
         ! 
         !   d/dx [ sigmaInv ( d/dxi (Bx) - d/dx (Bi) ) ] + 
         !   d/dy [ sigmaInv ( d/dxi (By) - d/dy (Bi) ) ] + 
         !   d/dz [ sigmaInv ( d/dxi (Bz) - d/dz (Bi) ) ]
         ! 
         ! Where d/dxi is the derivative wrt the direction dir.
         ! B is expected to be located at the cell center.
         ! 
         ! For variable mu, Bx,By,Bz must be divided by mu
         ! before calling this routine.
         ! 
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: div
         real(dpn),dimension(:,:,:),intent(in) :: Bx,By,Bz,sigmaInv
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp1
         integer,dimension(3) :: s
         s = shape(div)
         div = real(0.0,cp)
         allocate(temp1(s(1),s(2),s(3)))
         ! -------------------------- d/dx [ sigmaInv ( d/dxi (Bx/mu) - d/dx (Bi/mu) ) ]
         select case (dir)
         case (2)
         call myCCVaryDel(temp1,Bx,sigmaInv,gd,dir,1); div=div+temp1
         call myCCVaryDel(temp1,By,sigmaInv,gd,1,1); div=div-temp1
         case (3)
         call myCCVaryDel(temp1,Bx,sigmaInv,gd,dir,1); div=div+temp1
         call myCCVaryDel(temp1,Bz,sigmaInv,gd,1,1); div=div-temp1
         end select
         ! -------------------------- d/dy [ sigmaInv ( d/dxi (By/mu) - d/dy (Bi/mu) ) ]
         select case (dir)
         case (1)
         call myCCVaryDel(temp1,By,sigmaInv,gd,dir,2); div=div+temp1
         call myCCVaryDel(temp1,Bx,sigmaInv,gd,2,2); div=div-temp1
         case (3)
         call myCCVaryDel(temp1,By,sigmaInv,gd,dir,2); div=div+temp1
         call myCCVaryDel(temp1,Bz,sigmaInv,gd,2,2); div=div-temp1
         end select
         ! -------------------------- d/dz [ sigmaInv ( d/dxi (Bz/mu) - d/dz (Bi/mu) ) ]
         select case (dir)
         case (1)
         call myCCVaryDel(temp1,Bz,sigmaInv,gd,dir,3); div=div+temp1
         call myCCVaryDel(temp1,Bx,sigmaInv,gd,3,3); div=div-temp1
         case (2)
         call myCCVaryDel(temp1,Bz,sigmaInv,gd,dir,3); div=div+temp1
         call myCCVaryDel(temp1,By,sigmaInv,gd,3,3); div=div-temp1
         end select

         deallocate(temp1)
       end subroutine

       subroutine myCCJCrossB(jcrossB,Bx,By,Bz,mu,gd,dir) ! Finished, consider improvements
         ! There are a lot of local allocatables, consider passing some in?
         ! 
         ! Returns the ith component of the j cross B force 
         ! in the momentum equation:
         !   Bx [ ( d/dx (Bi/mu) - d/dxi (Bx/mu) ) ] + 
         !   By [ ( d/dx (Bi/mu) - d/dxi (By/mu) ) ] + 
         !   Bz [ ( d/dx (Bi/mu) - d/dxi (Bz/mu) ) ]
         ! Where d/dxi is the derivative wrt the direction dir.
         ! All variables are expected to be located at the cell corners.
         implicit none
         real(dpn),dimension(:,:,:),intent(inout) :: jcrossB
         real(dpn),dimension(:,:,:),intent(in) :: Bx,By,Bz,mu
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(dpn),dimension(:,:,:),allocatable :: temp1,temp2,temp3
         integer,dimension(3) :: s
         s = shape(jcrossB)
         jcrossB = real(0.0,cp)
         allocate(temp1(s(1),s(2),s(3)))
         allocate(temp2(s(1),s(2),s(3)))
         allocate(temp3(s(1),s(2),s(3)))
         ! -------------------------- d/dx (di Bx - dx Bi)
         select case (dir)
         case (2)
         temp3 = By/mu
         call myNode2NodeDel(temp2,temp3,gd,1)
         temp3 = Bx/mu
         call myNode2NodeDel(temp1,temp3,gd,dir)
         temp3 = temp1 - temp2
         call myNode2NodeDel(temp1,temp3,gd,1)
         jcrossB(2:s(1),:,:) = jcrossB(2:s(1),:,:) + temp1(1:s(1)-1,:,:)
         case (3)
         temp3 = Bz/mu
         call myNode2NodeDel(temp2,temp3,gd,1)
         temp3 = Bx/mu
         call myNode2NodeDel(temp1,temp3,gd,dir)
         temp3 = temp1 - temp2
         call myNode2NodeDel(temp1,temp3,gd,1)
         jcrossB(2:s(1),:,:) = jcrossB(2:s(1),:,:) + temp1(1:s(1)-1,:,:)
         end select
         ! -------------------------- d/dy (di By - dy Bi)
         select case (dir)
         case (1)
         temp3 = Bx/mu
         call myNode2NodeDel(temp2,temp3,gd,2)
         temp3 = By/mu
         call myNode2NodeDel(temp1,temp3,gd,dir)
         temp3 = temp1 - temp2
         call myNode2NodeDel(temp1,temp3,gd,2)
         jcrossB(:,2:s(2),:) = jcrossB(:,2:s(2),:) + temp1(:,1:s(2)-1,:)
         case (3)
         temp3 = Bz/mu
         call myNode2NodeDel(temp2,temp3,gd,2)
         temp3 = By/mu
         call myNode2NodeDel(temp1,temp3,gd,dir)
         temp3 = temp1 - temp2
         call myNode2NodeDel(temp1,temp3,gd,2)
         jcrossB(:,2:s(2),:) = jcrossB(:,2:s(2),:) + temp1(:,1:s(2)-1,:)
         end select
         ! -------------------------- d/dz (di Bz - dz Bi)
         select case (dir)
         case (1)
         temp3 = Bx/mu
         call myNode2NodeDel(temp2,temp3,gd,3)
         temp3 = Bz/mu
         call myNode2NodeDel(temp1,temp3,gd,dir)
         temp3 = temp1 - temp2
         call myNode2NodeDel(temp1,temp3,gd,3)
         jcrossB(:,:,2:s(3)) = jcrossB(:,:,2:s(3)) + temp1(:,:,1:s(3)-1)
         case (2)
         temp3 = By/mu
         call myNode2NodeDel(temp2,temp3,gd,3)
         temp3 = Bz/mu
         call myNode2NodeDel(temp1,temp3,gd,dir)
         temp3 = temp1 - temp2
         call myNode2NodeDel(temp1,temp3,gd,3)
         jcrossB(:,:,2:s(3)) = jcrossB(:,:,2:s(3)) + temp1(:,:,1:s(3)-1)
         end select

         deallocate(temp1,temp2,temp3)
       end subroutine

       subroutine computeCurrent(J,B,B0,mu,gd)
         implicit none
         type(vectorField),intent(inout) :: B,J
         type(vectorField),intent(in) :: B0
         type(scalarField),intent(in) :: mu
         type(grid),intent(in) :: gd
         ! B = (B + B0)/mu
         call add(B,B0)
         call divide(B,mu)
         call myCCCurl(J%x,B%x,B%y,B%z,gd,1)
         call myCCCurl(J%y,B%x,B%y,B%z,gd,2)
         call myCCCurl(J%z,B%x,B%y,B%z,gd,3)
         call multiply(B,mu)
         call subtract(B,B0)
         ! B = B*mu - B0
       end subroutine

       subroutine embedVelocity(U_fi,U_cct,U_cci,gd)
         implicit none
         type(vectorField),intent(in) :: U_fi
         type(vectorField),intent(inout) :: U_cct
         type(scalarField),intent(inout) :: U_cci
         type(grid),intent(in) :: gd
         integer,dimension(3) :: Ni

         Ni = (/gd%c(1)%sc-2,gd%c(2)%sc-2,gd%c(3)%sc-2/) ! minus fictitious cells
         ! (exclude fictitious cells)
         call myFace2CellCenter(U_cci%phi,U_fi%x,gd,1)
          U_cct%x(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         U_cci%phi(2:Ni(1)+1,2:Ni(2)+1,2:Ni(3)+1)

         call myFace2CellCenter(U_cci%phi,U_fi%y,gd,2)
          U_cct%y(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         U_cci%phi(2:Ni(1)+1,2:Ni(2)+1,2:Ni(3)+1)

         call myFace2CellCenter(U_cci%phi,U_fi%z,gd,3)
          U_cct%z(Nice1(1):Nice2(1),Nice1(2):Nice2(2),Nice1(3):Nice2(3)) = &
         U_cci%phi(2:Ni(1)+1,2:Ni(2)+1,2:Ni(3)+1)
       end subroutine

       end module