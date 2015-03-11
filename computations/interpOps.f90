       module interpOps_mod
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
       use scalarField_mod
       use vectorField_mod
       use grid_mod
       implicit none

       ! Compiler flags: ( _DEBUG_INTERP_ , fopenmp )

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
       ! Base interpolation
       public :: interp               ! call interp(f,g,gd,dir,gridType)

       ! Derived interpolations
       public :: myFaceAverage        ! call myFaceAverage(faceAve,f,gd,dir,aveLoc)
       public :: myFace2Node          ! call myFace2Node(nodeAverage,face,gd,dir)
       public :: myFace2CellCenter    ! call myFace2CellCenter(cellCenter,face,gd,faceDir)
       public :: myFace2Edge          ! call myFace2Edge(edge,gd,face,edgeDir)

       public :: myCellCenter2Face    ! call myCellCenter2Face(face,cellCenter,gd,faceDir)
       public :: myCellCenter2Edge    ! call myCellCenter2Edge(edge,cellCenter,gd,edgeDir)
       public :: myCellCenter2Node    ! call myCellCenter2Node(nodeAverage,cellCenter,dir,gd)

       public :: myNode2Face          ! call myNode2Face(face,node,gd,faceDir)
       public :: myNode2Edge          ! call myNode2Edge(edge,node,gd,edgeDir)


       interface interp;     module procedure interpO2;            end interface
       interface extrap;     module procedure extrapO2;            end interface

       contains

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ********************************* BASE INTERPOLATION ***********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine interpO2(f,g,gd,dir,gridType) ! Seems to be working.
         ! interpO2 interpolates g from the primary grid to the
         ! dual grid using a 2nd order accurate stencil for non-uniform 
         ! grids. f lives on the dual grid. It is expected that
         ! f lives between g.
         ! 
         ! Reminder: f = interp(g)
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
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(:,:,:),intent(in) :: g
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir,gridType
         integer :: i,j,k,t,x,y,z
         real(cp) :: alpha
         integer,dimension(3) :: sg,sf
         sg = shape(g); sf = shape(f)

         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           stop 'Error: dir must = 1,2,3 in interpO2.'
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
               alpha = (gd%c(dir)%hn(t) - gd%c(dir)%hc(t+1))/(gd%c(dir)%hc(t) - gd%c(dir)%hc(t+1))
               f(i,j,k) = g(i,j,k)*alpha +&
                             g(i+x,j+y,k+z)*(real(1.0,cp)-alpha)
             enddo
           enddo
         enddo
         !$OMP END DO
         !$OMP END PARALLEL
         case default
           stop 'gridType must be 1 or 2. Terminating.'
         end select

       end subroutine

       subroutine extrapO2(f,g,gd,dir) ! Seems to be working.
         ! extrapO2 extrapolates g from the primary grid to the
         ! dual grid using a 2nd order accurate stencil for 
         ! non-uniform grids. f lives on the dual grid. 
         ! It is expected that f lives between g.
         ! 
         ! Reminder: f = extrap(g)
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
         real(cp),dimension(:,:,:),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp) :: a1,a2 ! differences in Taylor expansion
         real(cp) :: c1,c2 ! Coefficients of function values
         integer,dimension(3) :: sg,sf
         sg = shape(g); sf = shape(f)
         ! BACKWARD EXTRAPOLATION (* = assigned)
         !            f  g  f  g  f  g  f
         !            o--|--o--|--o--|--o   --> dir
         !            *
         a1 = gd%c(dir)%hc(1) - gd%c(dir)%hn(1)
         a2 = gd%c(dir)%hn(1) - gd%c(dir)%hn(2)
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
         a1 = gd%c(dir)%hc(sf(dir)) - gd%c(dir)%hn(sf(dir)-1)
         a2 = gd%c(dir)%hn(sg(dir)) - gd%c(dir)%hn(sg(dir)-1)
         c1 = real(1.0,cp) + a1/a2
         c2 = -a1/a2
         select case (dir)
         case (1); f(sf(1),:,:)=c1*g(sg(1),:,:)+c2*g(sg(1)-1,:,:)
         case (2); f(:,sf(2),:)=c1*g(:,sg(2),:)+c2*g(:,sg(2)-1,:)
         case (3); f(:,:,sf(3))=c1*g(:,:,sg(3))+c2*g(:,:,sg(3)-1)
         end select
       end subroutine


       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* DERIVED INTERPOLATIONS *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ********************************* FACE INTERPOLATIONS **********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myFace2CellCenter(cellCenter,face,gd,faceDir) ! Needs checking
         ! 1 interpolation routine (no allocation required)
         implicit none
         real(cp),dimension(:,:,:),intent(in)    :: face        ! size = (Nn+1,Nt+2)
         real(cp),dimension(:,:,:),intent(inout) :: cellCenter  ! size = (Nn+2,Nt+2)
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         cellCenter = real(0.0,cp) ! This is expensive, consider changing
         call interp(cellCenter,face,gd,faceDir,1)
         call extrap(cellCenter,face,gd,faceDir)
       end subroutine

       subroutine myFace2Edge(edge,face,gd,faceDir,edgeDir) ! Seems to be working
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
         real(cp),dimension(:,:,:),intent(in)    :: face ! size = (Nn+1,Nt+2)
         real(cp),dimension(:,:,:),intent(inout) :: edge ! size = (Nn+1,Nt+1,Nt+2)
         type(grid),intent(in) :: gd
         integer,intent(in) :: edgeDir,faceDir
         real(cp),dimension(:,:,:),allocatable :: tempCC
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
         real(cp),dimension(:,:,:),intent(in) :: face
         real(cp),dimension(:,:,:),intent(inout) :: faceAve
         real(cp),dimension(:,:,:),allocatable :: cellCenter
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
           stop 'Error: faceDir must = 1,2,3 in myFaceAverage.'
         end select
         allocate(cellCenter(s(1)+x,s(2)+y,s(3)+z))
         call myFace2CellCenter(cellCenter,face,gd,faceDir)
         call myCellCenter2Face(faceAve,cellCenter,gd,aveLoc)
         deallocate(cellCenter)
       end subroutine

       subroutine myFace2Node(nodeAverage,face,gd,faceDir) ! Needs improvement, big time
         ! This needs to be fixed becuase only 2 interpolations are required.
         ! right now, there are 4, this may introduce significant errors.
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: face
         real(cp),dimension(:,:,:),intent(inout) :: nodeAverage
         real(cp),dimension(:,:,:),allocatable :: tempcc
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
           stop 'Error: faceDir must = 1,2,3 in myFace2Node.'
         end select
         allocate(tempcc(s(1)+x,s(2)+y,s(3)+z))
         call myFace2CellCenter(tempcc,face,gd,faceDir)
         call myCellCenter2Node(nodeAverage,tempcc,gd)
         deallocate(tempcc)
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! *********************************** CC INTERPOLATIONS **********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myCellCenter2Face(face,cellCenter,gd,faceDir) ! Needs checking
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         real(cp),dimension(:,:,:),intent(inout) :: face
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         call interp(face,cellCenter,gd,faceDir,2)
       end subroutine

       subroutine myCellCenter2Node(nodeAverage,cellCenter,gd) ! Finished, needs improvement
         ! Try to allocate less
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         real(cp),dimension(:,:,:),intent(inout) :: nodeAverage
         type(grid),intent(in) :: gd
         real(cp),dimension(:,:,:),allocatable :: temp,tempcc
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
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         real(cp),dimension(:,:,:),intent(inout) :: edge
         type(grid),intent(in) :: gd
         integer,intent(in) :: edgeDir
         real(cp),dimension(:,:,:),allocatable :: faceTemp
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
           stop 'Error: edgeDir must = 1,2,3 in myCellCenter2Edge.'
         end select
         deallocate(faceTemp)
       end subroutine


       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ********************************* NODE INTERPOLATIONS **********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myNode2Edge(edge,node,gd,edgeDir) ! Needs checking
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: node       ! size = Nn+1,Nt+1
         real(cp),dimension(:,:,:),intent(inout) :: edge    ! size = Nn+2,Nt+1
         type(grid),intent(in) :: gd
         integer,intent(in) :: edgeDir
         call interp(edge,node,gd,edgeDir,1)
         call extrap(edge,node,gd,edgeDir)
       end subroutine

       subroutine myNode2Face(face,node,gd,faceDir) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: face
         real(cp),dimension(:,:,:),intent(in) :: node
         real(cp),dimension(:,:,:),allocatable :: tempe
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
           stop 'Error: faceDir must = 1,2,3 in myNode2Face.'
         end select
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ********************************* EDGE INTERPOLATIONS **********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myEdge2Face(face,edge,gd,edgeDir,faceDir) ! Needs checking
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: edge    ! size = Ne+2,N+1
         real(cp),dimension(:,:,:),intent(inout) :: face ! size = Nf+1,Nt+2
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir,edgeDir
         integer :: orthDir
         orthDir = orthogonalDirection(edgeDir,faceDir)
         call interp(face,edge,gd,orthDir,1)
         call extrap(face,edge,gd,orthDir)
       end subroutine


       ! ****************************************************************************************
       ! ****************************************************************************************
       ! *********************************** AUXILIARY ROUTINES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

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
               stop 'Error: bad input to orthogonalDirection'
             end select
           case (2);
             select case (dir2)
             case (1); orthDir = 3
             case (3); orthDir = 1
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case (3);
             select case (dir2)
             case (1); orthDir = 2
             case (2); orthDir = 1
             case default
               stop 'Error: bad input to orthogonalDirection'
             end select
           case default
             stop 'Error: bad input to orthogonalDirection'
         end select
#ifdef _DEBUG_INTERP_
         if (dir1.eq.dir2) then
           stop 'Error: There are no orthogonal directions';
         endif
#endif
       end function

       end module