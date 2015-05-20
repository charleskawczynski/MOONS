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
       use grid_mod
       use vectorField_mod
       implicit none

       ! Compiler flags: ( fopenmp, _DEBUG_INTERP_ )

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
       public :: interp               ! call interp(f,g,gd,dir)

       ! Derived interpolations
       public :: myFaceAverage        ! call myFaceAverage(faceAve,f,g,dir,aveLoc)
       public :: myFace2Node          ! call myFace2Node(nodeAverage,face,g,dir)           *
       public :: myFace2CellCenter    ! call myFace2CellCenter(cellCenter,face,g,faceDir)  *
       public :: myFace2Edge          ! call myFace2Edge(edge,g,face,edgeDir)

       public :: myCellCenter2Face    ! call myCellCenter2Face(face,cellCenter,g,faceDir)  *
       public :: myCellCenter2Edge    ! call myCellCenter2Edge(edge,cellCenter,g,edgeDir)  *
       public :: myCellCenter2Node    ! call myCellCenter2Node(nodeAverage,cellCenter,g)   *

       public :: myEdge2Face          ! call myEdge2Face(face,edge,g,edgeDir,faceDir)
       public :: myEdge2Node          ! call myEdge2Node(node,edge,g,edgeDir)              *
       public :: myEdge2CellCenter    ! call myEdge2CellCenter(cellCenter,edge,g,edgeDir)
       
       public :: myNode2Face          ! call myNode2Face(face,node,g,faceDir)              *
       public :: myNode2Edge          ! call myNode2Edge(edge,node,g,edgeDir)              *

       ! * = has vector interface

       interface interp;              module procedure interpO2;             end interface
       interface extrap;              module procedure extrapO2;             end interface

       interface myFace2Node;         module procedure myFace2NodeSF;        end interface
       interface myFace2Node;         module procedure myFace2NodeVF;        end interface
       interface myFace2CellCenter;   module procedure myFace2CellCenterSF;  end interface
       interface myFace2CellCenter;   module procedure myFace2CellCenterVF;  end interface

       interface myCellCenter2Face;   module procedure myCellCenter2FaceSF;  end interface
       interface myCellCenter2Face;   module procedure myCellCenter2FaceVF;  end interface
       interface myCellCenter2Face;   module procedure myCellCenter2Face2VF; end interface
       interface myCellCenter2Edge;   module procedure myCellCenter2EdgeSF;  end interface
       interface myCellCenter2Edge;   module procedure myCellCenter2EdgeVF;  end interface
       interface myCellCenter2Edge;   module procedure myCellCenter2Edge2VF; end interface
       interface myCellCenter2Node;   module procedure myCellCenter2NodeSF;  end interface
       interface myCellCenter2Node;   module procedure myCellCenter2NodeVF;  end interface

       interface myEdge2Node;         module procedure myEdge2NodeSF;        end interface
       interface myEdge2Node;         module procedure myEdge2NodeVF;        end interface
       interface myEdge2CellCenter;   module procedure myEdge2CellCenterSF;  end interface
       interface myEdge2CellCenter;   module procedure myEdge2CellCenterVF;  end interface

       interface myNode2Face;         module procedure myNode2FaceSF;        end interface
       interface myNode2Face;         module procedure myNode2FaceVF;        end interface
       interface myNode2Face;         module procedure myNode2Face2VF;       end interface
       interface myNode2Edge;         module procedure myNode2EdgeSF;        end interface
       interface myNode2Edge;         module procedure myNode2EdgeVF;        end interface
       interface myNode2Edge;         module procedure myNode2Edge2VF;       end interface

       contains

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ********************************* BASE INTERPOLATION ***********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine interpO2(f,g,gd,dir)
         ! interpO2 interpolates g from the primary grid to the
         ! dual grid using a 2nd order accurate stencil for non-uniform 
         ! grids. f lives on the dual grid. It is expected that
         ! f lives between g.
         ! 
         ! Reminder: f = interp(g)
         ! 
         ! * = assigned in this routine. This way, the entire
         ! array of f and g may be passed, without having to 
         ! index.
         ! 
         ! Therefore, shape(g) = shape(f) + 1 along dir
         ! Otherwise, shape(g) = shape(f)
         ! 
         ! Add internal checking of shape of f and g and grid
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(:,:,:),intent(in) :: g
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         integer :: i,j,k,t,x,y,z
         real(cp) :: alpha
         integer,dimension(3) :: sg,sf
         sf = shape(f); sg = shape(g)

         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           stop 'Error: dir must = 1,2,3 in interpO2.'
         end select

#ifdef _DEBUG_INTERP_
         select case (dir)
         case (1)
         if (sf(2).ne.sg(2)) stop 'Erorr: shape mismatch in interpOps.f90'
         if (sf(3).ne.sg(3)) stop 'Erorr: shape mismatch in interpOps.f90'
         case (2)
         if (sf(1).ne.sg(1)) stop 'Erorr: shape mismatch in interpOps.f90'
         if (sf(3).ne.sg(3)) stop 'Erorr: shape mismatch in interpOps.f90'
         case (3)
         if (sf(1).ne.sg(1)) stop 'Erorr: shape mismatch in interpOps.f90'
         if (sf(2).ne.sg(2)) stop 'Erorr: shape mismatch in interpOps.f90'
         case default
           stop 'Error: dir must = 1,2,3 in interpO2.'
         end select
#endif

         if ((sf(dir).eq.gd%c(dir)%sc).and.(sg(dir).eq.gd%c(dir)%sn)) then
           ! f(cc grid), g(node/face grid)
           !         g  f  g  f  g  f  g  f  g
           !         |--o--|--o--|--o--|--o--|   --> dir
           !            *     *     *     *
           
           !$OMP PARALLEL DO
           do k=1,sg(3)-z
             do j=1,sg(2)-y
               do i=1,sg(1)-x
                 f(i,j,k) = (g(i,j,k)+g(i+x,j+y,k+z))/real(2.0,cp)
               enddo
             enddo
           enddo
           !$OMP END PARALLEL DO
         elseif ((sf(dir).eq.gd%c(dir)%sn).and.(sg(dir).eq.gd%c(dir)%sc)) then
           ! f(node/face grid), g(cc grid)
           !         f  g  f  g  f  g  f  g  f
           !         |--o--|--o--|--o--|--o--|      --> dir
           !               *     *     *

           !$OMP PARALLEL PRIVATE(alpha,t)
           !$OMP DO
           do k=1,sg(3)-z
             do j=1,sg(2)-y
               do i=1,sg(1)-x
                 t = i*x + j*y + k*z
                 alpha = (gd%c(dir)%hn(t+1) - gd%c(dir)%hc(t))/(gd%c(dir)%hc(t+1) - gd%c(dir)%hc(t))
                 f(i+x,j+y,k+z) = g(i+x,j+y,k+z)*alpha + &
                                  g(i,j,k)*(real(1.0,cp)-alpha)
               enddo
             enddo
           enddo
           !$OMP END DO
           !$OMP END PARALLEL
           call extrapO2(f,g,dir)
         else
           stop 'gridType must be 1 or 2 in interpO2. Terminating.'
         endif
       end subroutine

       subroutine extrapO2(f,g,dir)
         ! extrapO2 extrapolates g from the primary grid to the
         ! dual grid using a 2nd order accurate stencil for 
         ! non-uniform grids. f lives on the dual grid. 
         ! It is expected that f lives between g.
         ! 
         ! Reminder: f = extrap(g)
         ! 
         ! For interpO2, this corresponds to the case:
         ! f(cc grid), g(node/face grid)
         ! 
         !         f  g  f  g  f  g  f  g  f
         !         |--o--|--o--|--o--|--o--|    --> dir
         !         *                       *
         ! 
         ! * = assigned in this routine. This way, the entire
         ! array of f and g may be passed, without having to 
         ! index.
         ! 
         ! Therefore, size(f) = size(g) + 1 along dir
         ! Otherwise, size(f) = size(g)
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,intent(in) :: dir
         integer,dimension(3) :: sg,sf
         sg = shape(g); sf = shape(f)
         ! Both cases are simple since the size of the 
         ! first 2 cells are equal, we have
         ! 
         ! f_ghost + f_boundary
         ! --------------------  = g    -->  f_ghost = 2g - f_boundary
         !          2
         ! 
         ! BACKWARD EXTRAPOLATION (* = assigned)
         !         f  g  f  g  f  g  f  g  f
         !         |--o--|--o--|--o--|--o--|    --> dir
         !         *
         ! 
         select case (dir)
         case (1); f(1,:,:) = real(2.0,cp)*g(1,:,:) - f(2,:,:)
         case (2); f(:,1,:) = real(2.0,cp)*g(:,1,:) - f(:,2,:)
         case (3); f(:,:,1) = real(2.0,cp)*g(:,:,1) - f(:,:,2)
         end select
         ! FORWARD EXTRAPOLATION (* = assigned)
         !         f  g  f  g  f  g  f  g  f
         !         |--o--|--o--|--o--|--o--|    --> dir
         !                                 *
         select case (dir)
         case (1); f(sf(1),:,:) = real(2.0,cp)*g(sg(1),:,:) - f(sf(1)-1,:,:)
         case (2); f(:,sf(2),:) = real(2.0,cp)*g(:,sg(2),:) - f(:,sf(2)-1,:)
         case (3); f(:,:,sf(3)) = real(2.0,cp)*g(:,:,sg(3)) - f(:,:,sf(3)-1)
         end select
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* DERIVED INTERPOLATIONS *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       ! ****************************************************************************************
       ! ********************************* FACE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myFace2CellCenterSF(cellCenter,face,g,faceDir)
         ! 1 interpolation routine (no allocation required)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: cellCenter
         real(cp),dimension(:,:,:),intent(in)    :: face
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir
         call interp(cellCenter,face,g,faceDir)
       end subroutine

       subroutine myFace2Edge(edge,face,g,faceDir,edgeDir)
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
         real(cp),dimension(:,:,:),intent(inout) :: edge
         real(cp),dimension(:,:,:),intent(in)    :: face
         type(grid),intent(in) :: g
         integer,intent(in) :: edgeDir,faceDir

         real(cp),dimension(:,:,:),allocatable :: tempCC
         integer :: orthDir
         if (edgeDir.ne.faceDir) then ! requires 1 interpolation (no allocations)
           orthDir = orthogonalDirection(edgeDir,faceDir)
           call interp(edge,face,g,orthDir)
           call extrap(edge,face,orthDir)
         else ! Requires 3 interpolations ()
           allocate(tempCC(g%c(1)%sc,g%c(2)%sc,g%c(3)%sc))
           call myFace2CellCenter(tempCC,face,g,faceDir)
           call myCellCenter2Edge(edge,tempCC,g,edgeDir)
           deallocate(tempCC)
         endif
       end subroutine

       subroutine myFaceAverage(faceAve,face,g,faceDir,aveLoc)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: faceAve
         real(cp),dimension(:,:,:),intent(in) :: face
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir,aveLoc
         real(cp),dimension(:,:,:),allocatable :: cellCenter
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
         allocate(cellCenter(s(1)-x,s(2)-y,s(3)-z))
         call myFace2CellCenter(cellCenter,face,g,faceDir)
         call myCellCenter2Face(faceAve,cellCenter,g,aveLoc)
         deallocate(cellCenter)
       end subroutine

       subroutine myFace2NodeSF(node,face,g,faceDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: node
         real(cp),dimension(:,:,:),intent(in) :: face
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir
         real(cp),dimension(:,:,:),allocatable :: edge
         integer,dimension(3) :: s,sn
         integer :: x,y,z,edgeDir
         s = shape(face); sn = shape(node)
         select case (faceDir)
         case (1); edgeDir = 2
         case (2); edgeDir = 1
         case (3); edgeDir = 1
         case default
           stop 'Error: faceDir must = 1,2,3 in myFace2Node.'
         end select
         select case (edgeDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           stop 'Error: faceDir must = 1,2,3 in myFace2Node.'
         end select

         allocate(edge(sn(1)-x,sn(2)-y,sn(3)-z))
         call myFace2Edge(edge,face,g,faceDir,edgeDir)
         call myEdge2Node(node,edge,g,edgeDir)
         deallocate(edge)
       end subroutine

       ! ****************************************************************************************
       ! *********************************** CC INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myCellCenter2FaceSF(face,cellCenter,g,faceDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: face
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir
         call interp(face,cellCenter,g,faceDir)
         call extrap(face,cellCenter,faceDir)
       end subroutine

       subroutine myCellCenter2NodeSF(node,cellCenter,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: node
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),allocatable :: face,edge
         integer,dimension(3) :: sc,sn
         sc = shape(cellCenter)
         sn = shape(node)
         allocate(face(sn(1),sc(2),sc(3)))
         call myCellCenter2Face(face,cellCenter,g,1)
         allocate(edge(sn(1),sn(2),sc(3)))
         call myFace2Edge(edge,face,g,1,3)
         deallocate(face)
         call myEdge2Node(node,edge,g,3)
         deallocate(edge)
       end subroutine

       subroutine myCellCenter2EdgeSF(edge,cellCenter,g,edgeDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: edge
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         integer,intent(in) :: edgeDir
         real(cp),dimension(:,:,:),allocatable :: faceTemp
         integer,dimension(3) :: s
         s = shape(cellCenter)

         select case (edgeDir)
         case(1); allocate(faceTemp(g%c(1)%sc,g%c(2)%sn,g%c(3)%sc))
         call myCellCenter2Face(faceTemp,cellCenter,g,2)
         call myFace2Edge(edge,faceTemp,g,2,1)
         case(2); allocate(faceTemp(g%c(1)%sn,g%c(2)%sc,g%c(3)%sc))
         call myCellCenter2Face(faceTemp,cellCenter,g,1)
         call myFace2Edge(edge,faceTemp,g,1,2)
         case(3); allocate(faceTemp(g%c(1)%sn,g%c(2)%sc,g%c(3)%sc))
         call myCellCenter2Face(faceTemp,cellCenter,g,1)
         call myFace2Edge(edge,faceTemp,g,1,3)
         case default
           stop 'Error: edgeDir must = 1,2,3 in myCellCenter2Edge.'
         end select
         deallocate(faceTemp)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* NODE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myNode2EdgeSF(edge,node,g,edgeDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: edge
         real(cp),dimension(:,:,:),intent(in) :: node
         type(grid),intent(in) :: g
         integer,intent(in) :: edgeDir
         call interp(edge,node,g,edgeDir)
       end subroutine

       subroutine myNode2FaceSF(face,node,g,faceDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: face
         real(cp),dimension(:,:,:),intent(in) :: node
         real(cp),dimension(:,:,:),allocatable :: tempe
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir
         integer,dimension(3) :: s
         s = shape(node)
         select case (faceDir)
         case (1)
           allocate(tempe(g%c(1)%sn,g%c(2)%sc,g%c(3)%sn))
           call myNode2Edge(tempe,node,g,2)
           call myEdge2Face(face,tempe,g,2,faceDir)
           deallocate(tempe)
         case (2)
           allocate(tempe(g%c(1)%sc,g%c(2)%sn,g%c(3)%sn))
           call myNode2Edge(tempe,node,g,1)
           call myEdge2Face(face,tempe,g,1,faceDir)
           deallocate(tempe)
         case (3)
           allocate(tempe(g%c(1)%sc,g%c(2)%sn,g%c(3)%sn))
           call myNode2Edge(tempe,node,g,1)
           call myEdge2Face(face,tempe,g,1,faceDir)
           deallocate(tempe)
         case default
           stop 'Error: faceDir must = 1,2,3 in myNode2Face.'
         end select
       end subroutine

       ! ****************************************************************************************
       ! ********************************* EDGE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myEdge2Face(face,edge,g,edgeDir,faceDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: face
         real(cp),dimension(:,:,:),intent(in) :: edge
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir,edgeDir
         integer :: orthDir
         orthDir = orthogonalDirection(edgeDir,faceDir)
         call interp(face,edge,g,orthDir)
       end subroutine

       subroutine myEdge2NodeSF(node,edge,g,edgeDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: node
         real(cp),dimension(:,:,:),intent(in) :: edge
         type(grid),intent(in) :: g
         integer,intent(in) :: edgeDir
         call interp(node,edge,g,edgeDir)
         call extrap(node,edge,edgeDir)
       end subroutine

       subroutine myEdge2CellCenterSF(cellCenter,edge,g,edgeDir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: cellCenter
         real(cp),dimension(:,:,:),intent(in) :: edge
         type(grid),intent(in) :: g
         integer,intent(in) :: edgeDir
         real(cp),dimension(:,:,:),allocatable :: tempF
         integer :: faceDir
         select case (edgeDir)
         case (1); faceDir = 2; allocate(tempF(g%c(1)%sc,g%c(2)%sn,g%c(3)%sc))
         case (2); faceDir = 3; allocate(tempF(g%c(1)%sc,g%c(2)%sc,g%c(3)%sn))
         case (3); faceDir = 1; allocate(tempF(g%c(1)%sn,g%c(2)%sc,g%c(3)%sc))
         case default
         stop 'Error: edgeDir must = 1,2,3 in myEdge2CellCenterSF in interpOps.f90'
         end select
         call myEdge2Face(tempF,edge,g,edgeDir,faceDir)
         call myFace2CellCenter(cellCenter,tempF,g,faceDir)
         deallocate(tempF)
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ****************************** VECTOR-FIELD INTERPOLATIONS *****************************
       ! ****************************************************************************************
       ! ****************************************************************************************


       ! ****************************************************************************************
       ! ********************************* FACE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myFace2CellCenterVF(cellCenter,face,g)
         implicit none
         type(vectorField),intent(inout) :: cellCenter
         type(vectorField),intent(in)    :: face
         type(grid),intent(in) :: g
         call myFace2CellCenter(cellCenter%x,face%x,g,1)
         call myFace2CellCenter(cellCenter%y,face%y,g,2)
         call myFace2CellCenter(cellCenter%z,face%z,g,3)
       end subroutine

       subroutine myFace2NodeVF(node,face,g)
         implicit none
         type(vectorField),intent(inout) :: node
         type(vectorField),intent(in) :: face
         type(grid),intent(in) :: g
         call myFace2Node(node%x,face%x,g,1)
         call myFace2Node(node%y,face%y,g,2)
         call myFace2Node(node%z,face%z,g,3)
       end subroutine

       ! ****************************************************************************************
       ! *********************************** CC INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myCellCenter2FaceVF(face,cellCenter,g)
         implicit none
         type(vectorField),intent(inout) :: face
         type(vectorField),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         call myCellCenter2Face(face%x,cellCenter%x,g,1)
         call myCellCenter2Face(face%y,cellCenter%y,g,2)
         call myCellCenter2Face(face%z,cellCenter%z,g,3)
       end subroutine

       subroutine myCellCenter2Face2VF(face,cellCenter,g)
         implicit none
         type(vectorField),intent(inout) :: face
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         call myCellCenter2Face(face%x,cellCenter,g,1)
         call myCellCenter2Face(face%y,cellCenter,g,2)
         call myCellCenter2Face(face%z,cellCenter,g,3)
       end subroutine

       subroutine myCellCenter2NodeVF(node,cellCenter,g)
         implicit none
         type(vectorField),intent(inout) :: node
         type(vectorField),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         call myCellCenter2Node(node%x,cellCenter%x,g)
         call myCellCenter2Node(node%y,cellCenter%y,g)
         call myCellCenter2Node(node%z,cellCenter%z,g)
       end subroutine

       subroutine myCellCenter2EdgeVF(edge,cellCenter,g)
         implicit none
         type(vectorField),intent(inout) :: edge
         type(vectorField),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         call myCellCenter2Edge(edge%x,cellCenter%x,g,1)
         call myCellCenter2Edge(edge%y,cellCenter%y,g,2)
         call myCellCenter2Edge(edge%z,cellCenter%z,g,3)
       end subroutine

       subroutine myCellCenter2Edge2VF(edge,cellCenter,g)
         implicit none
         type(vectorField),intent(inout) :: edge
         real(cp),dimension(:,:,:),intent(in) :: cellCenter
         type(grid),intent(in) :: g
         call myCellCenter2Edge(edge%x,cellCenter,g,1)
         call myCellCenter2Edge(edge%y,cellCenter,g,2)
         call myCellCenter2Edge(edge%z,cellCenter,g,3)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* NODE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myNode2EdgeVF(edge,node,g)
         implicit none
         type(vectorField),intent(inout) :: edge
         type(vectorField),intent(in) :: node
         type(grid),intent(in) :: g
         call myNode2Edge(edge%x,node%x,g,1)
         call myNode2Edge(edge%y,node%y,g,2)
         call myNode2Edge(edge%z,node%z,g,3)
       end subroutine

       subroutine myNode2Edge2VF(edge,node,g)
         implicit none
         type(vectorField),intent(inout) :: edge
         real(cp),dimension(:,:,:),intent(in) :: node
         type(grid),intent(in) :: g
         call myNode2Edge(edge%x,node,g,1)
         call myNode2Edge(edge%y,node,g,2)
         call myNode2Edge(edge%z,node,g,3)
       end subroutine

       subroutine myNode2FaceVF(face,node,g)
         implicit none
         type(vectorField),intent(inout) :: face
         type(vectorField),intent(in) :: node
         type(grid),intent(in) :: g
         call myNode2Face(face%x,node%x,g,1)
         call myNode2Face(face%y,node%y,g,2)
         call myNode2Face(face%z,node%z,g,3)
       end subroutine

       subroutine myNode2Face2VF(face,node,g)
         implicit none
         type(vectorField),intent(inout) :: face
         real(cp),dimension(:,:,:),intent(in) :: node
         type(grid),intent(in) :: g
         call myNode2Face(face%x,node,g,1)
         call myNode2Face(face%y,node,g,2)
         call myNode2Face(face%z,node,g,3)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* EDGE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine myEdge2NodeVF(node,edge,g)
         implicit none
         type(vectorField),intent(inout) :: node
         type(vectorField),intent(in) :: edge
         type(grid),intent(in) :: g
         call myEdge2Node(node%x,edge%x,g,1)
         call myEdge2Node(node%y,edge%y,g,2)
         call myEdge2Node(node%z,edge%z,g,3)
       end subroutine

       subroutine myEdge2CellCenterVF(cellCenter,edge,g)
         implicit none
         type(vectorField),intent(inout) :: cellCenter
         type(vectorField),intent(in) :: edge
         type(grid),intent(in) :: g
         call myEdge2CellCenter(cellCenter%x,edge%x,g,1)
         call myEdge2CellCenter(cellCenter%y,edge%y,g,2)
         call myEdge2CellCenter(cellCenter%z,edge%z,g,3)
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
       end function

       end module