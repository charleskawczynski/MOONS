       module ops_interp_mod
       ! Compiler flags: ( fopenmp, _DEBUG_INTERP_ )
       !
       ! Directions are frequently used in MOONS.
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
       use current_precision_mod
       use GF_interp_mod
       use GF_extrap_mod
       use face_edge_corner_indexing_mod
       use grid_mod
       use mesh_extend_mod
       use data_location_mod
       use apply_BCs_mod
       use SF_extend_mod
       use VF_mod
       use TF_mod
       implicit none


       ! VECTOR INTEGFACES:
       !        face2Face_VF(faceX,faceY,faceZ,face,m,tempCC)
       !        face2CellCenter_VF(cellCenter,face,m)
       !        face2Node_VF(node,face,m,tempE)
       !        cellCenter2Face_VF(face,cellCenter,m)
       !        cellCenter2Face2_VF(face,cellCenter,m)
       !        cellCenter2Node_VF(node,cellCenter,m,tempF,tempE)
       !        cellCenter2Edge_VF_VF(edge,cellCenter,m,tempF)
       !        cellCenter2Edge_VF_SF(edge,cellCenter,m,tempF)
       !        node2Edge_VF(edge,node,m)
       !        node2Edge2_VF(edge,node,m)
       !        node2Face_VF_VF(face,node,m,tempE)
       !        node2Face_VF_SF(face,node,m,tempE)
       !        edge2Node_VF(node,edge,m)
       !        edge2CellCenter_VF(cellCenter,edge,m,tempF)

       private

       ! ------------------------------- POSSIBLE ALIASES ---------------------------------
       ! public :: F2F,F2N,F2CC,F2E,CC2F,CC2E,CC2N,E2F,E2N,E2CC,N2F,N2E

       ! ------------------------------- INTERPOLATION ROUTINES ---------------------------------
       ! Base interpolation
       public :: interp               ! call interp(f,m,gd,dir)
       public :: extrap               ! call extrap(f,m,gd,dir)

       ! Derived interpolations (* = has vector interface)
       public :: face2Face            ! call face2Face(faceAve,f,m,dir,aveLoc)
       public :: face2Face_no_diag
       public :: face2Node            ! call face2Node(nodeAverage,face,m,dir)           *
       public :: face2CellCenter      ! call face2CellCenter(cellCenter,face,m,faceDir)  *
       public :: face2Edge            ! call face2Edge(edge,m,face,edgeDir)
       public :: face2Edge_no_diag

       public :: cellCenter2Face      ! call cellCenter2Face(face,cellCenter,m,faceDir)  *
       public :: cellCenter2Edge      ! call cellCenter2Edge(edge,cellCenter,m,edgeDir)  *
       public :: cellCenter2Node      ! call cellCenter2Node(nodeAverage,cellCenter,m)   *

       public :: edge2Face            ! call edge2Face(face,edge,m,edgeDir,faceDir)
       public :: edge2Face_no_diag
       public :: edge2Node            ! call edge2Node(node,edge,m,edgeDir)              *
       public :: edge2CellCenter      ! call edge2CellCenter(cellCenter,edge,m,edgeDir)

       public :: node2Face            ! call node2Face(face,node,m,faceDir)              *
       public :: node2Edge            ! call node2Edge(edge,node,m,edgeDir)              *
       public :: node2CellCenter      ! call node2CellCenter(CC,node,m,E_x,F_y)          *

       public :: any_to_node

       ! ****************** Raw interpolation / extrapolation routines ******************

       interface interp;              module procedure interpO2_SF;          end interface
       interface extrap;              module procedure extrapO2_SF;          end interface
       interface extrap;              module procedure extrapO2_VF;          end interface
       interface extrap;              module procedure extrapO2_TF;          end interface

       ! ********************************** SF routines **********************************

       interface face2CellCenter;     module procedure face2CellCenter_SF;    end interface
       interface face2Node;           module procedure face2Node_SF;          end interface
       interface face2Face;           module procedure face2Face_SF;          end interface
       interface face2Face;           module procedure face2Face_TF;          end interface
       interface face2Face_no_diag;   module procedure face2Face_no_diag_TF;  end interface
       interface face2Edge;           module procedure face2Edge_SF_3;        end interface
       interface face2Edge;           module procedure face2Edge_SF_1;        end interface
       interface face2Edge;           module procedure face2Edge_TF;          end interface
       interface face2Edge_no_diag;   module procedure face2Edge_TF_no_diag;  end interface

       interface cellCenter2Face;     module procedure cellCenter2Face_SF;    end interface
       interface cellCenter2Node;     module procedure cellCenter2Node_SF;    end interface
       interface cellCenter2Edge;     module procedure cellCenter2Edge_SF;    end interface

       interface node2Edge;           module procedure node2Edge_SF;          end interface
       interface node2Face;           module procedure node2Face_SF;          end interface
       interface node2CellCenter;     module procedure node2CellCenter_SF;    end interface

       interface edge2Face;           module procedure edge2Face_SF;          end interface
       interface edge2CellCenter;     module procedure edge2CellCenter_SF;    end interface
       interface edge2Node;           module procedure edge2Node_SF;          end interface
       interface edge2Face_no_diag;   module procedure edge2Face_no_diag_TF;  end interface

       interface any_to_node;         module procedure any_to_node_SF;        end interface

       ! ********************************** VF routines **********************************

       interface face2Face;           module procedure face2Face_VF;          end interface
       interface face2CellCenter;     module procedure face2CellCenter_VF;    end interface
       interface face2Node;           module procedure face2Node_VF;          end interface

       interface cellCenter2Face;     module procedure cellCenter2Face_VF_VF; end interface
       interface cellCenter2Face;     module procedure cellCenter2Face_VF_SF; end interface
       interface cellCenter2Face;     module procedure cellCenter2Face_TF_VF; end interface
       interface cellCenter2Node;     module procedure cellCenter2Node_VF;    end interface
       interface cellCenter2Edge;     module procedure cellCenter2Edge_VF_VF; end interface
       interface cellCenter2Edge;     module procedure cellCenter2Edge_VF_SF; end interface

       interface edge2Node;           module procedure edge2Node_VF;          end interface
       interface edge2CellCenter;     module procedure edge2CellCenter_VF;    end interface

       interface node2Face;           module procedure node2Face_VF_VF;       end interface
       interface node2Face;           module procedure node2Face_VF_SF;       end interface

       interface node2Edge;           module procedure node2Edge_VF;          end interface
       interface node2Edge;           module procedure node2Edge2_VF;         end interface

       interface any_to_node;         module procedure any_to_node_VF;        end interface

       contains

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ********************************* BASE INTERPOLATION ***********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine extrapO2_VF(f,m)
         implicit none
         type(VF),intent(inout) :: f
         type(mesh),intent(in) :: m
         call extrap(f%x,m)
         call extrap(f%y,m)
         call extrap(f%z,m)
       end subroutine

       subroutine extrapO2_TF(f,m)
         implicit none
         type(TF),intent(inout) :: f
         type(mesh),intent(in) :: m
         call extrap(f%x,m)
         call extrap(f%y,m)
         call extrap(f%z,m)
       end subroutine

       subroutine extrapO2_SF(f,m)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer :: i,k
         do k=1,3; do i=1,m%s
           call extrap(f%BF(i)%GF,k) ! self-extrapolation
         enddo; enddo
       end subroutine

       subroutine interpO2_SF(f,g,m,dir)
         implicit none
         type(SF),intent(inout) :: f
         type(SF),intent(in) :: g
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer :: i
         if (N_along(f%DL,dir).and.CC_along(g%DL,dir)) then
           do i=1,m%s
             call interp_C2N(f%BF(i)%GF,&
                             g%BF(i)%GF,&
                             m%B(i)%g,&
                             dir,&
                             m%MP%int_tensor(dir)%eye(1),&
                             m%MP%int_tensor(dir)%eye(2),&
                             m%MP%int_tensor(dir)%eye(3))
           enddo
         elseif (CC_along(f%DL,dir).and.N_along(g%DL,dir)) then
           do i=1,m%s
             call interp_N2C(f%BF(i)%GF,&
                             g%BF(i)%GF,&
                             dir,&
                             m%MP%int_tensor(dir)%eye(1),&
                             m%MP%int_tensor(dir)%eye(2),&
                             m%MP%int_tensor(dir)%eye(3))
           enddo
         else
           write(*,*) ' ---------------- f ---------------- '; call print(f%DL)
           write(*,*) ' ---------------- g ---------------- '; call print(g%DL)
           stop 'Error: bad DLs in interpO2_SF in ops_interp.f90'
         endif
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ****************************** SCALAR-FIELD INTERPOLATIONS *****************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       ! ****************************************************************************************
       ! ********************************* FACE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine face2CellCenter_SF(cellCenter,face,m,faceDir)
         ! 1 interpolation routine (no allocation required)
         implicit none
         type(SF),intent(inout) :: cellCenter
         type(SF),intent(in)    :: face
         type(mesh),intent(in) :: m
         integer,intent(in) :: faceDir
         call interp(cellCenter,face,m,faceDir)
       end subroutine

       subroutine face2Edge_SF_1(edge,face,m,faceDir,edgeDir)
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
         type(SF),intent(inout) :: edge
         type(SF),intent(in)    :: face
         type(mesh),intent(in) :: m
         integer,intent(in) :: edgeDir,faceDir
         integer :: orthDir
         if (edgeDir.ne.faceDir) then ! Requires 1 interpolation (no allocations)
           orthDir = orth_dir((/edgeDir,faceDir/))
           call interp(edge,face,m,orthDir)
         else ! Requires 3 interpolations ()
           stop 'Error: use face2Edge_SF_3 for edgeDir = faceDir in face2Edge_SF_1 in ops_interp.f90'
         endif
       end subroutine

       subroutine face2Edge_SF_3(edge,face,m,tempCC,tempF,faceDir,edgeDir)
         ! Requires 3 interpolations
         ! NOTE: tempF = (y,x,x) for edgeDir = (1,2,3)
         implicit none
         type(SF),intent(inout) :: edge,tempF,tempCC
         type(SF),intent(in)    :: face
         type(mesh),intent(in) :: m
         integer,intent(in) :: edgeDir,faceDir
         if (edgeDir.eq.faceDir) then ! requires 1 interpolation (no allocations)
           call face2CellCenter(tempCC,face,m,faceDir)
           call cellCenter2Edge(edge,tempCC,m,tempF,edgeDir)
         else; stop 'Error: edgeDir must = faceDir in this face2Edge_SF_3'
         endif
       end subroutine

       subroutine face2Face_SF(faceAve,face,m,tempCC,faceDir,aveLoc)
         implicit none
         type(SF),intent(inout) :: faceAve,tempCC
         type(SF),intent(in) :: face
         type(mesh),intent(in) :: m
         integer,intent(in) :: faceDir,aveLoc
         call face2CellCenter(tempCC,face,m,faceDir)
         call cellCenter2Face(faceAve,tempCC,m,aveLoc)
       end subroutine

       subroutine face2Node_SF(node,face,m,faceDir,tempE)
         implicit none
         type(SF),intent(inout) :: node
         type(SF),intent(in) :: face
         type(mesh),intent(in) :: m
         integer,intent(in) :: faceDir
         type(SF),intent(inout) :: tempE
         integer :: edgeDir
         select case (faceDir)
         case (1); edgeDir = 2
         case (2); edgeDir = 1
         case (3); edgeDir = 1
         case default; stop 'Error: faceDir must = 1,2,3 in face2Node_SF in ops_interp.f90'
         end select
         call face2Edge(tempE,face,m,faceDir,edgeDir)
         call edge2Node(node,tempE,m,edgeDir)
       end subroutine

       ! ****************************************************************************************
       ! *********************************** CC INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine cellCenter2Face_SF(face,cellCenter,m,faceDir)
         implicit none
         type(SF),intent(inout) :: face
         type(SF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         integer,intent(in) :: faceDir
         call interp(face,cellCenter,m,faceDir)
       end subroutine

       subroutine cellCenter2Node_SF(node,cellCenter,m,tempFx,tempEz)
         implicit none
         type(SF),intent(inout) :: node
         type(SF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempFx,tempEz
         call cellCenter2Face(tempFx,cellCenter,m,1)
         call face2Edge(tempEz,tempFx,m,1,3)
         call edge2Node(node,tempEz,m,3)
       end subroutine

       subroutine cellCenter2Edge_SF(edge,cellCenter,m,tempF,edgeDir)
         implicit none
         type(SF),intent(inout) :: edge,tempF
         type(SF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         integer,intent(in) :: edgeDir
         integer :: d_F2E,d_C2F
         select case (edgeDir)
         case(1); d_F2E = 2; d_C2F = 2
         case(2); d_F2E = 1; d_C2F = 1
         case(3); d_F2E = 1; d_C2F = 1
         case default; stop 'Error: edgeDir must = 1,2,3 in cellCenter2Edge_SF in ops_interp.f90'
         end select
         call cellCenter2Face(tempF,cellCenter,m,d_C2F)
         call face2Edge(edge,tempF,m,d_F2E,edgeDir)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* NODE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine node2Face_SF(face,node,m,tempEyxx,faceDir)
         implicit none
         type(SF),intent(inout) :: face,tempEyxx
         type(SF),intent(in) :: node
         type(mesh),intent(in) :: m
         integer,intent(in) :: faceDir
         integer :: d
         select case (faceDir)
         case (1); d = 2
         case (2); d = 1
         case (3); d = 1
         case default; stop 'Error: faceDir must = 1,2,3 in node2Face_SF in ops_interp.f90'
         end select
         call node2Edge(tempEyxx,node,m,d)
         call edge2Face(face,tempEyxx,m,d,faceDir)
       end subroutine

       subroutine node2CellCenter_SF(CC,node,m,E_x,F_y)
         implicit none
         type(SF),intent(inout) :: CC,E_x,F_y
         type(SF),intent(in) :: node
         type(mesh),intent(in) :: m
         call node2Edge(E_x,node,m,1)
         call edge2Face(F_y,E_x,m,1,2)
         call face2CellCenter(CC,F_y,m,2)
       end subroutine

       subroutine node2Edge_SF(edge,node,m,edgeDir)
         implicit none
         type(SF),intent(inout) :: edge
         type(SF),intent(in) :: node
         type(mesh),intent(in) :: m
         integer,intent(in) :: edgeDir
         call interp(edge,node,m,edgeDir)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* EDGE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine edge2Face_SF(face,edge,m,edgeDir,faceDir)
         implicit none
         type(SF),intent(inout) :: face
         type(SF),intent(in) :: edge
         type(mesh),intent(in) :: m
         integer,intent(in) :: faceDir,edgeDir
         integer :: orthDir
         if (edgeDir.ne.faceDir) then ! Requires 1 interpolation (no allocations)
           orthDir = orth_dir((/edgeDir,faceDir/))
           call interp(face,edge,m,orthDir)
         else ! Requires 3 interpolations ()
           stop 'Error: edgeDir=faceDir, need more temps for this interp in edge2Face_SF in ops_interp.f90'
         endif
       end subroutine

       subroutine edge2CellCenter_SF(cellCenter,edge,m,tempF,edgeDir)
         implicit none
         type(SF),intent(inout) :: cellCenter
         type(SF),intent(in) :: edge
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempF
         integer,intent(in) :: edgeDir
         integer :: faceDir
         select case (edgeDir)
         case (1); faceDir = 2
         case (2); faceDir = 3
         case (3); faceDir = 1
         case default; stop 'Error: edgeDir must = 1,2,3 in edge2CellCenter_GF in interpOps.f90'
         end select
         call edge2Face(tempF,edge,m,edgeDir,faceDir)
         call face2CellCenter(cellCenter,tempF,m,faceDir)
       end subroutine

       subroutine edge2Node_SF(node,edge,m,edgeDir)
         implicit none
         type(SF),intent(inout) :: node
         type(SF),intent(in) :: edge
         type(mesh),intent(in) :: m
         integer,intent(in) :: edgeDir
         call interp(node,edge,m,edgeDir)
       end subroutine


       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ****************************** VECTOR-FIELD INTERPOLATIONS *****************************
       ! ****************************************************************************************
       ! ****************************************************************************************


       ! ****************************************************************************************
       ! ********************************* FACE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine face2Face_VF(faceX,faceY,faceZ,face,m,tempCC)
         ! [U_ave,V_ave,W_ave] = interp(U)
         implicit none
         type(VF),intent(inout) :: faceX,faceY,faceZ
         type(VF),intent(in)    :: face
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempCC

         call assign(faceX%x,face%x)
         call assign(faceY%y,face%y)
         call assign(faceZ%z,face%z)

         call face2Face(faceX%y,face%x,m,tempCC,1,2)
         call face2Face(faceX%z,face%x,m,tempCC,1,3)

         call face2Face(faceY%x,face%y,m,tempCC,2,1)
         call face2Face(faceY%z,face%y,m,tempCC,2,3)

         call face2Face(faceZ%x,face%z,m,tempCC,3,1)
         call face2Face(faceZ%y,face%z,m,tempCC,3,2)
       end subroutine

       subroutine face2Face_TF(face_TF,face_VF,m,tempCC)
         ! [U_ave,V_ave,W_ave] = interp(U)
         implicit none
         type(TF),intent(inout) :: face_TF
         type(VF),intent(in)    :: face_VF
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempCC
         call assign(face_TF%x%x,face_VF%x)
         call face2Face(face_TF%x%y,face_VF%x,m,tempCC,1,2)
         call face2Face(face_TF%x%z,face_VF%x,m,tempCC,1,3)

         call assign(face_TF%y%y,face_VF%y)
         call face2Face(face_TF%y%x,face_VF%y,m,tempCC,2,1)
         call face2Face(face_TF%y%z,face_VF%y,m,tempCC,2,3)

         call assign(face_TF%z%z,face_VF%z)
         call face2Face(face_TF%z%x,face_VF%z,m,tempCC,3,1)
         call face2Face(face_TF%z%y,face_VF%z,m,tempCC,3,2)
       end subroutine

       subroutine face2Face_no_diag_TF(face_TF,face_VF,m,tempCC)
         ! [U_ave,V_ave,W_ave] = interp(U)
         implicit none
         type(TF),intent(inout) :: face_TF
         type(VF),intent(in)    :: face_VF
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempCC
         call face2Face(face_TF%x%y,face_VF%x,m,tempCC,1,2)
         call face2Face(face_TF%x%z,face_VF%x,m,tempCC,1,3)
         call face2Face(face_TF%y%x,face_VF%y,m,tempCC,2,1)
         call face2Face(face_TF%y%z,face_VF%y,m,tempCC,2,3)
         call face2Face(face_TF%z%x,face_VF%z,m,tempCC,3,1)
         call face2Face(face_TF%z%y,face_VF%z,m,tempCC,3,2)
       end subroutine

       subroutine face2Edge_TF(edge,face,m,tempCC,tempF)
         ! [U_ave,V_ave,W_ave] = interp(U)
         implicit none
         type(TF),intent(inout) :: edge
         type(VF),intent(in) :: face
         type(VF),intent(inout) :: tempF
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempCC
         ! tempF = (y,x,x) for edgeDir = (1,2,3) tempCC,tempF
         call face2Edge(edge%x%x,face%x,m,tempCC,tempF%y,1,1)
         call face2Edge(edge%x%y,face%x,m,1,2)
         call face2Edge(edge%x%z,face%x,m,1,3)
         call face2Edge(edge%y%x,face%y,m,2,1)
         call face2Edge(edge%y%y,face%y,m,tempCC,tempF%x,2,2)
         call face2Edge(edge%y%z,face%y,m,2,3)
         call face2Edge(edge%z%x,face%z,m,3,1)
         call face2Edge(edge%z%y,face%z,m,3,2)
         call face2Edge(edge%z%z,face%z,m,tempCC,tempF%x,3,3)
       end subroutine

       subroutine face2Edge_TF_no_diag(edge,face,m)
         ! [U_ave,V_ave,W_ave] = interp(U)
         implicit none
         type(TF),intent(inout) :: edge
         type(VF),intent(in) :: face
         type(mesh),intent(in) :: m
         ! tempF = (y,x,x) for edgeDir = (1,2,3) tempCC,tempF
         call face2Edge(edge%x%y,face%x,m,1,2)
         call face2Edge(edge%x%z,face%x,m,1,3)
         call face2Edge(edge%y%x,face%y,m,2,1)
         call face2Edge(edge%y%z,face%y,m,2,3)
         call face2Edge(edge%z%x,face%z,m,3,1)
         call face2Edge(edge%z%y,face%z,m,3,2)
       end subroutine

       subroutine face2CellCenter_VF(cellCenter,face,m)
         implicit none
         type(VF),intent(inout) :: cellCenter
         type(VF),intent(in)    :: face
         type(mesh),intent(in) :: m
         call face2CellCenter(cellCenter%x,face%x,m,1)
         call face2CellCenter(cellCenter%y,face%y,m,2)
         call face2CellCenter(cellCenter%z,face%z,m,3)
       end subroutine

       subroutine face2Node_VF(node,face,m,tempE)
         implicit none
         type(VF),intent(inout) :: node,tempE
         type(VF),intent(in) :: face
         type(mesh),intent(in) :: m
         call face2Node(node%x,face%x,m,1,tempE%y)
         call face2Node(node%y,face%y,m,2,tempE%x)
         call face2Node(node%z,face%z,m,3,tempE%x)
       end subroutine

       ! ****************************************************************************************
       ! *********************************** CC INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine cellCenter2Face_VF_VF(face,cellCenter,m)
         implicit none
         type(VF),intent(inout) :: face
         type(VF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         call cellCenter2Face(face%x,cellCenter%x,m,1)
         call cellCenter2Face(face%y,cellCenter%y,m,2)
         call cellCenter2Face(face%z,cellCenter%z,m,3)
       end subroutine

       subroutine cellCenter2Face_VF_SF(face,cellCenter,m)
         implicit none
         type(VF),intent(inout) :: face
         type(SF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         call cellCenter2Face(face%x,cellCenter,m,1)
         call cellCenter2Face(face%y,cellCenter,m,2)
         call cellCenter2Face(face%z,cellCenter,m,3)
       end subroutine

       subroutine cellCenter2Face_TF_VF(face,cellCenter,m)
         implicit none
         type(TF),intent(inout) :: face
         type(VF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         call cellCenter2Face(face%x,cellCenter%x,m) ! Interp SF to VF
         call cellCenter2Face(face%y,cellCenter%y,m) ! Interp SF to VF
         call cellCenter2Face(face%z,cellCenter%z,m) ! Interp SF to VF
       end subroutine

       subroutine cellCenter2Node_VF(node,cellCenter,m,tempF,tempE)
         implicit none
         type(VF),intent(inout) :: node,tempF,tempE
         type(VF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         call cellCenter2Node(node%x,cellCenter%x,m,tempF%x,tempE%z)
         call cellCenter2Node(node%y,cellCenter%y,m,tempF%x,tempE%z)
         call cellCenter2Node(node%z,cellCenter%z,m,tempF%x,tempE%z)
       end subroutine

       subroutine cellCenter2Edge_VF_VF(edge,cellCenter,m,tempF)
         implicit none
         type(VF),intent(inout) :: edge,tempF
         type(VF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         call cellCenter2Edge(edge%x,cellCenter%x,m,tempF%y,1)
         call cellCenter2Edge(edge%y,cellCenter%y,m,tempF%x,2)
         call cellCenter2Edge(edge%z,cellCenter%z,m,tempF%x,3)
       end subroutine

       subroutine cellCenter2Edge_VF_SF(edge,cellCenter,m,tempF)
         implicit none
         type(VF),intent(inout) :: edge,tempF
         type(SF),intent(in) :: cellCenter
         type(mesh),intent(in) :: m
         call cellCenter2Edge(edge%x,cellCenter,m,tempF%y,1)
         call cellCenter2Edge(edge%y,cellCenter,m,tempF%x,2)
         call cellCenter2Edge(edge%z,cellCenter,m,tempF%x,3)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* NODE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine node2Edge_VF(edge,node,m)
         implicit none
         type(VF),intent(inout) :: edge
         type(VF),intent(in) :: node
         type(mesh),intent(in) :: m
         call node2Edge(edge%x,node%x,m,1)
         call node2Edge(edge%y,node%y,m,2)
         call node2Edge(edge%z,node%z,m,3)
       end subroutine

       subroutine node2Edge2_VF(edge,node,m)
         implicit none
         type(VF),intent(inout) :: edge
         type(SF),intent(in) :: node
         type(mesh),intent(in) :: m
         call node2Edge(edge%x,node,m,1)
         call node2Edge(edge%y,node,m,2)
         call node2Edge(edge%z,node,m,3)
       end subroutine

       subroutine node2Face_VF_VF(face,node,m,tempE)
         implicit none
         type(VF),intent(inout) :: face,tempE
         type(VF),intent(in) :: node
         type(mesh),intent(in) :: m
         call node2Face(face%x,node%x,m,tempE%y,1)
         call node2Face(face%y,node%y,m,tempE%x,2)
         call node2Face(face%z,node%z,m,tempE%x,3)
       end subroutine

       subroutine node2Face_VF_SF(face,node,m,tempE)
         implicit none
         type(VF),intent(inout) :: face,tempE
         type(SF),intent(in) :: node
         type(mesh),intent(in) :: m
         call node2Face(face%x,node,m,tempE%y,1)
         call node2Face(face%y,node,m,tempE%x,2)
         call node2Face(face%z,node,m,tempE%x,3)
       end subroutine

       ! ****************************************************************************************
       ! ********************************* EDGE INTERPOLATIONS **********************************
       ! ****************************************************************************************

       subroutine edge2Node_VF(node,edge,m)
         implicit none
         type(VF),intent(inout) :: node
         type(VF),intent(in) :: edge
         type(mesh),intent(in) :: m
         call edge2Node(node%x,edge%x,m,1)
         call edge2Node(node%y,edge%y,m,2)
         call edge2Node(node%z,edge%z,m,3)
       end subroutine

       subroutine edge2CellCenter_VF(cellCenter,edge,m,tempF)
         implicit none
         type(VF),intent(inout) :: cellCenter,tempF
         type(VF),intent(in) :: edge
         type(mesh),intent(in) :: m
         call edge2CellCenter(cellCenter%x,edge%x,m,tempF%y,1) ! temp%F is correct. Refer to edge2CellCenter
         call edge2CellCenter(cellCenter%y,edge%y,m,tempF%z,2) ! temp%F is correct. Refer to edge2CellCenter
         call edge2CellCenter(cellCenter%z,edge%z,m,tempF%x,3) ! temp%F is correct. Refer to edge2CellCenter
       end subroutine

       subroutine edge2Face_no_diag_TF(face,edge,m)
         implicit none
         type(TF),intent(inout) :: face
         type(VF),intent(in) :: edge
         type(mesh),intent(in) :: m
         call edge2Face(face%x%y,edge%x,m,1,2)
         call edge2Face(face%x%z,edge%x,m,1,3)
         call edge2Face(face%y%x,edge%y,m,2,1)
         call edge2Face(face%y%z,edge%y,m,2,3)
         call edge2Face(face%z%x,edge%z,m,3,1)
         call edge2Face(face%z%y,edge%z,m,3,2)
       end subroutine

       subroutine any_to_node_SF(m,node,x,edge,face)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         type(SF),intent(inout) :: node,edge,face
         integer,dimension(3) :: i
         integer :: i_f,i_e
         if (is_CC(x%DL)) then
           call init_Face(face,m,1)
           call init_Edge(edge,m,3)
           call init_Node(node,m)
           call cellcenter2Node(node,x,m,face,edge)
           call delete(face)
           call delete(edge)
         elseif (is_Node(x%DL)) then
         call init_Node(node,m)
         call assign(node,x)
         elseif (is_Face(x%DL)) then
           i = (/2,1,1/)
           i_f = get_Face(x%DL)
           call init_Edge(edge,m,i(i_f))
           call init_Node(node,m)
           call face2Node(node,x,m,i_f,edge)
           call delete(edge)
         elseif (is_Edge(x%DL)) then
           i_e = get_Edge(x%DL)
           call init_Node(node,m)
           call edge2Node(node,x,m,i_e)
         else; stop 'Error: bad DL to any_to_node_SF in ops_interp.f90'
         endif
       end subroutine

       subroutine any_to_node_VF(m,node,x,edge,face)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         type(VF),intent(inout) :: node,edge,face
         if (is_CC(x)) then
#ifdef _DEBUG_INTERP_
           call insist_collocated(x,'any_to_node_VF')
#endif
           call init_Face(face,m)
           call init_Edge(edge,m)
           call init_Node(node,m)
           call cellcenter2Node(node,x,m,face,edge)
           call delete(face)
           call delete(edge)
         elseif (is_Node(x)) then
#ifdef _DEBUG_INTERP_
           call insist_collocated(x,'any_to_node_VF')
#endif
         call init_Node(node,m)
         call assign(node,x)
         elseif (is_Face(x)) then
           call init_Edge(edge,m)
           call init_Node(node,m)
           call face2Node(node,x,m,edge)
           call delete(edge)
         elseif (is_Edge(x)) then
           call init_Node(node,m)
           call edge2Node(node,x,m)
         else; stop 'Error: bad DL to any_to_node_VF in ops_interp.f90'
         endif
       end subroutine

       end module