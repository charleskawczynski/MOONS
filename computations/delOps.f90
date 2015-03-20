       module delOps_mod
       ! This module uses del frequently. For a reference,
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
       use del_mod
       use grid_mod
       use interpOps_mod

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
       public :: CC2CCLap             ! call CC2CCLap(lapU,u,gd)
       public :: myCC2FaceGrad        ! call myCC2FaceGrad(gradx,grady,gradz,p,gd)
       public :: myCC2CCDel           ! call myCC2CCDel(gradp,p,gd,dir)
       public :: myCCCurl             ! call myCCCurl(curl,u,v,w,gd,dir)
       public :: myCC2EdgeCurl        ! call myCC2EdgeCurl(curl,u,v,w,gd,dir)
       public :: myCCVaryDel          ! call myCCVaryDel(ddf,u,v,w,k,gd,dir1,dir2)

       ! Node-center based derivatives
       ! public :: myNodeGrad           ! call myNodeGrad(gradx,grady,gradz,f,gd)
       ! public :: myNode2NodeDel       ! call myNode2NodeDel(df,f,gd,dir)
       ! public :: myNodeDiv            ! call myNodeDiv(divU,u,v,w,gd)
       public :: myNodeLap            ! call myNodeLap(lapU,u,gd)
       ! public :: myNodeCurl           ! call myNodeCurl(curlU,u,v,w,gd[,n])

       ! Edge-centered derivatives
       public :: myEdge2FaceCurl      ! call myEdge2FaceCurl(curl,u,v,w,gd,curlDir)
       
       ! ----------------------------- SPECIAL TERMS -------------------------------------------
       public :: myCCBfieldAdvect            ! call myCCBfieldAdvect(div,u,v,w,Bx,By,Bz,gd,dir)
       public :: myCCBfieldDiffuse           ! call myCCBfieldDiffuse(div,Bx,By,Bz,sigma,mu,gd,dir)

       interface CC2CCLap;   module procedure CC2CCLapVariCoeff;   end interface
       interface CC2CCLap;   module procedure CC2CCLapUnifCoeff;   end interface


       contains

       ! ******************************* OTHER ROUTINES *********************************

       subroutine myNodeMagnitude(mag,u,v,w) ! Finished
         ! This routine was made in order to compare norm(B) with
         ! results from Salah
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         real(cp),dimension(:,:,:),intent(inout) :: mag
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(u)
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           mag(i,j,k) = sqrt(u(i,j,k)**real(2.0,cp) +&
           v(i,j,k)**real(2.0,cp) + w(i,j,k)**real(2.0,cp))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine myCollocatedCross(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir) ! Finished
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: AcrossB
         real(cp),dimension(:,:,:),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,intent(in) :: dir
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(Ax)
         select case (dir)
         case (1)
         !$OMP PARALLEL DO
          do k=1,s(3); do j=1,s(2); do i=1,s(1); 
            AcrossB(i,j,k) = Ay(i,j,k)*Bz(i,j,k) - Az(i,j,k)*By(i,j,k)
          enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (2)
         !$OMP PARALLEL DO
          do k=1,s(3); do j=1,s(2); do i=1,s(1)
            AcrossB(i,j,k) = -(Ax(i,j,k)*Bz(i,j,k) - Az(i,j,k)*Bx(i,j,k))
          enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (3)
         !$OMP PARALLEL DO
          do k=1,s(3); do j=1,s(2); do i=1,s(1)
            AcrossB(i,j,k) = Ax(i,j,k)*By(i,j,k) - Ay(i,j,k)*Bx(i,j,k)
          enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case default
           stop 'Error: dir must = 1,2,3 in myCollocatedCross.'
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
#ifdef _DEBUG_VECTOROPS_
         if (dir1.eq.dir2) then
           stop 'There are no orthogonal directions'
         endif
#endif
       end function

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* FACE BASED DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myFaceDiv(divF,u,v,w,g) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divF
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         type(del) :: d
         integer,dimension(3) :: s

         call d%assign(divF,u,g,1,1,1) ! Padding avoids calcs on fictive cells
            call d%add(divF,v,g,1,2,1) ! Padding avoids calcs on fictive cells
            call d%add(divF,w,g,1,3,1) ! Padding avoids calcs on fictive cells
         s = shape(divF)
         divF(1,:,:) = real(0.0,cp); divF(s(1),:,:) = real(0.0,cp)
         divF(:,1,:) = real(0.0,cp); divF(:,s(2),:) = real(0.0,cp)
         divF(:,:,1) = real(0.0,cp); divF(:,:,s(3)) = real(0.0,cp)
       end subroutine

       subroutine myFaceLap(lapF,f,g) ! Finished
         ! Consider migrating to momentum solver
         ! try to zero boundaries instead of the whole 3D volume
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapF
         real(cp),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: g
         type(del) :: d
         integer,dimension(3) :: s,sdf
         ! Padding is necessary here!!!
         call d%assign(lapF,f,g,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapF,f,g,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapF,f,g,2,3,1) ! Padding avoids calcs on fictive cells
         s = shape(f); sdf = shape(lapF)

         ! Remove the source term on the boundaries:
         ! This seems to be necessary to enforce div(U) = 0
         ! This term acts as a source in Poisson's equation to
         ! solve for pressure. When the wall-normal boundary values
         ! are non-zero, the velocity does not satisfy div(u) = 0
         ! after the pressure correction.
         ! 
         ! Using this seems to produce the same result that 
         ! MOONS was producing earlier. This has not been tested for
         ! Neumann BCs yet.
         ! 
         ! Note that the non-linear term is zero on the boundary since the
         ! derivative of a velocity in any tangential direction will always
         ! be zero within machine accuracy (for dirichlet conditions).
         if (s(1).eq.g%c(1)%sn) then
           lapF(2,:,:) = real(0.0,cp); lapF(sdf(1)-1,:,:) = real(0.0,cp)
         elseif (s(2).eq.g%c(2)%sn) then
           lapF(:,2,:) = real(0.0,cp); lapF(:,sdf(2)-1,:) = real(0.0,cp)
         elseif (s(3).eq.g%c(3)%sn) then
           lapF(:,:,2) = real(0.0,cp); lapF(:,:,sdf(3)-1) = real(0.0,cp)
         endif
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
         real(cp),dimension(:,:,:),intent(inout) :: psi
         real(cp),dimension(:,:,:),intent(in) :: u,v,w,phi
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp,tempAve
         integer,dimension(3) :: s
         type(del) :: d

         s = shape(phi)

         allocate(temp(s(1),s(2),s(3)))
         allocate(tempAve(s(1),s(2),s(3)))

         psi = real(0.0,cp)
         ! -------------------------- u d/dx --------------------------
         call myFaceAverage(tempAve,u,gd,1,dir)
         if (dir.eq.1) then
               call d%assign(temp,phi,gd,1,1,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,gd,1,1,1)
         endif
         psi = tempAve*temp

         ! -------------------------- v d/dy --------------------------
         call myFaceAverage(tempAve,v,gd,2,dir)

         if (dir.eq.2) then
               call d%assign(temp,phi,gd,1,2,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,gd,1,2,1)
         endif
         psi = psi + tempAve*temp

         ! -------------------------- w d/dz --------------------------
         call myFaceAverage(tempAve,w,gd,3,dir)
         if (dir.eq.3) then
               call d%assign(temp,phi,gd,1,3,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,gd,1,3,1)
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
         real(cp),dimension(:,:,:),intent(inout) :: psi
         real(cp),dimension(:,:,:),intent(in) :: u,v,w,ui
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         real(cp),dimension(:,:,:),allocatable :: tempAveCC
         real(cp),dimension(:,:,:),allocatable :: tempAveE1,tempAveE2
         integer,dimension(3) :: s
         integer :: x,y,z,orthDir
         type(del) ::d

         s = shape(ui)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must =1,2,3 in myFaceAdvectDonor.'; stop
         end select

         allocate(tempAveCC(s(1)-x,s(2)-y,s(3)-z))

         psi = real(0.0,cp)
         ! -------------------------- d/dx (u u_i) --------------------------
         if (faceDir.eq.1) then
           call myFace2CellCenter(tempAveCC,u,gd,1)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,gd,1,1,1)
         else
           allocate(tempAveE1(s(1)+1,s(2),s(3)))
           allocate(tempAveE2(s(1)+1,s(2),s(3)))

           orthDir = orthogonalDirection(faceDir,1)
           call myFace2Edge(tempAveE1,u,gd,1,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,gd,1,1,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dy (v u_i) --------------------------
         if (faceDir.eq.2) then
           call myFace2CellCenter(tempAveCC,v,gd,2)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,gd,1,2,1)
         else
           allocate(tempAveE1(s(1),s(2)+1,s(3)))
           allocate(tempAveE2(s(1),s(2)+1,s(3)))
           orthDir = orthogonalDirection(faceDir,2)
           call myFace2Edge(tempAveE1,v,gd,2,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,gd,1,2,1) ! upwind from edge to cc
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dz (w u_i) --------------------------
         if (faceDir.eq.3) then
           call myFace2CellCenter(tempAveCC,w,gd,3)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,gd,1,3,1)
         else
           allocate(tempAveE1(s(1),s(2),s(3)+1))
           allocate(tempAveE2(s(1),s(2),s(3)+1))

           orthDir = orthogonalDirection(faceDir,3)
           call myFace2Edge(tempAveE1,w,gd,3,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,gd,1,3,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         deallocate(tempAveCC)
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
         real(cp),dimension(:,:,:),intent(inout) :: psi
         real(cp),dimension(:,:,:),intent(in) :: u,v,w,ui
         type(grid),intent(in) :: gd
         integer,intent(in) :: faceDir
         real(cp),dimension(:,:,:),allocatable :: tempAveCC
         real(cp),dimension(:,:,:),allocatable :: tempAveE1,tempAveE2
         integer,dimension(3) :: s
         integer :: x,y,z,orthDir
         type(del) :: d

         s = shape(ui)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must = 1,2,3 in myFaceAdvectHybrid.'; stop
         end select

         allocate(tempAveCC(s(1)-x,s(2)-y,s(3)-z))

         psi = real(0.0,cp)
         ! -------------------------- d/dx (u u_i) --------------------------
         if (faceDir.eq.1) then
           call myFace2CellCenter(tempAveCC,u,gd,1)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,gd,1,1,1)
         else
           allocate(tempAveE1(s(1)+1,s(2),s(3)))
           allocate(tempAveE2(s(1)+1,s(2),s(3)))

           orthDir = orthogonalDirection(faceDir,1)
           call myFace2Edge(tempAveE1,u,gd,1,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,gd,1,1,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dy (v u_i) --------------------------
         if (faceDir.eq.2) then
           call myFace2CellCenter(tempAveCC,v,gd,2)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,gd,1,2,1)
         else
           allocate(tempAveE1(s(1),s(2)+1,s(3)))
           allocate(tempAveE2(s(1),s(2)+1,s(3)))
           orthDir = orthogonalDirection(faceDir,2)
           call myFace2Edge(tempAveE1,v,gd,2,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,gd,1,2,1) ! upwind from edge to cc
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dz (w u_i) --------------------------
         if (faceDir.eq.3) then
           call myFace2CellCenter(tempAveCC,w,gd,3)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,gd,1,3,1)
         else
           allocate(tempAveE1(s(1),s(2),s(3)+1))
           allocate(tempAveE2(s(1),s(2),s(3)+1))

           orthDir = orthogonalDirection(faceDir,3)
           call myFace2Edge(tempAveE1,w,gd,3,orthDir)
           call myFace2Edge(tempAveE2,ui,gd,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,gd,1,3,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         deallocate(tempAveCC)
       end subroutine

       subroutine myFaceCurl(curlU,u,v,w,g,dir) ! Finished?
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curlU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         type(del) :: d

         select case (dir)
         case (1) ! x component
           call d%assign(curlU,w,g,1,2,0)
           call d%subtract(curlU,v,g,1,3,0)
         case (2) ! y component
           call d%assign(curlU,u,g,1,3,0)
           call d%subtract(curlU,w,g,1,1,0)
         case (3) ! z component
           call d%assign(curlU,v,g,1,1,0)
           call d%subtract(curlU,u,g,1,2,0)
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
         real(cp),dimension(:,:,:),intent(inout) :: divU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         type(del) :: d
         call d%assign(divU,u,gd,1,1,0)
            call d%add(divU,v,gd,1,2,0)
            call d%add(divU,w,gd,1,3,0)
       end subroutine

       subroutine CC2CCLapUnifCoeff(lapU,u,gd) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: gd
         type(del) :: d
         call d%assign(lapU,u,gd,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,gd,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,gd,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine CC2CCLapVariCoeff(df,f,k,gd,dir) ! Finished, needs to be checked
         ! myCCVaryDel computes
         ! 
         !  df = ∇(k∇f)
         ! 
         ! Where f and k live at the cell center.
         ! Note that k may vary in space.
         ! 
         ! del should be capable of computing conservative derivatives.
         ! This routine needs adjusting to use this capability since
         ! it uses multiple cores more efficiently.
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: df
         real(cp),dimension(:,:,:),intent(in) :: f,k
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         integer :: x,y,z
         type(del) :: d
         s = shape(f)
         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myCCVaryDel.'; stop
         end select

         allocate(temp1(s(1)+x,s(2)+y,s(3)+z))
         allocate(temp2(s(1)+x,s(2)+y,s(3)+z))
         call d%assign(temp1,f,gd,1,dir,0)
         call myCellCenter2Face(temp2,k,gd,dir)
         temp1 = temp1*temp2
         call d%assign(temp2,temp1,gd,1,dir,0)
         df(1+x:s(1)-x,1+y:s(2)-y,1+z:s(3)-z) = temp2(1:s(1)-2*x,1:s(2)-2*y,1:s(3)-2*z)
         deallocate(temp1,temp2)
       end subroutine

       subroutine myCC2FaceGrad(gradx,grady,gradz,p,gd) ! Finsihed
         ! This cell center grad operator takes cell centered data,
         ! takes the gradient, and the result lands on the cell faces (approximately).
         ! Ask Eldgredge if using the collocated first derivative formula
         ! is more appropriate here.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(cp),dimension(:,:,:),intent(in) :: p
         type(grid),intent(in) :: gd
         type(del) :: d
         call d%assign(gradx,p,gd,1,1,1) ! Padding avoids calcs on fictive cells
         call d%assign(grady,p,gd,1,2,1) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,p,gd,1,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine myCC2CCDel(gradp,p,gd,dir) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: gradp
         real(cp),dimension(:,:,:),intent(in) :: p
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         type(del) :: d
         call d%assign(gradp,p,gd,1,dir,0)
       end subroutine

       subroutine myCCCurl(curl,u,v,w,gd,dir) ! Finished, needs to be checked
         ! myCCCurl computes the component dir of the cell center-based
         ! curl using a collocated derivative.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curl
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         type(del) :: d
         select case (dir)
         case (1); call d%assign(curl,w,gd,1,2,0)
                 call d%subtract(curl,v,gd,1,3,0)
         case (2); call d%assign(curl,u,gd,1,3,0)
                 call d%subtract(curl,w,gd,1,1,0)
         case (3); call d%assign(curl,v,gd,1,1,0)
                 call d%subtract(curl,u,gd,1,2,0)
         case default
           stop 'Error: dir must = 1,2,3 in myCCCurl.'
         end select
       end subroutine

       subroutine myCC2EdgeCurl(curl,u,v,w,gd,dir) ! Finished, needs to be checked
         ! myCCCurl computes the component dir of the curl of (u,v,w). The
         ! result lives on the edge. 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curl
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: tempfx,tempfy,tempfz
         integer,dimension(3) :: s
         type(del) :: d
         s = shape(u)
         select case (dir)
         case (1)
           allocate(tempfy(s(1),s(2)+1,s(3)))
           allocate(tempfz(s(1),s(2),s(3)+1))
           call myCellCenter2Face(tempfz,w,gd,3)
           call myCellCenter2Face(tempfy,v,gd,2)
           call d%assign(curl,tempfz,gd,1,2,0) ! dw/dy
           call d%subtract(curl,tempfy,gd,1,3,0) ! dv/dz
           deallocate(tempfy,tempfz)
         case (2)
           allocate(tempfx(s(1)+1,s(2),s(3)))
           allocate(tempfz(s(1),s(2),s(3)+1))
           call myCellCenter2Face(tempfz,w,gd,3)
           call myCellCenter2Face(tempfx,u,gd,1)
           call d%assign(curl,tempfx,gd,1,3,0) ! du/dz
           call d%subtract(curl,tempfz,gd,1,1,0) ! dw/dx
           deallocate(tempfx,tempfz)
         case (3)
           allocate(tempfx(s(1)+1,s(2),s(3)))
           allocate(tempfy(s(1),s(2)+1,s(3)))
           call myCellCenter2Face(tempfy,v,gd,2)
           call myCellCenter2Face(tempfx,u,gd,1)
           call d%assign(curl,tempfy,gd,1,1,0) ! dv/dx
           call d%subtract(curl,tempfx,gd,1,2,0) ! du/dy
           deallocate(tempfx,tempfy)
         end select
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
         ! 
         ! del should be capable of computing conservative derivatives.
         ! This routine needs adjusting to use this capability since
         ! it uses multiple cores more efficiently.
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: df
         real(cp),dimension(:,:,:),intent(in) :: f,k
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir1,dir2
         real(cp),dimension(:,:,:),allocatable :: temp1,temp2
         integer,dimension(3) :: s
         integer :: x,y,z
         type(del) :: d
         s = shape(f)
         select case (dir1)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir1 must = 1,2,3 in myCCVaryDel.'; stop
         end select

         if (dir1.eq.dir2) then
           allocate(temp1(s(1)+x,s(2)+y,s(3)+z))
           allocate(temp2(s(1)+x,s(2)+y,s(3)+z))
           call d%assign(temp1,f,gd,1,dir1,0)
           call myCellCenter2Face(temp2,k,gd,dir1)
           temp1 = temp1*temp2
           call d%assign(temp2,temp1,gd,1,dir2,0)
           df(1+x:s(1)-x,1+y:s(2)-y,1+z:s(3)-z) = temp2(1:s(1)-2*x,1:s(2)-2*y,1:s(3)-2*z)
           deallocate(temp1,temp2)
         else
           allocate(temp1(s(1),s(2),s(3)))
           allocate(temp2(s(1),s(2),s(3)))
           call d%assign(temp1,f,gd,1,dir1,0)
           temp1 = temp1*k
           call d%assign(temp2,temp1,gd,1,dir2,0)
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
         real(cp),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(cp),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: gd
         type(del) :: d
         ! call myNode2CellCenter(cellCenter,node,1,gd)
         ! call myCellGrad(gradx,grady,gradz,f,gd)
         call d%assign(gradx,f,gd,1,1,0)
         call d%assign(grady,f,gd,1,2,0)
         call d%assign(gradz,f,gd,1,3,0)
       end subroutine

       subroutine myNode2NodeDel(df,f,gd,dir) ! Finished but maybe not necessary?
         ! This is the correct approach to taking the div(B).
         ! it results in VERY low div(B) for the low Rem
         ! approximation in both the source term AND
         ! in the result.
         ! node->del->node (collocated derivative)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: df
         real(cp),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         type(del) :: d
         ! Padding is necessary for the divergence.
         call d%assign(df,f,gd,1,dir,0)
       end subroutine

       subroutine myNode2NodeDelUpwind(df,f,gd,dir) ! Finished but maybe not necessary?
         ! This is the correct approach to taking the div(B).
         ! it results in VERY low div(B) for the low Rem
         ! approximation in both the source term AND
         ! in the result.
         ! node->del->node (collocated derivative)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: df
         real(cp),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         integer,dimension(3) :: s
         integer :: x,y,z
         real(cp),dimension(:,:,:),allocatable :: tempe
         type(del) :: d
         ! Padding is necessary for the divergence.
         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myNode2NodeDelUpwind.'; stop
         end select
         s = shape(df)
         allocate(tempe(s(1)-x,s(2)-y,s(3)-z))
         call myNode2edge(tempe,f,gd,dir)
         call d%assign(df,tempe,gd,1,dir,0)
         deallocate(tempe)
       end subroutine

       subroutine myNodeDiv(divU,u,v,w,gd) ! Finished, but needs improvement
         ! Consider removing dependence on myNode2NodeDel, 
         ! the derivative type is simple
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         real(cp),dimension(:,:,:),allocatable :: temp
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
         real(cp),dimension(:,:,:),intent(inout) :: curl
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         type(del) :: d

         select case (dir)
         case (1)
           call d%assign(curl,w,gd,1,2,0)
           call d%subtract(curl,v,gd,1,3,0)
         case (2)
           call d%assign(curl,u,gd,1,3,0)
           call d%subtract(curl,w,gd,1,1,0)
         case (3)
           call d%assign(curl,v,gd,1,1,0)
           call d%subtract(curl,u,gd,1,2,0)
         case default
           write(*,*) 'Error: dir must = 1,2,3 in myNodeCurl.'; stop
         end select
       end subroutine

       subroutine myNodeLap(lapU,u,g) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(lapU,u,g,2,1,1)
         call d%add(   lapU,u,g,2,2,1)
         call d%add(   lapU,u,g,2,3,1)
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ******************************* EDGE BASED DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine myEdge2FaceCurl(curl,u,v,w,gd,curlDir) ! Needs checking
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curl
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: curlDir
         type(del) :: d
         select case (curlDir)
         case (1);   call d%assign(curl,w,gd,1,2,0)
                   call d%subtract(curl,v,gd,1,3,0)
         case (2);   call d%assign(curl,u,gd,1,3,0)
                   call d%subtract(curl,w,gd,1,1,0)
         case (3);   call d%assign(curl,v,gd,1,1,0)
                   call d%subtract(curl,u,gd,1,2,0)
         case default
           stop 'Error: curlDir must = 1,2,3 in myEdge2FaceCurl.'
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
         ! All variables are expected to be located at the cell centers.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: div
         real(cp),dimension(:,:,:),intent(in) :: u,v,w,Bx,By,Bz
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp
         integer,dimension(3) :: s
         type(del) :: d

         s = shape(div)
         allocate(temp(s(1),s(2),s(3)))
         div = real(0.0,cp)

         ! * = needs interpolation --         *    *
         ! -------------------------- d/dx (u Bi - ui Bx)
         select case (dir)
         case (2); temp = u*By - v*Bx
         call d%add(div,temp,gd,1,1,0)
         case (3); temp = u*Bz - w*Bx
         call d%add(div,temp,gd,1,1,0)
         end select
         ! * = needs interpolation --         *    *
         ! -------------------------- d/dy (v Bi - ui By)
         select case (dir)
         case (1); temp = v*Bx - u*By
         call d%add(div,temp,gd,1,2,0)
         case (3); temp = v*Bz - w*By
         call d%add(div,temp,gd,1,2,0)
         end select

         ! * = needs interpolation --         *    *
         ! -------------------------- d/dz (w Bi - ui Bz)
         select case (dir)
         case (1); temp = w*Bx - u*Bz
         call d%add(div,temp,gd,1,3,0)
         case (2); temp = w*By - v*Bz
         call d%add(div,temp,gd,1,3,0)
         end select

         deallocate(temp)
       end subroutine

       ! subroutine myCCBfieldDiffuse(div,Bx,By,Bz,sigmaInv,gd,dir) ! Finished, needs testing
       !   ! There are a lot of local allocatables
       !   ! 
       !   ! Returns the ith component of the diffusion term 
       !   ! in the induction equation:
       !   ! 
       !   !   d/dxj [ sigmaInv ( d/dxi (Bj) - d/dxj (Bi) ) ]
       !   ! 
       !   ! Where d/dxi is the derivative wrt the direction dir.
       !   ! B is expected to be located at the cell center.
       !   ! 
       !   ! For variable mu, Bx,By,Bz must be divided by mu
       !   ! before calling this routine.
       !   ! 
       !   implicit none
       !   real(cp),dimension(:,:,:),intent(inout) :: div
       !   real(cp),dimension(:,:,:),intent(in) :: Bx,By,Bz,sigmaInv
       !   type(grid),intent(in) :: gd
       !   integer,intent(in) :: dir
       !   div = real(0.0,cp)
       !   ! -------------------------- d/dx [ sigmaInv ( d/dxi (Bx/mu) - d/dx (Bi/mu) ) ]
       !   select case (dir)
       !   case (2)
       !   call d%add(div,Bx,sigmaInv,gd,dir,1)
       !   call d%subtract(div,By,sigmaInv,gd,1,1)
       !   case (3)
       !   call d%add(div,Bx,sigmaInv,gd,dir,1)
       !   call d%subtract(div,Bz,sigmaInv,gd,1,1)
       !   end select
       !   ! -------------------------- d/dy [ sigmaInv ( d/dxi (By/mu) - d/dy (Bi/mu) ) ]
       !   select case (dir)
       !   case (1)
       !   call d%add(div,By,sigmaInv,gd,dir,2)
       !   call d%subtract(div,Bx,sigmaInv,gd,2,2)
       !   case (3)
       !   call d%add(div,By,sigmaInv,gd,dir,2)
       !   call d%subtract(div,Bz,sigmaInv,gd,2,2)
       !   end select
       !   ! -------------------------- d/dz [ sigmaInv ( d/dxi (Bz/mu) - d/dz (Bi/mu) ) ]
       !   select case (dir)
       !   case (1)
       !   call d%add(div,Bz,sigmaInv,gd,dir,3)
       !   call d%subtract(div,Bx,sigmaInv,gd,3,3)
       !   case (2)
       !   call d%add(div,Bz,sigmaInv,gd,dir,3)
       !   call d%subtract(div,By,sigmaInv,gd,3,3)
       !   end select
       ! end subroutine

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
         real(cp),dimension(:,:,:),intent(inout) :: div
         real(cp),dimension(:,:,:),intent(in) :: Bx,By,Bz,sigmaInv
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp1
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

       end module