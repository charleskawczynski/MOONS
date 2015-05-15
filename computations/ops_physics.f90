       module ops_physics_mod
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
       use delVC_mod
       use grid_mod
       use vectorField_mod
       use interpOps_mod
       use ops_discrete_mod

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

       public :: faceAdvect
       interface faceAdvect;    module procedure faceAdvectSF;   end interface
       interface faceAdvect;    module procedure faceAdvectVF;   end interface

       public :: faceAdvectDonor
       interface faceAdvectDonor;    module procedure faceAdvectDonorSF;   end interface
       interface faceAdvectDonor;    module procedure faceAdvectDonorVF;   end interface

       public :: faceAdvectHybrid
       interface faceAdvectHybrid;    module procedure faceAdvectHybridSF;   end interface
       interface faceAdvectHybrid;    module procedure faceAdvectHybridVF;   end interface

       public :: CCBfieldAdvect
       interface CCBfieldAdvect;    module procedure CCBfieldAdvectSF;   end interface
       interface CCBfieldAdvect;    module procedure CCBfieldAdvectVF;   end interface

       public :: CCBfieldDiffuse
       interface CCBfieldDiffuse;    module procedure CCBfieldDiffuseSF;   end interface
       interface CCBfieldDiffuse;    module procedure CCBfieldDiffuseVF;   end interface

       contains

       ! ******************************* AUXILIARY ROUTINES *********************************

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

       subroutine square(A,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: A
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           A(i,j,k) = A(i,j,k)*A(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine mult(A,B,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: A
         real(cp),dimension(:,:,:),intent(inout) :: B
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           A(i,j,k) = A(i,j,k)*B(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine addMult(C,A,B,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: C
         real(cp),dimension(:,:,:),intent(in) :: A,B
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           C(i,j,k) = C(i,j,k) + A(i,j,k)*B(i,j,k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine
       
       ! ******************************* FACE BASED DERIVATIVES *********************************

       subroutine faceAdvectSF(psi,u,v,w,phi,g,dir) ! Finished
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
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp,tempAve
         integer,dimension(3) :: s
         type(del) :: d

         s = shape(phi)

         allocate(temp(s(1),s(2),s(3)))
         allocate(tempAve(s(1),s(2),s(3)))

         psi = real(0.0,cp)
         ! -------------------------- u d/dx --------------------------
         call myFaceAverage(tempAve,u,g,1,dir)
         if (dir.eq.1) then
               call d%assign(temp,phi,g,1,1,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,g,1,1,1)
         endif
         psi = tempAve*temp

         ! -------------------------- v d/dy --------------------------
         call myFaceAverage(tempAve,v,g,2,dir)

         if (dir.eq.2) then
               call d%assign(temp,phi,g,1,2,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,g,1,2,1)
         endif
         psi = psi + tempAve*temp

         ! -------------------------- w d/dz --------------------------
         call myFaceAverage(tempAve,w,g,3,dir)
         if (dir.eq.3) then
               call d%assign(temp,phi,g,1,3,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,g,1,3,1)
         endif
         psi = psi + tempAve*temp

         deallocate(tempAve)
         deallocate(temp)
       end subroutine

       subroutine faceAdvectDonorSF(psi,u,v,w,ui,g,faceDir) ! Finished, maybe improvements
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
         type(grid),intent(in) :: g
         integer,intent(in) :: faceDir
         real(cp),dimension(:,:,:),allocatable :: tempAveCC
         real(cp),dimension(:,:,:),allocatable :: tempAveE1,tempAveE2
         integer,dimension(3) :: s,sc,se
         integer :: x,y,z,orthDir
         type(del) ::d

         s = shape(ui)
         select case (faceDir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           write(*,*) 'Error: faceDir must =1,2,3 in FaceAdvectDonor.'; stop
         end select

         sc = (/g%c(1)%sc,g%c(2)%sc,g%c(3)%sc/)

         allocate(tempAveCC(sc(1),sc(2),sc(3)))

         psi = real(0.0,cp)
         ! -------------------------- d/dx (u u_i) --------------------------
         if (faceDir.eq.1) then
           call myFace2CellCenter(tempAveCC,u,g,1)
           call square(tempAveCC,sc)
           call d%add(psi,tempAveCC,g,1,1,1)
         else
           se = (/s(1)+1,s(2),s(3)/)
           allocate(tempAveE1(se(1),se(2),se(3)))
           allocate(tempAveE2(se(1),se(2),se(3)))

           orthDir = orthogonalDirection(faceDir,1)
           call myFace2Edge(tempAveE1,u,g,1,orthDir)
           call myFace2Edge(tempAveE2,ui,g,faceDir,orthDir)

           call mult(tempAveE1,tempAveE2,se)
           call d%add(psi,tempAveE1,g,1,1,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dy (v u_i) --------------------------
         if (faceDir.eq.2) then
           call myFace2CellCenter(tempAveCC,v,g,2)
           call square(tempAveCC,sc)
           call d%add(psi,tempAveCC,g,1,2,1)
         else
           se = (/s(1),s(2)+1,s(3)/)
           allocate(tempAveE1(se(1),se(2),se(3)))
           allocate(tempAveE2(se(1),se(2),se(3)))
           orthDir = orthogonalDirection(faceDir,2)
           call myFace2Edge(tempAveE1,v,g,2,orthDir)
           call myFace2Edge(tempAveE2,ui,g,faceDir,orthDir)

           call mult(tempAveE1,tempAveE2,se)
           call d%add(psi,tempAveE1,g,1,2,1) ! upwind from edge to cc
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dz (w u_i) --------------------------
         if (faceDir.eq.3) then
           call myFace2CellCenter(tempAveCC,w,g,3)
           call square(tempAveCC,sc)
           call d%add(psi,tempAveCC,g,1,3,1)
         else
           se = (/s(1),s(2),s(3)+1/)
           allocate(tempAveE1(se(1),se(2),se(3)))
           allocate(tempAveE2(se(1),se(2),se(3)))

           orthDir = orthogonalDirection(faceDir,3)
           call myFace2Edge(tempAveE1,w,g,3,orthDir)
           call myFace2Edge(tempAveE2,ui,g,faceDir,orthDir)

           call mult(tempAveE1,tempAveE2,se)
           call d%add(psi,tempAveE1,g,1,3,1)
           deallocate(tempAveE1,tempAveE2)
         endif
         deallocate(tempAveCC)
       end subroutine

       subroutine faceAdvectHybridSF(psi,u,v,w,ui,g,faceDir) ! Finished, maybe improvements
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
         type(grid),intent(in) :: g
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
           write(*,*) 'Error: faceDir must = 1,2,3 in FaceAdvectHybrid.'; stop
         end select

         allocate(tempAveCC(s(1)-x,s(2)-y,s(3)-z))

         psi = real(0.0,cp)
         ! -------------------------- d/dx (u u_i) --------------------------
         if (faceDir.eq.1) then
           call myFace2CellCenter(tempAveCC,u,g,1)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,g,1,1,1)
         else
           allocate(tempAveE1(s(1)+1,s(2),s(3)))
           allocate(tempAveE2(s(1)+1,s(2),s(3)))

           orthDir = orthogonalDirection(faceDir,1)
           call myFace2Edge(tempAveE1,u,g,1,orthDir)
           call myFace2Edge(tempAveE2,ui,g,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,g,1,1,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dy (v u_i) --------------------------
         if (faceDir.eq.2) then
           call myFace2CellCenter(tempAveCC,v,g,2)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,g,1,2,1)
         else
           allocate(tempAveE1(s(1),s(2)+1,s(3)))
           allocate(tempAveE2(s(1),s(2)+1,s(3)))
           orthDir = orthogonalDirection(faceDir,2)
           call myFace2Edge(tempAveE1,v,g,2,orthDir)
           call myFace2Edge(tempAveE2,ui,g,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,g,1,2,1) ! upwind from edge to cc
           deallocate(tempAveE1,tempAveE2)
         endif

         ! -------------------------- d/dz (w u_i) --------------------------
         if (faceDir.eq.3) then
           call myFace2CellCenter(tempAveCC,w,g,3)
           tempAveCC = tempAveCC*tempAveCC
           call d%add(psi,tempAveCC,g,1,3,1)
         else
           allocate(tempAveE1(s(1),s(2),s(3)+1))
           allocate(tempAveE2(s(1),s(2),s(3)+1))

           orthDir = orthogonalDirection(faceDir,3)
           call myFace2Edge(tempAveE1,w,g,3,orthDir)
           call myFace2Edge(tempAveE2,ui,g,faceDir,orthDir)

           tempAveE1 = tempAveE1*tempAveE2
           call d%add(psi,tempAveE1,g,1,3,1)
           deallocate(tempAveE1,tempAveE2)
         endif

         deallocate(tempAveCC)
       end subroutine


       ! ************************************ CELL CENTER *************************************

       subroutine CCBfieldAdvectSF(div,u,v,w,Bx,By,Bz,g,dir) ! Finished
         ! Returns the ith component of the advective term
         ! in the induction equation:
         !   d/dxj (uj Bi - ui Bj)
         ! All variables are expected to be located at the cell centers.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: div
         real(cp),dimension(:,:,:),intent(in) :: u,v,w,Bx,By,Bz
         type(grid),intent(in) :: g
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
         call d%add(div,temp,g,1,1,0)
         case (3); temp = u*Bz - w*Bx
         call d%add(div,temp,g,1,1,0)
         end select
         ! * = needs interpolation --         *    *
         ! -------------------------- d/dy (v Bi - ui By)
         select case (dir)
         case (1); temp = v*Bx - u*By
         call d%add(div,temp,g,1,2,0)
         case (3); temp = v*Bz - w*By
         call d%add(div,temp,g,1,2,0)
         end select

         ! * = needs interpolation --         *    *
         ! -------------------------- d/dz (w Bi - ui Bz)
         select case (dir)
         case (1); temp = w*Bx - u*Bz
         call d%add(div,temp,g,1,3,0)
         case (2); temp = w*By - v*Bz
         call d%add(div,temp,g,1,3,0)
         end select

         deallocate(temp)
       end subroutine

       subroutine CCBfieldDiffuseSF(div,B,sigmaInv,g,dir) ! Finished
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
         ! NOTE: sigmaInv lives on the cell face
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: div
         type(vectorField),intent(in) :: B,sigmaInv
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp
         integer,dimension(3) :: s
         type(delVC) :: d
         s = shape(div)
         div = real(0.0,cp)
         allocate(temp(s(1),s(2),s(3)))
         ! -------------------------- d/dx [ sigmaInv ( d/dxi (Bx/mu) - d/dx (Bi/mu) ) ]
         select case (dir)
         case (2)
         call mixed(temp,B%x,sigmaInv%y,g,dir,1); div=div+temp
         call d%subtract(div,B%y,sigmaInv%x,g,1,1)
         case (3)
         call mixed(temp,B%x,sigmaInv%z,g,dir,1); div=div+temp
         call d%subtract(div,B%z,sigmaInv%x,g,1,1)
         end select
         ! -------------------------- d/dy [ sigmaInv ( d/dxi (By/mu) - d/dy (Bi/mu) ) ]
         select case (dir)
         case (1)
         call mixed(temp,B%y,sigmaInv%x,g,dir,2); div=div+temp
         call d%subtract(div,B%x,sigmaInv%y,g,2,1)
         case (3)
         call mixed(temp,B%y,sigmaInv%z,g,dir,2); div=div+temp
         call d%subtract(div,B%z,sigmaInv%y,g,2,1)
         end select
         ! -------------------------- d/dz [ sigmaInv ( d/dxi (Bz/mu) - d/dz (Bi/mu) ) ]
         select case (dir)
         case (1)
         call mixed(temp,B%z,sigmaInv%x,g,dir,3); div=div+temp
         call d%subtract(div,B%x,sigmaInv%z,g,3,1)
         case (2)
         call mixed(temp,B%z,sigmaInv%y,g,dir,3); div=div+temp
         call d%subtract(div,B%y,sigmaInv%z,g,3,1)
         end select

         deallocate(temp)
       end subroutine

       ! ******************************* VECTOR-FIELD INTERFACE *******************************

       subroutine faceAdvectVF(psi,U,Ui,g)
         implicit none
         type(vectorField),intent(inout) :: psi
         type(vectorField),intent(in) :: U,Ui
         type(grid),intent(in) :: g
         call faceAdvect(psi%x,U%x,U%y,U%z,Ui%x,g,1)
         call faceAdvect(psi%y,U%x,U%y,U%z,Ui%y,g,2)
         call faceAdvect(psi%z,U%x,U%y,U%z,Ui%z,g,3)
       end subroutine

       subroutine faceAdvectDonorVF(psi,U,Ui,g)
         implicit none
         type(vectorField),intent(inout) :: psi
         type(vectorField),intent(in) :: U,Ui
         type(grid),intent(in) :: g
         call faceAdvectDonor(psi%x,U%x,U%y,U%z,Ui%x,g,1)
         call faceAdvectDonor(psi%y,U%x,U%y,U%z,Ui%y,g,2)
         call faceAdvectDonor(psi%z,U%x,U%y,U%z,Ui%z,g,3)
       end subroutine

       subroutine faceAdvectHybridVF(psi,U,Ui,g)
         implicit none
         type(vectorField),intent(inout) :: psi
         type(vectorField),intent(in) :: U,Ui
         type(grid),intent(in) :: g
         call faceAdvectHybrid(psi%x,U%x,U%y,U%z,Ui%x,g,1)
         call faceAdvectHybrid(psi%y,U%x,U%y,U%z,Ui%y,g,2)
         call faceAdvectHybrid(psi%z,U%x,U%y,U%z,Ui%z,g,3)
       end subroutine

       subroutine CCBfieldAdvectVF(div,U,B,g)
         implicit none
         type(vectorField),intent(inout) :: div
         type(vectorField),intent(in) :: U,B
         type(grid),intent(in) :: g
         call CCBfieldAdvect(div%x,U%x,U%y,U%z,B%x,B%y,B%z,g,1)
         call CCBfieldAdvect(div%y,U%x,U%y,U%z,B%x,B%y,B%z,g,2)
         call CCBfieldAdvect(div%z,U%x,U%y,U%z,B%x,B%y,B%z,g,3)
       end subroutine

       subroutine CCBfieldDiffuseVF(div,B,sigmaInv,g)
         implicit none
         type(vectorField),intent(inout) :: div
         type(vectorField),intent(in) :: B,sigmaInv
         type(grid),intent(in) :: g
         call CCBfieldDiffuse(div%x,B,sigmaInv,g,1)
         call CCBfieldDiffuse(div%y,B,sigmaInv,g,2)
         call CCBfieldDiffuse(div%z,B,sigmaInv,g,3)
       end subroutine


       ! ******************************* OLD... *******************************

       ! subroutine CCBfieldDiffuseSF(div,Bx,By,Bz,sigmaInv,g,dir) ! Finished
       !   ! There are a lot of local allocatables
       !   ! 
       !   ! Returns the ith component of the diffusion term 
       !   ! in the induction equation:
       !   ! 
       !   !   d/dxj [ sigmaInv ( d/dxi (Bj) - d/dxj (Bi) ) ]
       !   ! 
       !   ! or
       !   ! 
       !   !   d/dx [ sigmaInv ( d/dxi (Bx) - d/dx (Bi) ) ] + 
       !   !   d/dy [ sigmaInv ( d/dxi (By) - d/dy (Bi) ) ] + 
       !   !   d/dz [ sigmaInv ( d/dxi (Bz) - d/dz (Bi) ) ]
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
       !   type(grid),intent(in) :: g
       !   integer,intent(in) :: dir
       !   real(cp),dimension(:,:,:),allocatable :: temp
       !   integer,dimension(3) :: s
       !   s = shape(div)
       !   div = real(0.0,cp)
       !   allocate(temp(s(1),s(2),s(3)))
       !   ! -------------------------- d/dx [ sigmaInv ( d/dxi (Bx/mu) - d/dx (Bi/mu) ) ]
       !   select case (dir)
       !   case (2)
       !   call CCVaryDel(temp,Bx,sigmaInv,g,dir,1); div=div+temp
       !   call CCVaryDel(temp,By,sigmaInv,g,1,1); div=div-temp
       !   case (3)
       !   call CCVaryDel(temp,Bx,sigmaInv,g,dir,1); div=div+temp
       !   call CCVaryDel(temp,Bz,sigmaInv,g,1,1); div=div-temp
       !   end select
       !   ! -------------------------- d/dy [ sigmaInv ( d/dxi (By/mu) - d/dy (Bi/mu) ) ]
       !   select case (dir)
       !   case (1)
       !   call CCVaryDel(temp,By,sigmaInv,g,dir,2); div=div+temp
       !   call CCVaryDel(temp,Bx,sigmaInv,g,2,2); div=div-temp
       !   case (3)
       !   call CCVaryDel(temp,By,sigmaInv,g,dir,2); div=div+temp
       !   call CCVaryDel(temp,Bz,sigmaInv,g,2,2); div=div-temp
       !   end select
       !   ! -------------------------- d/dz [ sigmaInv ( d/dxi (Bz/mu) - d/dz (Bi/mu) ) ]
       !   select case (dir)
       !   case (1)
       !   call CCVaryDel(temp,Bz,sigmaInv,g,dir,3); div=div+temp
       !   call CCVaryDel(temp,Bx,sigmaInv,g,3,3); div=div-temp
       !   case (2)
       !   call CCVaryDel(temp,Bz,sigmaInv,g,dir,3); div=div+temp
       !   call CCVaryDel(temp,By,sigmaInv,g,3,3); div=div-temp
       !   end select
       !   deallocate(temp)
       ! end subroutine


       end module