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
       use scalarField_mod
       use ops_interp_mod
       use ops_discrete_mod
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

       public :: orthogonalDirection

       public :: faceAdvect
       interface faceAdvect;    module procedure faceAdvectSF;   end interface
       interface faceAdvect;    module procedure faceAdvectVF;   end interface

       public :: faceAdvectDonor,faceAdvectHybrid

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

       subroutine squareReal(A,s)
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
         call face2Face(tempAve,u,g,1,dir)
         if (dir.eq.1) then
               call d%assign(temp,phi,g,1,1,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,g,1,1,1)
         endif
         psi = tempAve*temp

         ! -------------------------- v d/dy --------------------------
         call face2Face(tempAve,v,g,2,dir)

         if (dir.eq.2) then
               call d%assign(temp,phi,g,1,2,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,g,1,2,1)
         endif
         psi = psi + tempAve*temp

         ! -------------------------- w d/dz --------------------------
         call face2Face(tempAve,w,g,3,dir)
         if (dir.eq.3) then
               call d%assign(temp,phi,g,1,3,1) ! Padding avoids calcs on fictive cells
         else; call d%assign(temp,phi,g,1,3,1)
         endif
         psi = psi + tempAve*temp

         deallocate(tempAve)
         deallocate(temp)
       end subroutine

       subroutine faceAdvectDonor(div,U,Ui,temp_E1,temp_E2,temp_CC,g)
         ! Computes
         ! 
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         !           temp_CC               --> cell center.
         ! 
         implicit none
         type(vectorField),intent(inout) :: div
         type(vectorField),intent(in) :: U,ui
         type(vectorField),intent(inout) :: temp_E1,temp_E2
         type(vectorField),intent(inout) :: temp_CC
         type(grid),intent(in) :: g
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next

         call zeroGhostPoints(div)
         
         ! d/dxj (uj ui) for i=j
         call face2CellCenter(temp_CC,U,g)

         call squareReal(temp_CC%x,temp_CC%sx)
         call d%assign(div%x,temp_CC%x,g,1,1,pad)

         call squareReal(temp_CC%y,temp_CC%sy)
         call d%assign(div%y,temp_CC%y,g,1,2,pad)

         call squareReal(temp_CC%z,temp_CC%sz)
         call d%assign(div%z,temp_CC%z,g,1,3,pad)

         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         ! x (y,z edges)
         call face2Edge(temp_E1%y,Ui%x,g,1,2)
         call face2Edge(temp_E2%y, U%z,g,3,2)
         call face2Edge(temp_E1%z,Ui%x,g,1,3)
         call face2Edge(temp_E2%z, U%y,g,2,3)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%add(div%x,temp_E1%y,g,1,3,pad)
         call d%add(div%x,temp_E1%z,g,1,2,pad)

         ! y (x,z edges)
         call face2Edge(temp_E1%z,Ui%y,g,2,3)
         call face2Edge(temp_E2%z, U%x,g,1,3)
         call face2Edge(temp_E1%x,Ui%y,g,2,1)
         call face2Edge(temp_E2%x, U%z,g,3,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%add(div%y,temp_E1%x,g,1,3,pad)
         call d%add(div%y,temp_E1%z,g,1,1,pad)

         ! z (x,y edges)
         call face2Edge(temp_E1%y,Ui%z,g,3,2)
         call face2Edge(temp_E2%y, U%x,g,1,2)
         call face2Edge(temp_E1%x,Ui%z,g,3,1)
         call face2Edge(temp_E2%x, U%y,g,2,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call d%add(div%z,temp_E1%x,g,1,2,pad)
         call d%add(div%z,temp_E1%y,g,1,1,pad)
       end subroutine

       subroutine faceAdvectHybrid(div,U,Ui,temp_E1,temp_E2,temp_CC,g)
         ! Computes
         ! 
         !           d
         !  div_i = --- (u_j u_i)
         !          dx_j
         ! 
         ! While minimizing interpolations.
         !           div_i, U, Ui          --> cell face.
         !           tempE1 and temp_E2    --> cell edge.
         !           temp_CC               --> cell center.
         ! 
         implicit none
         type(vectorField),intent(inout) :: div
         type(vectorField),intent(in) :: U,ui
         type(vectorField),intent(inout) :: temp_E1,temp_E2
         type(vectorField),intent(inout) :: temp_CC
         type(grid),intent(in) :: g
         type(del) ::d
         integer :: pad
         pad = 1 ! Currently running for pad = 1, try pad = 0 next

         call zeroGhostPoints(div)
         
         ! d/dxj (uj ui) for i=j
         call face2CellCenter(temp_CC,U,g)

         call squareReal(temp_CC%x,temp_CC%sx)
         call d%assign(div%x,temp_CC%x,g,1,1,pad)

         call squareReal(temp_CC%y,temp_CC%sy)
         call d%assign(div%y,temp_CC%y,g,1,2,pad)

         call squareReal(temp_CC%z,temp_CC%sz)
         call d%assign(div%z,temp_CC%z,g,1,3,pad)

         ! d/dxj (uj ui) for i≠j,  note that Ui must be included
         ! x (y,z edges)
         call face2Edge(temp_E1%y,Ui%x,g,1,2)
         call face2Edge(temp_E2%y, U%z,g,3,2)
         call face2Edge(temp_E1%z,Ui%x,g,1,3)
         call face2Edge(temp_E2%z, U%y,g,2,3)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%add(div%x,temp_E1%y,g,1,3,pad)
         call d%add(div%x,temp_E1%z,g,1,2,pad)

         ! y (x,z edges)
         call face2Edge(temp_E1%z,Ui%y,g,2,3)
         call face2Edge(temp_E2%z, U%x,g,1,3)
         call face2Edge(temp_E1%x,Ui%y,g,2,1)
         call face2Edge(temp_E2%x, U%z,g,3,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%z,temp_E2%z,temp_E1%sz)
         call d%add(div%y,temp_E1%x,g,1,3,pad)
         call d%add(div%y,temp_E1%z,g,1,1,pad)

         ! z (x,y edges)
         call face2Edge(temp_E1%y,Ui%z,g,3,2)
         call face2Edge(temp_E2%y, U%x,g,1,2)
         call face2Edge(temp_E1%x,Ui%z,g,3,1)
         call face2Edge(temp_E2%x, U%y,g,2,1)
         call mult(temp_E1%x,temp_E2%x,temp_E1%sx)
         call mult(temp_E1%y,temp_E2%y,temp_E1%sy)
         call d%add(div%z,temp_E1%x,g,1,2,pad)
         call d%add(div%z,temp_E1%y,g,1,1,pad)
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

       end module