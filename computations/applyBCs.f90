       module applyBCs_mod
       ! This is the applyBCs module. Here is an example of implementation
       ! of setting and applying BCs for the magnetic field using Psuedo-vacuum BCs.
       ! 
       !           type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
       !           integer :: Nx,Ny,Nz,neumann,dirichlet
       !  
       !           ! B-field boundary conditions
       !           call myAllocate(Nx,Ny,Nz,gd,BLoc)
       !           select case (BLoc)
       !           case (dom_cc_tot); dirichlet = 2; neumann = 5
       !           case (dom_n_tot);  dirichlet = 1; neumann = 4
       !           end select
       !           
       !           call setAllZero(Bx_bcs,Nx,Ny,Nz,dirichlet)
       !           call setXminType(Bx_bcs,neumann)
       !           call setXmaxType(Bx_bcs,neumann)
       !           call checkBCs(Bx_bcs)
       !  
       !           call setAllZero(By_bcs,Nx,Ny,Nz,dirichlet)
       !           call setYminType(By_bcs,neumann)
       !           call setYmaxType(By_bcs,neumann)
       !           call checkBCs(By_bcs)
       !  
       !           call setAllZero(Bz_bcs,Nx,Ny,Nz,dirichlet)
       !           call setZminType(Bz_bcs,neumann)
       !           call setZmaxType(Bz_bcs,neumann)
       !           call checkBCs(Bz_bcs)
       !           .
       !           .
       !           .
       !           call myAdvect(tempx,Bx0,By0,Bz0,u,gd)
       !           call myPoisson(Bx,-Rem*tempx,B_bcs,gd)
       !  
       !           call myAdvect(tempy,Bx0,By0,Bz0,v,gd)
       !           call myPoisson(By,-Rem*tempy,B_bcs,gd)
       !  
       !           call myAdvect(tempz,Bx0,By0,Bz0,w,gd)
       !           call myPoisson(Bz,-Rem*tempz,B_bcs,gd)
       !           .
       !           .
       !           .

       use constants_mod
       use myAllocate_mod
       use BCs_mod
       use griddata_mod
       use myExceptions_mod
       implicit none

       private

       public :: applyAllBCs,applyBCFace

       interface applyAllBCs
         module procedure applyAllBCs3D
       end interface

       interface applyBCs
         module procedure applyBCs3D
       end interface

       contains

       subroutine applyAllBCs3D(b,u,gd)
        ! Note that these boundary conditions are applied in a NON- arbitrary
        ! order and changing them WILL change the outcome of the results.
        ! Consider changing BCs to only affect interior data.
         implicit none
         type(BCs),intent(in) :: b
         real(dpn),dimension(:,:,:),intent(inout) :: u
         type(griddata),intent(in) :: gd
         call applyBCs(u,b%xMinType,1,b%xMinVals,b%xn,b%xc,b%s(1))
         call applyBCs(u,b%yMinType,2,b%yMinVals,b%yn,b%yc,b%s(2))
         call applyBCs(u,b%zMinType,3,b%zMinVals,b%zn,b%zc,b%s(3))
         call applyBCs(u,b%xMaxType,4,b%xMaxVals,b%xn,b%xc,b%s(1))
         call applyBCs(u,b%zMaxType,6,b%zMaxVals,b%zn,b%zc,b%s(3))
         call applyBCs(u,b%yMaxType,5,b%yMaxVals,b%yn,b%yc,b%s(2))
       end subroutine

       subroutine applyBCFace(b,u,gd,face)
         implicit none
         type(BCs),intent(in) :: b
         real(dpn),dimension(:,:,:),intent(inout) :: u
         type(griddata),intent(in) :: gd
         integer,intent(in) :: face
         select case (face)
         case (1); call applyBCs(u,b%xMinType,1,b%xMinVals,b%xn,b%xc,b%s(1))
         case (2); call applyBCs(u,b%yMinType,2,b%yMinVals,b%yn,b%yc,b%s(2))
         case (3); call applyBCs(u,b%zMinType,3,b%zMinVals,b%zn,b%zc,b%s(3))
         case (4); call applyBCs(u,b%xMaxType,4,b%xMaxVals,b%xn,b%xc,b%s(1))
         case (6); call applyBCs(u,b%zMaxType,6,b%zMaxVals,b%zn,b%zc,b%s(3))
         case (5); call applyBCs(u,b%yMaxType,5,b%yMaxVals,b%yn,b%yc,b%s(2))
         case default
         write(*,*) 'Error: face must = 1,2,3,4,5,6 in applyBCs.';stop
         end select
       end subroutine

       subroutine applyBCs3D(u,bctype,face,bvals,hn,hc,s)
         implicit none
         real(dpn),intent(inout),dimension(:,:,:) :: u
         real(dpn),intent(in),dimension(:) :: hn,hc
         real(dpn),dimension(:,:),intent(in) :: bvals
         integer,intent(in) :: bctype,face
         integer,intent(in) :: s
         integer :: i,j,k
         real(dpn) :: a,b ! (alpha,beta)
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1) ! Dirichlet - direct - wall coincident
           select case (face)
           case (1); u(1,:,:) = bvals
           case (2); u(:,1,:) = bvals
           case (3); u(:,:,1) = bvals
           case (4); u(s,:,:) = bvals
           case (5); u(:,s,:) = bvals
           case (6); u(:,:,s) = bvals
           end select
         case (2) ! Dirichlet - interpolated - wall incoincident
           select case (face)
           case (1);u(1,:,:)=u(2,:,:)+(hc(1)-hc(2))/(hn(1)-hc(2))*(bvals-u(2,:,:))
           case (2);u(:,1,:)=u(:,2,:)+(hc(1)-hc(2))/(hn(1)-hc(2))*(bvals-u(:,2,:))
           case (3);u(:,:,1)=u(:,:,2)+(hc(1)-hc(2))/(hn(1)-hc(2))*(bvals-u(:,:,2))
           case (4);u(s,:,:)=u(s-1,:,:)+(hc(s)-hc(s-1))/(hn(s-1)-hc(s-1))*(bvals-u(s-1,:,:))
           case (5);u(:,s,:)=u(:,s-1,:)+(hc(s)-hc(s-1))/(hn(s-1)-hc(s-1))*(bvals-u(:,s-1,:))
           case (6);u(:,:,s)=u(:,:,s-1)+(hc(s)-hc(s-1))/(hn(s-1)-hc(s-1))*(bvals-u(:,:,s-1))
           end select
         ! *************************** NEUMANN *****************************
         case (3) ! Neumann - direct - wall coincident ~O(dh^2)
           select case (face)
           case (1); a = hn(2)-hn(1); b = hn(3) - hn(1); i = 1; k = 1; j = 2
           u(i,:,:)=(bvals*(b-a)-b/a*u(i+k,:,:)+a/b*u(i+j,:,:))/(a/b-b/a)
           case (2); a = hn(2)-hn(1); b = hn(3) - hn(1); i = 1; k = 1; j = 2
           u(:,i,:)=(bvals*(b-a)-b/a*u(:,i+k,:)+a/b*u(:,i+j,:))/(a/b-b/a)
           case (3); a = hn(2)-hn(1); b = hn(3) - hn(1); i = 1; k = 1; j = 2
           u(:,:,i)=(bvals*(b-a)-b/a*u(:,:,i+k)+a/b*u(:,:,i+j))/(a/b-b/a)
           case (4); a = -(hn(s)-hn(s-1)); b = -(hn(s) - hn(s-2)); i = s; k = -1; j = -2
           u(i,:,:)=(u(i,:,:)*(b-a)-b/a*u(i+k,:,:)+a/b*u(i+j,:,:))/(a/b-b/a)
           case (5); a = -(hn(s)-hn(s-1)); b = -(hn(s) - hn(s-2)); i = s; k = -1; j = -2
           u(:,i,:)=(bvals*(b-a)-b/a*u(:,i+k,:)+a/b*u(:,i+j,:))/(a/b-b/a)
           case (6); a = -(hn(s)-hn(s-1)); b = -(hn(s) - hn(s-2)); i = s; k = -1; j = -2
           u(:,:,i)=(bvals*(b-a)-b/a*u(:,:,i+k)+a/b*u(:,:,i+j))/(a/b-b/a)
           end select
         case (4) ! Neumann - direct - wall coincident ~O(dh)
           select case (face)
           case (1); u(1,:,:) = u(2,:,:) - (hn(2)-hn(1))*bvals
           case (2); u(:,1,:) = u(:,2,:) - (hn(2)-hn(1))*bvals
           case (3); u(:,:,1) = u(:,:,2) - (hn(2)-hn(1))*bvals
           case (4); u(s,:,:) = u(s-1,:,:) + (hn(s-1)-hn(s-2))*bvals
           case (5); u(:,s,:) = u(:,s-1,:) + (hn(s-1)-hn(s-2))*bvals
           case (6); u(:,:,s) = u(:,:,s-1) + (hn(s-1)-hn(s-2))*bvals
           end select
         case (5) ! Neumann - interpolated - wall incoincident ~O(dh)
           select case (face)
           case (1); u(1,:,:) = u(2,:,:) + (hc(1)-hc(2))*bvals
           case (2); u(:,1,:) = u(:,2,:) + (hc(1)-hc(2))*bvals
           case (3); u(:,:,1) = u(:,:,2) + (hc(1)-hc(2))*bvals
           case (4); u(s,:,:) = u(s-1,:,:) + (hc(s)-hc(s-1))*bvals
           case (5); u(:,s,:) = u(:,s-1,:) + (hc(s)-hc(s-1))*bvals
           case (6); u(:,:,s) = u(:,:,s-1) + (hc(s)-hc(s-1))*bvals
           end select
         ! *************************** PERIODIC *****************************
         ! These have not yet been prepared for non-uniform grids:
         case (6) ! Periodic - direct - wall coincident ~O(dh)
           select case (face)
           case (1); u(1,:,:) = 0.5*(u(2,:,:) + u(s-1,:,:))
           case (2); u(:,1,:) = 0.5*(u(:,2,:) + u(:,s-1,:))
           case (3); u(:,:,1) = 0.5*(u(:,:,2) + u(:,:,s-1))
           case (4); u(s,:,:) = 0.5*(u(s-1,:,:) + u(2,:,:))
           case (5); u(:,s,:) = 0.5*(u(:,s-1,:) + u(:,2,:))
           case (6); u(:,:,s) = 0.5*(u(:,:,s-1) + u(:,:,2))
           end select
         case (7) ! Periodic - interpolated - wall incoincident ~O(dh)
           select case (face)
           case (1); u(1,:,:) = 0.5*(u(2,:,:) + u(s-1,:,:))
           case (2); u(:,1,:) = 0.5*(u(:,2,:) + u(:,s-1,:))
           case (3); u(:,:,1) = 0.5*(u(:,:,2) + u(:,:,s-1))
           case (4); u(s,:,:) = 0.5*(u(s-1,:,:) + u(2,:,:))
           case (5); u(:,s,:) = 0.5*(u(:,s-1,:) + u(:,2,:))
           case (6); u(:,:,s) = 0.5*(u(:,:,s-1) + u(:,:,2))
           end select
         case (8) ! Periodic - interpolated - wall incoincident ~O(dh^2)
           select case (face)
           case (1); u(1,:,:) = 1.0/3.0*(3.0*u(2,:,:) + u(s-1,:,:) - u(3,:,:))
           case (2); u(:,1,:) = 1.0/3.0*(3.0*u(:,2,:) + u(:,s-1,:) - u(:,3,:))
           case (3); u(:,:,1) = 1.0/3.0*(3.0*u(:,:,2) + u(:,:,s-1) - u(:,:,3))
           case (4); u(s,:,:) = -1.0/3.0*(u(s-2,:,:) - 3.0*u(s-1,:,:) - u(2,:,:))
           case (5); u(:,s,:) = -1.0/3.0*(u(:,s-2,:) - 3.0*u(:,s-1,:) - u(:,2,:))
           case (6); u(:,:,s) = -1.0/3.0*(u(:,:,s-2) - 3.0*u(:,:,s-1) - u(:,:,2))
           end select
         end select
       end subroutine

       end module