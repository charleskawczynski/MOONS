       module applyBCs_mod
       ! This is the applyBCs module. Here is an example of implementation
       ! of setting and applying BCs for the magnetic field using Psuedo-vacuum BCs.
       ! 
       !           type(BCs),intent(inout) :: Bx_bcs,By_bcs,Bz_bcs
       !           integer :: Nx,Ny,Nz,neumann,dirichlet
       !  
       !           ! B-field boundary conditions
       !           Nx = g%c(1)%sc; Ny = g%c(2)%sc; Nz = g%c(3)%sc
       !           dirichlet = 2; neumann = 5
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

       use BCs_mod
       use grid_mod
       implicit none

       private
       public :: applyAllBCs,applyBCFace
       public :: applyGhostFace,applyAllGhost

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface applyAllBCs;    module procedure applyAllBCs3D;     end interface
       interface applyBCs;       module procedure applyBCs3D;        end interface

       contains

       subroutine applyAllBCs3D(b,u,g)
        ! Note that these boundary conditions are applied in a NON- arbitrary
        ! order and changing them WILL change the outcome of the results.
        ! Consider changing BCs to only affect interior data.
         implicit none
         type(BCs),intent(in) :: b
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(grid),intent(in) :: g
         call applyBCs(u,b%xMinType,1,b%xMinVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         call applyBCs(u,b%yMinType,2,b%yMinVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         call applyBCs(u,b%zMinType,3,b%zMinVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         call applyBCs(u,b%xMaxType,4,b%xMaxVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         call applyBCs(u,b%zMaxType,6,b%zMaxVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         call applyBCs(u,b%yMaxType,5,b%yMaxVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
       end subroutine

       subroutine applyBCFace(b,u,g,face)
         implicit none
         type(BCs),intent(in) :: b
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(grid),intent(in) :: g
         integer,intent(in) :: face
         select case (face)
         case (1); call applyBCs(u,b%xMinType,1,b%xMinVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         case (2); call applyBCs(u,b%yMinType,2,b%yMinVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         case (3); call applyBCs(u,b%zMinType,3,b%zMinVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         case (4); call applyBCs(u,b%xMaxType,4,b%xMaxVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         case (6); call applyBCs(u,b%zMaxType,6,b%zMaxVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         case (5); call applyBCs(u,b%yMaxType,5,b%yMaxVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         case default
         write(*,*) 'Error: face must = 1,2,3,4,5,6 in applyBCs.';stop
         end select
       end subroutine

       subroutine applyGhostFace(b,u,g,face)
         implicit none
         type(BCs),intent(in) :: b
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(grid),intent(in) :: g
         integer,intent(in) :: face
         select case (face)
         case (1); call applyGhostPoints3D(u,b%xMinType,1,b%xMinVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         case (2); call applyGhostPoints3D(u,b%yMinType,2,b%yMinVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         case (3); call applyGhostPoints3D(u,b%zMinType,3,b%zMinVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         case (4); call applyGhostPoints3D(u,b%xMaxType,4,b%xMaxVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         case (6); call applyGhostPoints3D(u,b%zMaxType,6,b%zMaxVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         case (5); call applyGhostPoints3D(u,b%yMaxType,5,b%yMaxVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         case default
         write(*,*) 'Error: face must = 1,2,3,4,5,6 in applyBCs.';stop
         end select
       end subroutine

       subroutine applyAllGhost(b,u,g)
        ! Note that these boundary conditions are applied in a NON- arbitrary
        ! order and changing them WILL change the outcome of the results.
        ! Consider changing BCs to only affect interior data.
         implicit none
         type(BCs),intent(in) :: b
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(grid),intent(in) :: g
         call applyGhostPoints3D(u,b%xMinType,1,b%xMinVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         call applyGhostPoints3D(u,b%yMinType,2,b%yMinVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         call applyGhostPoints3D(u,b%zMinType,3,b%zMinVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         call applyGhostPoints3D(u,b%xMaxType,4,b%xMaxVals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         call applyGhostPoints3D(u,b%zMaxType,6,b%zMaxVals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         call applyGhostPoints3D(u,b%yMaxType,5,b%yMaxVals,g%c(2)%hn,g%c(2)%hc,b%s(2))
       end subroutine


       subroutine applyBCs3D(u,bctype,face,bvals,hn,hc,s)
         implicit none
         real(cp),intent(inout),dimension(:,:,:) :: u
         real(cp),intent(in),dimension(:) :: hn,hc
         real(cp),dimension(:,:),intent(in) :: bvals
         integer,intent(in) :: bctype,face
         integer,intent(in) :: s
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1) ! Dirichlet - direct - wall coincident
           select case (face)
           case (1); u(2,:,:) = bvals
           case (2); u(:,2,:) = bvals
           case (3); u(:,:,2) = bvals
           case (4); u(s-1,:,:) = bvals
           case (5); u(:,s-1,:) = bvals
           case (6); u(:,:,s-1) = bvals
           end select
           select case (face) ! Linearly extrapolate to ghost node
           case (1); u(1,:,:) = real(2.0,cp)*bvals - u(3,:,:)
           case (2); u(:,1,:) = real(2.0,cp)*bvals - u(:,3,:)
           case (3); u(:,:,1) = real(2.0,cp)*bvals - u(:,:,3)
           case (4); u(s,:,:) = real(2.0,cp)*bvals - u(s-2,:,:)
           case (5); u(:,s,:) = real(2.0,cp)*bvals - u(:,s-2,:)
           case (6); u(:,:,s) = real(2.0,cp)*bvals - u(:,:,s-2)
           end select
         case (2) ! Dirichlet - interpolated - wall incoincident
           select case (face)
           case (1); u(1,:,:) = real(2.0,cp)*bvals - u(2,:,:)
           case (2); u(:,1,:) = real(2.0,cp)*bvals - u(:,2,:)
           case (3); u(:,:,1) = real(2.0,cp)*bvals - u(:,:,2)
           case (4); u(s,:,:) = real(2.0,cp)*bvals - u(s-1,:,:)
           case (5); u(:,s,:) = real(2.0,cp)*bvals - u(:,s-1,:)
           case (6); u(:,:,s) = real(2.0,cp)*bvals - u(:,:,s-1)
           end select
         ! *************************** NEUMANN *****************************
         case (3) ! Neumann - direct - wall coincident ~O(dh^2)
           select case (face)
           case (1); u(1,:,:) = u(2,:,:) + (hn(1)-hn(2))*bvals
           case (2); u(:,1,:) = u(:,2,:) + (hn(1)-hn(2))*bvals
           case (3); u(:,:,1) = u(:,:,2) + (hn(1)-hn(2))*bvals
           case (4); u(s,:,:) = u(s-1,:,:) + (hn(s)-hn(s-1))*bvals
           case (5); u(:,s,:) = u(:,s-1,:) + (hn(s)-hn(s-1))*bvals
           case (6); u(:,:,s) = u(:,:,s-1) + (hn(s)-hn(s-1))*bvals
           end select
         case (4) ! Neumann - direct - wall coincident ~O(dh^2)
           select case (face)
           case (1); u(1,:,:) = u(3,:,:) - real(2.0,cp)*bvals*(hn(1)-hn(2))
           case (2); u(:,1,:) = u(:,3,:) - real(2.0,cp)*bvals*(hn(1)-hn(2))
           case (3); u(:,:,1) = u(:,:,3) - real(2.0,cp)*bvals*(hn(1)-hn(2))
           case (4); u(s,:,:) = u(s-2,:,:) - real(2.0,cp)*bvals*(hn(s)-hn(s-1))
           case (5); u(:,s,:) = u(:,s-2,:) - real(2.0,cp)*bvals*(hn(s)-hn(s-1))
           case (6); u(:,:,s) = u(:,:,s-2) - real(2.0,cp)*bvals*(hn(s)-hn(s-1))
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
           case (1); u(1,:,:) = real(0.5,cp)*(u(2,:,:) + u(s-1,:,:))
           case (2); u(:,1,:) = real(0.5,cp)*(u(:,2,:) + u(:,s-1,:))
           case (3); u(:,:,1) = real(0.5,cp)*(u(:,:,2) + u(:,:,s-1))
           case (4); u(s,:,:) = real(0.5,cp)*(u(s-1,:,:) + u(2,:,:))
           case (5); u(:,s,:) = real(0.5,cp)*(u(:,s-1,:) + u(:,2,:))
           case (6); u(:,:,s) = real(0.5,cp)*(u(:,:,s-1) + u(:,:,2))
           end select
         case (7) ! Periodic - interpolated - wall incoincident ~O(dh)
           select case (face)
           case (1); u(1,:,:) = real(0.5,cp)*(u(2,:,:) + u(s-1,:,:))
           case (2); u(:,1,:) = real(0.5,cp)*(u(:,2,:) + u(:,s-1,:))
           case (3); u(:,:,1) = real(0.5,cp)*(u(:,:,2) + u(:,:,s-1))
           case (4); u(s,:,:) = real(0.5,cp)*(u(s-1,:,:) + u(2,:,:))
           case (5); u(:,s,:) = real(0.5,cp)*(u(:,s-1,:) + u(:,2,:))
           case (6); u(:,:,s) = real(0.5,cp)*(u(:,:,s-1) + u(:,:,2))
           end select
         case (8) ! Periodic - interpolated - wall incoincident ~O(dh^2)
           select case (face)
           case (1); u(1,:,:) = real(1.0,cp)/real(3.0,cp)*(real(3.0,cp)*u(2,:,:) + u(s-1,:,:) - u(3,:,:))
           case (2); u(:,1,:) = real(1.0,cp)/real(3.0,cp)*(real(3.0,cp)*u(:,2,:) + u(:,s-1,:) - u(:,3,:))
           case (3); u(:,:,1) = real(1.0,cp)/real(3.0,cp)*(real(3.0,cp)*u(:,:,2) + u(:,:,s-1) - u(:,:,3))
           case (4); u(s,:,:) = real(-1.0,cp)/real(3.0,cp)*(u(s-2,:,:) - real(3.0,cp)*u(s-1,:,:) - u(2,:,:))
           case (5); u(:,s,:) = real(-1.0,cp)/real(3.0,cp)*(u(:,s-2,:) - real(3.0,cp)*u(:,s-1,:) - u(:,2,:))
           case (6); u(:,:,s) = real(-1.0,cp)/real(3.0,cp)*(u(:,:,s-2) - real(3.0,cp)*u(:,:,s-1) - u(:,:,2))
           end select
         end select
       end subroutine

       subroutine applyGhostPoints3D(u,bctype,face,bvals,hn,hc,s)
         implicit none
         real(cp),intent(inout),dimension(:,:,:) :: u
         real(cp),intent(in),dimension(:) :: hn,hc
         real(cp),dimension(:,:),intent(in) :: bvals
         integer,intent(in) :: bctype,face
         integer,intent(in) :: s
         call applyBCs3D(u,bctype,face,bvals,hn,hc,s)
       end subroutine

       end module