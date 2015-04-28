       module ops_aux_mod
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
       use vectorField_mod
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

       public :: collocatedMagnitude
       interface collocatedMagnitude;     module procedure collocatedMagnitudeSF;   end interface
       interface collocatedMagnitude;     module procedure collocatedMagnitudeVF;   end interface

       public :: totalEnergy
       interface totalEnergy;             module procedure totalEnergySF;           end interface
       interface totalEnergy;             module procedure totalEnergyVF;           end interface

       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPointsSF;       end interface
       interface zeroGhostPoints;         module procedure zeroGhostPointsVF;       end interface

       public :: printPhysicalMinMax
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxSF;   end interface
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxVF;   end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* SCALAR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitudeSF(mag,x,y,z) ! Finished
         ! This routine was made in order to compare norm(B) with
         ! results from Salah
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: x,y,z
         real(cp),dimension(:,:,:),intent(inout) :: mag
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(x)
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           mag(i,j,k) = sqrt(x(i,j,k)**real(2.0,cp) +&
                             y(i,j,k)**real(2.0,cp) +&
                             z(i,j,k)**real(2.0,cp))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine totalEnergySF(e,x,y,z,g) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: x,y,z
         real(cp),intent(inout) :: e
         type(grid),intent(in) :: g
         real(cp) :: eTemp
         integer :: i,j,k
         integer,dimension(3) :: s
         s = shape(x)
         eTemp = real(0.0,cp) ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(g), REDUCTION(+:eTemp)
         do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
           eTemp = eTemp + (x(i,j,k)**real(2.0,cp) +&
                            y(i,j,k)**real(2.0,cp) +&
                            z(i,j,k)**real(2.0,cp))*g%c(1)%dhn(i)*&
                                                    g%c(2)%dhn(j)*&
                                                    g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = etemp/g%volume
       end subroutine

       subroutine zeroGhostPointsSF(f)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3) :: s
         s = shape(f)
         f(1,:,:) = real(0.0,cp); f(s(1),:,:) = real(0.0,cp)
         f(:,1,:) = real(0.0,cp); f(:,s(2),:) = real(0.0,cp)
         f(:,:,1) = real(0.0,cp); f(:,:,s(3)) = real(0.0,cp)
       end subroutine

       subroutine printPhysicalMinMaxSF(u,s,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         integer,dimension(3),intent(in) :: s
         character(len=*),intent(in) :: name
         write(*,*) 'Min/Max ('//name//') = ',minval(u(2:s(1)-1,2:s(2)-1,2:s(3)-1)),&
                                              maxval(u(2:s(1)-1,2:s(2)-1,2:s(3)-1))
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitudeVF(mag,V)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: mag
         type(vectorfield),intent(in) :: V
         call collocatedMagnitude(mag,V%x,V%y,V%z)
       end subroutine

       subroutine totalEnergyVF(e,VF,g)
         implicit none
         type(vectorField),intent(in) :: VF
         real(cp),intent(inout) :: e
         type(grid),intent(in) :: g
         call totalEnergy(e,VF%x,VF%y,VF%z,g)
       end subroutine

       subroutine zeroGhostPointsVF(VF)
         implicit none
         type(vectorField),intent(inout) :: VF
         call zeroGhostPoints(VF%x)
         call zeroGhostPoints(VF%y)
         call zeroGhostPoints(VF%z)
       end subroutine

       subroutine printPhysicalMinMaxVF(u,namex,namey,namez)
         implicit none
         type(vectorField),intent(in) :: u
         character(len=*),intent(in) :: namex,namey,namez
         call printPhysicalMinMax(U%x,U%sx,namex)
         call printPhysicalMinMax(U%y,U%sy,namey)
         call printPhysicalMinMax(U%z,U%sz,namez)
       end subroutine

       end module