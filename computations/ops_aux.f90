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
       use scalarField_mod
       use interpOps_mod
       use IO_scalarFields_mod

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

       public :: stabilityTerms
       interface stabilityTerms;          module procedure stabilityTermsSF;        end interface
       interface stabilityTerms;          module procedure stabilityTermsVF;        end interface

       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPointsSF;       end interface
       interface zeroGhostPoints;         module procedure zeroGhostPointsVF;       end interface

       public :: printPhysicalMinMax
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxSF;   end interface
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxVF;   end interface

       public :: printGlobalMinMax
       interface printGlobalMinMax;     module procedure printGlobalMinMaxSF;   end interface
       interface printGlobalMinMax;     module procedure printGlobalMinMaxVF;   end interface

       public :: checkGlobalMinMax
       interface checkGlobalMinMax;     module procedure checkGlobalMinMaxSF;   end interface
       interface checkGlobalMinMax;     module procedure checkGlobalMinMaxSF2;  end interface
       interface checkGlobalMinMax;     module procedure checkGlobalMinMaxVF;   end interface

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

       subroutine stabilityTermsSF(fo,fi,g,n,dir) ! Finished
         ! Computes
         !                     |  fi  |
         !    fo =  max( fo  , | ---- | )
         !                     | dh^n |
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: fo
         real(cp),dimension(:,:,:),intent(in) :: fi
         type(grid),intent(in) :: g
         integer,intent(in) :: n,dir
         integer :: i,j,k,t,x,y,z
         integer,dimension(3) :: s
         s = shape(fi)
         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
           stop 'Error: dir must = 1,2,3 in stabilityTermsSF in ops_aux.f90'
         end select
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           t = i*x + j*y + k*z
           fo(i,j,k) = maxval((/fo(i,j,k),&
                                          abs(fi(i,j,k)/g%c(dir)%dhn(t)**real(n,cp))/))
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

       subroutine printGlobalMinMaxSF(u,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         character(len=*),intent(in) :: name
         write(*,*) 'Min/Max ('//name//') = ',minval(u),maxval(u)
       end subroutine

       subroutine checkGlobalMinMaxSF(du,name,i)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: du
         integer,intent(inout) :: i
         character(len=*),intent(in) :: name
         real(cp) :: tol
         tol = real(10.0**(-32.0),cp)
         if (maxval(abs(du)).gt.tol) then
           write(*,*) 'Min/Max ('//name//') = ',minval(du),maxval(du)
           i = 1
         else; i = 0
         endif
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

       subroutine stabilityTermsVF(fo,fi,g,n)
         implicit none
         type(scalarField),intent(inout) :: fo
         type(vectorField),intent(in) :: fi
         type(grid),intent(in) :: g
         integer,intent(in) :: n
         call assign(fo,real(0.0,cp))
         call stabilityTerms(fo%phi,fi%x,g,n,1)
         call stabilityTerms(fo%phi,fi%y,g,n,2)
         call stabilityTerms(fo%phi,fi%z,g,n,3)
         call zeroGhostPoints(fo%phi)
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

       subroutine printPhysicalMinMaxVF(U,namex,namey,namez)
         implicit none
         type(vectorField),intent(in) :: U
         character(len=*),intent(in) :: namex,namey,namez
         call printPhysicalMinMax(U%x,U%sx,namex)
         call printPhysicalMinMax(U%y,U%sy,namey)
         call printPhysicalMinMax(U%z,U%sz,namez)
       end subroutine

       subroutine printGlobalMinMaxVF(U,namex,namey,namez)
         implicit none
         type(vectorField),intent(in) :: U
         character(len=*),intent(in) :: namex,namey,namez
         call printGlobalMinMax(U%x,namex)
         call printGlobalMinMax(U%y,namey)
         call printGlobalMinMax(U%z,namez)
       end subroutine

       subroutine checkGlobalMinMaxVF(u,g,name)
         implicit none
         type(vectorField),intent(in) :: u
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: name
         type(vectorField) :: temp
         type(del) :: d
         real(cp),dimension(3) :: t
         integer :: i

         call allocateVectorField(temp,u)

         ! Resize x-direction:
         if (u%sx(1).eq.g%c(1)%sn) then
           call allocateX(temp,g%c(1)%sc,u%sx(2),u%sx(3))
         elseif (u%sx(1).eq.g%c(1)%sc) then
           call allocateX(temp,g%c(1)%sn,u%sx(2),u%sx(3))
         else
          stop 'Error: bad sizes in checkGlobalMinMaxVF in ops_aux.f90'
         endif

         if (u%sy(1).eq.g%c(1)%sn) then
           call allocateY(temp,g%c(1)%sc,u%sy(2),u%sy(3))
         elseif (u%sy(1).eq.g%c(1)%sc) then
           call allocateY(temp,g%c(1)%sn,u%sy(2),u%sy(3))
         else
          stop 'Error: bad sizes in checkGlobalMinMaxVF in ops_aux.f90'
         endif

         if (u%sz(1).eq.g%c(1)%sn) then
           call allocateZ(temp,g%c(1)%sc,u%sz(2),u%sz(3))
         elseif (u%sz(1).eq.g%c(1)%sc) then
           call allocateZ(temp,g%c(1)%sn,u%sz(2),u%sz(3))
         else
          stop 'Error: bad sizes in checkGlobalMinMaxVF in ops_aux.f90'
         endif

         call d%assign(temp%x,u%x,g,1,1,0)
         call d%assign(temp%y,u%y,g,1,1,0)
         call d%assign(temp%z,u%z,g,1,1,0)
         call checkGlobalMinMax(temp%x,name//'_x',i)
         call checkGlobalMinMax(temp%y,name//'_y',i)
         call checkGlobalMinMax(temp%z,name//'_z',i)
         if (i.eq.1) then
          call writeToFile(g,temp%x,'',name//'_x')
          call writeToFile(g,temp%y,'',name//'_y')
          call writeToFile(g,temp%z,'',name//'_z')
          stop 'Done'
         endif
         t(1) = maxval(abs(u%x)); t(2) = maxval(abs(u%y)); t(3) = maxval(abs(u%z))
         write(*,*) 'Good Max ('//name//') = ',maxval(t)
         ! t(1) = maxval(abs(temp%x)); t(2) = maxval(abs(temp%y)); t(3) = maxval(abs(temp%z))
         ! write(*,*) 'Good Max (d '//name//') = ',maxval(t)
         call delete(temp)
       end subroutine

       subroutine checkGlobalMinMaxSF2(u,g,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: name
         type(scalarField) :: temp
         type(del) :: d
         integer :: i
         integer,dimension(3) :: s
         s = shape(u)

         ! Resize x-direction:
         if (s(1).eq.g%c(1)%sn) then
           call allocateField(temp,g%c(1)%sc,s(2),s(3))
         elseif (s(1).eq.g%c(1)%sc) then
           call allocateField(temp,g%c(1)%sn,s(2),s(3))
         else
          stop 'Error: bad sizes in checkGlobalMinMaxVF in ops_aux.f90'
         endif

         call d%assign(temp%phi,u,g,1,1,0)
         call checkGlobalMinMax(temp%phi,name//'_phi',i)
         if (i.eq.1) then
          call writeToFile(g,temp%phi,'',name//'_phi')
          stop 'Done'
         endif
         write(*,*) 'Good Max ('//name//') = ',maxval(abs(u))
         call delete(temp)
       end subroutine

       end module