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
       use VF_mod
       use SF_mod
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
       interface collocatedMagnitude;     module procedure collocatedMagnitudeReal;   end interface
       interface collocatedMagnitude;     module procedure collocatedMagnitudeVF;     end interface

       public :: totalEnergy
       interface totalEnergy;             module procedure totalEnergyReal;           end interface
       interface totalEnergy;             module procedure totalEnergyVF;             end interface

       public :: stabilityTerms
       interface stabilityTerms;          module procedure stabilityTermsReal;        end interface
       interface stabilityTerms;          module procedure stabilityTermsVF;          end interface

       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPointsReal;       end interface
       interface zeroGhostPoints;         module procedure zeroGhostPointsVF;         end interface
       interface zeroGhostPoints;         module procedure zeroGhostPointsSF;         end interface

       public :: zeroInterior
       interface zeroInterior;            module procedure zeroInteriorReal;          end interface
       interface zeroInterior;            module procedure zeroInteriorVF;            end interface
       interface zeroInterior;            module procedure zeroInteriorSF;            end interface

       public :: treatInterface
       interface treatInterface;          module procedure treatInterfaceReal;        end interface
       interface treatInterface;          module procedure treatInterfaceVF;          end interface
       interface treatInterface;          module procedure treatInterfaceSF;          end interface

       public :: printPhysicalMinMax
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxReal;   end interface
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxVF;     end interface

       public :: printGlobalMinMax
       interface printGlobalMinMax;       module procedure printGlobalMinMaxReal;     end interface
       interface printGlobalMinMax;       module procedure printGlobalMinMaxVF;       end interface

       public :: checkGlobalMinMax
       interface checkGlobalMinMax;       module procedure checkGlobalMinMaxReal;     end interface
       interface checkGlobalMinMax;       module procedure checkGlobalMinMaxReal2;    end interface
       interface checkGlobalMinMax;       module procedure checkGlobalMinMaxVF;       end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ********************************* REAL ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitudeReal(mag,x,y,z) ! Finished
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
           mag(i,j,k) = sqrt(x(i,j,k)**2.0_cp +&
                             y(i,j,k)**2.0_cp +&
                             z(i,j,k)**2.0_cp)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine stabilityTermsReal(fo,fi,g,n,dir) ! Finished
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
           stop 'Error: dir must = 1,2,3 in stabilityTermsReal in ops_aux.f90'
         end select
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           t = i*x + j*y + k*z
           fo(i,j,k) = maxval((/fo(i,j,k),&
                                          abs(fi(i,j,k)/g%c(dir)%dhn(t)**real(n,cp))/))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine totalEnergyReal(e,x,y,z,g) ! Finished
         ! Computes
         ! 
         !   e = ∫∫∫ ( x² + y² + z² ) dx dy dz
         ! 
         ! Where x,y,z lives in the cell center.
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
           eTemp = eTemp + (x(i,j,k)**2.0_cp +&
                            y(i,j,k)**2.0_cp +&
                            z(i,j,k)**2.0_cp)*g%c(1)%dhn(i)*&
                                                    g%c(2)%dhn(j)*&
                                                    g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = etemp/g%volume
       end subroutine

       subroutine zeroGhostPointsReal(f)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3) :: s
         s = shape(f)
         f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
         f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
         f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
       end subroutine

       subroutine zeroInteriorReal(f)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3) :: s
         s = shape(f)
         f(2:s(1)-1,:,:) = 0.0_cp
         f(:,2:s(2)-1,:) = 0.0_cp
         f(:,:,2:s(3)-1) = 0.0_cp
       end subroutine

       subroutine treatInterfaceReal(f)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3) :: s
         integer :: i,j,k
         real(cp) :: top,bot,int,mi,ma
         mi = minval(f); ma = maxval(f)
         int = 0.5_cp*(mi+ma)
         top = 0.5_cp*(ma+int)
         bot = 0.5_cp*(mi+int)
         s = shape(f)
         ! Make interface property the min/max of
         ! fluid / wall domain depending on treatment

         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
         if ((f(i,j,k).gt.mi).and.(f(i,j,k).lt.ma)) then
          f(i,j,k) = ma
         endif
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine printPhysicalMinMaxReal(u,s,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         integer,dimension(3),intent(in) :: s
         character(len=*),intent(in) :: name
         write(*,*) 'Min/Max ('//name//') = ',minval(u(2:s(1)-1,2:s(2)-1,2:s(3)-1)),&
                                              maxval(u(2:s(1)-1,2:s(2)-1,2:s(3)-1))
       end subroutine

       subroutine printGlobalMinMaxReal(u,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         character(len=*),intent(in) :: name
         write(*,*) 'Min/Max ('//name//') = ',minval(u),maxval(u)
       end subroutine

       subroutine checkGlobalMinMaxReal(du,name,i)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: du
         integer,intent(inout) :: i
         character(len=*),intent(in) :: name
         real(cp) :: tol
         tol = 10.0_cp**(-32.0_cp)
         if (maxval(abs(du)).gt.tol) then
           write(*,*) 'Min/Max ('//name//') = ',minval(du),maxval(du)
           i = 1
         else; i = 0
         endif
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* SCALAR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine zeroGhostPointsSF(f)
         implicit none
         type(SF),intent(inout) :: f
         call zeroGhostPoints(f%phi)
       end subroutine

       subroutine zeroInteriorSF(f)
         implicit none
         type(SF),intent(inout) :: f
         call zeroInterior(f%phi)
       end subroutine

       subroutine treatInterfaceSF(f)
         implicit none
         type(SF),intent(inout) :: f
         call treatInterface(f%phi)
       end subroutine


       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitudeVF(mag,V)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: mag
         type(VF),intent(in) :: V
         call collocatedMagnitude(mag,V%x,V%y,V%z)
       end subroutine

       subroutine stabilityTermsVF(fo,fi,g,n)
         implicit none
         type(SF),intent(inout) :: fo
         type(VF),intent(in) :: fi
         type(grid),intent(in) :: g
         integer,intent(in) :: n
         call assign(fo,0.0_cp)
         call stabilityTerms(fo%phi,fi%x,g,n,1)
         call stabilityTerms(fo%phi,fi%y,g,n,2)
         call stabilityTerms(fo%phi,fi%z,g,n,3)
         call zeroGhostPoints(fo%phi)
       end subroutine

       subroutine totalEnergyVF(e,f,g)
         implicit none
         type(VF),intent(in) :: f
         real(cp),intent(inout) :: e
         type(grid),intent(in) :: g
         call totalEnergy(e,f%x,f%y,f%z,g)
       end subroutine

       subroutine zeroGhostPointsVF(f)
         implicit none
         type(VF),intent(inout) :: f
         call zeroGhostPoints(f%x)
         call zeroGhostPoints(f%y)
         call zeroGhostPoints(f%z)
       end subroutine

       subroutine zeroInteriorVF(f)
         implicit none
         type(VF),intent(inout) :: f
         call zeroInterior(f%x)
         call zeroInterior(f%y)
         call zeroInterior(f%z)
       end subroutine

       subroutine treatInterfaceVF(f)
         implicit none
         type(VF),intent(inout) :: f
         call treatInterface(f%x)
         call treatInterface(f%y)
         call treatInterface(f%z)
       end subroutine

       subroutine printPhysicalMinMaxVF(U,namex,namey,namez)
         implicit none
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: namex,namey,namez
         call printPhysicalMinMax(U%x,U%sx,namex)
         call printPhysicalMinMax(U%y,U%sy,namey)
         call printPhysicalMinMax(U%z,U%sz,namez)
       end subroutine

       subroutine printGlobalMinMaxVF(U,namex,namey,namez)
         implicit none
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: namex,namey,namez
         call printGlobalMinMax(U%x,namex)
         call printGlobalMinMax(U%y,namey)
         call printGlobalMinMax(U%z,namez)
       end subroutine

       subroutine checkGlobalMinMaxVF(u,g,name)
         implicit none
         type(VF),intent(in) :: u
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: name
         type(VF) :: temp
         type(del) :: d
         real(cp),dimension(3) :: t
         integer :: i

         call init(temp,u)

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

       subroutine checkGlobalMinMaxReal2(u,g,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: name
         type(SF) :: temp
         type(del) :: d
         integer :: i
         integer,dimension(3) :: s
         s = shape(u)

         ! Resize x-direction:
         if (s(1).eq.g%c(1)%sn) then
           call init(temp,g%c(1)%sc,s(2),s(3))
         elseif (s(1).eq.g%c(1)%sc) then
           call init(temp,g%c(1)%sn,s(2),s(3))
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