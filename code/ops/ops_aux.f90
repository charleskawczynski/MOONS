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
       use ops_del_mod
       use grid_mod
       use mesh_mod
       use domain_mod
       use ops_embedExtract_mod
       use VF_mod
       use SF_mod

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
       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

       ! ----------------------------------- OTHER ROUTINES ------------------------------------

       public :: dot_product
       interface dot_product;             module procedure dot_product_SF;            end interface
       interface dot_product;             module procedure dot_product_VF;            end interface

       public :: collocatedMagnitude
       interface collocatedMagnitude;     module procedure collocatedMagnitude_RF;    end interface
       interface collocatedMagnitude;     module procedure collocatedMagnitude_VF;    end interface

       public :: boundaryFlux
       interface boundaryFlux;            module procedure boundaryFlux_VF;           end interface
       interface boundaryFlux;            module procedure boundaryFlux_VF_SD;        end interface

       public :: subtract_physical_mean
       interface subtract_physical_mean;  module procedure subtract_phys_mean_SF;     end interface
       interface subtract_physical_mean;  module procedure subtract_phys_mean_vol_SF; end interface

       public :: physical_mean
       interface physical_mean;           module procedure phys_mean_vol_SF;          end interface
       interface physical_mean;           module procedure phys_mean_SF;              end interface

       public :: stabilityTerms
       interface stabilityTerms;          module procedure stabilityTerms_RF;         end interface
       interface stabilityTerms;          module procedure stabilityTerms_SF;         end interface
       interface stabilityTerms;          module procedure stabilityTerms_VF;         end interface

       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPoints_RF;        end interface
       interface zeroGhostPoints;         module procedure zeroGhostPoints_VF;        end interface
       interface zeroGhostPoints;         module procedure zeroGhostPoints_SF;        end interface

       public :: zeroWall
       interface zeroWall;                module procedure zeroWall_RF;               end interface
       interface zeroWall;                module procedure zeroWall_SF;               end interface
       interface zeroWall;                module procedure zeroWall_VF;               end interface
        
       public :: zeroWall_conditional
       interface zeroWall_conditional;    module procedure zeroWall_conditional_SF;   end interface
       interface zeroWall_conditional;    module procedure zeroWall_conditional_VF;   end interface
       interface zeroWall_conditional;    module procedure zeroWall_conditional_SF2;  end interface
       interface zeroWall_conditional;    module procedure zeroWall_conditional_VF2;  end interface

       public :: zeroInterior
       interface zeroInterior;            module procedure zeroInterior_RF;           end interface
       interface zeroInterior;            module procedure zeroInterior_VF;           end interface
       interface zeroInterior;            module procedure zeroInterior_SF;           end interface

       public :: sineWaves
       interface sineWaves;               module procedure sineWaves_SF;              end interface

       public :: cosineWaves
       interface cosineWaves;             module procedure cosineWaves_SF;            end interface

       public :: assign_gradGhost
       interface assign_gradGhost;        module procedure assign_gradGhost_SF;       end interface
       interface assign_gradGhost;        module procedure assign_gradGhost_RF;       end interface

       public :: treatInterface
       interface treatInterface;          module procedure treatInterface_RF;         end interface
       interface treatInterface;          module procedure treatInterface_VF;         end interface
       interface treatInterface;          module procedure treatInterface_SF;         end interface

       public :: printPhysicalMinMax
       interface printPhysicalMinMax;     module procedure printPhysicalMinMax_SF;    end interface
       interface printPhysicalMinMax;     module procedure printPhysicalMinMax_VF;    end interface

       public :: printGlobalMinMax
       interface printGlobalMinMax;       module procedure printGlobalMinMax_SF;      end interface
       interface printGlobalMinMax;       module procedure printGlobalMinMax_VF;      end interface

       public :: noise
       interface noise;                   module procedure noise_RF;                  end interface
       interface noise;                   module procedure noise_SF;                  end interface
       interface noise;                   module procedure noise_VF;                  end interface

       public :: unitVector
       interface unitVector;              module procedure unitVector_SF;             end interface

       public :: deleteUnitVector
       interface deleteUnitVector;        module procedure deleteUnitVector_SF;       end interface

       ! public :: get_multi_index
       ! interface get_multi_index;         module procedure get_multi_index_SF;        end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ********************************* REAL ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitude_RF(mag,x,y,z,s) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: x,y,z
         real(cp),dimension(:,:,:),intent(inout) :: mag
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           mag(i,j,k) = sqrt(x(i,j,k)**2.0_cp +&
                             y(i,j,k)**2.0_cp +&
                             z(i,j,k)**2.0_cp)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine stabilityTerms_RF(fo,fi,g,n,s,dir) ! Finished
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
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k,t,x,y,z
         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default; stop 'Error: dir must = 1,2,3 in stabilityTerms_RF in ops_aux.f90'
         end select
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           t = i*x + j*y + k*z
           fo(i,j,k) = maxval((/fo(i,j,k),&
                                          abs(fi(i,j,k)/g%c(dir)%dhn(t)**real(n,cp))/))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine boundaryFlux_VF(BF,u,m)
         ! Computes
         ! 
         !   BF = ∫∫ u•n dA
         ! 
         implicit none
         type(VF),intent(in) :: u
         real(cp),intent(inout) :: BF
         type(mesh),intent(in) :: m
         real(cp) :: BFtemp
         integer :: i,j,k,t
         BFtemp = 0.0_cp ! temp is necessary for reduction
         BF = 0.0_cp
         do t=1,m%s
           if (.not.m%g(t)%st_faces(1)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
               BFtemp = BFtemp + u%x%RF(t)%f(2,j,k)*m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_faces(2)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
               BFtemp = BFtemp + u%x%RF(t)%f(u%x%RF(t)%s(1)-1,j,k)*m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
         do t=1,m%s
           if (.not.m%g(t)%st_faces(3)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
               BFtemp = BFtemp + u%y%RF(t)%f(i,2,k)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_faces(4)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
               BFtemp = BFtemp + u%y%RF(t)%f(i,u%y%RF(t)%s(2)-1,k)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
         do t=1,m%s
           if (.not.m%g(t)%st_faces(5)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
               BFtemp = BFtemp + u%z%RF(t)%f(i,j,2)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(2)%dhn(j)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_faces(6)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
               BFtemp = BFtemp + u%z%RF(t)%f(i,j,u%z%RF(t)%s(3)-1)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(2)%dhn(j)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
       end subroutine

       subroutine subtract_phys_mean_SF(u)
         ! Subtracts the physical mean from scalar field u
         ! 
         !      u = u - mean(u)
         ! 
         ! Where this mean operation is only in the interior domain
         implicit none
         type(SF),intent(inout) :: u
         real(cp) :: meanU
         meanU = physical_mean(u)
         call subtract(u,meanU)
         call zeroGhostPoints(u)
       end subroutine

       subroutine subtract_phys_mean_vol_SF(u,vol,temp)
         ! Subtracts the physical mean from scalar field u
         ! 
         !      u = u - mean(u)
         ! 
         ! Where this mean operation is only in the interior domain
         implicit none
         type(SF),intent(inout) :: u,temp
         type(SF),intent(in) :: vol
         real(cp) :: meanU
         meanU = physical_mean(u,vol,temp)
         call subtract(u,meanU)
         call zeroGhostPoints(u)
       end subroutine

       function phys_mean_vol_SF(u,vol,temp) result(meanU)
         ! Computes physical mean from scalar field u
         !      mean(u*volume)
         implicit none
         type(SF),intent(inout) :: u,temp
         type(SF),intent(in) :: vol
         real(cp) :: meanU
         call multiply(temp,u,vol)
         meanU = sum(temp,1)/real(u%numPhysEl,cp)
       end function

       function phys_mean_SF(u) result(meanU)
         ! Computes physical mean from scalar field u
         !      mean(u)
         implicit none
         type(SF),intent(in) :: u
         real(cp) :: meanU
         meanU = sum(u,1)/real(u%numPhysEl,cp)
       end function

       subroutine zeroGhostPoints_RF(f,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
         f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
         f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
       end subroutine

       subroutine zeroWall_RF(f,s,face)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: face
         select case (face)
         case (1); f(2,:,:) = 0.0_cp
         case (3); f(:,2,:) = 0.0_cp
         case (5); f(:,:,2) = 0.0_cp
         case (2); f(s(1)-1,:,:) = 0.0_cp
         case (4); f(:,s(2)-1,:) = 0.0_cp
         case (6); f(:,:,s(3)-1) = 0.0_cp
         case default; stop 'Error: face must = 1:6 in zeroWall_RF in ops_aux.f90'
         end select
       end subroutine

       subroutine zeroInterior_RF(f,s,px,py,pz)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: px,py,pz
         integer :: i,j,k
         ! $OMP PARALLEL DO
         do k=2+px,s(1)-1-px; do j=2+py,s(2)-1-py; do i=2+pz,s(3)-1-pz
         f(i,j,k) = 0.0_cp
         enddo; enddo; enddo
         ! $OMP END PARALLEL DO
       end subroutine

       subroutine assign_gradGhost_SF(CC,F)
         implicit none
         type(SF),intent(inout) :: CC
         type(VF),intent(in) :: F
         integer :: i
         do i=1,CC%s
         call assign_gradGhost_RF(CC%RF(i)%f,F%x%RF(i)%f,F%y%RF(i)%f,F%z%RF(i)%f,CC%RF(i)%s)
         enddo
       end subroutine

       subroutine assign_gradGhost_RF(CC,Fx,Fy,Fz,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: CC
         real(cp),dimension(:,:,:),intent(in) :: Fx,Fy,Fz
         integer,dimension(3),intent(in) :: s
         CC(1,:,:) = Fx(2,:,:); CC(s(1),:,:) = Fx(s(1),:,:)
         CC(:,1,:) = Fy(:,2,:); CC(:,s(2),:) = Fy(:,s(2),:)
         CC(:,:,1) = Fz(:,:,2); CC(:,:,s(3)) = Fz(:,:,s(3))
       end subroutine

       subroutine treatInterface_RF(f,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         real(cp) :: top,bot,int,mi,ma
         mi = minval(f); ma = maxval(f)
         int = 0.5_cp*(mi+ma)
         top = 0.5_cp*(ma+int)
         bot = 0.5_cp*(mi+int)
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

       subroutine noise_RF(f,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         integer :: i,j,k
         real(cp) :: r
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
         call random_number(r)
         f(i,j,k) = r
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine sineWaves_SF(f,m,wavenum,phi)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         real(cp),dimension(3),intent(in) :: wavenum,phi
         integer :: i,j,k,t
         if (f%is_CC) then
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hn(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hn(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hn(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (f%is_Node) then
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hn(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hn(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hn(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (f%is_Face) then
         select case (f%face)
         case (1)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hn(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hc(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hc(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hc(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hn(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hc(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hc(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hc(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hn(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case default; stop 'Error: face must = 1,2,3 in sineWaves_RF'
         end select
         elseif (f%is_Edge) then
         select case (f%face)
         case (1)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hc(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hn(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hn(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hn(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hc(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hn(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = sin(wavenum(1)*PI*(m%g(t)%c(1)%hn(i) - phi(1)))*&
                                sin(wavenum(2)*PI*(m%g(t)%c(2)%hn(j) - phi(2)))*&
                                sin(wavenum(3)*PI*(m%g(t)%c(3)%hc(k) - phi(3)))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case default; stop 'Error: face must = 1,2,3 in sineWaves_RF'
         end select
         else; stop 'Error: bad input to sineWaves_RF in ops_aux.f90'
         endif
       end subroutine

       subroutine cosineWaves_SF(f,m,wavenum)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         real(cp),dimension(3),intent(in) :: wavenum
         integer :: i,j,k,t
         if (f%is_CC) then
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hn(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hn(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hn(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (f%is_Node) then
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hn(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hn(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hn(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (f%is_Face) then
         select case (f%face)
         case (1)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hn(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hc(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hc(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hc(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hn(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hc(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hc(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hc(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hn(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case default; stop 'Error: face must = 1,2,3 in sineWaves_RF'
         end select
         elseif (f%is_Edge) then
         select case (f%face)
         case (1)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hc(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hn(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hn(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hn(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hc(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hn(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do t=1,f%s; do k=1,f%RF(t)%s(3); do j=1,f%RF(t)%s(2); do i=1,f%RF(t)%s(1)
             f%RF(t)%f(i,j,k) = cos(wavenum(1)*PI*m%g(t)%c(1)%hn(i))*&
                                cos(wavenum(2)*PI*m%g(t)%c(2)%hn(j))*&
                                cos(wavenum(3)*PI*m%g(t)%c(3)%hc(k))
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case default; stop 'Error: face must = 1,2,3 in sineWaves_RF'
         end select
         else; stop 'Error: bad input to sineWaves_RF in ops_aux.f90'
         endif
       end subroutine

      function dot_product_VF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: A,B,x
        type(VF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp%x,1) + sum(temp%y,1) + sum(temp%z,1)
      end function

      function dot_product_SF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: A,B,x
        type(SF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp,1)
      end function

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* SCALAR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine zeroGhostPoints_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         integer :: i
         do i=1,f%s; call zeroGhostPoints(f%RF(i)%f,f%RF(i)%s); enddo
       end subroutine

       subroutine zeroWall_SF(f,m)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         integer :: i
         if (Node_along(f,1)) then
           do i=1,m%s
             call zeroWall(f%RF(i)%f,f%RF(i)%s,1)
             call zeroWall(f%RF(i)%f,f%RF(i)%s,2)
           enddo
         endif
         if (Node_along(f,2)) then
           do i=1,m%s
             call zeroWall(f%RF(i)%f,f%RF(i)%s,3)
             call zeroWall(f%RF(i)%f,f%RF(i)%s,4)
           enddo
         endif
         if (Node_along(f,3)) then
           do i=1,m%s
             call zeroWall(f%RF(i)%f,f%RF(i)%s,5)
             call zeroWall(f%RF(i)%f,f%RF(i)%s,6)
           enddo
         endif
       end subroutine

       subroutine zeroWall_conditional_SF(f,m)
         ! Sets wall coincident values to zero if 
         ! boundary conditions of u are NOT Neumann (bctype=3)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         logical :: TF
         integer :: i
         if (Node_along(f,1)) then
           do i=1,m%s
             TF = (.not.m%g(i)%st_faces(1)%TF).and.(.not.f%RF(i)%b%f(1)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,1)
             TF = (.not.m%g(i)%st_faces(2)%TF).and.(.not.f%RF(i)%b%f(2)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,2)
           enddo
         endif
         if (Node_along(f,2)) then
           do i=1,m%s
             TF = (.not.m%g(i)%st_faces(3)%TF).and.(.not.f%RF(i)%b%f(3)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,3)
             TF = (.not.m%g(i)%st_faces(4)%TF).and.(.not.f%RF(i)%b%f(4)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,4)
           enddo
         endif
         if (Node_along(f,3)) then
           do i=1,m%s
             TF = (.not.m%g(i)%st_faces(5)%TF).and.(.not.f%RF(i)%b%f(5)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,5)
             TF = (.not.m%g(i)%st_faces(6)%TF).and.(.not.f%RF(i)%b%f(6)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,6)
           enddo
         endif
       end subroutine

       subroutine zeroWall_conditional_SF2(f,m,u)
         ! Sets wall coincident values to zero if 
         ! boundary conditions of u are NOT Neumann (bctype=3)
         implicit none
         type(SF),intent(inout) :: f
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         logical :: TF
         integer :: i
         if (Node_along(f,1)) then
           do i=1,m%s
             TF = (.not.m%g(i)%st_faces(1)%TF).and.(.not.u%RF(i)%b%f(1)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,1)
             TF = (.not.m%g(i)%st_faces(2)%TF).and.(.not.u%RF(i)%b%f(2)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,2)
           enddo
         endif
         if (Node_along(f,2)) then
           do i=1,m%s
             TF = (.not.m%g(i)%st_faces(3)%TF).and.(.not.u%RF(i)%b%f(3)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,3)
             TF = (.not.m%g(i)%st_faces(4)%TF).and.(.not.u%RF(i)%b%f(4)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,4)
           enddo
         endif
         if (Node_along(f,3)) then
           do i=1,m%s
             TF = (.not.m%g(i)%st_faces(5)%TF).and.(.not.u%RF(i)%b%f(5)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,5)
             TF = (.not.m%g(i)%st_faces(6)%TF).and.(.not.u%RF(i)%b%f(6)%b%Neumann)
             if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,6)
           enddo
         endif
       end subroutine

       subroutine zeroInterior_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         integer :: i,x,y,z
         call C0_N1_tensor(f,x,y,z)
         do i=1,f%s; call zeroInterior(f%RF(i)%f,f%RF(i)%s,x,y,z); enddo
       end subroutine

       subroutine treatInterface_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         integer :: i
         do i=1,f%s; call treatInterface(f%RF(i)%f,f%RF(i)%s); enddo
       end subroutine

       subroutine printPhysicalMinMax_SF(U,name)
         implicit none
         type(SF),intent(in) :: U
         character(len=*),intent(in) :: name
         write(*,*) 'Min/Max ('//name//') = ',min(u,1),max(u,1)
       end subroutine

       subroutine printGlobalMinMax_SF(U,name)
         implicit none
         type(SF),intent(in) :: U
         character(len=*),intent(in) :: name
         write(*,*) 'Min/Max ('//name//') = ',min(u),max(u)
       end subroutine

       subroutine noise_SF(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: i
         do i=1,U%s; call noise(U%RF(i)%f,U%RF(i)%s); enddo
       end subroutine

       subroutine unitVector_SF(U,un)
         implicit none
         type(SF),intent(inout) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         if (un.lt.1) stop 'Error: un must > 0 in unitVector_SF in ops_aux.f90'
         if (un.gt.U%numEl) stop 'Error: un must < U%numEl in unitVector_SF in ops_aux.f90'
         call get_3D_index(i,j,k,t,U,un)
         U%RF(t)%f(i,j,k) = 1.0_cp
       end subroutine

       subroutine deleteUnitVector_SF(U,un)
         implicit none
         type(SF),intent(inout) :: U
         integer,intent(in) :: un
         integer :: i,j,k,t
         if (un.lt.1) stop 'Error: un must > 0 in unitVector_consecutive_SF in ops_aux.f90'
         if (un.gt.U%numEl) stop 'Error: un must < U%numEl in unitVector_consecutive_SF in ops_aux.f90'
         call get_3D_index(i,j,k,t,U,un)
         U%RF(t)%f(i,j,k) = 0.0_cp
       end subroutine

       subroutine stabilityTerms_SF(fo,fi,m,n,dir)
         implicit none
         type(SF),intent(inout) :: fo
         type(SF),intent(in) :: fi
         type(mesh),intent(in) :: m
         integer,intent(in) :: n,dir
         integer :: i
         call assign(fo,0.0_cp)
         do i=1,fi%s; call stabilityTerms(fo%RF(i)%f,fi%RF(i)%f,m%g(i),n,fi%RF(i)%s,dir); enddo
         call zeroGhostPoints(fo)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitude_VF(mag,V)
         implicit none
         type(SF),intent(inout) :: mag
         type(VF),intent(in) :: V
         integer :: i
         do i=1,mag%s
           call collocatedMagnitude(mag%RF(i)%f,V%x%RF(i)%f,&
                                                V%y%RF(i)%f,&
                                                V%z%RF(i)%f,&
                                                V%x%RF(i)%s)
         enddo
       end subroutine

       subroutine zeroWall_VF(V,m)
         implicit none
         type(VF),intent(inout) :: V
         type(mesh),intent(in) :: m
         call zeroWall(V%x,m); call zeroWall(V%y,m); call zeroWall(V%z,m)
       end subroutine

       subroutine zeroWall_conditional_VF2(V,m,U)
         implicit none
         type(VF),intent(inout) :: V
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call zeroWall_conditional(V%x,m,U%x)
         call zeroWall_conditional(V%y,m,U%y)
         call zeroWall_conditional(V%z,m,U%z)
       end subroutine

       subroutine zeroWall_conditional_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call zeroWall_conditional(U%x,m)
         call zeroWall_conditional(U%y,m)
         call zeroWall_conditional(U%z,m)
       end subroutine

       subroutine stabilityTerms_VF(fo,fi,m,n)
         implicit none
         type(SF),intent(inout) :: fo
         type(VF),intent(in) :: fi
         type(mesh),intent(in) :: m
         integer,intent(in) :: n
         call stabilityTerms(fo,fi%x,m,n,1)
         call stabilityTerms(fo,fi%y,m,n,2)
         call stabilityTerms(fo,fi%z,m,n,3)
       end subroutine

       subroutine boundaryFlux_VF_SD(BF,f,D)
         implicit none
         type(VF),intent(in) :: f
         real(cp),intent(inout) :: BF
         type(domain),intent(in) :: D
         type(VF) :: temp
         if (.not.f%is_Face) stop 'Error: Boundary flux must be computed on face in boundaryFlux_VF_SD in ops_aux.f90'
         call init_Face(temp,D%m_in)
         call extractFace(temp,f,D)
         call boundaryFlux(BF,temp,D%m_in)
         call delete(temp)
       end subroutine

       ! is this right???
       ! subroutine boundaryShear_VF(BF,f,m,temp_F,temp_F_TF)
       !   implicit none
       !   type(VF),intent(in) :: f
       !   real(cp),intent(inout) :: BF
       !   type(mesh),intent(in) :: m
       !   type(VF),intent(inout) :: temp_F
       !   type(TF),intent(inout) :: temp_F_TF
       !   type(VF) :: temp
       !   if (.not.f%is_Face) stop 'Error: Boundary flux must be computed on face in boundaryFlux_VF_SD in ops_aux.f90'
       !   call face2face(temp_F_TF,f,m)
       !   call init_Face(temp,m)
       !   call assign(temp%x,temp_F_TF%x%x)
       !   call assign(temp%y,temp_F_TF%y%y)
       !   call assign(temp%z,temp_F_TF%z%z)
       !   call boundaryFlux(BF,temp,m)
       !   call delete(temp)
       ! end subroutine

       subroutine zeroGhostPoints_VF(f)
         implicit none
         type(VF),intent(inout) :: f
         call zeroGhostPoints(f%x)
         call zeroGhostPoints(f%y)
         call zeroGhostPoints(f%z)
       end subroutine

       subroutine zeroInterior_VF(f)
         implicit none
         type(VF),intent(inout) :: f
         call zeroInterior(f%x)
         call zeroInterior(f%y)
         call zeroInterior(f%z)
       end subroutine

       subroutine treatInterface_VF(f)
         implicit none
         type(VF),intent(inout) :: f
         call treatInterface(f%x)
         call treatInterface(f%y)
         call treatInterface(f%z)
       end subroutine

       subroutine printPhysicalMinMax_VF(U,name)
         implicit none
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: name
         call printPhysicalMinMax(U%x,name//'_x')
         call printPhysicalMinMax(U%y,name//'_y')
         call printPhysicalMinMax(U%z,name//'_z')
       end subroutine

       subroutine printGlobalMinMax_VF(U,name)
         implicit none
         type(VF),intent(in) :: U
         character(len=*),intent(in) :: name
         call printGlobalMinMax(U%x,name//'_x')
         call printGlobalMinMax(U%y,name//'_y')
         call printGlobalMinMax(U%z,name//'_z')
       end subroutine

       subroutine noise_VF(U)
         implicit none
         type(VF),intent(inout) :: U
         call noise(U%x); call noise(U%y); call noise(U%z)
       end subroutine

       end module