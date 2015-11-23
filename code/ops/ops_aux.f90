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

       ! ----------------------------------- OTHER ROUTINES ------------------------------------

       public :: dot_product
       interface dot_product;             module procedure dot_product_SF;            end interface
       interface dot_product;             module procedure dot_product_VF;            end interface

       public :: collocatedMagnitude
       interface collocatedMagnitude;     module procedure collocatedMagnitude_RF;    end interface
       interface collocatedMagnitude;     module procedure collocatedMagnitude_VF;    end interface

       public :: totalEnergy
       interface totalEnergy;             module procedure totalEnergy_VF;            end interface
       interface totalEnergy;             module procedure totalEnergy_VF_SD;         end interface

       public :: volume
       interface volume;                  module procedure volume_SF;                 end interface
       interface volume;                  module procedure volume_VF;                 end interface

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

       ! public :: perturb
       ! interface perturb;       module procedure perturb_SF;      end interface
       ! interface perturb;       module procedure perturb_VF;      end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ********************************* REAL ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedMagnitude_RF(mag,x,y,z,s) ! Finished
         ! This routine was made in order to compare norm(B) with
         ! results from Salah
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
         case default
           stop 'Error: dir must = 1,2,3 in stabilityTerms_RF in ops_aux.f90'
         end select
         !$OMP PARALLEL DO
         do k=1,s(3); do j=1,s(2); do i=1,s(1)
           t = i*x + j*y + k*z
           fo(i,j,k) = maxval((/fo(i,j,k),&
                                          abs(fi(i,j,k)/g%c(dir)%dhn(t)**real(n,cp))/))
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine totalEnergy_VF(e,u,m) ! Finished
         ! Computes
         ! 
         !          1
         !   e = -------- ∫∫∫ ( u_x² + u_y² + u_z² ) dx dy dz
         !        volume
         ! 
         ! Where x,y,z lives in the cell center.
         ! This will yields expected results ONLY
         ! when fluid domains are completely contained
         ! by the total domain (case 1 in define_CE in subdomain.f90).
         implicit none
         type(VF),intent(in) :: u
         real(cp),intent(inout) :: e
         type(mesh),intent(in) :: m
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(m), REDUCTION(+:eTemp)
         do t=1,m%s
           do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + (u%x%RF(t)%f(i,j,k)**2.0_cp +&
                              u%y%RF(t)%f(i,j,k)**2.0_cp +&
                              u%z%RF(t)%f(i,j,k)**2.0_cp)*m%g(t)%c(1)%dhn(i)*&
                                                          m%g(t)%c(2)%dhn(j)*&
                                                          m%g(t)%c(3)%dhn(k)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = etemp/m%volume
       end subroutine

       subroutine volume_SF(u,m)
         ! Computes
         ! 
         !   volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
         implicit none
         type(SF),intent(inout) :: u
         type(mesh),intent(in) :: m
         integer :: i,j,k,t
         if (u%is_CC) then
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                (m%g(t)%c(2)%dhn(j))*&
                                (m%g(t)%c(3)%dhn(k))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         elseif (u%is_Node) then
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                (m%g(t)%c(2)%dhc(j-1))*&
                                (m%g(t)%c(3)%dhc(k-1))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         elseif (u%is_Face) then
         select case (u%face)
         case (1);
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                (m%g(t)%c(2)%dhn(j))*&
                                (m%g(t)%c(3)%dhn(k))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (2);
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                (m%g(t)%c(2)%dhc(j-1))*&
                                (m%g(t)%c(3)%dhn(k))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (3);
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                (m%g(t)%c(2)%dhn(j))*&
                                (m%g(t)%c(3)%dhc(k-1))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case default; stop 'Error: SF has no face location in volume_SF in ops_aux.f90'
         end select
         elseif (u%is_Edge) then
         select case (u%edge)
         case (1);
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                (m%g(t)%c(2)%dhc(j-1))*&
                                (m%g(t)%c(3)%dhc(k-1))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (2);
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                (m%g(t)%c(2)%dhn(j))*&
                                (m%g(t)%c(3)%dhc(k-1))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case (3);
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                (m%g(t)%c(2)%dhc(j-1))*&
                                (m%g(t)%c(3)%dhn(k))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         case default; stop 'Error: SF has no face location in volume_SF in ops_aux.f90'
         end select
         else; stop 'Error: SF has no location in volume_SF in ops_aux.f90'
         endif
       end subroutine

       subroutine zeroGhostPoints_RF(f,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
         f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
         f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
       end subroutine

       subroutine zeroWall_RF(f,s,dir,face)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir,face
         select case (dir)
         case (1); select case (face)
                   case (1); f(2,:,:) = 0.0_cp
                   case (2); f(s(1)-1,:,:) = 0.0_cp
                   case default; stop 'Error: face must = 1:6 in zeroWall_RF in ops_aux.f90'
                   end select
         case (2); select case (face)
                   case (3); f(:,2,:) = 0.0_cp
                   case (4); f(:,s(2)-1,:) = 0.0_cp
                   case default; stop 'Error: face must = 1:6 in zeroWall_RF in ops_aux.f90'
                   end select
         case (3); select case (face)
                   case (5); f(:,:,2) = 0.0_cp
                   case (6); f(:,:,s(3)-1) = 0.0_cp
                   case default; stop 'Error: face must = 1:6 in zeroWall_RF in ops_aux.f90'
                   end select
         case default; stop 'Error: dir must = 1,2,3 in zeroWall_RF in ops_aux.f90'
         end select
       end subroutine

       subroutine zeroInterior_RF(f,s)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         f(2:s(1)-1,:,:) = 0.0_cp
         f(:,2:s(2)-1,:) = 0.0_cp
         f(:,:,2:s(3)-1) = 0.0_cp
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

!        subroutine perturbAll(f,m)
!          implicit none
!          real(cp),dimension(:,:,:),intent(inout) :: f
!          type(mesh),intent(in) :: m
!          real(cp),dimension(3) :: wavenum,eps
!          integer,dimension(3) :: s
!          integer :: i,j,k
!          s = shape(f)
!          wavenum = 0.1_cp
!          eps = 0.01_cp
!          if (all((/(s(i).eq.m%c(i)%sn,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(1)*sin(wavenum(1)*PI*m%c(1)%hn(i)) +&
!                                            eps(2)*sin(wavenum(2)*PI*m%c(2)%hn(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hn(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          elseif (all((/(s(i).eq.m%c(i)%sc,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(1)*sin(wavenum(1)*PI*m%c(1)%hc(i)) +&
!                                            eps(2)*sin(wavenum(2)*PI*m%c(2)%hc(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hc(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          else
!           stop 'Error: unmatched case in perturbAll in inductionSolver.f90'
!          endif
!        end subroutine

!        subroutine perturb(f,m)
!          implicit none
!          real(cp),dimension(:,:,:),intent(inout) :: f
!          type(mesh),intent(in) :: m
!          real(cp),dimension(3) :: wavenum,eps
!          integer,dimension(3) :: s
!          integer :: i,j,k
!          s = shape(f)
!          wavenum = 10.0_cp
!          eps = 0.1_cp
!          if (all((/(s(i).eq.m%c(i)%sn,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(2)*sin(wavenum(2)*PI*m%c(2)%hn(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hn(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          elseif (all((/(s(i).eq.m%c(i)%sc,i=1,3)/))) then
!            !$OMP PARALLEL DO
!            do k=1,s(3); do j=1,s(2); do i=1,s(1)
!              f(i,j,k) = f(i,j,k)*(1.0_cp + eps(2)*sin(wavenum(2)*PI*m%c(2)%hc(j)) +&
!                                            eps(3)*sin(wavenum(3)*PI*m%c(3)%hc(k)) )
!            enddo; enddo; enddo
!            !$OMP END PARALLEL DO
!          else
!           stop 'Error: unmatched case in perturb in inductionSolver.f90'
!          endif
!        end subroutine

      function dot_product_VF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: A,B,x
        type(VF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp%x) + sum(temp%y) + sum(temp%z)
      end function

      function dot_product_SF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: A,B,x
        type(SF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp)
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
         do i=1,m%s
           call zeroWall(f%RF(i)%f,f%RF(i)%s,1,1)
           call zeroWall(f%RF(i)%f,f%RF(i)%s,1,2)
           call zeroWall(f%RF(i)%f,f%RF(i)%s,2,3)
           call zeroWall(f%RF(i)%f,f%RF(i)%s,2,4)
           call zeroWall(f%RF(i)%f,f%RF(i)%s,3,5)
           call zeroWall(f%RF(i)%f,f%RF(i)%s,3,6)
         enddo
       end subroutine

       subroutine zeroWall_conditional_SF(f,m)
         ! Sets wall coincident values to zero if 
         ! boundary conditions of u are NOT Neumann (bctype=3)
         implicit none
         type(SF),intent(inout) :: f
         type(mesh),intent(in) :: m
         logical :: TF
         integer :: i
         do i=1,m%s
           TF = (.not.m%g(i)%st_face%hmin(1)).and.(Node_along(f,1)).and.(.not.f%RF(i)%b%f(1)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,1,1)
           TF = (.not.m%g(i)%st_face%hmax(1)).and.(Node_along(f,1)).and.(.not.f%RF(i)%b%f(2)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,1,2)

           TF = (.not.m%g(i)%st_face%hmin(2)).and.(Node_along(f,2)).and.(.not.f%RF(i)%b%f(3)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,2,3)
           TF = (.not.m%g(i)%st_face%hmax(2)).and.(Node_along(f,2)).and.(.not.f%RF(i)%b%f(4)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,2,4)

           TF = (.not.m%g(i)%st_face%hmin(3)).and.(Node_along(f,3)).and.(.not.f%RF(i)%b%f(5)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,3,5)
           TF = (.not.m%g(i)%st_face%hmax(3)).and.(Node_along(f,3)).and.(.not.f%RF(i)%b%f(6)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,3,6)
         enddo
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
         do i=1,m%s
           TF = (.not.m%g(i)%st_face%hmin(1)).and.(Node_along(f,1)).and.(.not.u%RF(i)%b%f(1)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,1,1)
           TF = (.not.m%g(i)%st_face%hmax(1)).and.(Node_along(f,1)).and.(.not.u%RF(i)%b%f(2)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,1,2)

           TF = (.not.m%g(i)%st_face%hmin(2)).and.(Node_along(f,2)).and.(.not.u%RF(i)%b%f(3)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,2,3)
           TF = (.not.m%g(i)%st_face%hmax(2)).and.(Node_along(f,2)).and.(.not.u%RF(i)%b%f(4)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,2,4)

           TF = (.not.m%g(i)%st_face%hmin(3)).and.(Node_along(f,3)).and.(.not.u%RF(i)%b%f(5)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,3,5)
           TF = (.not.m%g(i)%st_face%hmax(3)).and.(Node_along(f,3)).and.(.not.u%RF(i)%b%f(6)%b%Neumann)
           if (TF) call zeroWall(f%RF(i)%f,f%RF(i)%s,3,6)
         enddo
       end subroutine

       subroutine zeroInterior_SF(f)
         implicit none
         type(SF),intent(inout) :: f
         integer :: i
         do i=1,f%s; call zeroInterior(f%RF(i)%f,f%RF(i)%s); enddo
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
         integer :: i,j,k,t,m
         m = 1
         if (un.lt.1) stop 'Error: un must > 0 in unitVector_SF in ops_aux.f90'
         if (un.gt.U%numEl) stop 'Error: un must < U%numEl in unitVector_SF in ops_aux.f90'
         do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
         if (m.eq.un) then; U%RF(t)%f(i,j,k) = 1.0_cp
         else;              U%RF(t)%f(i,j,k) = 0.0_cp
         endif
         m = m + 1
         enddo; enddo; enddo; enddo
       end subroutine

       subroutine volume_VF(u,m)
         implicit none
         type(VF),intent(inout) :: u
         type(mesh),intent(in) :: m
         call volume(u%x,m); call volume(u%y,m); call volume(u%z,m)
       end subroutine

       subroutine stabilityTerms_SF(fo,fi,m,n,dir)
         implicit none
         type(SF),intent(inout) :: fo
         type(SF),intent(in) :: fi
         type(mesh),intent(in) :: m
         integer,intent(in) :: n,dir
         integer :: i
         do i=1,fi%s
           call assign(fo%RF(i),0.0_cp)
           call stabilityTerms(fo%RF(i)%f,fi%RF(i)%f,m%g(i),n,fi%RF(i)%s,dir)
           call zeroGhostPoints(fo%RF(i)%f,fo%RF(i)%s)
         enddo
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

       subroutine totalEnergy_VF_SD(e,f,D)
         implicit none
         type(VF),intent(in) :: f
         real(cp),intent(inout) :: e
         type(domain),intent(in) :: D
         type(VF) :: temp
         if (.not.f%x%is_CC) stop 'Error: Total energy must be computed on CC in totalEnergy_VF_SD in ops_aux.f90'
         call init_CC(temp,D%m_in)
         call extractCC(temp,f,D)
         call totalEnergy(e,temp,D%m_in)
         call delete(temp)
       end subroutine

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