       module delOps_mod
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
       use myError_mod
       use interpOps_mod
       use IO_scalarFields_mod

       implicit none

       ! Compiler Flags (_PARALLELIZE_DELOPS_,
       !                 _DEBUG_DELOPS_,
       !                 _CHECK_SYMMETRY_DELOPS_)

       private

#ifdef _CHECK_SYMMETRY_
       integer,parameter :: symmetryPlane = 2
       public :: symmetryPlane
#endif

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

       public :: zeroGhostPoints
       interface zeroGhostPoints;         module procedure zeroGhostPointsSF;       end interface

       public :: totalEnergy
       interface totalEnergy;             module procedure totalEnergySF;           end interface

       public :: collocatedMagnitude
       interface collocatedMagnitude;     module procedure collocatedMagnitudeSF;   end interface

       public :: printPhysicalMinMax
       interface printPhysicalMinMax;     module procedure printPhysicalMinMaxSF;   end interface

       ! public :: checkSymmetry

       ! --------------------------------- DERIVATIVE ROUTINES ---------------------------------
       public :: lap
       interface lap;                     module procedure lapUniformCoeffSF;       end interface
       interface lap;                     module procedure lapVariableCoeffSF;      end interface

       public :: div
       interface div;                     module procedure divSF;                   end interface

       public :: grad
       interface grad;                    module procedure gradSF;                  end interface

       public :: curl
       interface curl;                    module procedure curlSF;                  end interface

       public :: cross
       interface cross;                   module procedure crossSF;                 end interface

       ! Special Cell-center based derivatives
       public :: CC2EdgeCurl
       public :: myCCVaryDel

       ! ----------------------------- SPECIAL TERMS -------------------------------------------
       public :: myFaceAdvect
       public :: myFaceAdvectDonor
       public :: myFaceAdvectHybrid

       public :: myCCBfieldAdvect
       public :: myCCBfieldDiffuse

       contains

       ! ******************************* OTHER ROUTINES *********************************

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
         integer :: i,j,k,pad
         integer,dimension(3) :: s
         s = shape(x)
         pad = 15
         eTemp = real(0.0,cp) ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(g), REDUCTION(+:eTemp)
         do k=2,s(3)-1; do j=2,s(2)-1; do i=2+pad,s(1)-1-pad
           eTemp = eTemp + (x(i,j,k)**real(2.0,cp) +&
                            y(i,j,k)**real(2.0,cp) +&
                            z(i,j,k)**real(2.0,cp))*g%c(1)%dhn(i)*&
                                                    g%c(2)%dhn(j)*&
                                                    g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = etemp/g%volume

         ! eTemp = real(0.0,cp) ! temp is necessary for reduction
         ! !$OMP PARALLEL DO SHARED(g), REDUCTION(+:eTemp)
         ! do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
         !   eTemp = eTemp + (x(i,j,k)**real(2.0,cp) +&
         !                    y(i,j,k)**real(2.0,cp) +&
         !                    z(i,j,k)**real(2.0,cp))*g%c(1)%dhn(i)*&
         !                                            g%c(2)%dhn(j)*&
         !                                            g%c(3)%dhn(k)
         ! enddo; enddo; enddo
         ! !$OMP END PARALLEL DO
         ! e = etemp/g%volume
       end subroutine

       ! ******************************* HELPER ROUTINES ********************************

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

       ! ******************************* DEBUGGING ROUTINES ******************************

       subroutine checkSymmetry(u,plane,name,g)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: u
         character(len=*),intent(in) :: name
         integer,intent(in) :: plane
         type(grid),intent(in) :: g
         type(myError) :: e
         real(cp),dimension(:,:,:),allocatable :: u_left,u_right,u_symm
         integer,dimension(3) :: s,N
         real(cp) :: tol
         integer :: p,x,y,z,k
         tol = real(10.0**(-3.0),cp)
         s = shape(u); p = plane

         select case (p)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default; stop 'Error: plane must = 1,2,3 in checkSymmetry in delOps.f90'
         end select
         k = mod(s(p),2)
         N = s
         N(p) = (N(p)-k)/2
         allocate(u_left(N(1),N(2),N(3)))
         allocate(u_right(N(1),N(2),N(3)))
         allocate(u_symm(N(1),N(2),N(3)))

          select case (p)
          case (1); u_left = u(1:N(p),:,:); u_right = u(s(p):N(p)+1+k:-1,:,:)
          case (2); u_left = u(:,1:N(p),:); u_right = u(:,s(p):N(p)+1+k:-1,:)
          case (3); u_left = u(:,:,1:N(p)); u_right = u(:,:,s(p):N(p)+1+k:-1)
          end select

         u_symm = abs(abs(u_left) - abs(u_right))
         call compute(e,real(0.0,cp),u_symm)

         if (getLinf(e).gt.tol) then
         ! if (.true.) then
           call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,u,'out/','test')
           select case (p)
           case (1); call writeToFile(g%c(1)%hc(1:N(p)),g%c(2)%hc,g%c(3)%hc,u_left,'out/','u_left')
           case (2); call writeToFile(g%c(1)%hc,g%c(2)%hc(1:N(p)),g%c(3)%hc,u_left,'out/','u_left')
           case (3); call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc(1:N(p)),u_left,'out/','u_left')
           end select
           select case (p)
           case (1); call writeToFile(g%c(1)%hc(s(p):N(p)+1+k:-1),g%c(2)%hc,g%c(3)%hc,u_right,'out/','u_right')
           case (2); call writeToFile(g%c(1)%hc,g%c(2)%hc(s(p):N(p)+1+k:-1),g%c(3)%hc,u_right,'out/','u_right')
           case (3); call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc(s(p):N(p)+1+k:-1),u_right,'out/','u_right')
           end select
           select case (p)
           case (1); call writeToFile(g%c(1)%hc(1:N(p)),g%c(2)%hc,g%c(3)%hc,u_symm,'out/','u_symm')
           case (2); call writeToFile(g%c(1)%hc,g%c(2)%hc(1:N(p)),g%c(3)%hc,u_symm,'out/','u_symm')
           case (3); call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc(1:N(p)),u_symm,'out/','u_symm')
           end select

           write(*,*) 'Case = ',mod(s(p),2)
           write(*,*) 'shape(u) = ',shape(u)
           write(*,*) 'shape(u_symm) = ',shape(u_symm)
           write(*,*) 'shape(u_symm) = ',shape(u_symm)

           write(*,*) 'maxval(u_symm) = ',maxval(abs(u_symm))
           write(*,*) 'maxloc(u_symm) = ',maxloc(abs(u_symm))

           write(*,*) 'L1 = ',getL1(e)
           write(*,*) 'L2 = ',getL2(e)
           write(*,*) 'Linf = ',getLinf(e)
           write(*,*) 'Symmetry is broken from above field = ',name
           stop 'Done'
         else
           ! write(*,*) 'Good symmetry in ',name
           ! write(*,*) 'maxval(u_left) = ',maxval(abs(u_left))
         endif
         deallocate(u_right,u_left,u_symm)
       end subroutine
        
       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ***************************** DATA GENERAL DERIVATIVES *********************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine crossSF(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir) ! Finished
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
          do k=1,s(3); do j=1,s(2); do i=1,s(1)
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
           stop 'Error: dir must = 1,2,3 in collocatedCrossSF.'
         end select
       end subroutine

       subroutine lapUniformCoeffSF(lapU,u,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(lapU,u,g,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine lapVariableCoeffSF(lapU,u,k,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u,k
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),allocatable :: temp1
         integer,dimension(3) :: s
         s = shape(lapU)
         allocate(temp1(s(1),s(2),s(3)))
         call myCCVaryDel(temp1,u,k,g,1,1)
         lapU = temp1
         call myCCVaryDel(temp1,u,k,g,2,2)
         lapU = lapU+temp1
         call myCCVaryDel(temp1,u,k,g,3,3)
         lapU = lapU+temp1
         deallocate(temp1)
       end subroutine

       subroutine divSF(divU,u,v,w,g) ! Finished
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         type(del) :: d
         integer,dimension(3) :: s
         call d%assign(divU,u,g,1,1,1) ! Padding avoids calcs on fictive cells
            call d%add(divU,v,g,1,2,1) ! Padding avoids calcs on fictive cells
            call d%add(divU,w,g,1,3,1) ! Padding avoids calcs on fictive cells
         call zeroGhostPoints(divU)
       end subroutine

       subroutine gradSF(gradx,grady,gradz,u,g) ! Finsihed
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(gradx,u,g,1,1,1) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,g,1,2,1) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,g,1,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine curlSF(curlU,u,v,w,gd,dir) ! Finished, needs to be checked
         ! collocatedCurl computes curlU (component dir) of
         ! the collocated u,v,w field.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curlU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: gd
         integer,intent(in) :: dir
         type(del) :: d
         select case (dir)
         case (1); call d%assign(curlU,w,gd,1,2,0)
                 call d%subtract(curlU,v,gd,1,3,0)
         case (2); call d%assign(curlU,u,gd,1,3,0)
                 call d%subtract(curlU,w,gd,1,1,0)
         case (3); call d%assign(curlU,v,gd,1,1,0)
                 call d%subtract(curlU,u,gd,1,2,0)
         case default
           stop 'Error: dir must = 1,2,3 in myCCCurl.'
         end select
       end subroutine

       ! ****************************************************************************************
       ! ****************************************************************************************
       ! ************************************ SPECIAL TERMS *************************************
       ! ****************************************************************************************
       ! ****************************************************************************************

       subroutine CC2EdgeCurl(curl,u,v,w,gd,dir) ! Finished, needs to be checked
         ! myCCCurl computes the component dir of the curl of (u,v,w). The
         ! result lives on the edge. 
         ! 
         ! This routine essentially interpolates the data from
         ! the CC to F then performs a face2EdgeCurl.
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

       ! ******************************* FACE BASED DERIVATIVES *********************************

       subroutine myFaceAdvect(psi,u,v,w,phi,g,dir) ! Finished
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

       subroutine myFaceAdvectDonor(psi,u,v,w,ui,g,faceDir) ! Finished, maybe improvements
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

       subroutine myFaceAdvectHybrid(psi,u,v,w,ui,g,faceDir) ! Finished, maybe improvements
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
           write(*,*) 'Error: faceDir must = 1,2,3 in myFaceAdvectHybrid.'; stop
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

       subroutine myCCBfieldAdvect(div,u,v,w,Bx,By,Bz,g,dir) ! Finished
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

       ! subroutine myCCBfieldDiffuse(div,Bx,By,Bz,sigmaInv,g,dir) ! Finished, needs testing
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
       !   type(grid),intent(in) :: g
       !   integer,intent(in) :: dir
       !   div = real(0.0,cp)
       !   ! -------------------------- d/dx [ sigmaInv ( d/dxi (Bx/mu) - d/dx (Bi/mu) ) ]
       !   select case (dir)
       !   case (2)
       !   call d%add(div,Bx,sigmaInv,g,dir,1)
       !   call d%subtract(div,By,sigmaInv,g,1,1)
       !   case (3)
       !   call d%add(div,Bx,sigmaInv,g,dir,1)
       !   call d%subtract(div,Bz,sigmaInv,g,1,1)
       !   end select
       !   ! -------------------------- d/dy [ sigmaInv ( d/dxi (By/mu) - d/dy (Bi/mu) ) ]
       !   select case (dir)
       !   case (1)
       !   call d%add(div,By,sigmaInv,g,dir,2)
       !   call d%subtract(div,Bx,sigmaInv,g,2,2)
       !   case (3)
       !   call d%add(div,By,sigmaInv,g,dir,2)
       !   call d%subtract(div,Bz,sigmaInv,g,2,2)
       !   end select
       !   ! -------------------------- d/dz [ sigmaInv ( d/dxi (Bz/mu) - d/dz (Bi/mu) ) ]
       !   select case (dir)
       !   case (1)
       !   call d%add(div,Bz,sigmaInv,g,dir,3)
       !   call d%subtract(div,Bx,sigmaInv,g,3,3)
       !   case (2)
       !   call d%add(div,Bz,sigmaInv,g,dir,3)
       !   call d%subtract(div,By,sigmaInv,g,3,3)
       !   end select
       ! end subroutine

       subroutine myCCBfieldDiffuse(div,Bx,By,Bz,sigmaInv,g,dir) ! Finished, needs testing
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
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         real(cp),dimension(:,:,:),allocatable :: temp1
         integer,dimension(3) :: s
         s = shape(div)
         div = real(0.0,cp)
         allocate(temp1(s(1),s(2),s(3)))
         ! -------------------------- d/dx [ sigmaInv ( d/dxi (Bx/mu) - d/dx (Bi/mu) ) ]
         select case (dir)
         case (2)
         call myCCVaryDel(temp1,Bx,sigmaInv,g,dir,1); div=div+temp1
         call myCCVaryDel(temp1,By,sigmaInv,g,1,1); div=div-temp1
         case (3)
         call myCCVaryDel(temp1,Bx,sigmaInv,g,dir,1); div=div+temp1
         call myCCVaryDel(temp1,Bz,sigmaInv,g,1,1); div=div-temp1
         end select
         ! -------------------------- d/dy [ sigmaInv ( d/dxi (By/mu) - d/dy (Bi/mu) ) ]
         select case (dir)
         case (1)
         call myCCVaryDel(temp1,By,sigmaInv,g,dir,2); div=div+temp1
         call myCCVaryDel(temp1,Bx,sigmaInv,g,2,2); div=div-temp1
         case (3)
         call myCCVaryDel(temp1,By,sigmaInv,g,dir,2); div=div+temp1
         call myCCVaryDel(temp1,Bz,sigmaInv,g,2,2); div=div-temp1
         end select
         ! -------------------------- d/dz [ sigmaInv ( d/dxi (Bz/mu) - d/dz (Bi/mu) ) ]
         select case (dir)
         case (1)
         call myCCVaryDel(temp1,Bz,sigmaInv,g,dir,3); div=div+temp1
         call myCCVaryDel(temp1,Bx,sigmaInv,g,3,3); div=div-temp1
         case (2)
         call myCCVaryDel(temp1,Bz,sigmaInv,g,dir,3); div=div+temp1
         call myCCVaryDel(temp1,By,sigmaInv,g,3,3); div=div-temp1
         end select

         deallocate(temp1)
       end subroutine

       end module