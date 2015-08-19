       module ops_discrete_mod
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
       use SF_mod
       use VF_mod
       use ops_interp_mod
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

       public :: cross
       interface cross;     module procedure collocatedCrossReal;     end interface
       interface cross;     module procedure collocatedCrossVF;       end interface

       public :: lap
       interface lap;       module procedure lapUniformCoeffReal;     end interface
       interface lap;       module procedure lapVariableCoeff1Real;   end interface
       interface lap;       module procedure lapVariableCoeff2Real;   end interface
       interface lap;       module procedure lapUniformCoeffSF;       end interface
       interface lap;       module procedure lapUniformCoeffVF;       end interface
       interface lap;       module procedure lapVariableCoeff1VF;     end interface
       interface lap;       module procedure lapVariableCoeff2VF;     end interface

       public :: div
       interface div;       module procedure divReal;                 end interface
       interface div;       module procedure divVF;                   end interface

       public :: grad
       interface grad;      module procedure gradReal;                end interface
       interface grad;      module procedure gradVF;                  end interface

       public :: curl
       interface curl;      module procedure curlReal;                end interface
       interface curl;      module procedure curlVF;                  end interface

       public :: curlcurl
       interface curlcurl;  module procedure curlcurlCoeffVF;         end interface
       interface curlcurl;  module procedure curlcurlUniformVF;       end interface

       public :: CCVaryDel ! Needs to be removed eventually

       public :: mixed
       interface mixed;     module procedure mixed_uniformCoeffReal;  end interface
       interface mixed;     module procedure mixed_uniformCoeffVF;    end interface
       interface mixed;     module procedure mixed_variableCoeffReal; end interface
       interface mixed;     module procedure mixed_variableCoeffVF;   end interface

       contains

       subroutine collocatedCrossReal(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir)
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
           stop 'Error: dir must = 1,2,3 in crossReal.'
         end select
       end subroutine

       subroutine lapUniformCoeffReal(lapU,u,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(lapU,u,g,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine lapVariableCoeff1Real(lapU,u,kx,ky,kz,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u,kx,ky,kz
         type(grid),intent(in) :: g
         type(delVC) :: d
         call d%assign(lapU,u,kx,g,1,1)
            call d%add(lapU,u,ky,g,2,1)
            call d%add(lapU,u,kz,g,3,1)
       end subroutine

       subroutine lapVariableCoeff2Real(lapU,u,k,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u,k
         type(grid),intent(in) :: g
         type(delVC) :: d
         call d%assign(lapU,u,k,g,1,1)
            call d%add(lapU,u,k,g,2,1)
            call d%add(lapU,u,k,g,3,1)
       end subroutine

       subroutine divReal(divU,u,v,w,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(divU,u,g,1,1,0) ! Padding avoids calcs on fictive cells
            call d%add(divU,v,g,1,2,0) ! Padding avoids calcs on fictive cells
            call d%add(divU,w,g,1,3,0) ! Padding avoids calcs on fictive cells

         ! Note that padding above does not zero wall normal values
         ! from being calculated. Removing ghost nodes is still necessary:
         call zeroGhostPoints(divU) ! padding avoids boundaries
       end subroutine

       subroutine gradReal(gradx,grady,gradz,u,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: gradx,grady,gradz
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(gradx,u,g,1,1,1) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,g,1,2,1) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,g,1,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine curlReal(curlU,u,v,w,g,dir)
         ! curlReal computes curlU (component dir) of
         ! the collocated u,v,w field.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: curlU
         real(cp),dimension(:,:,:),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         type(del) :: d
         select case (dir)
         case (1); call d%assign(curlU,w,g,1,2,0)
                 call d%subtract(curlU,v,g,1,3,0)
         case (2); call d%assign(curlU,u,g,1,3,0)
                 call d%subtract(curlU,w,g,1,1,0)
         case (3); call d%assign(curlU,v,g,1,1,0)
                 call d%subtract(curlU,u,g,1,2,0)
         case default
           stop 'Error: dir must = 1,2,3 in curlReal.'
         end select
       end subroutine

       subroutine CCVaryDel(df,f,k,g,dir1,dir2)
         ! This is a depricated routine and needs to be deleted once the 
         ! new routines (mixed_uniformCoeff,mixed_variableCoeff) have been tested.
         ! 
         ! 
         ! Call laplacian if dir1=dir2.
         ! 
         ! The issue with this routine is that the mixed derivatives are ONLY
         ! for CC data, all of the other operators can do computations for N/CC and 
         ! operator specific data. This needs to be modified to operate on general data
         ! in a natural way. See the discrete operators.tex file for documentation.
         ! 
         ! 
         ! CCVaryDel computes
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
         type(grid),intent(in) :: g
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
           write(*,*) 'Error: dir1 must = 1,2,3 in CCVaryDel.'; stop
         end select

         if (dir1.eq.dir2) then
           allocate(temp1(s(1)+x,s(2)+y,s(3)+z))
           allocate(temp2(s(1)+x,s(2)+y,s(3)+z))
           call d%assign(temp1,f,g,1,dir1,0)
           call cellCenter2Face(temp2,k,g,dir1)
           temp1 = temp1*temp2
           call d%assign(temp2,temp1,g,1,dir2,0)
           df(1+x:s(1)-x,1+y:s(2)-y,1+z:s(3)-z) = temp2(1:s(1)-2*x,1:s(2)-2*y,1:s(3)-2*z)
           deallocate(temp1,temp2)
         else
           allocate(temp1(s(1),s(2),s(3)))
           allocate(temp2(s(1),s(2),s(3)))
           call d%assign(temp1,f,g,1,dir1,0)
           temp1 = temp1*k
           call d%assign(temp2,temp1,g,1,dir2,0)
           df = temp2
           deallocate(temp1,temp2)
         endif
       end subroutine

       subroutine mixed_uniformCoeffReal(mix,f,g,temp,dir1,dir2)
         ! Computes
         !     mix =  d/dxj (df/dxi)
         !               ^       ^
         !               |       |
         !              dir2    dir1
         !
         !     if dir1 == dir2  --> Error (call Laplacian instead)
         !     if dir1 ≠ dir2   --> see below
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: mix
         real(cp),dimension(:,:,:),intent(in) :: f
         real(cp),dimension(:,:,:),intent(inout) :: temp
         type(grid),intent(in) :: g
         integer,intent(in) :: dir1,dir2
         type(del) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_uniformCoeffReal in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,g,1,dir1,1)
         call d%assign(mix,temp,g,1,dir2,1)
       end subroutine

       subroutine mixed_variableCoeffReal(mix,f,k,g,temp,dir1,dir2)
         ! Computes
         !     mix =  d/dxj (k df/dxi)
         !               ^         ^
         !               |         |
         !              dir2      dir1
         !
         !     if dir1 == dir2  --> Error (call laplacian instead)
         !     if dir1 ≠ dir2   --> see below
         ! 
         ! NOTE: k must live in same domain as df/dxi
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: mix
         real(cp),dimension(:,:,:),intent(inout) :: temp
         real(cp),dimension(:,:,:),intent(in) :: f,k
         type(grid),intent(in) :: g
         integer,intent(in) :: dir1,dir2
         type(del) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_variableCoeffReal in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,g,1,dir1,1)
         temp = temp*k
         call d%assign(mix,temp,g,1,dir2,1)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* SCALAR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine lapUniformCoeffSF(lapU,u,g)
         implicit none
         type(SF),intent(inout) :: lapU
         real(cp),dimension(:,:,:),intent(in) :: u
         type(grid),intent(in) :: g
         call lap(lapU%phi,u,g)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedCrossVF(AcrossB,A,B)
         implicit none
         type(VF),intent(inout) :: AcrossB
         type(VF),intent(in) :: A,B
         call cross(AcrossB%x,A%x,A%y,A%z,B%x,B%y,B%z,1)
         call cross(AcrossB%y,A%x,A%y,A%z,B%x,B%y,B%z,2)
         call cross(AcrossB%z,A%x,A%y,A%z,B%x,B%y,B%z,3)
       end subroutine

       subroutine lapUniformCoeffVF(lapU,u,g)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,g)
         call lap(lapU%y,U%y,g)
         call lap(lapU%z,U%z,g)
       end subroutine

       subroutine lapVariableCoeff1VF(lapU,U,k,g)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: U,k
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,k%x,k%y,k%z,g)
         call lap(lapU%y,U%y,k%x,k%y,k%z,g)
         call lap(lapU%z,U%z,k%x,k%y,k%z,g)
       end subroutine

       subroutine lapVariableCoeff2VF(lapU,U,k,g)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: U
         type(SF),intent(in) :: k
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,k%phi,g)
         call lap(lapU%y,U%y,k%phi,g)
         call lap(lapU%z,U%z,k%phi,g)
       end subroutine

       subroutine divVF(divU,U,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: divU
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         call div(divU,U%x,U%y,U%z,g)
       end subroutine

       subroutine gradVF(gradU,U,g)
         implicit none
         type(VF),intent(inout) :: gradU
         real(cp),dimension(:,:,:),intent(in) :: U
         type(grid),intent(in) :: g
         call grad(gradU%x,gradU%y,gradU%z,U,g)
       end subroutine

       subroutine curlVF(curlU,U,g)
         implicit none
         type(VF),intent(inout) :: curlU
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         call curl(curlU%x,U%x,U%y,U%z,g,1)
         call curl(curlU%y,U%x,U%y,U%z,g,2)
         call curl(curlU%z,U%x,U%y,U%z,g,3)
       end subroutine

       subroutine curlcurlCoeffVF(curlcurlU,U,k,temp,g)
         ! Computes
         !     curl( k curl(U) )
         ! 
         ! NOTE: curl(U) will live at the same location as k
         ! 
         implicit none
         type(VF),intent(inout) :: curlcurlU
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: U,k
         type(grid),intent(in) :: g
         call curl(temp%x,U%x,U%y,U%z,g,1)
         call curl(temp%y,U%x,U%y,U%z,g,2)
         call curl(temp%z,U%x,U%y,U%z,g,3)
         call multiply(temp,k)
         call curl(curlcurlU%x,temp%x,temp%y,temp%z,g,1)
         call curl(curlcurlU%y,temp%x,temp%y,temp%z,g,2)
         call curl(curlcurlU%z,temp%x,temp%y,temp%z,g,3)
       end subroutine

       subroutine curlcurlUniformVF(curlcurlU,U,temp,g)
         ! Computes
         !     curl( curl(U) )
         ! 
         ! NOTE: curl(U) will live at the same location as k
         ! 
         implicit none
         type(VF),intent(inout) :: curlcurlU
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         call curl(temp%x,U%x,U%y,U%z,g,1)
         call curl(temp%y,U%x,U%y,U%z,g,2)
         call curl(temp%z,U%x,U%y,U%z,g,3)
         call curl(curlcurlU%x,temp%x,temp%y,temp%z,g,1)
         call curl(curlcurlU%y,temp%x,temp%y,temp%z,g,2)
         call curl(curlcurlU%z,temp%x,temp%y,temp%z,g,3)
       end subroutine

       subroutine mixed_uniformCoeffVF(mix,f,g,temp)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         real(cp),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: g
         call mixed(mix%x,f,g,temp%x,2,3)
         call mixed(mix%y,f,g,temp%y,1,3)
         call mixed(mix%z,f,g,temp%z,1,2)
       end subroutine

       subroutine mixed_variableCoeffVF(mix,f,k,g,temp)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: k
         real(cp),dimension(:,:,:),intent(in) :: f
         type(grid),intent(in) :: g
         call mixed(mix%x,f,k%y,g,temp%y,2,3) ! Shape of k must match dir1
         call mixed(mix%y,f,k%x,g,temp%x,1,3) ! Shape of k must match dir1
         call mixed(mix%z,f,k%x,g,temp%x,1,2) ! Shape of k must match dir1
         call multiply(temp,k)
       end subroutine

       end module