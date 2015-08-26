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
       interface cross;     module procedure collocatedCross_RF;       end interface
       interface cross;     module procedure collocatedCross_SF;       end interface
       interface cross;     module procedure collocatedCross_VF;       end interface

       public :: lap
       interface lap;       module procedure lapUniformCoeff_SF;       end interface
       interface lap;       module procedure lapUniformCoeff_VF;       end interface

       public :: div
       interface div;       module procedure div_SF;                   end interface
       interface div;       module procedure div_VF;                   end interface

       public :: grad
       interface grad;      module procedure grad_SF;                  end interface
       interface grad;      module procedure grad_VF;                  end interface

       public :: curl
       interface curl;      module procedure curl_SF;                  end interface
       interface curl;      module procedure curl_VF;                  end interface

       public :: curlcurl
       interface curlcurl;  module procedure curlcurlCoeff_VF;         end interface
       interface curlcurl;  module procedure curlcurlUniform_VF;       end interface

       public :: mixed
       interface mixed;     module procedure mixed_uniformCoeff_SF;    end interface
       interface mixed;     module procedure mixed_uniformCoeff_VF;    end interface
       interface mixed;     module procedure mixed_variableCoeff_SF;   end interface
       interface mixed;     module procedure mixed_variableCoeff_VF;   end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ***************************** REAL FIELD ROUTINES *******************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedCross_RF(AcrossB,Ax,Ay,Az,Bx,By,Bz,s,dir)
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: AcrossB
         real(cp),dimension(:,:,:),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir
         integer :: i,j,k
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
           stop 'Error: dir must = 1,2,3 in cross_RF.'
         end select
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! *************************** SCALAR FIELD ROUTINES *******************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedCross_SF(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir)
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         type(SF),intent(inout) :: AcrossB
         type(SF),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,intent(in) :: dir
         integer :: i
         do i=1,AcrossB%s
           call cross(AcrossB%RF(i)%f,&
           Ax%RF(i)%f,Ay%RF(i)%f,Az%RF(i)%f,&
           Bx%RF(i)%f,By%RF(i)%f,Bz%RF(i)%f,&
           Ax%RF(i)%s,dir)
         enddo
       end subroutine

       subroutine lapUniformCoeff_SF(lapU,u,g)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(lapU,u,g,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,g,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine div_SF(divU,u,v,w,g)
         implicit none
         type(SF),intent(inout) :: divU
         type(SF),intent(in) :: u,v,w
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(divU,u,g,1,1,0) ! Padding avoids calcs on fictive cells
            call d%add(divU,v,g,1,2,0) ! Padding avoids calcs on fictive cells
            call d%add(divU,w,g,1,3,0) ! Padding avoids calcs on fictive cells

         ! Note that padding above does not zero wall normal values
         ! from being calculated. Removing ghost nodes is still necessary:
         call zeroGhostPoints(divU) ! padding avoids boundaries
       end subroutine

       subroutine grad_SF(gradx,grady,gradz,u,g)
         implicit none
         type(SF),intent(inout) :: gradx,grady,gradz
         type(SF),intent(in) :: u
         type(grid),intent(in) :: g
         type(del) :: d
         call d%assign(gradx,u,g,1,1,1) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,g,1,2,1) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,g,1,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine curl_SF(curlU,u,v,w,g,dir)
         ! curl_RF computes curlU (component dir) of
         ! the collocated u,v,w field.
         implicit none
         type(SF),intent(inout) :: curlU
         type(SF),intent(in) :: u,v,w
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
           stop 'Error: dir must = 1,2,3 in curl_RF.'
         end select
       end subroutine

       subroutine mixed_uniformCoeff_SF(mix,f,g,temp,dir1,dir2)
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
         type(SF),intent(inout) :: mix,temp
         type(SF),intent(in) :: f
         type(grid),intent(in) :: g
         integer,intent(in) :: dir1,dir2
         type(del) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_uniformCoeff_RF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,g,1,dir1,1)
         call d%assign(mix,temp,g,1,dir2,1)
       end subroutine

       subroutine mixed_variableCoeff_SF(mix,f,k,g,temp,dir1,dir2)
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
         type(SF),intent(inout) :: mix,temp
         type(SF),intent(in) :: f,k
         type(grid),intent(in) :: g
         integer,intent(in) :: dir1,dir2
         type(del) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_variableCoeff_RF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,g,1,dir1,1)
         call multiply(temp,k)
         call d%assign(mix,temp,g,1,dir2,1)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedCross_VF(AcrossB,A,B)
         implicit none
         type(VF),intent(inout) :: AcrossB
         type(VF),intent(in) :: A,B
         call cross(AcrossB%x,A%x,A%y,A%z,B%x,B%y,B%z,1)
         call cross(AcrossB%y,A%x,A%y,A%z,B%x,B%y,B%z,2)
         call cross(AcrossB%z,A%x,A%y,A%z,B%x,B%y,B%z,3)
       end subroutine

       subroutine lapUniformCoeff_VF(lapU,u,g)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(grid),intent(in) :: g
         call lap(lapU%x,U%x,g)
         call lap(lapU%y,U%y,g)
         call lap(lapU%z,U%z,g)
       end subroutine

       subroutine div_VF(divU,U,g)
         implicit none
         type(SF),intent(inout) :: divU
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         call div(divU,U%x,U%y,U%z,g)
       end subroutine

       subroutine grad_VF(gradU,U,g)
         implicit none
         type(VF),intent(inout) :: gradU
         type(SF),intent(in) :: U
         type(grid),intent(in) :: g
         call grad(gradU%x,gradU%y,gradU%z,U,g)
       end subroutine

       subroutine curl_VF(curlU,U,g)
         implicit none
         type(VF),intent(inout) :: curlU
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         call curl(curlU%x,U%x,U%y,U%z,g,1)
         call curl(curlU%y,U%x,U%y,U%z,g,2)
         call curl(curlU%z,U%x,U%y,U%z,g,3)
       end subroutine

       subroutine curlcurlCoeff_VF(curlcurlU,U,k,temp,g)
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

       subroutine curlcurlUniform_VF(curlcurlU,U,temp,g)
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

       subroutine mixed_uniformCoeff_VF(mix,f,g,temp)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(SF),intent(in) :: f
         type(grid),intent(in) :: g
         call mixed(mix%x,f,g,temp%x,2,3)
         call mixed(mix%y,f,g,temp%y,1,3)
         call mixed(mix%z,f,g,temp%z,1,2)
       end subroutine

       subroutine mixed_variableCoeff_VF(mix,f,k,g,temp)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: k
         type(SF),intent(in) :: f
         type(grid),intent(in) :: g
         call mixed(mix%x,f,k%y,g,temp%y,2,3) ! Shape of k must match dir1
         call mixed(mix%y,f,k%x,g,temp%x,1,3) ! Shape of k must match dir1
         call mixed(mix%z,f,k%x,g,temp%x,1,2) ! Shape of k must match dir1
       end subroutine

       end module