       module ops_discrete_local_mod
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
       use ops_del_local_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
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
       interface lap;       module procedure lapVarCoeff_SF;           end interface
       interface lap;       module procedure lapVarCoeff_VF;           end interface

       public :: div
       interface div;       module procedure div_SF;                   end interface
       interface div;       module procedure div_VF;                   end interface

       public :: grad
       interface grad;      module procedure grad_SF;                  end interface
       interface grad;      module procedure grad_VF;                  end interface

       public :: curl
       interface curl;      module procedure curl_SF;                  end interface
       interface curl;      module procedure curl_VF;                  end interface
       interface curl;      module procedure curl_TF;                  end interface

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

       subroutine collocatedCross_RF(AcrossB,Ax,Ay,Az,Bx,By,Bz,s,dir,i_3D,j_3D,k_3D)
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: AcrossB
         real(cp),dimension(:,:,:),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir
         integer,intent(in) :: i_3D,j_3D,k_3D
         integer :: i,j,k
         integer :: i_padl,i_padr,j_padl,j_padr,k_padl,k_padr

         i_padl = 3; i_padr = 3
         j_padl = 3; j_padr = 3
         k_padl = 3; k_padr = 3

         if (i_3D.eq.1) i_padl=0; if (i_3D.eq.s(1)) i_padr=0
         if (j_3D.eq.1) j_padl=0; if (j_3D.eq.s(2)) j_padr=0
         if (k_3D.eq.1) k_padl=0; if (k_3D.eq.s(3)) k_padr=0
        
         if (i_3D.eq.2) i_padl=1; if (i_3D.eq.s(1)-1) i_padr=1
         if (j_3D.eq.2) j_padl=1; if (j_3D.eq.s(2)-1) j_padr=1
         if (k_3D.eq.2) k_padl=1; if (k_3D.eq.s(3)-1) k_padr=1
        
         if (i_3D.eq.3) i_padl=2; if (i_3D.eq.s(1)-3) i_padr=2
         if (j_3D.eq.3) j_padl=2; if (j_3D.eq.s(2)-3) j_padr=2
         if (k_3D.eq.3) k_padl=2; if (k_3D.eq.s(3)-3) k_padr=2

         select case (dir)
         case (1)
           !$OMP PARALLEL DO
           do k=k_3D-k_padl,k_3D+k_padr; do j=j_3D-j_padl,j_3D+j_padr; do i=i_3D-i_padl,i_3D+i_padr
             AcrossB(i,j,k) = Ay(i,j,k)*Bz(i,j,k) - Az(i,j,k)*By(i,j,k)
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do k=k_3D-k_padl,k_3D+k_padr; do j=j_3D-j_padl,j_3D+j_padr; do i=i_3D-i_padl,i_3D+i_padr
             AcrossB(i,j,k) = -(Ax(i,j,k)*Bz(i,j,k) - Az(i,j,k)*Bx(i,j,k))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do k=k_3D-k_padl,k_3D+k_padr; do j=j_3D-j_padl,j_3D+j_padr; do i=i_3D-i_padl,i_3D+i_padr
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

       subroutine collocatedCross_SF(AcrossB,Ax,Ay,Az,Bx,By,Bz,dir,index_1D)
         ! This routine computes the ith component of A x B on a collocated mesh
         ! dir = (1,2,3)
         implicit none
         type(SF),intent(inout) :: AcrossB
         type(SF),intent(in) :: Ax,Ay,Az,Bx,By,Bz
         integer,intent(in) :: dir
         integer,intent(in) :: index_1D
         integer :: i
         integer :: i_3D,j_3D,k_3D,t_3D
         call get_3D_index(i_3D,j_3D,k_3D,t_3D,Ax,index_1D) ! This is probably not right...
         do i=1,t_3D
           call cross(AcrossB%RF(i)%f,&
           Ax%RF(i)%f,Ay%RF(i)%f,Az%RF(i)%f,&
           Bx%RF(i)%f,By%RF(i)%f,Bz%RF(i)%f,&
           Ax%RF(i)%s,dir,i_3D,j_3D,k_3D)
         enddo
       end subroutine

       subroutine lapUniformCoeff_SF(lapU,u,m,index_1D)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         type(del_local) :: d
         call d%assign(lapU,u,m,2,1,1,index_1D) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,m,2,2,1,index_1D) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,m,2,3,1,index_1D) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine lapVarCoeff_SF(lapU,u,k,m,temp,dir,index_1D)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u,k
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp
         integer,intent(in) :: dir
         integer,intent(in) :: index_1D
         type(del_local) :: d
         call d%assign(temp,u,m,1,dir,1,index_1D)
         call multiply(temp,k)
         call d%assign(lapU,temp,m,1,dir,1,index_1D)
       end subroutine

       subroutine div_SF(divU,u,v,w,m,index_1D)
         implicit none
         type(SF),intent(inout) :: divU
         type(SF),intent(in) :: u,v,w
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         type(del_local) :: d
         call d%assign(divU,u,m,1,1,0,index_1D) ! Padding avoids calcs on fictive cells
            call d%add(divU,v,m,1,2,0,index_1D) ! Padding avoids calcs on fictive cells
            call d%add(divU,w,m,1,3,0,index_1D) ! Padding avoids calcs on fictive cells

         ! Note that padding above does not zero wall normal values
         ! from being calculated. Removing ghost nodes is still necessary:
         call zeroGhostPoints(divU) ! padding avoids boundaries
       end subroutine

       subroutine grad_SF(gradx,grady,gradz,u,m,index_1D)
         implicit none
         type(SF),intent(inout) :: gradx,grady,gradz
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         type(del_local) :: d
         call d%assign(gradx,u,m,1,1,1,index_1D) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,m,1,2,1,index_1D) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,m,1,3,1,index_1D) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine curl_SF(curlU,u,v,w,m,dir,index_1D)
         ! curl_RF computes curlU (component dir) of
         ! the collocated u,v,w field.
         implicit none
         type(SF),intent(inout) :: curlU
         type(SF),intent(in) :: u,v,w
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer,intent(in) :: index_1D
         type(del_local) :: d
         select case (dir)
         case (1); call d%assign(curlU,w,m,1,2,0,index_1D)
                 call d%subtract(curlU,v,m,1,3,0,index_1D)
         case (2); call d%assign(curlU,u,m,1,3,0,index_1D)
                 call d%subtract(curlU,w,m,1,1,0,index_1D)
         case (3); call d%assign(curlU,v,m,1,1,0,index_1D)
                 call d%subtract(curlU,u,m,1,2,0,index_1D)
         case default
           stop 'Error: dir must = 1,2,3 in curl_RF.'
         end select
       end subroutine

       subroutine mixed_uniformCoeff_SF(mix,f,m,temp,dir1,dir2,index_1D)
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
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir1,dir2
         integer,intent(in) :: index_1D
         type(del_local) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_uniformCoeff_RF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,m,1,dir1,1,index_1D)
         call d%assign(mix,temp,m,1,dir2,1,index_1D)
       end subroutine

       subroutine mixed_variableCoeff_SF(mix,f,k,m,temp,dir1,dir2,index_1D)
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
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir1,dir2
         integer,intent(in) :: index_1D
         type(del_local) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_variableCoeff_RF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,m,1,dir1,1,index_1D)
         call multiply(temp,k)
         call d%assign(mix,temp,m,1,dir2,1,index_1D)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine collocatedCross_VF(AcrossB,A,B,index_1D)
         implicit none
         type(VF),intent(inout) :: AcrossB
         type(VF),intent(in) :: A,B
         integer,intent(in) :: index_1D
         call cross(AcrossB%x,A%x,A%y,A%z,B%x,B%y,B%z,1,index_1D)
         call cross(AcrossB%y,A%x,A%y,A%z,B%x,B%y,B%z,2,index_1D)
         call cross(AcrossB%z,A%x,A%y,A%z,B%x,B%y,B%z,3,index_1D)
       end subroutine

       subroutine lapUniformCoeff_VF(lapU,u,m,index_1D)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call lap(lapU%x,U%x,m,index_1D)
         call lap(lapU%y,U%y,m,index_1D)
         call lap(lapU%z,U%z,m,index_1D)
       end subroutine

       subroutine lapVarCoeff_VF(lapU,u,k,m,temp,index_1D)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(VF),intent(in) :: k
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp ! same shape as k
         integer,intent(in) :: index_1D
         call grad(temp,u,m,index_1D)
         call multiply(temp,k)
         call div(lapU,temp,m,index_1D)
       end subroutine

       subroutine div_VF(divU,U,m,index_1D)
         implicit none
         type(SF),intent(inout) :: divU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call div(divU,U%x,U%y,U%z,m,index_1D)
       end subroutine

       subroutine grad_VF(gradU,U,m,index_1D)
         implicit none
         type(VF),intent(inout) :: gradU
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call grad(gradU%x,gradU%y,gradU%z,U,m,index_1D)
       end subroutine

       subroutine curl_VF(curlU,U,m,index_1D)
         implicit none
         type(VF),intent(inout) :: curlU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call curl(curlU%x,U%x,U%y,U%z,m,1,index_1D)
         call curl(curlU%y,U%x,U%y,U%z,m,2,index_1D)
         call curl(curlU%z,U%x,U%y,U%z,m,3,index_1D)
       end subroutine

       subroutine curl_TF(curlU,U,m,index_1D)
         implicit none
         type(VF),intent(inout) :: curlU
         type(TF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call curl(curlU%x,U%x%x,U%y%z,U%z%y,m,1,index_1D) ! Note the diagonal input does not matter
         call curl(curlU%y,U%x%z,U%y%y,U%z%x,m,2,index_1D) ! Note the diagonal input does not matter
         call curl(curlU%z,U%x%y,U%y%x,U%z%z,m,3,index_1D) ! Note the diagonal input does not matter
       end subroutine

       subroutine curl_TF_version2(curlU,U,m,index_1D)
         implicit none
         type(VF),intent(inout) :: curlU
         type(TF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call curl(curlU%x,U%x%x,U%y%y,U%z%z,m,1,index_1D) ! Note the diagonal input does not matter
         call curl(curlU%y,U%x%x,U%y%y,U%z%z,m,2,index_1D) ! Note the diagonal input does not matter
         call curl(curlU%z,U%x%x,U%y%y,U%z%z,m,3,index_1D) ! Note the diagonal input does not matter
       end subroutine

       subroutine curlcurlCoeff_VF(curlcurlU,U,k,temp,m,index_1D)
         ! Computes
         !     curl( k curl(U) )
         ! 
         ! NOTE: curl(U) will live at the same location as k
         ! 
         implicit none
         type(VF),intent(inout) :: curlcurlU
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: U,k
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call curl(temp,U,m,index_1D)
         call multiply(temp,k)
         call curl(curlcurlU,temp,m,index_1D)
       end subroutine

       subroutine curlcurlUniform_VF(curlcurlU,U,temp,m,index_1D)
         ! Computes
         !     curl( curl(U) )
         ! 
         ! NOTE: curl(U) will live at the same location as k
         ! 
         implicit none
         type(VF),intent(inout) :: curlcurlU
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call curl(temp,U,m,index_1D)
         call curl(curlcurlU,temp,m,index_1D)
       end subroutine

       subroutine mixed_uniformCoeff_VF(mix,f,m,temp,index_1D)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(SF),intent(in) :: f
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call mixed(mix%x,f,m,temp%x,2,3,index_1D)
         call mixed(mix%y,f,m,temp%y,1,3,index_1D)
         call mixed(mix%z,f,m,temp%z,1,2,index_1D)
       end subroutine

       subroutine mixed_variableCoeff_VF(mix,f,k,m,temp,index_1D)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: k
         type(SF),intent(in) :: f
         type(mesh),intent(in) :: m
         integer,intent(in) :: index_1D
         call mixed(mix%x,f,k%y,m,temp%y,2,3,index_1D) ! Shape of k must match dir1
         call mixed(mix%y,f,k%x,m,temp%x,1,3,index_1D) ! Shape of k must match dir1
         call mixed(mix%z,f,k%x,m,temp%x,1,2,index_1D) ! Shape of k must match dir1
       end subroutine

       end module