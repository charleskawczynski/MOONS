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
       use ops_del_mod
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
       interface cross;           module procedure collocatedCross_RF;        end interface
       interface cross;           module procedure collocatedCross_SF;        end interface
       interface cross;           module procedure collocatedCross_VF;        end interface
       interface cross;           module procedure collocatedCross_TF;        end interface

       public :: lap
       interface lap;             module procedure lapUniformCoeff_SF;        end interface
       interface lap;             module procedure lapUniformCoeff_VF;        end interface
       interface lap;             module procedure lapVarCoeff_SF;            end interface
       interface lap;             module procedure lapVarCoeff_VF;            end interface

       public :: lap_centered
       interface lap_centered;    module procedure lap_centered_SF;           end interface
       interface lap_centered;    module procedure lap_centered_VF;           end interface

       public :: div
       interface div;             module procedure div_SF;                    end interface
       interface div;             module procedure div_VF;                    end interface

       public :: grad
       interface grad;            module procedure grad_SF;                   end interface
       interface grad;            module procedure grad_VF;                   end interface
       interface grad;            module procedure grad_TF;                   end interface

       public :: divGrad
       interface divGrad;         module procedure divGrad_VF;                end interface

       public :: curl
       interface curl;            module procedure curl_SF;                   end interface
       interface curl;            module procedure curl_VF;                   end interface
       interface curl;            module procedure curl_TF;                   end interface
       
       public :: curl_parallel
       interface curl_parallel;   module procedure curl_TF_Parallel;          end interface

       public :: curlcurl
       interface curlcurl;        module procedure curlcurlCoeff_VF;          end interface
       interface curlcurl;        module procedure curlcurlUniform_VF;        end interface

       public :: mixed
       interface mixed;           module procedure mixed_uniformCoeff_SF;     end interface
       interface mixed;           module procedure mixed_uniformCoeff_VF;     end interface
       interface mixed;           module procedure mixed_variableCoeff_SF;    end interface
       interface mixed;           module procedure mixed_variableCoeff_VF;    end interface

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

       subroutine lapUniformCoeff_SF(lapU,u,m)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         type(del) :: d
         call d%assign(lapU,u,m,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,m,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,m,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine lap_centered_SF(lapU,u,m,dir,addTo)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         logical,intent(in) :: addTo
         type(SF) :: temp
         type(del) :: d
         if (u%is_Face) then
         select case (u%face)
         case (1); select case (dir)
                   case (1); call init_CC(temp,m)
                   case (2); call init_Edge(temp,m,3)
                   case (3); call init_Edge(temp,m,2)
                   case default; stop 'Error: dir must = 1,2,3 in lap_centered_SF in ops_discrete.f90'
                   end select
         case (2); select case (dir)
                   case (1); call init_Edge(temp,m,3)
                   case (2); call init_CC(temp,m)
                   case (3); call init_Edge(temp,m,1)
                   case default; stop 'Error: dir must = 1,2,3 in lap_centered_SF in ops_discrete.f90'
                   end select
         case (3); select case (dir)
                   case (1); call init_Edge(temp,m,2)
                   case (2); call init_Edge(temp,m,1)
                   case (3); call init_CC(temp,m)
                   case default; stop 'Error: dir must = 1,2,3 in lap_centered_SF in ops_discrete.f90'
                   end select
         case default; stop 'Error: Bad case in lap_centered_SF in ops_discrete.f90'
         end select
         else; stop 'Error: non-face input to lap_centered in ops_discrete.f90'
         endif
         call d%assign(temp,u,m,1,dir,1)
         if (addTo) then; call d%add(lapU,temp,m,1,dir,1)
         else;            call d%assign(lapU,temp,m,1,dir,1)
         endif
         call delete(temp)
       end subroutine

       subroutine lap_centered_VF(lapU,u,m)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         call lap_centered(lapU%x,u%x,m,1,.false.)
         call lap_centered(lapU%x,u%x,m,2,.true.)
         call lap_centered(lapU%x,u%x,m,3,.true.)

         call lap_centered(lapU%y,u%y,m,1,.false.)
         call lap_centered(lapU%y,u%y,m,2,.true.)
         call lap_centered(lapU%y,u%y,m,3,.true.)

         call lap_centered(lapU%z,u%z,m,1,.false.)
         call lap_centered(lapU%z,u%z,m,2,.true.)
         call lap_centered(lapU%z,u%z,m,3,.true.)
       end subroutine

       subroutine lapVarCoeff_SF(lapU,u,k,m,temp,dir)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u,k
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp
         integer,intent(in) :: dir
         type(del) :: d
         call d%assign(temp,u,m,1,dir,1)
         call multiply(temp,k)
         call d%assign(lapU,temp,m,1,dir,1)
       end subroutine

       subroutine div_SF(divU,u,v,w,m)
         implicit none
         type(SF),intent(inout) :: divU
         type(SF),intent(in) :: u,v,w
         type(mesh),intent(in) :: m
         type(del) :: d
         call d%assign(divU,u,m,1,1,0) ! Padding avoids calcs on fictive cells
            call d%add(divU,v,m,1,2,0) ! Padding avoids calcs on fictive cells
            call d%add(divU,w,m,1,3,0) ! Padding avoids calcs on fictive cells

         ! Note that padding above does not zero wall normal values
         ! from being calculated. Removing ghost nodes is still necessary:
         call zeroGhostPoints(divU) ! padding avoids boundaries
       end subroutine

       subroutine grad_SF(gradx,grady,gradz,u,m)
         implicit none
         type(SF),intent(inout) :: gradx,grady,gradz
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         type(del) :: d
         call d%assign(gradx,u,m,1,1,1) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,m,1,2,1) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,m,1,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine curl_SF(curlU,u,v,w,m,dir)
         ! curl_RF computes curlU (component dir) of
         ! the collocated u,v,w field.
         implicit none
         type(SF),intent(inout) :: curlU
         type(SF),intent(in) :: u,v,w
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         type(del) :: d
         select case (dir)
         case (1); call d%assign(curlU,w,m,1,2,0)
                 call d%subtract(curlU,v,m,1,3,0)
         case (2); call d%assign(curlU,u,m,1,3,0)
                 call d%subtract(curlU,w,m,1,1,0)
         case (3); call d%assign(curlU,v,m,1,1,0)
                 call d%subtract(curlU,u,m,1,2,0)
         case default
           stop 'Error: dir must = 1,2,3 in curl_RF.'
         end select
       end subroutine

       subroutine mixed_uniformCoeff_SF(mix,f,m,temp,dir1,dir2)
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
         type(del) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_uniformCoeff_RF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,m,1,dir1,1)
         call d%assign(mix,temp,m,1,dir2,1)
       end subroutine

       subroutine mixed_variableCoeff_SF(mix,f,k,m,temp,dir1,dir2)
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
         type(del) :: d
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_variableCoeff_RF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
         call d%assign(temp,f,m,1,dir1,1)
         call multiply(temp,k)
         call d%assign(mix,temp,m,1,dir2,1)
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

       subroutine collocatedCross_TF(AcrossB,A,B)
         ! First index refers to vector direction.
         ! Second index refers to vector location.
         !      For example, in A%x%y, the direction of the vector
         !      will be in x, and it will be located on whatever 
         !      location is defined by the y-component (y-face for face data
         !      or y-edge for edge data).
         ! Since this is a collocated operation, the second
         ! index should be the same (data should be collocated).
         implicit none
         type(VF),intent(inout) :: AcrossB
         type(TF),intent(in) :: A,B
         call cross(AcrossB%x,A%x%x,A%y%x,A%z%x,B%x%x,B%y%x,B%z%x,1)
         call cross(AcrossB%y,A%x%y,A%y%y,A%z%y,B%x%y,B%y%y,B%z%y,2)
         call cross(AcrossB%z,A%x%z,A%y%z,A%z%z,B%x%z,B%y%z,B%z%z,3)
       end subroutine

       subroutine lapUniformCoeff_VF(lapU,u,m)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         call lap(lapU%x,U%x,m)
         call lap(lapU%y,U%y,m)
         call lap(lapU%z,U%z,m)
       end subroutine

       subroutine lapVarCoeff_VF(lapU,u,k,m,temp)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(VF),intent(in) :: k
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp ! same shape as k
         call grad(temp,u,m)
         call multiply(temp,k)
         call div(lapU,temp,m)
       end subroutine

       subroutine div_VF(divU,U,m)
         implicit none
         type(SF),intent(inout) :: divU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call div(divU,U%x,U%y,U%z,m)
       end subroutine

       subroutine grad_VF(gradU,U,m)
         implicit none
         type(VF),intent(inout) :: gradU
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         call grad(gradU%x,gradU%y,gradU%z,U,m)
       end subroutine

       subroutine grad_TF(gradU,U,m)
         implicit none
         type(TF),intent(inout) :: gradU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call grad(gradU%x,U%x,m)
         call grad(gradU%y,U%y,m)
         call grad(gradU%z,U%z,m)
       end subroutine

       subroutine divGrad_VF(divGradU,U,m,temp_TF)
         ! This routine achieves consecutive staggered derivatives
         ! to compute lap(U). If U lives on the cell face, then
         ! 
         !            _          _
         !           |  CC E  E   |
         ! temp_TF = |  E  CC E   |
         !           |_ E  E  CC _|
         ! 
         implicit none
         type(VF),intent(inout) :: divGradU
         type(VF),intent(in) :: U
         type(TF),intent(inout) :: temp_TF
         type(mesh),intent(in) :: m
         call grad(temp_TF,U,m)
         call div(divGradU%x,temp_TF%x,m)
         call div(divGradU%y,temp_TF%y,m)
         call div(divGradU%z,temp_TF%z,m)
       end subroutine

       subroutine curl_VF(curlU,U,m)
         implicit none
         type(VF),intent(inout) :: curlU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call curl(curlU%x,U%x,U%y,U%z,m,1)
         call curl(curlU%y,U%x,U%y,U%z,m,2)
         call curl(curlU%z,U%x,U%y,U%z,m,3)
       end subroutine

       subroutine curl_TF_Parallel(curlU,U,m)
         ! For a staggered implementation, this curl performs:
         !           U  = {CC}
         !      curl(U) = {F}
         ! Or
         !           U  = {F}
         !      curl(U) = {CC}
         ! In other words, the curl lands PARALLEL TO
         ! the plane of the cell face
         implicit none
         type(VF),intent(inout) :: curlU
         type(TF),intent(in) :: U
         type(mesh),intent(in) :: m
         call curl(curlU%x,U%x%x,U%y%z,U%z%y,m,1) ! Note the diagonal input does not matter
         call curl(curlU%y,U%x%z,U%y%y,U%z%x,m,2) ! Note the diagonal input does not matter
         call curl(curlU%z,U%x%y,U%y%x,U%z%z,m,3) ! Note the diagonal input does not matter
       end subroutine

       subroutine curl_TF(curlU,U,m)
         ! For a staggered implementation, this curl performs:
         !           U  = {E}
         !      curl(U) = {F}
         ! Or
         !           U  = {F}
         !      curl(U) = {E}
         ! In other words, the curl lands NORMAL TO
         ! the plane of the cell face
         implicit none
         type(VF),intent(inout) :: curlU
         type(TF),intent(in) :: U
         type(mesh),intent(in) :: m
         call curl(curlU%x,U%x%x,U%y%y,U%z%z,m,1) ! Note the diagonal input does not matter
         call curl(curlU%y,U%x%x,U%y%y,U%z%z,m,2) ! Note the diagonal input does not matter
         call curl(curlU%z,U%x%x,U%y%y,U%z%z,m,3) ! Note the diagonal input does not matter
       end subroutine

       subroutine curlcurlCoeff_VF(curlcurlU,U,k,temp,m)
         ! Computes
         !     ∇x(k∇x)
         ! 
         ! NOTE: curl(U) will live at the same location as k
         ! 
         implicit none
         type(VF),intent(inout) :: curlcurlU
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: U,k
         type(mesh),intent(in) :: m
         call curl(temp,U,m)
         call multiply(temp,k)
         call curl(curlcurlU,temp,m)
       end subroutine

       subroutine curlcurlUniform_VF(curlcurlU,U,temp,m)
         ! Computes
         !     ∇x∇x
         ! 
         ! NOTE: curl(U) will live at the same location as k
         ! 
         implicit none
         type(VF),intent(inout) :: curlcurlU
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call curl(temp,U,m)
         call curl(curlcurlU,temp,m)
       end subroutine

       subroutine mixed_uniformCoeff_VF(mix,f,m,temp)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(SF),intent(in) :: f
         type(mesh),intent(in) :: m
         call mixed(mix%x,f,m,temp%x,2,3)
         call mixed(mix%y,f,m,temp%y,1,3)
         call mixed(mix%z,f,m,temp%z,1,2)
       end subroutine

       subroutine mixed_variableCoeff_VF(mix,f,k,m,temp)
         implicit none
         type(VF),intent(inout) :: mix
         type(VF),intent(inout) :: temp
         type(VF),intent(in) :: k
         type(SF),intent(in) :: f
         type(mesh),intent(in) :: m
         call mixed(mix%x,f,k%y,m,temp%y,2,3) ! Shape of k must match dir1
         call mixed(mix%y,f,k%x,m,temp%x,1,3) ! Shape of k must match dir1
         call mixed(mix%z,f,k%x,m,temp%x,1,2) ! Shape of k must match dir1
       end subroutine

       end module