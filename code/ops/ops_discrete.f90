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
       use current_precision_mod
       use ops_del_mod
       use mesh_mod
       use data_location_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_interp_mod
       use ops_aux_mod

       implicit none

       private
       public :: lap
       interface lap;             module procedure lapUniformCoeff_SF;        end interface
       interface lap;             module procedure lapUniformCoeff_VF;        end interface
       interface lap;             module procedure lapVarCoeff_SF;            end interface
       interface lap;             module procedure lapVarCoeff_VF;            end interface

       public :: lap_component
       interface lap_component;   module procedure lap_component_SF;          end interface
       interface lap_component;   module procedure lap_component_VF;          end interface

       public :: lap_centered
       interface lap_centered;    module procedure lap_centered_SF_dynamic;   end interface
       interface lap_centered;    module procedure lap_centered_VF_dynamic;   end interface

       public :: div
       interface div;             module procedure div_SF;                    end interface
       interface div;             module procedure div_VF;                    end interface

       public :: grad
       interface grad;            module procedure grad_SF;                   end interface
       interface grad;            module procedure grad_VF;                   end interface
       interface grad;            module procedure grad_TF;                   end interface

       public :: grad_component
       interface grad_component;  module procedure grad_component_SF;         end interface
       interface grad_component;  module procedure grad_component_VF;         end interface

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
       ! *************************** SCALAR FIELD ROUTINES *******************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine lapUniformCoeff_SF(lapU,u,m)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         type(del) :: d
         call d%assign(lapU,u,m,2,1,0)
            call d%add(lapU,u,m,2,2,0)
            call d%add(lapU,u,m,2,3,0)
       end subroutine

       subroutine lap_component_SF(lapU,u,m,dir)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         type(del) :: d
         call d%assign(lapU,u,m,2,dir,0)
       end subroutine

       subroutine lap_component_VF(lapU,u,m)
         implicit none
         type(VF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         call lap_component(lapU%x,u,m,1)
         call lap_component(lapU%y,u,m,2)
         call lap_component(lapU%z,u,m,3)
       end subroutine

       subroutine lap_centered_SF_given_both(lapU,U,m,tempx,tempy,tempz)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: tempx,tempy,tempz
         type(del) :: d
         call d%assign(tempx,U,m,1,1,0); call d%assign(lapU,tempx,m,1,1,0)
         call d%assign(tempy,U,m,1,2,0); call d%add   (lapU,tempy,m,1,2,0)
         call d%assign(tempz,U,m,1,3,0); call d%add   (lapU,tempz,m,1,3,0)
       end subroutine

       subroutine lap_centered_VF_dynamic(lapU,U,m)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(TF) :: TF_temp
         type(VF) :: VF_temp
         if     (is_CC(U)) then
           call init_Face(VF_temp,m)
           call lap_centered_SF_given_both(lapU%x,U%x,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call lap_centered_SF_given_both(lapU%y,U%y,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call lap_centered_SF_given_both(lapU%z,U%z,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call delete(VF_temp)
         elseif (is_Node(U)) then
           call init_Edge(VF_temp,m)
           call lap_centered_SF_given_both(lapU%x,U%x,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call lap_centered_SF_given_both(lapU%y,U%y,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call lap_centered_SF_given_both(lapU%z,U%z,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call delete(VF_temp)
         elseif (is_Face(U)) then
           call init_Face_compliment(TF_temp,m)
           call lap_centered_SF_given_both(lapU%x,U%x,m,TF_temp%x%x,TF_temp%x%y,TF_temp%x%z)
           call lap_centered_SF_given_both(lapU%y,U%y,m,TF_temp%y%x,TF_temp%y%y,TF_temp%y%z)
           call lap_centered_SF_given_both(lapU%z,U%z,m,TF_temp%z%x,TF_temp%z%y,TF_temp%z%z)
           call delete(TF_temp)
         elseif (is_Edge(U)) then
           call init_Edge_compliment(TF_temp,m)
           call lap_centered_SF_given_both(lapU%x,U%x,m,TF_temp%x%x,TF_temp%x%y,TF_temp%x%z)
           call lap_centered_SF_given_both(lapU%y,U%y,m,TF_temp%y%x,TF_temp%y%y,TF_temp%y%z)
           call lap_centered_SF_given_both(lapU%z,U%z,m,TF_temp%z%x,TF_temp%z%y,TF_temp%z%z)
           call delete(TF_temp)
         else; stop 'Error: bad data type in lap_centered_VF_dynamic in ops_discrete.f90'
         endif
       end subroutine

       subroutine lap_centered_SF_dynamic(lapU,U,m)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         type(TF) :: TF_temp
         type(VF) :: VF_temp
         if     (is_CC(U%DL)) then
           call init_Face(VF_temp,m)
           call lap_centered_SF_given_both(lapU,U,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call delete(VF_temp)
         elseif (is_Node(U%DL)) then
           call init_Edge(VF_temp,m)
           call lap_centered_SF_given_both(lapU,U,m,VF_temp%x,VF_temp%y,VF_temp%z)
           call delete(VF_temp)
         elseif (is_Face(U%DL)) then
           call init_Face_compliment(TF_temp,m)
           select case (get_Face(U%DL))
           case (1); call lap_centered_SF_given_both(lapU,U,m,TF_temp%x%x,TF_temp%x%y,TF_temp%x%z)
           case (2); call lap_centered_SF_given_both(lapU,U,m,TF_temp%y%x,TF_temp%y%y,TF_temp%y%z)
           case (3); call lap_centered_SF_given_both(lapU,U,m,TF_temp%z%x,TF_temp%z%y,TF_temp%z%z)
           end select
           call delete(TF_temp)
         elseif (is_Edge(U%DL)) then
           call init_Edge_compliment(TF_temp,m)
           select case (get_Edge(U%DL))
           case (1); call lap_centered_SF_given_both(lapU,U,m,TF_temp%x%x,TF_temp%x%y,TF_temp%x%z)
           case (2); call lap_centered_SF_given_both(lapU,U,m,TF_temp%y%x,TF_temp%y%y,TF_temp%y%z)
           case (3); call lap_centered_SF_given_both(lapU,U,m,TF_temp%z%x,TF_temp%z%y,TF_temp%z%z)
           end select
           call delete(TF_temp)
         else; stop 'Error: bad data type in lap_centered_VF_dynamic in ops_discrete.f90'
         endif
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
         call assign_ghost_XPeriodic(divU,0.0_cp) ! padding avoids boundaries
       end subroutine

       subroutine grad_SF(gradx,grady,gradz,u,m)
         implicit none
         type(SF),intent(inout) :: gradx,grady,gradz
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         type(del) :: d
         call d%assign(gradx,u,m,1,1,0) ! Padding avoids calcs on fictive cells
         call d%assign(grady,u,m,1,2,0) ! Padding avoids calcs on fictive cells
         call d%assign(gradz,u,m,1,3,0) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine grad_component_SF(grad,u,m,dir)
         implicit none
         type(SF),intent(inout) :: grad
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         type(del) :: d
         call d%assign(grad,u,m,1,dir,0)
       end subroutine

       subroutine grad_component_VF(grad,u,m)
         implicit none
         type(VF),intent(inout) :: grad
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         call grad_component(grad%x,u%x,m,1)
         call grad_component(grad%y,u%y,m,2)
         call grad_component(grad%z,u%z,m,3)
       end subroutine

       subroutine curl_SF(curlU,u,v,w,m,dir)
         ! curl_GF computes curlU (component dir) of
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
         case default; stop 'Error: dir must = 1,2,3 in curl_SF in ops_discrete.f90'
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
#if _DEBUG_DISCRETE_OPS_
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_uniformCoeff_GF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
#endif
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
#if _DEBUG_DISCRETE_OPS_
         if (dir1.eq.dir2) then
           write(*,*) 'Error: dir1=dir2 in mixed_variableCoeff_GF in ops_discrete.f90'
           stop 'Call laplacian operator instead.'
         endif
#endif
         call d%assign(temp,f,m,1,dir1,1)
         call multiply(temp,k)
         call d%assign(mix,temp,m,1,dir2,1)
       end subroutine

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine lapUniformCoeff_VF(lapU,u,m)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         call lap(lapU%x,U%x,m)
         call lap(lapU%y,U%y,m)
         call lap(lapU%z,U%z,m)
       end subroutine

       subroutine lapVarCoeff_VF(lapU,u,k,m,tempk)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(VF),intent(in) :: k
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: tempk
         call grad(tempk,u,m)
         call multiply(tempk,k)
         call div(lapU,tempk,m)
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