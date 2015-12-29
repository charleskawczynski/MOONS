       module ops_discrete_implicit_mod
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
       use ops_del_implicit_mod
       use ops_discrete_mod
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

       public :: lap_imp
       interface lap_imp;                module procedure lapUniformCoeff_SF;        end interface
       interface lap_imp;                module procedure lapUniformCoeff_VF;        end interface
       interface lap_imp;                module procedure lapVarCoeff_SF;            end interface
       interface lap_imp;                module procedure lapVarCoeff_VF;            end interface

       public :: lap_centered_implicit
       interface lap_centered_implicit;  module procedure lap_centered_SF;           end interface
       interface lap_centered_implicit;  module procedure lap_centered_VF;           end interface

       public :: div_imp
       interface div_imp;                module procedure div_SF;                    end interface
       interface div_imp;                module procedure div_VF;                    end interface

       public :: grad_imp
       interface grad_imp;               module procedure grad_SF;                   end interface
       interface grad_imp;               module procedure grad_VF;                   end interface

       public :: curl_imp
       interface curl_imp;               module procedure curl_SF;                   end interface
       interface curl_imp;               module procedure curl_VF;                   end interface
       interface curl_imp;               module procedure curl_TF;                   end interface
       
       public :: curl_parallel_imp
       interface curl_parallel_imp;      module procedure curl_TF_Parallel;          end interface

       public :: curlcurl_imp
       interface curlcurl_imp;           module procedure curlcurlCoeff_VF;          end interface
       interface curlcurl_imp;           module procedure curlcurlUniform_VF;        end interface

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
         type(del_implicit) :: d
         call d%assign(lapU,u,m,2,1,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,m,2,2,1) ! Padding avoids calcs on fictive cells
            call d%add(lapU,u,m,2,3,1) ! Padding avoids calcs on fictive cells
       end subroutine

       subroutine lapVarCoeff_SF(lapU,u,k,m,temp,dir)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u,k
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: temp
         integer,intent(in) :: dir
         type(del_implicit) :: d
         call d%assign(temp,u,m,1,dir,1)
         call multiply(temp,k)
         call d%assign(lapU,temp,m,1,dir,1)
       end subroutine

       subroutine div_SF(divU,u,v,w,m)
         implicit none
         type(SF),intent(inout) :: divU
         type(SF),intent(in) :: u,v,w
         type(mesh),intent(in) :: m
         type(del_implicit) :: d
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
         type(del_implicit) :: d
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
         type(del_implicit) :: d
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
         call lap_imp(lapU%x,U%x,m)
         call lap_imp(lapU%y,U%y,m)
         call lap_imp(lapU%z,U%z,m)
       end subroutine

       subroutine lapVarCoeff_VF(lapU,u,k,m,temp)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(VF),intent(in) :: k
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp ! same shape as k
         call grad_imp(temp,u,m)
         call multiply(temp,k)
         call div(lapU,temp,m)
       end subroutine

       subroutine lap_centered_SF(lapU,u,m,dir)
         implicit none
         type(SF),intent(inout) :: lapU
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         type(SF) :: temp
         type(del_implicit) :: di
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
         call di%assign(temp,u,m,1,dir,1)
         call d%assign(lapU,temp,m,1,dir,1)
       end subroutine

       subroutine lap_centered_VF(lapU,u,m)
         implicit none
         type(VF),intent(inout) :: lapU
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         call lap_centered_implicit(lapU%x,u%x,m,1)
         call lap_centered_implicit(lapU%y,u%y,m,2)
         call lap_centered_implicit(lapU%z,u%z,m,3)
       end subroutine

       subroutine div_VF(divU,U,m)
         implicit none
         type(SF),intent(inout) :: divU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call div_imp(divU,U%x,U%y,U%z,m)
       end subroutine

       subroutine grad_VF(gradU,U,m)
         implicit none
         type(VF),intent(inout) :: gradU
         type(SF),intent(in) :: U
         type(mesh),intent(in) :: m
         call grad_imp(gradU%x,gradU%y,gradU%z,U,m)
       end subroutine

       subroutine curl_VF(curlU,U,m)
         implicit none
         type(VF),intent(inout) :: curlU
         type(VF),intent(in) :: U
         type(mesh),intent(in) :: m
         call curl_imp(curlU%x,U%x,U%y,U%z,m,1)
         call curl_imp(curlU%y,U%x,U%y,U%z,m,2)
         call curl_imp(curlU%z,U%x,U%y,U%z,m,3)
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
         call curl_imp(curlU%x,U%x%x,U%y%z,U%z%y,m,1) ! Note the diagonal input does not matter
         call curl_imp(curlU%y,U%x%z,U%y%y,U%z%x,m,2) ! Note the diagonal input does not matter
         call curl_imp(curlU%z,U%x%y,U%y%x,U%z%z,m,3) ! Note the diagonal input does not matter
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
         call curl_imp(curlU%x,U%x%x,U%y%y,U%z%z,m,1) ! Note the diagonal input does not matter
         call curl_imp(curlU%y,U%x%x,U%y%y,U%z%z,m,2) ! Note the diagonal input does not matter
         call curl_imp(curlU%z,U%x%x,U%y%y,U%z%z,m,3) ! Note the diagonal input does not matter
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
         call curl_imp(temp,U,m)
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
         call curl_imp(temp,U,m)
         call curl(curlcurlU,temp,m)
       end subroutine

       end module