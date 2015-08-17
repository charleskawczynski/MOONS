       module ops_discrete_MG_mod
       ! This module uses the discrete operators module and
       ! multigrid operators to make multigrid discrete operators
       ! 
       ! To start, only the curlcurl operator will be developed,
       ! since this is the one of interest right now.
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

       public :: curlcurl
       interface curlcurl;  module procedure curlcurlCoeffVF          end interface
       interface curlcurl;  module procedure curlcurlUniformVF        end interface

       contains

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

       end module