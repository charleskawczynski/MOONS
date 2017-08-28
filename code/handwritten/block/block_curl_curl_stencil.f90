       module block_curl_curl_stencil_mod
       use current_precision_mod
       use grid_mod
       use GF_mod
       use stencil_1D_mod
       use stencil_3D_mod
       use data_location_mod
       implicit none

       private
       public :: init_curl_curl
       interface init_curl_curl;     module procedure init_curl_curl_U;      end interface
       interface init_curl_curl;     module procedure init_curl_curl_VP;     end interface

       contains

       subroutine init_curl_curl_U(curl_curlX,curl_curlY,curl_curlZ,g)
         implicit none
         type(stencil_3D),dimension(3),intent(inout) :: curl_curlX,curl_curlY,curl_curlZ
         type(grid),intent(in) :: g
         integer :: i
         do i=1,3; call init(curl_curlX(i),g,DL_Face(1)); enddo
         do i=1,3; call init(curl_curlY(i),g,DL_Face(2)); enddo
         do i=1,3; call init(curl_curlZ(i),g,DL_Face(3)); enddo
         ! Term-component = result-component:
         call assign_consecutive(curl_curlX(1)%S(2))
         call assign_consecutive(curl_curlX(1)%S(3))
         call multiply(curl_curlX(1),-1.0_cp)
         call add_diagonals(curl_curlX(1))
         call assign_consecutive(curl_curlY(2)%S(1))
         call assign_consecutive(curl_curlY(2)%S(3))
         call multiply(curl_curlY(2),-1.0_cp)
         call add_diagonals(curl_curlY(2))
         call assign_consecutive(curl_curlZ(3)%S(1))
         call assign_consecutive(curl_curlZ(3)%S(2))
         call multiply(curl_curlZ(3),-1.0_cp)
         call add_diagonals(curl_curlZ(3))
         ! X-component result:
         call assign_mixed(curl_curlY(1),(/1,2/))
         call assign_mixed(curl_curlZ(1),(/1,3/))
         ! Y-component result:
         call assign_mixed(curl_curlX(2),(/2,1/))
         call assign_mixed(curl_curlZ(2),(/2,3/))
         ! Z-component result:
         call assign_mixed(curl_curlX(3),(/3,1/))
         call assign_mixed(curl_curlY(3),(/3,2/))
         ! Deallocate unecessary data for run-time:
         call clean(curl_curlX(1))
         call clean(curl_curlY(2))
         call clean(curl_curlZ(3))
       end subroutine

       subroutine init_curl_curl_VP(curl_curlX,curl_curlY,curl_curlZ,g,sig)
         implicit none
         type(stencil_3D),dimension(3),intent(inout) :: curl_curlX,curl_curlY,curl_curlZ
         type(grid),intent(in) :: g
         type(grid_field),dimension(3),intent(in) :: sig
         integer :: i
         do i=1,3; call init(curl_curlX(i),g,DL_Face(i)); enddo
         do i=1,3; call init(curl_curlY(i),g,DL_Face(i)); enddo
         do i=1,3; call init(curl_curlZ(i),g,DL_Face(i)); enddo
         ! Term-component = result-component:
         call assign_consecutive(curl_curlX(1)%S(2),sig)
         call assign_consecutive(curl_curlX(1)%S(3),sig)
         call multiply(curl_curlX(1),-1.0_cp)
         call add_diagonals(curl_curlX(1))
         call assign_consecutive(curl_curlY(2)%S(1),sig)
         call assign_consecutive(curl_curlY(2)%S(3),sig)
         call multiply(curl_curlY(2),-1.0_cp)
         call add_diagonals(curl_curlY(2))
         call assign_consecutive(curl_curlZ(3)%S(1),sig)
         call assign_consecutive(curl_curlZ(3)%S(2),sig)
         call multiply(curl_curlZ(3),-1.0_cp)
         call add_diagonals(curl_curlZ(3))

         ! X-component result:
         call assign_mixed(curl_curlY(1),(/1,2/),sig)
         call assign_mixed(curl_curlZ(1),(/1,3/),sig)
         ! Y-component result:
         call assign_mixed(curl_curlX(2),(/2,1/),sig)
         call assign_mixed(curl_curlZ(2),(/2,3/),sig)
         ! Z-component result:
         call assign_mixed(curl_curlX(3),(/3,1/),sig)
         call assign_mixed(curl_curlY(3),(/3,2/),sig)

         ! Deallocate unecessary data for run-time:
         call clean(curl_curlX(1))
         call clean(curl_curlY(2))
         call clean(curl_curlZ(3))
       end subroutine

       end module