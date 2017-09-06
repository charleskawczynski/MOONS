       module mesh_stencils_mod
       use current_precision_mod
       use mesh_extend_mod
       use block_extend_mod
       use SF_mod
       use VF_mod
       use stencil_3D_mod
       implicit none

       private

       public :: init_Laplacian_SF
       interface init_Laplacian_SF;        module procedure init_Laplacian_SF_mesh;    end interface
       interface init_Laplacian_SF;        module procedure init_Laplacian_SF_mesh_VP; end interface

       public :: init_Laplacian_VF
       interface init_Laplacian_VF;        module procedure init_Laplacian_VF_mesh;    end interface
       interface init_Laplacian_VF;        module procedure init_Laplacian_VF_mesh_VP; end interface

       public :: add_Laplacian_SF
       public :: add_Laplacian_VF
       public :: multiply_Laplacian_SF
       public :: multiply_Laplacian_VF
       interface add_Laplacian_SF;         module procedure add_Laplacian_SF_mesh;     end interface
       interface add_Laplacian_VF;         module procedure add_Laplacian_VF_mesh;     end interface
       interface multiply_Laplacian_SF;    module procedure multiply_Laplacian_SF_mesh;end interface
       interface multiply_Laplacian_VF;    module procedure multiply_Laplacian_VF_mesh;end interface

       public :: init_curl_curl
       interface init_curl_curl;           module procedure init_curl_curl_mesh;       end interface
       interface init_curl_curl;           module procedure init_curl_curl_mesh_VP;    end interface

       public :: add_curl_curl
       public :: multiply_curl_curl
       interface add_curl_curl;            module procedure add_curl_curl_mesh;        end interface
       interface multiply_curl_curl;       module procedure multiply_curl_curl_mesh;   end interface

       contains

       ! *******************************************************************
       ! **************************** LAPLACIAN ****************************
       ! *******************************************************************

       subroutine init_Laplacian_SF_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         do i=1,m%s; call init_Laplacian_SF(m%B(i)); enddo
       end subroutine

       subroutine init_Laplacian_SF_mesh_VP(m,sig)
         implicit none
         type(mesh),intent(inout) :: m
         type(VF),intent(in) :: sig
         integer :: i
         do i=1,m%s; call init_Laplacian_SF(m%B(i),(/sig%x%BF(i)%GF,sig%y%BF(i)%GF,sig%z%BF(i)%GF/)); enddo
       end subroutine

       subroutine init_Laplacian_VF_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         do i=1,m%s; call init_Laplacian_VF(m%B(i)); enddo
       end subroutine

       subroutine init_Laplacian_VF_mesh_VP(m,sig)
         implicit none
         type(mesh),intent(inout) :: m
         type(VF),intent(in) :: sig
         integer :: i
         do i=1,m%s; call init_Laplacian_VF(m%B(i),(/sig%x%BF(i)%GF,sig%y%BF(i)%GF,sig%z%BF(i)%GF/)); enddo
       end subroutine

       subroutine add_Laplacian_SF_mesh(m,val)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: val
         integer :: i
         do i=1,m%s; call add_to_diag(m%B(i)%lap_SF,val); enddo
       end subroutine

       subroutine add_Laplacian_VF_mesh(m,val)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: val
         integer :: i,j
         do j=1,3; do i=1,m%s; call add_to_diag(m%B(i)%lap_VF(j),val); enddo; enddo
       end subroutine

       subroutine multiply_Laplacian_SF_mesh(m,val)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: val
         integer :: i
         do i=1,m%s; call multiply(m%B(i)%lap_SF,val); enddo
       end subroutine

       subroutine multiply_Laplacian_VF_mesh(m,val)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: val
         integer :: i,j
         do j=1,3; do i=1,m%s; call multiply(m%B(i)%lap_VF(j),val); enddo; enddo
       end subroutine

       ! *******************************************************************
       ! **************************** CURL-CURL ****************************
       ! *******************************************************************

       subroutine init_curl_curl_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         do i=1,m%s; call init_curl_curl(m%B(i)); enddo
       end subroutine

       subroutine init_curl_curl_mesh_VP(m,sig)
         implicit none
         type(mesh),intent(inout) :: m
         type(VF),intent(in) :: sig
         integer :: i
         do i=1,m%s; call init_curl_curl(m%B(i),(/sig%x%BF(i)%GF,sig%y%BF(i)%GF,sig%z%BF(i)%GF/)); enddo
       end subroutine

       subroutine add_curl_curl_mesh(m,val)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: val
         integer :: i
         do i=1,m%s; call add_to_diag(m%B(i)%curl_curlX(1),val); enddo
         do i=1,m%s; call add_to_diag(m%B(i)%curl_curlY(2),val); enddo
         do i=1,m%s; call add_to_diag(m%B(i)%curl_curlZ(3),val); enddo
       end subroutine

       subroutine multiply_curl_curl_mesh(m,val)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: val
         integer :: i,j
         do j=1,3; do i=1,m%s; call multiply(m%B(i)%curl_curlX(j),val); enddo; enddo
         do j=1,3; do i=1,m%s; call multiply(m%B(i)%curl_curlY(j),val); enddo; enddo
         do j=1,3; do i=1,m%s; call multiply(m%B(i)%curl_curlZ(j),val); enddo; enddo
       end subroutine

       end module