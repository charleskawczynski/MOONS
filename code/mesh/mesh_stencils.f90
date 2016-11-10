       module mesh_stencils_mod
       use mesh_mod
       use block_mod
       use VF_mod
       implicit none

       private
       public :: init_curl_curl
       public :: init_Laplacian

       interface init_Laplacian; module procedure init_Laplacian_mesh;    end interface
       interface init_Laplacian; module procedure init_Laplacian_mesh_VP; end interface
       interface init_curl_curl; module procedure init_curl_curl_mesh;    end interface
       interface init_curl_curl; module procedure init_curl_curl_mesh_VP; end interface

       contains

       subroutine init_Laplacian_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         do i=1,m%s; call init_Laplacian(m%B(i)); enddo
       end subroutine

       subroutine init_Laplacian_mesh_VP(m,sig)
         implicit none
         type(mesh),intent(inout) :: m
         type(VF),intent(in) :: sig
         integer :: i
         do i=1,m%s; call init_Laplacian(m%B(i),(/sig%x%BF(i)%GF,sig%y%BF(i)%GF,sig%z%BF(i)%GF/)); enddo
       end subroutine

       subroutine init_curl_curl_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         do i=1,m%s; call init_curl_curl(m%B(i)); enddo
       end subroutine

       subroutine init_curl_curl_mesh_VP(m,sig,sig_F)
         implicit none
         type(mesh),intent(inout) :: m
         type(VF),intent(in) :: sig,sig_F
         integer :: i
         do i=1,m%s; call init_curl_curl(m%B(i),&
          (/sig%x%BF(i)%GF,sig%y%BF(i)%GF,sig%z%BF(i)%GF/),&
          (/sig_F%x%BF(i)%GF,sig_F%y%BF(i)%GF,sig_F%z%BF(i)%GF/)); enddo
       end subroutine

       end module