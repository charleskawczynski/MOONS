       module mesh_stencils_mod
       use current_precision_mod
       use mesh_mod
       use block_mod
       use VF_mod
       implicit none

       private
 
       public :: init_Laplacian_SF
       interface init_Laplacian_SF;        module procedure init_Laplacian_SF_mesh;    end interface
       interface init_Laplacian_SF;        module procedure init_Laplacian_SF_mesh_VP; end interface

       public :: init_Laplacian_VF
       interface init_Laplacian_VF;        module procedure init_Laplacian_VF_mesh;    end interface
       interface init_Laplacian_VF;        module procedure init_Laplacian_VF_mesh_VP; end interface

       public :: modify_Laplacian_SF
       public :: modify_Laplacian_VF
       interface modify_Laplacian_SF;      module procedure modify_Laplacian_SF_mesh;  end interface
       interface modify_Laplacian_VF;      module procedure modify_Laplacian_VF_mesh;  end interface

       public :: init_curl_curl
       interface init_curl_curl;           module procedure init_curl_curl_mesh;       end interface
       interface init_curl_curl;           module procedure init_curl_curl_mesh_VP;    end interface
 
       public :: modify_curl_curl
       interface modify_curl_curl;         module procedure modify_curl_curl_mesh;     end interface

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

       subroutine modify_Laplacian_SF_mesh(m,multiply_by,add_to)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: multiply_by,add_to
         integer :: i
         do i=1,m%s; call modify_Laplacian_SF(m%B(i),multiply_by,add_to); enddo
       end subroutine

       subroutine modify_Laplacian_VF_mesh(m,multiply_by,add_to)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: multiply_by,add_to
         integer :: i
         do i=1,m%s; call modify_Laplacian_VF(m%B(i),multiply_by,add_to); enddo
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

       subroutine modify_curl_curl_mesh(m,multiply_by,add_to)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: multiply_by,add_to
         integer :: i
         do i=1,m%s; call modify_curl_curl(m%B(i),multiply_by,add_to); enddo
       end subroutine

       end module