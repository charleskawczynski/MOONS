      module index_mapping_mod
      use current_precision_mod
      use IO_tools_mod
      use mesh_extend_mod
      use SF_extend_mod
      use VF_extend_mod
      implicit none
      private

      ! Initialization / Deletion (allocate/deallocate)
      public :: get_3D_index
      public :: get_1D_index
      public :: get_val

      interface get_3D_index;        module procedure get_3D_index_SF;        end interface
      interface get_3D_index;        module procedure get_3D_index_VF;        end interface
      interface get_3D_index;        module procedure get_3D_index_m;         end interface
      interface get_1D_index;        module procedure get_1D_index_SF;        end interface

      interface get_val;             module procedure get_val_SF;             end interface
      interface get_val;             module procedure get_val_VF;             end interface

      contains

      ! ****************************************************************
      ! *************************** INDEXES ****************************
      ! ****************************************************************

      function get_1D_index_SF(i,j,k,t,U) result(index_1D)
        ! Notes:
        !     For i=1,j=1,k=1 we have
        !     m = 1 + im*(0 + 0) = 1
        !     For i=im,j=jm,k=km we have
        !     m = im + im*((jm-1) + jm*(km-1))
        !       = im + im*jm - im + im*jm*(km-1)
        !       =      im*jm      + im*jm*km-im*jm
        !       =                 + im*jm*km
        !     Which should equal
        !     m = im*jm*km
        implicit none
        type(SF),intent(in) :: U
        integer,intent(in) :: i,j,k,t
        integer :: im,jm,km
        real(cp) :: index_1D
        if (U%s.gt.1) stop 'Error: get_1D_index_SF not developed for U%s>1 in SF.f90'
        if (t.gt.1) stop 'Error: get_1D_index_SF not developed for t>1 in SF.f90'
        im = U%BF(1)%GF%s(1)
        jm = U%BF(1)%GF%s(2)
        km = U%BF(1)%GF%s(3)
        index_1D = i + im*( (j-1) + jm*(k-1) )
      end function

      subroutine get_3D_index_m(i_3D,j_3D,k_3D,t_3D,m,index_1D)
        implicit none
        integer,intent(inout) :: i_3D,j_3D,k_3D,t_3D
        type(mesh),intent(in) :: m
        integer,intent(in) :: index_1D
        integer :: im,jm,km
        if (m%s.gt.1) stop 'Error: get_3D_index_m not developed for U%s>1 in SF.f90'
        im = m%B(1)%g%c(1)%N
        jm = m%B(1)%g%c(2)%N
        km = m%B(1)%g%c(3)%N
        t_3D = 1
        k_3D = (index_1D-1)/(im*jm)+1
        j_3D = ((index_1D-1) - ((k_3D-1)*im*jm))/im+1
        i_3D = index_1D - (j_3D-1)*im - (k_3D-1)*im*jm
      end subroutine

      subroutine get_3D_index_SF(i_3D,j_3D,k_3D,t_3D,U,index_1D)
        implicit none
        integer,intent(inout) :: i_3D,j_3D,k_3D,t_3D
        type(SF),intent(in) :: U
        integer,intent(in) :: index_1D
        integer :: im,jm,km
        if (U%s.gt.1) stop 'Error: get_3D_index_SF not developed for U%s>1 in SF.f90'
        im = U%BF(1)%GF%s(1)
        jm = U%BF(1)%GF%s(2)
        km = U%BF(1)%GF%s(3)
        t_3D = 1
        k_3D = (index_1D-1)/(im*jm)+1
        j_3D = ((index_1D-1) - ((k_3D-1)*im*jm))/im+1
        i_3D = index_1D - (j_3D-1)*im - (k_3D-1)*im*jm
      end subroutine

      subroutine get_3D_index_VF(i_3D,j_3D,k_3D,t_3D,U,index_1D)
        implicit none
        integer,intent(inout) :: i_3D,j_3D,k_3D,t_3D
        type(VF),intent(in) :: U
        integer,intent(in) :: index_1D
        integer :: im,jm,km
        if (U%x%s.gt.1) stop 'Error: get_3D_index_VF not developed for U%s>1 in SF.f90'
        im = U%x%BF(1)%GF%s(1)
        jm = U%x%BF(1)%GF%s(2)
        km = U%x%BF(1)%GF%s(3)
        t_3D = 1
        k_3D = (index_1D-1)/(im*jm)+1
        j_3D = ((index_1D-1) - ((k_3D-1)*im*jm))/im+1
        i_3D = index_1D - (j_3D-1)*im - (k_3D-1)*im*jm
      end subroutine

      ! ****************************************************************
      ! **************************** VALUES ****************************
      ! ****************************************************************

      function get_val_SF(u,index_1D) result(val)
        implicit none
        type(SF),intent(in) :: u
        integer,intent(in) :: index_1D
        real(cp) :: val
        integer :: i_3D,j_3D,k_3D,t_3D
        call get_3D_index(i_3D,j_3D,k_3D,t_3D,u,index_1D); val = u%BF(t_3D)%GF%f(i_3D,j_3D,k_3D)
      end function

      function get_val_VF(u,index_1D) result(val)
        implicit none
        type(VF),intent(in) :: u
        integer,intent(in) :: index_1D
        real(cp),dimension(3) :: val
        val(1) = get_val(u%x,index_1D)
        val(2) = get_val(u%y,index_1D)
        val(3) = get_val(u%z,index_1D)
      end function

      end module