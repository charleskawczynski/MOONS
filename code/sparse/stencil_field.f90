      module stencil_field_mod
      use current_precision_mod
      use datatype_conversion_mod
      use data_location_mod
      use array_mod
      use sparse_mod
      use stencil_mod
      use grid_mod
      use GF_mod
      implicit none
      private
      public :: stencil_field
      public :: init,delete,display,print,export,import

      public :: multiply
      public :: insist_allocated

      type stencil_field
        type(grid_field),dimension(3) :: L,U  ! lower and upper diagonal fields
        type(grid_field) :: D                 ! diagonal field
        type(stencil) :: S                    ! 3D stencil in sparse format for 1D loops
      end type

      interface init;               module procedure init_stencil_field;              end interface
      interface init;               module procedure init_Copy;                       end interface
      interface delete;             module procedure delete_stencil_field;            end interface
      interface print;              module procedure print_stencil_field;             end interface
      interface display;            module procedure display_stencil_field;           end interface
      interface import;             module procedure import_stencil_field;            end interface
      interface export;             module procedure export_stencil_field;            end interface

      interface set_GF;             module procedure set_GF_SF;                       end interface
      interface multiply;           module procedure multiply_stencil_field_SF;       end interface
      interface insist_allocated;   module procedure insist_allocated_stencil_field;  end interface

      contains

      subroutine init_stencil_field(SF,S,g,DL)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(stencil),intent(in) :: S
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        call delete(SF)
        call init(SF%S,S)
        call set_GF(SF,g,DL)
      end subroutine

      subroutine set_GF_SF(SF,g,DL)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        type(grid_field),dimension(3) :: D
        integer :: i,j,k,t
        integer,dimension(3) :: s
        do i=1,3; call init(SF%L(i),g,DL); enddo
        do i=1,3; call init(SF%U(i),g,DL); enddo
        call init(SF%D,g,DL)
        do t=1,3; call assign(SF%L(t),0.0_cp); enddo
        do t=1,3; call assign(SF%U(t),0.0_cp); enddo
        call assign(SF%D,0.0_cp)
        s = SF%D%s

        do i=2,s(1)-1; call assign_plane_x(SF%L(1),SF%S%S(1)%L%f(i-1),i); enddo
        do i=2,s(1)-1; call assign_plane_x(SF%U(1),SF%S%S(1)%U%f(i-1),i); enddo
        do j=2,s(2)-1; call assign_plane_y(SF%L(2),SF%S%S(2)%L%f(j-1),j); enddo
        do j=2,s(2)-1; call assign_plane_y(SF%U(2),SF%S%S(2)%U%f(j-1),j); enddo
        do k=2,s(3)-1; call assign_plane_z(SF%L(3),SF%S%S(3)%L%f(k-1),k); enddo
        do k=2,s(3)-1; call assign_plane_z(SF%U(3),SF%S%S(3)%U%f(k-1),k); enddo

        ! Combine diagonals:
        do i=1,3; call init(D(i),g,DL); enddo
        do i=2,s(1)-1; call assign_plane_x(D(1),SF%S%S(1)%D%f(i-1),i); enddo
        do j=2,s(2)-1; call assign_plane_y(D(2),SF%S%S(2)%D%f(j-1),j); enddo
        do k=2,s(3)-1; call assign_plane_z(D(3),SF%S%S(3)%D%f(k-1),k); enddo
        call assign(SF%D,D(1))
        call    add(SF%D,D(2))
        call    add(SF%D,D(3))
        do i=1,3; call delete(D(i)); enddo
      end subroutine

      subroutine multiply_stencil_field_SF(SF,U)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid_field),intent(in) :: U
        integer :: i,j,k,t
#ifdef _DEBUG_STENCIL_
        call insist_allocated(SF,'multiply_stencil_field_SF')
#endif
        do t=1,3
        do k=1,SF%D%s(3); do j=1,SF%D%s(2); do i=1,SF%D%s(1)
          SF%L(t)%f(i,j,k) = SF%L(t)%f(i,j,k)*U%f(i,j,k)
          SF%U(t)%f(i,j,k) = SF%U(t)%f(i,j,k)*U%f(i,j,k)
        enddo; enddo; enddo
        enddo
        do k=1,SF%D%s(3); do j=1,SF%D%s(2); do i=1,SF%D%s(1)
          SF%D%f(i,j,k)    = SF%D%f(i,j,k)   *U%f(i,j,k)
        enddo; enddo; enddo
      end subroutine

      subroutine init_Copy(SF,SF_in)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(stencil_field),intent(in) :: SF_in
        integer :: i
        call delete(SF)
        do i=1,3
          call init(SF%L(i),SF_in%L(i))
          call init(SF%U(i),SF_in%U(i))
        enddo
        call init(SF%D,SF_in%D)
        call init(SF%S,SF_in%S)
      end subroutine

      subroutine delete_stencil_field(SF)
        implicit none
        type(stencil_field),intent(inout) :: SF
        integer :: i
        do i=1,3
          call delete(SF%L(i))
          call delete(SF%U(i))
        enddo
        call delete(SF%D)
        call delete(SF%S)
      end subroutine

      subroutine print_stencil_field(SF)
        implicit none
        type(stencil_field),intent(in) :: SF
        integer :: i
        call print(SF%S)
        do i=1,3
          call print(SF%L(i))
          call print(SF%U(i))
        enddo
        call print(SF%D)
      end subroutine

      subroutine display_stencil_field(SF,un)
        implicit none
        type(stencil_field),intent(in) :: SF
        integer,intent(in) :: un
        integer :: i
        call display(SF%S,un)
        do i=1,3
          call display(SF%L(i),un)
          call display(SF%U(i),un)
        enddo
        call display(SF%D,un)
      end subroutine

      subroutine export_stencil_field(SF,un)
        implicit none
        type(stencil_field),intent(in) :: SF
        integer,intent(in) :: un
        integer :: i
        call export(SF%S,un)
        do i=1,3
          call export(SF%L(i),un)
          call export(SF%U(i),un)
        enddo
        call export(SF%D,un)
      end subroutine

      subroutine import_stencil_field(SF,un)
        implicit none
        type(stencil_field),intent(inout) :: SF
        integer,intent(in) :: un
        integer :: i
        call import(SF%S,un)
        do i=1,3
          call import(SF%L(i),un)
          call import(SF%U(i),un)
        enddo
        call import(SF%D,un)
      end subroutine

      ! *********************************************************************

      subroutine insist_allocated_stencil_field(SF,caller)
        implicit none
        type(stencil_field),intent(in) :: SF
        character(len=*),intent(in) :: caller
        call insist_allocated(SF%S,caller//' S')
      end subroutine

      end module