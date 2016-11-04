      module stencil_field_mod
      use current_precision_mod
      use datatype_conversion_mod
      use data_location_mod
      use face_edge_corner_indexing_mod
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
      public :: init_3D_diagonal
      public :: init_face_laplacian_SF

      public :: insist_allocated

      type stencil_field
        type(grid_field),dimension(3) :: L,D,U  ! lower,diagonal and upper diagonal fields
        type(grid_field) :: D_3D                ! 3D diagonal
        type(stencil) :: S                      ! 3D stencil in sparse format for 1D loops
      end type

      interface init;               module procedure init_stencil_field;              end interface
      interface init;               module procedure init_Copy;                       end interface
      interface delete;             module procedure delete_stencil_field;            end interface
      interface print;              module procedure print_stencil_field;             end interface
      interface display;            module procedure display_stencil_field;           end interface
      interface import;             module procedure import_stencil_field;            end interface
      interface export;             module procedure export_stencil_field;            end interface

      interface set_GF;             module procedure set_GF_SF;                       end interface
      interface init_3D_diagonal;   module procedure init_3D_diagonal_SF;             end interface
      interface multiply;           module procedure multiply_stencil_field_SF;       end interface
      interface multiply;           module procedure multiply_stencil_field_SF_cp;    end interface
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
        call set_GF_dir_SF(SF,g,DL,1)
        call set_GF_dir_SF(SF,g,DL,2)
        call set_GF_dir_SF(SF,g,DL,3)
      end subroutine

      subroutine set_GF_dir_SF(SF,g,DL,dir)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: dir
        integer,dimension(3) :: s
        integer :: i
        call init(SF%L(dir),g,DL); call assign(SF%L(dir),0.0_cp)
        call init(SF%D(dir),g,DL); call assign(SF%D(dir),0.0_cp)
        call init(SF%U(dir),g,DL); call assign(SF%U(dir),0.0_cp)
        s = SF%D(dir)%s(dir)
        select case (dir)
        case (1); do i=2,SF%S%S(dir)%L%N; call assign_plane_x(SF%L(dir),SF%S%S(dir)%L%f(i-1),i); enddo
                  do i=2,SF%S%S(dir)%D%N; call assign_plane_x(SF%D(dir),SF%S%S(dir)%D%f(i-1),i); enddo
                  do i=2,SF%S%S(dir)%U%N; call assign_plane_x(SF%U(dir),SF%S%S(dir)%U%f(i-1),i); enddo
        case (2); do i=2,SF%S%S(dir)%L%N; call assign_plane_y(SF%L(dir),SF%S%S(dir)%L%f(i-1),i); enddo
                  do i=2,SF%S%S(dir)%D%N; call assign_plane_y(SF%D(dir),SF%S%S(dir)%D%f(i-1),i); enddo
                  do i=2,SF%S%S(dir)%U%N; call assign_plane_y(SF%U(dir),SF%S%S(dir)%U%f(i-1),i); enddo
        case (3); do i=2,SF%S%S(dir)%L%N; call assign_plane_z(SF%L(dir),SF%S%S(dir)%L%f(i-1),i); enddo
                  do i=2,SF%S%S(dir)%D%N; call assign_plane_z(SF%D(dir),SF%S%S(dir)%D%f(i-1),i); enddo
                  do i=2,SF%S%S(dir)%U%N; call assign_plane_z(SF%U(dir),SF%S%S(dir)%U%f(i-1),i); enddo
        end select
        call init_3D_diagonal(SF)
      end subroutine

      subroutine init_consecutive(SF,g,DL)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,dimension(2) :: a
        integer :: i,dir
        if (is_Face(DL)) then
          dir = get_Face(DL); a = adj_dir_given_dir(dir)
          call init_consecutive_CC(SF,g,DL,a(1))
          call init_consecutive_CC(SF,g,DL,a(2))
          call init_consecutive_N(SF,g,DL,dir)
        elseif (is_Edge(DL)) then
          dir = get_Edge(DL); a = adj_dir_given_dir(dir)
          call init_consecutive_N(SF,g,DL,a(1))
          call init_consecutive_N(SF,g,DL,a(2))
          call init_consecutive_CC(SF,g,DL,dir)
        elseif (is_CC(DL)) then
          do i=1,3; call init_consecutive_CC(SF,g,DL,i); enddo
        elseif (is_Node(DL)) then
          do i=1,3; call init_consecutive_N(SF,g,DL,i); enddo
        endif
        call init_3D_diagonal(SF)
      end subroutine

      subroutine init_consecutive_CC(SF,g,DL,dir)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: dir
        integer :: i,j,k,t
        real(cp) :: DC_1,UC_1,k_1
        real(cp) :: DC_2,DN_2,UC_2,UN_2,k_2
        integer,dimension(3) :: x
        x = eye_given_dir(dir)
        call init(SF%L(dir),g,DL)
        call init(SF%D(dir),g,DL)
        call init(SF%U(dir),g,DL)

        do k=2,SF%D(dir)%s(3)-1; do j=2,SF%D(dir)%s(2)-1; do i=2,SF%D(dir)%s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          DC_1 = g%c(dir)%stagCC2N%D%f(t-1)
          UC_1 = g%c(dir)%stagCC2N%U%f(t-1)
          DC_2 = g%c(dir)%stagCC2N%D%f(t)
          UC_2 = g%c(dir)%stagCC2N%U%f(t)
          DN_2 = g%c(dir)%stagN2CC%D%f(t)
          UN_2 = g%c(dir)%stagN2CC%U%f(t)
          k_1 = 1.0_cp ! sig%f(i,j,k)
          k_2 = 1.0_cp ! sig%f(i+x(1),j+x(2),k+x(3))
          SF%L(dir)%f(i,j,k) = DC_1*DN_2*k_1
          SF%D(dir)%f(i,j,k) = DN_2*UC_1*k_1 + DC_2*UN_2*k_2
          SF%U(dir)%f(i,j,k) = UC_2*UN_2*k_2
        enddo; enddo; enddo

        call init(SF%S%S(dir),SF%D(dir)%s(dir))
        call assign(SF%S%S(dir),0.0_cp)
        do t=2,SF%D(dir)%s(dir)-1
          DC_1 = g%c(dir)%stagCC2N%D%f(t-1)
          UC_1 = g%c(dir)%stagCC2N%U%f(t-1)
          DC_2 = g%c(dir)%stagCC2N%D%f(t)
          UC_2 = g%c(dir)%stagCC2N%U%f(t)
          DN_2 = g%c(dir)%stagN2CC%D%f(t)
          UN_2 = g%c(dir)%stagN2CC%U%f(t)
          call assign(SF%S%S(dir)%L,DC_1*DN_2,t)
          call assign(SF%S%S(dir)%D,DN_2*UC_1 + DC_2*UN_2,t)
          call assign(SF%S%S(dir)%U,UC_2*UN_2,t)
        enddo
        SF%S%S(dir)%staggered = .false.
      end subroutine

      subroutine init_consecutive_N(SF,g,DL,dir)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: dir
        integer :: i,j,k,t
        real(cp) :: DC_1,DN_1,UC_1,UN_1,k_1
        real(cp) :: DN_2,UN_2,k_2
        integer,dimension(3) :: x
        x = eye_given_dir(dir)
        call init(SF%L(dir),g,DL)
        call init(SF%D(dir),g,DL)
        call init(SF%U(dir),g,DL)
        do k=2,SF%D(dir)%s(3)-1; do j=2,SF%D(dir)%s(2)-1; do i=2,SF%D(dir)%s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          DC_1 = g%c(dir)%stagCC2N%D%f(t-1)
          UC_1 = g%c(dir)%stagCC2N%U%f(t-1)
          DN_1 = g%c(dir)%stagN2CC%D%f(t-1)
          UN_1 = g%c(dir)%stagN2CC%U%f(t-1)
          DN_2 = g%c(dir)%stagN2CC%D%f(t)
          UN_2 = g%c(dir)%stagN2CC%U%f(t)
          k_1 = 1.0_cp ! sig%f(i,j,k)
          k_2 = 1.0_cp ! sig%f(i+x(1),j+x(2),k+x(3))
          SF%L(dir)%f(i,j,k) = DC_1*DN_1*k_1
          SF%D(dir)%f(i,j,k) = DN_2*UC_1*k_1 + DC_1*UN_1*k_2
          SF%U(dir)%f(i,j,k) = UC_1*UN_2*k_2
        enddo; enddo; enddo

        call init(SF%S%S(dir),SF%D(dir)%s(dir))
        call assign(SF%S%S(dir),0.0_cp)
        do t=2,SF%D(dir)%s(dir)-1
          DC_1 = g%c(dir)%stagCC2N%D%f(t-1)
          UC_1 = g%c(dir)%stagCC2N%U%f(t-1)
          DN_1 = g%c(dir)%stagN2CC%D%f(t-1)
          UN_1 = g%c(dir)%stagN2CC%U%f(t-1)
          DN_2 = g%c(dir)%stagN2CC%D%f(t)
          UN_2 = g%c(dir)%stagN2CC%U%f(t)
          call assign(SF%S%S(dir)%L,DC_1*DN_1,t)
          call assign(SF%S%S(dir)%D,DN_2*UC_1 + DC_1*UN_1,t)
          call assign(SF%S%S(dir)%U,UC_1*UN_2,t)
        enddo
        SF%S%S(dir)%staggered = .false.
      end subroutine

      subroutine init_3D_diagonal_SF(SF)
        implicit none
        type(stencil_field),intent(inout) :: SF
        call init(SF%D_3D,SF%D(1))
        call assign(SF%D_3D,SF%D(1))
        call    add(SF%D_3D,SF%D(2))
        call    add(SF%D_3D,SF%D(3))
      end subroutine

      subroutine init_face_laplacian_SF(SF,g,DL)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        call init_consecutive(SF,g,DL)
      end subroutine

      subroutine multiply_stencil_field_SF(SF,F)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(grid_field),intent(in) :: F
        integer :: i
#ifdef _DEBUG_STENCIL_
        call insist_allocated(SF,'multiply_stencil_field_SF')
#endif
        do i=1,3; call multiply(SF%L(i),F); enddo
        do i=1,3; call multiply(SF%D(i),F); enddo
        do i=1,3; call multiply(SF%U(i),F); enddo
        call multiply(SF%D_3D,F)
      end subroutine

      subroutine multiply_stencil_field_SF_cp(SF,F)
        implicit none
        type(stencil_field),intent(inout) :: SF
        real(cp),intent(in) :: F
        integer :: i
#ifdef _DEBUG_STENCIL_
        call insist_allocated(SF,'multiply_stencil_field_SF_cp')
#endif
        do i=1,3; call multiply(SF%L(i),F); enddo
        do i=1,3; call multiply(SF%D(i),F); enddo
        do i=1,3; call multiply(SF%U(i),F); enddo
        call multiply(SF%D_3D,F)
      end subroutine

      subroutine init_Copy(SF,SF_in)
        implicit none
        type(stencil_field),intent(inout) :: SF
        type(stencil_field),intent(in) :: SF_in
        integer :: i
        call delete(SF)
        do i=1,3
          call init(SF%L(i),SF_in%L(i))
          call init(SF%D(i),SF_in%D(i))
          call init(SF%U(i),SF_in%U(i))
        enddo
        call init(SF%D_3D,SF_in%D_3D)
        call init(SF%S,SF_in%S)
      end subroutine

      subroutine delete_stencil_field(SF)
        implicit none
        type(stencil_field),intent(inout) :: SF
        integer :: i
        do i=1,3
          call delete(SF%L(i))
          call delete(SF%D(i))
          call delete(SF%U(i))
        enddo
        call delete(SF%D_3D)
        call delete(SF%S)
      end subroutine

      subroutine print_stencil_field(SF)
        implicit none
        type(stencil_field),intent(in) :: SF
        integer :: i
        call print(SF%S)
        do i=1,3
          call print(SF%L(i))
          call print(SF%D(i))
          call print(SF%U(i))
        enddo
        call print(SF%D_3D)
        call print(SF%S)
      end subroutine

      subroutine display_stencil_field(SF,un)
        implicit none
        type(stencil_field),intent(in) :: SF
        integer,intent(in) :: un
        integer :: i
        call display(SF%S,un)
        do i=1,3
          call display(SF%L(i),un)
          call display(SF%D(i),un)
          call display(SF%U(i),un)
        enddo
        call display(SF%D_3D,un)
        call display(SF%S,un)
      end subroutine

      subroutine export_stencil_field(SF,un)
        implicit none
        type(stencil_field),intent(in) :: SF
        integer,intent(in) :: un
        integer :: i
        call export(SF%S,un)
        do i=1,3
          call export(SF%L(i),un)
          call export(SF%D(i),un)
          call export(SF%U(i),un)
        enddo
        call export(SF%D_3D,un)
        call export(SF%S,un)
      end subroutine

      subroutine import_stencil_field(SF,un)
        implicit none
        type(stencil_field),intent(inout) :: SF
        integer,intent(in) :: un
        integer :: i
        call import(SF%S,un)
        do i=1,3
          call import(SF%L(i),un)
          call import(SF%D(i),un)
          call import(SF%U(i),un)
        enddo
        call import(SF%D_3D,un)
        call import(SF%S,un)
      end subroutine

      ! *********************************************************************

      subroutine insist_allocated_stencil_field(SF,caller)
        implicit none
        type(stencil_field),intent(in) :: SF
        character(len=*),intent(in) :: caller
        call insist_allocated(SF%S,caller//' S')
      end subroutine

      end module