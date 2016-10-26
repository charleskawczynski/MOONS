      module GF_base_mod
        use current_precision_mod
        use grid_mod
        use data_location_mod
        implicit none
        private

        public :: grid_field
        public :: init,delete,display,print,export,import ! Essentials

        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: insist_shape_match

        type grid_field
          integer :: s_1D                            ! size
          integer,dimension(3) :: s                  ! Dimension
          real(cp),dimension(:,:,:),allocatable :: f ! field
        end type

        interface init;                     module procedure init_DL_GF;             end interface
        interface init;                     module procedure init_GF_copy;           end interface
        interface delete;                   module procedure delete_GF;              end interface
        interface display;                  module procedure display_GF;             end interface
        interface print;                    module procedure print_GF;               end interface
        interface export;                   module procedure export_GF;              end interface
        interface import;                   module procedure import_GF;              end interface

        interface init_CC;                  module procedure init_GF_CC;             end interface
        interface init_Face;                module procedure init_GF_Face;           end interface
        interface init_Edge;                module procedure init_GF_Edge;           end interface
        interface init_Node;                module procedure init_GF_Node;           end interface

        interface insist_shape_match;       module procedure insist_shape_match_GF;  end interface

       contains

        ! **********************************************************
        ! ********************* ESSENTIALS *************************
        ! **********************************************************

        subroutine init_DL_GF(a,g,DL)
          implicit none
          type(grid_field),intent(inout) :: a
          type(data_location),intent(in) :: DL
          type(grid),intent(in) :: g
          if (is_CC(DL)) then; call init_CC(a,g)
          elseif (is_Node(DL)) then; call init_Node(a,g)
          elseif (is_Face(DL)) then; call init_Face(a,g,get_face(DL))
          elseif (is_Edge(DL)) then; call init_Edge(a,g,get_edge(DL))
          else; stop 'Erorr: bad input to init_DL_GF in GF_base.f90'
          endif
        end subroutine

        subroutine init_GF_shape(a,Nx,Ny,Nz)
          implicit none
          type(grid_field),intent(inout) :: a
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(Nx,Ny,Nz))
          a%s = shape(a%f)
          a%s_1D = a%s(1)*a%s(2)*a%s(3)
        end subroutine

        subroutine init_GF_CC(a,g)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          call init_GF_shape(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
        end subroutine

        subroutine init_GF_Face(a,g,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_GF_shape(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
          case (2); call init_GF_shape(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
          case (3); call init_GF_shape(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
          case default; stop 'Error: dir must = 1,2,3 in init_GF_Face in GF.f90'
          end select
        end subroutine

        subroutine init_GF_Edge(a,g,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_GF_shape(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
          case (2); call init_GF_shape(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
          case (3); call init_GF_shape(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
          case default; stop 'Error: dir must = 1,2,3 in init_GF_Face in GF.f90'
          end select
        end subroutine

        subroutine init_GF_Node(a,g)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          call init_GF_shape(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sn)
        end subroutine

        subroutine init_GF_copy(f1,f2)
          implicit none
          type(grid_field),intent(inout) :: f1
          type(grid_field),intent(in) :: f2
          integer,dimension(3) :: s
          if (.not.allocated(f2%f)) stop 'Error: trying to copy unallocated GF in GF.f90'
          s = shape(f2%f)
          if (allocated(f1%f)) deallocate(f1%f)
          allocate(f1%f(s(1),s(2),s(3)))
          f1%s = shape(f1%f)
          f1%s_1D = f2%s_1D
        end subroutine

        subroutine delete_GF(a)
          implicit none
          type(grid_field),intent(inout) :: a
          if (allocated(a%f)) deallocate(a%f)
          a%s = 0
          a%s_1D = 0
        end subroutine

        subroutine display_GF(a,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
              write(un,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

        subroutine print_GF(a)
          implicit none
          type(grid_field),intent(in) :: a
          call display(a,6)
        end subroutine

        subroutine export_GF(a,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
          write(un,*) 'shape(f) = '
          write(un,*) a%s
          write(un,*) 'size(f) = '
          write(un,*) a%s_1D
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            write(un,*) a%f(i,j,k)
          enddo; enddo; enddo
          else; stop 'Error: trying to export unallocated GF in export_GF in GF.f90'
          endif
        end subroutine

        subroutine import_GF(a,un)
          implicit none
          type(grid_field),intent(inout) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          call delete(a)
          read(un,*) 
          read(un,*) a%s
          read(un,*) 
          read(un,*) a%s_1D
          allocate(a%f(a%s(1),a%s(2),a%s(3)))
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            read(un,*) a%f(i,j,k)
          enddo; enddo; enddo
        end subroutine

        ! *******************************************************************************

        subroutine insist_shape_match_GF(A,B,caller)
          implicit none
          type(grid_field),intent(in) :: A,B
          character(len=*),intent(in) :: caller
          logical,dimension(3) :: L
          L(1) = A%s(1).ne.B%s(1)
          L(2) = A%s(2).ne.B%s(2)
          L(3) = A%s(3).ne.B%s(3)
          if (any(L)) then
            write(*,*) 'Error: shape mismatch in ',caller,' in GF_base.f90'
            write(*,*) 'A%s = ',A%s
            write(*,*) 'B%s = ',B%s
            stop 'Done'
          endif
        end subroutine

      end module