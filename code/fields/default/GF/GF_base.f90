      module GF_base_mod
        use current_precision_mod
        use grid_mod
        use array_mod
        use face_edge_corner_indexing_mod
        use IO_tools_mod
        use data_location_mod
        implicit none
        private

        public :: grid_field
        public :: init,delete,display,print,export,import ! Essentials

        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: print_info
        public :: display_info
        public :: print_physical
        public :: insist_shape_match
        public :: insist_shape_staggered
        public :: insist_allocated

        type grid_field
          integer :: s_1D                            ! size
          integer,dimension(3) :: s                  ! Dimension
          real(cp),dimension(:,:,:),allocatable :: f ! field
        end type

        interface init;                     module procedure init_DL_GF;                   end interface
        interface init;                     module procedure init_shape_GF;                end interface
        interface init;                     module procedure init_shape_GF_2;              end interface
        interface init;                     module procedure init_GF_copy;                 end interface
        interface delete;                   module procedure delete_GF;                    end interface
        interface display;                  module procedure display_GF;                   end interface
        interface display;                  module procedure display_pad_GF;               end interface
        interface print;                    module procedure print_GF;                     end interface
        interface export;                   module procedure export_GF;                    end interface
        interface import;                   module procedure import_GF;                    end interface

        interface export;                   module procedure export_wrapper_GF_DL;         end interface
        interface export;                   module procedure export_wrapper_GF;            end interface

        interface init_CC;                  module procedure init_GF_CC;                   end interface
        interface init_Face;                module procedure init_GF_Face;                 end interface
        interface init_Edge;                module procedure init_GF_Edge;                 end interface
        interface init_Node;                module procedure init_GF_Node;                 end interface

        interface print_physical;           module procedure print_physical_GF;            end interface
        interface print_info;               module procedure print_info_GF;                end interface
        interface display_info;             module procedure display_info_GF;              end interface
        interface insist_shape_match;       module procedure insist_shape_match_GF;        end interface
        interface insist_shape_match;       module procedure insist_shape_match_plane_GF;  end interface
        interface insist_shape_staggered;   module procedure insist_shape_staggered_dir_GF;end interface
        interface insist_allocated;         module procedure insist_allocated_GF;          end interface

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

        subroutine init_shape_GF(a,Nx,Ny,Nz)
          implicit none
          type(grid_field),intent(inout) :: a
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(Nx,Ny,Nz))
          a%s = shape(a%f)
          a%s_1D = a%s(1)*a%s(2)*a%s(3)
        end subroutine

        subroutine init_shape_GF_2(a,s)
          implicit none
          type(grid_field),intent(inout) :: a
          integer,dimension(3),intent(in) :: s
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(s(1),s(2),s(3)))
          a%s = shape(a%f)
          a%s_1D = a%s(1)*a%s(2)*a%s(3)
        end subroutine

        subroutine init_GF_CC(a,g)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          call init_shape_GF(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
        end subroutine

        subroutine init_GF_Face(a,g,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_shape_GF(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
          case (2); call init_shape_GF(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
          case (3); call init_shape_GF(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
          case default; stop 'Error: dir must = 1,2,3 in init_GF_Face in GF.f90'
          end select
        end subroutine

        subroutine init_GF_Edge(a,g,dir)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_shape_GF(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
          case (2); call init_shape_GF(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
          case (3); call init_shape_GF(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
          case default; stop 'Error: dir must = 1,2,3 in init_GF_Face in GF.f90'
          end select
        end subroutine

        subroutine init_GF_Node(a,g)
          implicit none
          type(grid_field),intent(inout) :: a
          type(grid),intent(in) :: g
          call init_shape_GF(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sn)
        end subroutine

        subroutine init_GF_copy(f1,f2)
          implicit none
          type(grid_field),intent(inout) :: f1
          type(grid_field),intent(in) :: f2
          integer,dimension(3) :: s
          ! call insist_allocated_GF(f2,'init_GF_copy')
          call delete(f1)
          s = shape(f2%f)
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

        subroutine display_pad_GF(a,p,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un,p
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=1+p,a%s(3)-p; do j=1+p,a%s(2)-p; do i=1+p,a%s(1)-p
              write(un,*) 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

        subroutine print_GF(a)
          implicit none
          type(grid_field),intent(in) :: a
          call display(a,6)
        end subroutine

        subroutine print_physical_GF(a)
          implicit none
          type(grid_field),intent(in) :: a
          call display(a,1,6)
        end subroutine

        subroutine display_info_GF(a,un)
          implicit none
          type(grid_field),intent(in) :: a
          integer,intent(in) :: un
          write(un,*) 'shape(f) = ',a%s
          ! write(un,*) 'size(f) = ',a%s_1D
        end subroutine

        subroutine print_info_GF(a)
          implicit none
          type(grid_field),intent(in) :: a
          call display_info(a,6)
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

        subroutine export_wrapper_GF(a,hx,hy,hz,un,name)
          implicit none
          type(grid_field),intent(in) :: a
          real(cp),dimension(:),intent(in) :: hx,hy,hz
          integer,intent(in) :: un
          character(len=*),intent(in) :: name
          integer :: i,j,k
#ifdef _DEBUG_GF_
          call insist_allocated_GF(a,'export_wrapper_GF')
#endif
          write(un,*) 'TITLE = "3D Field"'
          write(un,*) 'VARIABLES = "x","y","z","',name,'"'
          write(un,*) 'ZONE,I=',a%s(1),',J=',a%s(2),',K=',a%s(3)
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
          write(un,*) hx(i),hy(j),hz(k),a%f(i,j,k)
          enddo; enddo; enddo
        end subroutine

        subroutine export_wrapper_GF_DL(a,g,dir,name,DL)
          implicit none
          type(grid_field),intent(in) :: a
          type(grid),intent(in) :: g
          character(len=*),intent(in) :: dir,name
          type(data_location),intent(in) :: DL
          type(array),dimension(3) :: h
          integer :: un,i
          un = new_and_open(dir,name)
          call get_coordinates_h(h,g,DL)
          call export(a,h(1)%f,h(2)%f,h(3)%f,un,name)
          call close_and_message(un,dir,name)
          do i=1,3; call delete(h(i)); enddo
        end subroutine

        subroutine insist_shape_match_GF(A,B,caller)
          implicit none
          type(grid_field),intent(in) :: A,B
          character(len=*),intent(in) :: caller
          logical,dimension(3) :: L
          L(1) = A%s(1).ne.B%s(1)
          L(2) = A%s(2).ne.B%s(2)
          L(3) = A%s(3).ne.B%s(3)
          if (any(L)) then
            write(*,*) 'Error: shape mismatch in insist_shape_match_GF in ',caller,' in GF_base.f90'
            write(*,*) 'A%s = ',A%s
            write(*,*) 'B%s = ',B%s
            stop 'Done'
          endif
        end subroutine

        subroutine insist_shape_match_plane_GF(A,B,dir,caller)
          implicit none
          type(grid_field),intent(in) :: A,B
          integer,intent(in) :: dir
          character(len=*),intent(in) :: caller
          logical,dimension(2) :: L
          integer,dimension(2) :: adj
          adj = adj_dir_given_dir(dir)
          L(1) = A%s(adj(1)).ne.B%s(adj(1))
          L(2) = A%s(adj(2)).ne.B%s(adj(2))
          if (any(L)) then
            write(*,*) 'Error: shape mismatch in insist_shape_match_plane_GF'
            write(*,*) 'in ',caller,' in GF_base.f90'
            write(*,*) 'A%s(adj(1)) = ',A%s(adj(1))
            write(*,*) 'A%s(adj(2)) = ',A%s(adj(2))
            write(*,*) 'B%s(adj(1)) = ',B%s(adj(1))
            write(*,*) 'B%s(adj(2)) = ',B%s(adj(2))
            stop 'Done'
          endif
        end subroutine

        subroutine insist_shape_staggered_dir_GF(C,N,dir,caller)
          implicit none
          type(grid_field),intent(in) :: C,N
          integer,intent(in) :: dir
          character(len=*),intent(in) :: caller
          if (.not.(C%s(dir).eq.N%s(dir)-1)) then
            write(*,*) 'Error: staggered shape mismatch in insist_shape_staggered_dir_GF'
            write(*,*) 'in ',caller,' in GF_base.f90'
            write(*,*) 'C%s(dir) = ',C%s(dir)
            write(*,*) 'N%s(dir) = ',N%s(dir)
            stop 'Done'
          endif
        end subroutine

        subroutine insist_allocated_GF(A,caller)
          implicit none
          type(grid_field),intent(in) :: A
          character(len=*),intent(in) :: caller
          if (.not.allocated(A%f)) then
            write(*,*) 'Error: GF not allocated in ',caller,' in GF_base.f90'
            stop 'Done'
          endif
        end subroutine

      end module