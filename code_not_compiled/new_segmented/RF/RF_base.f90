      module RF_base_mod
        ! Pre-processor directives: (_DEBUG_RF_,_PARALLELIZE_RF_)
        use current_precision_mod
        use grid_mod
        use BCs_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: realField
        public :: init,delete,display,print,export,import ! Essentials

        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! BC initialization
        public :: init_BCs

        ! Monitoring
        public :: print_physical

        type realField
          integer,dimension(3) :: s                  ! Dimension
          real(cp),dimension(:,:,:),allocatable :: f ! field
          type(BCs) :: b
        end type

        interface init;                     module procedure init_RF_size;           end interface
        interface init;                     module procedure init_RF_copy;           end interface
        interface delete;                   module procedure delete_RF;              end interface
        interface display;                  module procedure display_RF;             end interface
        interface print;                    module procedure print_RF;               end interface
        interface export;                   module procedure export_RF;              end interface
        interface import;                   module procedure import_RF;              end interface

        interface init_CC;                  module procedure init_RF_CC;             end interface
        interface init_Face;                module procedure init_RF_Face;           end interface
        interface init_Edge;                module procedure init_RF_Edge;           end interface
        interface init_Node;                module procedure init_RF_Node;           end interface

        interface init_BCs;                 module procedure init_BC_val;            end interface
        interface init_BCs;                 module procedure init_BC_vals;           end interface

      contains

        ! **********************************************************
        ! ********************* ESSENTIALS *************************
        ! **********************************************************

        subroutine init_RF_size(a,Nx,Ny,Nz)
          implicit none
          type(realField),intent(inout) :: a
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(Nx,Ny,Nz))
          a%s = shape(a%f)
        end subroutine

        subroutine init_RF_copy(f1,f2)
          implicit none
          type(realField),intent(inout) :: f1
          type(realField),intent(in) :: f2
          integer,dimension(3) :: s
          if (.not.allocated(f2%f)) stop 'Error: trying to copy unallocated RF in RF.f90'
          s = shape(f2%f)
          if (allocated(f1%f)) deallocate(f1%f)
          allocate(f1%f(s(1),s(2),s(3)))
          f1%s = shape(f1%f)
          if (f2%b%defined) call init(f1%b,f2%b)
        end subroutine

        subroutine delete_RF(a)
          implicit none
          type(realField),intent(inout) :: a
          if (allocated(a%f)) deallocate(a%f)
          call delete(a%b)
          a%s = 0
        end subroutine

        subroutine display_RF(a,un)
          implicit none
          type(realField),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
              write(un,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

        subroutine print_RF(a)
          implicit none
          type(realField),intent(in) :: a
          call display(a,6)
        end subroutine

        subroutine export_RF(a,un)
          implicit none
          type(realField),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          if (allocated(a%f)) then
          write(un,*) 'shape(f) = '
          write(un,*) a%s
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            write(un,*) a%f(i,j,k)
          enddo; enddo; enddo
          else; stop 'Error: trying to export unallocated RF in export_RF in RF.f90'
          endif
          call export(a%b,un)
        end subroutine

        subroutine import_RF(a,un)
          implicit none
          type(realField),intent(inout) :: a
          integer,intent(in) :: un
          integer :: i,j,k
          call delete(a)
          read(un,*) 
          read(un,*) a%s
          allocate(a%f(a%s(1),a%s(2),a%s(3)))
          do k=1,a%s(3); do j=1,a%s(2); do i=1,a%s(1)
            read(un,*) a%f(i,j,k)
          enddo; enddo; enddo
          call import(a%b,un)
        end subroutine

        ! **********************************************************
        ! **********************************************************
        ! **********************************************************

        function size_RF(a) result(s)
          implicit none
          type(realField),intent(in) :: a
          integer :: s
          s = a%s(1)*a%s(2)*a%s(3)
        end function

      ! ------------------- LOCATION-BASED ALLOCATE / DEALLOCATE --------------------

        subroutine init_RF_CC(a,g)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
        end subroutine

        subroutine init_RF_Face(a,g,dir)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
          case (2); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
          case (3); call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
          case default; stop 'Error: dir must = 1,2,3 in init_RF_Face in RF.f90'
          end select
        end subroutine

        subroutine init_RF_Edge(a,g,dir)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
          case (2); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
          case (3); call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
          case default; stop 'Error: dir must = 1,2,3 in init_RF_Face in RF.f90'
          end select
        end subroutine

        subroutine init_RF_Node(a,g)
          implicit none
          type(realField),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sn)
        end subroutine

        subroutine init_BC_val(f,val)
          implicit none
          type(realField),intent(inout) :: f
          real(cp),intent(in) :: val
          call init(f%b,val)
        end subroutine

        subroutine init_BC_vals(f,is_CC,is_Node)
          implicit none
          type(realField),intent(inout) :: f
          logical,intent(in) :: is_CC,is_Node
          logical,dimension(2) :: TF
          TF = (/is_CC,is_Node/)
          if (count(TF).gt.1) then
            stop 'Error: more than one datatype in init_BC_vals in RF.f90'
          endif
          if (is_Node) then
            call init(f%b,f%f(2,:,:),1)
            call init(f%b,f%f(:,2,:),3)
            call init(f%b,f%f(:,:,2),5)
            call init(f%b,f%f(f%s(1)-1,:,:),2)
            call init(f%b,f%f(:,f%s(2)-1,:),4)
            call init(f%b,f%f(:,:,f%s(3)-1),6)
          elseif (is_CC) then
            call init(f%b,0.5_cp*(f%f(1,:,:)+f%f(2,:,:)),1)
            call init(f%b,0.5_cp*(f%f(:,1,:)+f%f(:,2,:)),3)
            call init(f%b,0.5_cp*(f%f(:,:,1)+f%f(:,:,2)),5)
            call init(f%b,0.5_cp*(f%f(f%s(1),:,:)+f%f(f%s(1)-1,:,:)),2)
            call init(f%b,0.5_cp*(f%f(:,f%s(2),:)+f%f(:,f%s(2)-1,:)),4)
            call init(f%b,0.5_cp*(f%f(:,:,f%s(3))+f%f(:,:,f%s(3)-1)),6)
          else
            stop 'Error: field-based BC init is only available for N / CC data.'
          endif
        end subroutine

        subroutine print_physical_RF(a)
          implicit none
          type(realField),intent(in) :: a
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=2,a%s(3)-1; do j=2,a%s(2)-1; do i=2,a%s(1)-1
              write(*,'(A4,I1,A,I1,A,I1,A4,1F20.10)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

      end module