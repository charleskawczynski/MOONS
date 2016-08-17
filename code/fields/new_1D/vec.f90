      module vec_base_mod
        ! Pre-processor directives: (_DEBUG_VEC_,_PARALLELIZE_VEC_)
        use current_precision_mod
        use grid_mod
        use BCs_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: vec
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

        type vec
          integer :: s                           ! size
          integer,dimension(3) :: d              ! dimensions
          real(cp),dimension(:),allocatable :: f ! field
          integer,dimension(:),allocatable :: g0,g1,g2 ! ghost,1-away from ghost,1-away from ghost
          type(BCs) :: b
        end type

        interface init;                     module procedure init_vec_size;           end interface
        interface init;                     module procedure init_vec_copy;           end interface
        interface delete;                   module procedure delete_vec;              end interface
        interface display;                  module procedure display_vec;             end interface
        interface print;                    module procedure print_vec;               end interface
        interface export;                   module procedure export_vec;              end interface
        interface import;                   module procedure import_vec;              end interface

        interface init_CC;                  module procedure init_vec_CC;             end interface
        interface init_Face;                module procedure init_vec_Face;           end interface
        interface init_Edge;                module procedure init_vec_Edge;           end interface
        interface init_Node;                module procedure init_vec_Node;           end interface

        interface init_BCs;                 module procedure init_BC_val;            end interface
        interface init_BCs;                 module procedure init_BC_vals;           end interface

      contains

        ! **********************************************************
        ! ********************* ESSENTIALS *************************
        ! **********************************************************

        subroutine init_vec_size(a,Nx,Ny,Nz)
          implicit none
          type(vec),intent(inout) :: a
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(a%f)) deallocate(a%f)
          allocate(a%f(Nx*Ny*Nz))
          a%d = (/Nx,Ny,Nz/)
          a%s = Nx*Ny*Nz
        end subroutine

        subroutine init_vec_copy(f1,f2)
          implicit none
          type(vec),intent(inout) :: f1
          type(vec),intent(in) :: f2
          integer,dimension(3) :: d
          if (.not.allocated(f2%f)) stop 'Error: trying to copy unallocated vec in vec_base.f90'
          call delete(f1)
          allocate(f1%f(f2%s))
          f1%d = f2%d
          f1%s = f1%s
          if (f2%b%defined) call init(f1%b,f2%b)
        end subroutine

        subroutine init_g0(f)
          implicit none
          type(vec),intent(inout) :: f
          integer,dimension(3) :: d
          integer :: t,i,j,k,counter
          counter = 0
          do t=1,f%s
            call get_ijk(i,j,k,f,t)
            if (any((/(i.eq.1),(j.eq.1),(k.eq.1),f%d(1).eq.1),(f%d(2).eq.1),(f%d(3).eq.1)/)) then
              counter = counter + 1
            endif
          enddo
          allocate(f%g0(counter))
          do t=1,f%s
            call get_ijk(i,j,k,f,t)
          enddo
        end subroutine

        subroutine delete_vec(a)
          implicit none
          type(vec),intent(inout) :: a
          if (allocated(a%f)) deallocate(a%f)
          call delete(a%b)
          a%s = 0
          a%d = 0
        end subroutine

        pure function get_i_1D(i,j,k,U) result(index_1D)
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
          type(vec),intent(in) :: U
          integer,intent(in) :: i,j,k
          integer :: index_1D
          ! im = U%s(1); jm = U%s(2); km = U%s(3)
          ! index_1D = i + im*( (j-1) + jm*(k-1) )
          index_1D = i + U%s(1)*( (j-1) + U%s(2)*(k-1) )
        end function

        subroutine get_ijk(i_3D,j_3D,k_3D,U,index_1D) ! Expensive
          implicit none
          type(vec),intent(in) :: U
          integer,intent(inout) :: i_3D,j_3D,k_3D
          integer,intent(in) :: index_1D
          integer :: im,jm
          ! integer :: km; km = U%s(3)
          im = U%s(1); jm = U%s(2)
          k_3D = (index_1D-1)/(im*jm)+1
          j_3D = ((index_1D-1) - ((k_3D-1)*im*jm))/im+1
          i_3D = index_1D - (j_3D-1)*im - (k_3D-1)*im*jm
        end subroutine

        function get_f(U,i,j,k) result(f_ijk)
          implicit none
          type(vec),intent(in) :: U
          integer,intent(in) :: i,j,k
          real(cp) :: f_ijk
          f_ijk = U%f(get_i_1D(i,j,k))
        end function

        subroutine display_vec(a,un)
          implicit none
          type(vec),intent(in) :: a
          integer,intent(in) :: un
          integer :: i,j,k,t
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%d
            do t=1,a%s
              call get_ijk(i,j,k,a,t)
              write(un,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'f(',i,',',j,',',k,') = ',a%f(t)
            enddo
          endif
        end subroutine

        subroutine print_vec(a)
          implicit none
          type(vec),intent(in) :: a
          call display(a,6)
        end subroutine

        subroutine export_vec(a,un)
          implicit none
          type(vec),intent(in) :: a
          integer,intent(in) :: un
          integer :: t
          if (allocated(a%f)) then
          write(un,*) 'a%s = '
          write(un,*) a%s
          write(un,*) 'a%d = '
          write(un,*) a%d
          do t=1,a%s; write(un,*) a%f(t); enddo
          else; stop 'Error: trying to export unallocated vec in export_vec in vec.f90'
          endif
          call export(a%b,un)
        end subroutine

        subroutine import_vec(a,un)
          implicit none
          type(vec),intent(inout) :: a
          integer,intent(in) :: un
          integer :: t
          call delete(a)
          read(un,*) 
          read(un,*) a%s
          read(un,*) 
          read(un,*) a%d
          allocate(a%f(a%s))
          do t=1,a%s; read(un,*) a%f(t); enddo
          call import(a%b,un)
        end subroutine

        ! **********************************************************
        ! **********************************************************
        ! **********************************************************

        ! ------------------- LOCATION-BASED ALLOCATE / DEALLOCATE --------------------

        subroutine init_vec_CC(a,g)
          implicit none
          type(vec),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sc)
        end subroutine

        subroutine init_vec_Face(a,g,dir)
          implicit none
          type(vec),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sc)
          case (2); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sc)
          case (3); call init(a,g%c(1)%sc,g%c(2)%sc,g%c(3)%sn)
          case default; stop 'Error: dir must = 1,2,3 in init_vec_Face in vec.f90'
          end select
        end subroutine

        subroutine init_vec_Edge(a,g,dir)
          implicit none
          type(vec),intent(inout) :: a
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init(a,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
          case (2); call init(a,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
          case (3); call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
          case default; stop 'Error: dir must = 1,2,3 in init_vec_Face in vec.f90'
          end select
        end subroutine

        subroutine init_vec_Node(a,g)
          implicit none
          type(vec),intent(inout) :: a
          type(grid),intent(in) :: g
          call init(a,g%c(1)%sn,g%c(2)%sn,g%c(3)%sn)
        end subroutine

        subroutine init_BC_val(f,val)
          implicit none
          type(vec),intent(inout) :: f
          real(cp),intent(in) :: val
          call init(f%b,val)
        end subroutine

        subroutine init_BC_vals(f,is_CC,is_Node)
          implicit none
          type(vec),intent(inout) :: f
          logical,intent(in) :: is_CC,is_Node
          logical,dimension(2) :: TF
          TF = (/is_CC,is_Node/)
          if (count(TF).gt.1) then
            stop 'Error: more than one datatype in init_BC_vals in vec.f90'
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

        subroutine print_physical_vec(a)
          implicit none
          type(vec),intent(in) :: a
          integer :: i,j,k
          if (allocated(a%f)) then
            write(*,*) 'shape(f) = ',a%s
            do k=2,a%s(3)-1; do j=2,a%s(2)-1; do i=2,a%s(1)-1
              write(*,'(A4,I1,A,I1,A,I1,A4,1F20.10)') 'f(',i,',',j,',',k,') = ',a%f(i,j,k)
            enddo; enddo; enddo
          endif
        end subroutine

      end module