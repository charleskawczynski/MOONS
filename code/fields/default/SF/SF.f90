      module SF_mod
        ! Naming convention: name = operation_type1_type2
        ! 
        !      SF = type(SF)
        !      R  = real(cp),dimension(:,:,:)
        !      S  = real(cp)
        ! 
        ! Example(1): Adding a scalar to SF
        !             name = add_SF_S
        ! Example(2): Subtracting a real field from SF
        !             name = subtract_SF_R
        ! Example(3): Subtracting a SF from a real field
        !             name = subtract_R_SF
        ! 
        ! NOTES: SF stands for 'real field'
        ! 
        ! Rules:
        ! a = a + b => call add(a,b)
        ! a = a - b => call subtract(a,b)
        ! a = a * b => call multiply(a,b)
        ! a = a / b => call divide(a,b)
        ! a = b / a => call divide(b,a)
        ! OR
        ! c = a + b => call add(c,a,b)
        ! c = a - b => call subtract(c,a,b)
        ! c = a * b => call multiply(c,a,b)
        ! c = a / b => call divide(c,a,b)
        ! c = b / a => call divide(c,b,a)
        use current_precision_mod
        use IO_tools_mod
        use mesh_mod
        use domain_mod
        use BCs_mod
        use RF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: SF
        public :: init,delete,display,print,export,import ! Essentials
        ! Grid initialization
        public :: init_CC
        public :: init_Node
        public :: init_Face
        public :: init_Edge
        
        public :: volume
        public :: multiply_volume
        public :: mean_along_dir,subtract_mean_along_dir
        public :: N0_C1_tensor
        public :: C0_N1_tensor

        public :: init_BCs,init_BC_Dirichlet,init_BC_props,init_BC_mesh
        public :: dot_product

        ! Monitoring
        public :: print_physical
        public :: print_BCs
        public :: export_BCs

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: invert
        public :: add_product,swap
        ! Auxiliary
        public :: square,min,max,maxabs
        public :: maxabsdiff,mean,sum

        public :: zero_ghost_xmin_xmax
        public :: zero_ghost_ymin_ymax
        public :: zero_ghost_zmin_zmax

        type SF
          integer :: s ! Number of subdomains in domain decomposition
          type(realField),dimension(:),allocatable :: RF
          logical,dimension(3) :: CC_along
          logical,dimension(3) :: N_along
          logical :: is_CC,is_Node,is_face,is_edge
          logical :: all_neumann ! If necessary to subtract mean
          integer :: face = 0 ! Direction of face data
          integer :: edge = 0 ! Direction of edge data
          integer :: numEl,numPhysEl ! Number of physical elements, number of physical elements
          real(cp) :: vol
        end type

        interface init;                module procedure init_SF_copy;           end interface
        interface init;                module procedure init_SF_copy_domain;    end interface
        interface delete;              module procedure delete_SF;              end interface
        interface display;             module procedure display_SF;             end interface
        interface print;               module procedure print_SF;               end interface
        interface export;              module procedure export_SF;              end interface
        interface import;              module procedure import_SF;              end interface
        interface export;              module procedure export_wrapper_SF;      end interface
        interface import;              module procedure import_wrapper_SF;      end interface

        interface init_CC;             module procedure init_SF_CC;             end interface
        interface init_Node;           module procedure init_SF_Node;           end interface
        interface init_Face;           module procedure init_SF_Face;           end interface
        interface init_Edge;           module procedure init_SF_Edge;           end interface

        interface init_CC;             module procedure init_SF_CC_assign;      end interface
        interface init_Node;           module procedure init_SF_Node_assign;    end interface
        interface init_Face;           module procedure init_SF_Face_assign;    end interface
        interface init_Edge;           module procedure init_SF_Edge_assign;    end interface

        interface init_BCs;            module procedure init_BC_vals_SF;        end interface
        interface init_BC_Dirichlet;   module procedure init_BC_Dirichlet_SF;   end interface
        interface init_BCs;            module procedure init_BC_val_SF;         end interface
        interface init_BCs;            module procedure init_BCs_SF_SF;         end interface
        interface init_BC_props;       module procedure init_BC_props_SF;       end interface
        interface init_BC_mesh;        module procedure init_BC_mesh_SF;        end interface

        interface print_physical;      module procedure print_physical_SF;      end interface
        interface print_BCs;           module procedure print_BCs_SF;           end interface
        interface export_BCs;          module procedure export_BCs_SF;          end interface

        interface volume;              module procedure volume_SF;              end interface
        interface multiply_volume;     module procedure multiply_volume_SF;     end interface
        interface mean_along_dir;      module procedure mean_along_dir_SF;      end interface
        interface subtract_mean_along_dir;  module procedure subtract_mean_along_dir_SF;  end interface

        interface zero_ghost_xmin_xmax;module procedure zero_ghost_xmin_xmax_SF;end interface
        interface zero_ghost_ymin_ymax;module procedure zero_ghost_ymin_ymax_SF;end interface
        interface zero_ghost_zmin_zmax;module procedure zero_ghost_zmin_zmax_SF;end interface


        ! COMPUTATION ROUTINES:

        interface assign;              module procedure assign_SF_S;            end interface
        interface assign;              module procedure assign_SF_SF;           end interface
        interface assign_negative;     module procedure assign_negative_SF_SF;  end interface

        interface add;                 module procedure add_SF_SF;              end interface
        interface add;                 module procedure add_SF_SF_SF;           end interface
        interface add;                 module procedure add_SF_SF_SF_SF;        end interface
        interface add;                 module procedure add_SF_S;               end interface
        interface add;                 module procedure add_S_SF;               end interface
        interface add;                 module procedure add_SF_SF9;             end interface

        interface add_product;         module procedure add_product_SF_SF_S;    end interface
        interface add_product;         module procedure add_product_SF_SF_SF;   end interface

        interface multiply;            module procedure multiply_SF_SF;         end interface
        interface multiply;            module procedure multiply_SF_SF_SF;      end interface
        interface multiply;            module procedure multiply_SF_SF_S;       end interface
        interface multiply;            module procedure multiply_SF_S;          end interface
        interface multiply;            module procedure multiply_S_SF;          end interface

        interface subtract;            module procedure subtract_SF_SF;         end interface
        interface subtract;            module procedure subtract_SF_SF_SF;      end interface
        interface subtract;            module procedure subtract_SF_S;          end interface
        interface subtract;            module procedure subtract_S_SF;          end interface

        interface divide;              module procedure divide_SF_SF;           end interface
        interface divide;              module procedure divide_SF_SF_SF;        end interface
        interface divide;              module procedure divide_SF_S_SF;         end interface
        interface divide;              module procedure divide_SF_S;            end interface
        interface divide;              module procedure divide_S_SF;            end interface

        interface invert;              module procedure invert_SF;              end interface

        interface square;              module procedure square_SF;              end interface
        interface swap;                module procedure swap_SF;                end interface
        interface min;                 module procedure min_SF;                 end interface
        interface max;                 module procedure max_SF;                 end interface
        interface min;                 module procedure min_pad_SF;             end interface
        interface max;                 module procedure max_pad_SF;             end interface
        interface maxabs;              module procedure maxabs_SF;              end interface
        interface maxabsdiff;          module procedure maxabsdiff_SF;          end interface
        interface mean;                module procedure mean_SF;                end interface
        interface sum;                 module procedure sum_SF;                 end interface
        interface sum;                 module procedure sum_SF_pad;             end interface
        interface dot_product;         module procedure dot_product_SF;         end interface

      contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

        subroutine init_SF_copy(f1,f2)
          implicit none
          type(SF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          integer :: i
          call delete(f1)
          allocate(f1%RF(f2%s)); f1%s = f2%s
          do i=1,f1%s; call init(f1%RF(i),f2%RF(i)); enddo
          f1%numEl = f2%numEl
          f1%numPhysEl = f2%numPhysEl
          f1%is_CC = f2%is_CC
          f1%is_node = f2%is_node
          f1%is_face = f2%is_face
          f1%is_edge = f2%is_edge

          f1%face = f2%face
          f1%edge = f2%edge
          f1%CC_along = f2%CC_along
          f1%N_along = f2%N_along
        end subroutine

        subroutine init_SF_copy_domain(f1,f2,D)
          implicit none
          type(SF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          type(domain),intent(in) :: D
          type(SF) :: temp
          if (f2%is_CC) then;       call init_CC(temp,D%m_in)
          elseif (f2%is_Node) then; call init_Node(temp,D%m_in)
          elseif (f2%is_Face) then; call init_Face(temp,D%m_in,f2%face)
          elseif (f2%is_Edge) then; call init_Edge(temp,D%m_in,f2%edge)
          else; stop 'Error: bad datatype in init_SF_copy_domain in SF.f90'
          endif
          call init(f1,temp)
          call delete(temp)
        end subroutine

        subroutine delete_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          if (allocated(f%RF)) then
            do i=1,f%s; call delete(f%RF(i)); enddo
            deallocate(f%RF)
          endif
          f%s = 0
          f%numEl = 0
          f%numPhysEl = 0
        end subroutine

        subroutine display_SF(f,un)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: un
          integer :: i
          do i=1,f%s; call display(f%RF(i),un); enddo
        end subroutine

        subroutine print_SF(f)
          implicit none
          type(SF),intent(in) :: f
          call display(f,6)
        end subroutine

        subroutine export_SF(f,un)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: un
          integer :: i
          write(un,*) 'f%s'
          write(un,*) f%s
          write(un,*) f%is_CC,f%is_Node,f%is_face,f%is_edge,f%all_neumann
          write(un,*) f%face,f%edge,f%numEl,f%numPhysEl
          write(un,*) f%vol
          do i=1,f%s; call export(f%RF(i),un); enddo
        end subroutine

        subroutine import_SF(f,un)
          implicit none
          type(SF),intent(inout) :: f
          integer,intent(in) :: un
          integer :: i
          call delete(f)
          read(un,*) 
          read(un,*) f%s
          read(un,*) f%is_CC,f%is_Node,f%is_face,f%is_edge,f%all_neumann
          read(un,*) f%face,f%edge,f%numEl,f%numPhysEl
          read(un,*) f%vol
          allocate(f%RF(f%s))
          do i=1,f%s; call import(f%RF(i),un); enddo
        end subroutine

        subroutine export_wrapper_SF(f,dir,name)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          integer :: un
          un = new_and_open(dir,name)
          call export(f,un)
          call close_and_message(un,dir,name)
        end subroutine

        subroutine import_wrapper_SF(f,dir,name)
          implicit none
          type(SF),intent(inout) :: f
          character(len=*),intent(in) :: dir,name
          integer :: un
          un = new_and_open(dir,name)
          call import(f,un)
          call close_and_message(un,dir,name)
        end subroutine


       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

        subroutine N0_C1_tensor(U,x,y,z)
          implicit none
          type(SF),intent(in) :: U
          integer,intent(inout) :: x,y,z
          if (U%is_CC) then
            x = 1; y = 1; z = 1
          elseif (U%is_Node) then
            x = 0; y = 0; z = 0
          elseif (U%is_Face) then
            select case (U%face)
            case (1); x = 0; y = 1; z = 1
            case (2); x = 1; y = 0; z = 1
            case (3); x = 1; y = 1; z = 0
            case default; stop 'Error: face must = 1,2,3 in apply_stitches_faces.f90'
            end select
          elseif (U%is_Edge) then
            select case (U%edge)
            case (1); x = 1; y = 0; z = 0
            case (2); x = 0; y = 1; z = 0
            case (3); x = 0; y = 0; z = 1
            case default; stop 'Error: edge must = 1,2,3 in apply_stitches_faces.f90'
            end select
          else
           write(*,*) 'U%is_CC = ',U%is_CC
           write(*,*) 'U%is_Node = ',U%is_Node
           write(*,*) 'U%is_Face = ',U%is_Face
           write(*,*) 'U%is_Edge = ',U%is_Edge
           write(*,*) 'U%Face = ',U%Face
           write(*,*) 'U%Edge = ',U%Edge
           stop 'Error: data type not found in N0_C1_tensor in SF.f90'
          endif
        end subroutine

        subroutine C0_N1_tensor(U,x,y,z)
          implicit none
          type(SF),intent(in) :: U
          integer,intent(inout) :: x,y,z
          if (U%is_CC) then
            x = 0; y = 0; z = 0
          elseif (U%is_Node) then
            x = 1; y = 1; z = 1
          elseif (U%is_Face) then
            select case (U%face)
            case (1); x = 1; y = 0; z = 0
            case (2); x = 0; y = 1; z = 0
            case (3); x = 0; y = 0; z = 1
            case default; stop 'Error: face must = 1,2,3 in apply_stitches_faces.f90'
            end select
          elseif (U%is_Edge) then
            select case (U%edge)
            case (1); x = 0; y = 1; z = 1
            case (2); x = 1; y = 0; z = 1
            case (3); x = 1; y = 1; z = 0
            case default; stop 'Error: edge must = 1,2,3 in apply_stitches_faces.f90'
            end select
          else
           write(*,*) 'U%is_CC = ',U%is_CC
           write(*,*) 'U%is_Node = ',U%is_Node
           write(*,*) 'U%is_Face = ',U%is_Face
           write(*,*) 'U%is_Edge = ',U%is_Edge
           write(*,*) 'U%Face = ',U%Face
           write(*,*) 'U%Edge = ',U%Edge
           stop 'Error: data type not found in C0_N1_tensor in SF.f90'
          endif
        end subroutine

        subroutine deleteDataLocation(a)
          implicit none
          type(SF),intent(inout) :: a
          a%is_CC = .false.
          a%is_node = .false.
          a%is_face = .false.
          a%is_edge = .false.
        end subroutine

        subroutine computeNumEl(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          f%numEl = 0
          f%numPhysEl = 0
          do i=1,f%s
          f%numEl = f%numEl + f%RF(i)%s(1)*f%RF(i)%s(2)*f%RF(i)%s(3)
          f%numPhysEl = f%numPhysEl + (f%RF(i)%s(1)-2)*(f%RF(i)%s(2)-2)*(f%RF(i)%s(3)-2)
          enddo
        end subroutine

        subroutine init_CC_N_along(f)
          implicit none
          type(SF),intent(inout) :: f
          if (f%is_CC) then;       f%CC_along = .true.; f%N_along  = .false.
          elseif (f%is_Node) then; f%N_along  = .true.; f%CC_along = .false.
          elseif (f%is_Face) then
            f%CC_along = .not.diag_true(f%face)
            f%N_along  =      diag_true(f%face)
          elseif (f%is_Edge) then
            f%CC_along =      diag_true(f%edge)
            f%N_along  = .not.diag_true(f%edge)
          else; stop 'Error: bad data type in init_CC_N_along in SF.f90'
          endif
        end subroutine

        function diag_true(dir) result(L)
          implicit none
          integer,intent(in) :: dir
          logical,dimension(3) :: L
          select case(dir)
          case (1);L=(/.true.,.false.,.false./)
          case (2);L=(/.false.,.true.,.false./)
          case (3);L=(/.false.,.false.,.true./)
          case default; stop 'Error: dir must = 1,2,3 in diag_true in SF.f90'
          end select
        end function

        subroutine init_SF_CC(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          call delete(f)
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_CC(f%RF(i),m%g(i)); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_CC = .true.
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_CC_assign(f,m,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC(f,m); call assign(f,val)
        end subroutine

        subroutine init_SF_Face(f,m,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: i
          call delete(f)
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_Face(f%RF(i),m%g(i),dir); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_face = .true.
          f%face = dir
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_Face_assign(f,m,dir,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          real(cp),intent(in) :: val
          call init_Face(f,m,dir); call assign(f,val)
        end subroutine

        subroutine init_SF_Edge(f,m,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: i
          call delete(f)
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_Edge(f%RF(i),m%g(i),dir); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_edge = .true.
          f%edge = dir
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_Edge_assign(f,m,dir,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          real(cp),intent(in) :: val
          call init_Edge(f,m,dir); call assign(f,val)
        end subroutine

        subroutine init_SF_Node(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          call delete(f)
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_Node(f%RF(i),m%g(i)); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_node = .true.
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_Node_assign(f,m,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f,m); call assign(f,val)
        end subroutine

        subroutine init_BCs_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call init(f%RF(i)%b,g%RF(i)%b); enddo
        end subroutine

        subroutine init_BC_vals_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          if (f%is_CC) then
            do i=1,f%s; call init_BCs(f%RF(i),.true.,.false.); enddo
          elseif (f%is_Node) then
            do i=1,f%s; call init_BCs(f%RF(i),.false.,.true.); enddo
          else
            stop 'Error: no datatype found in init_BC_vals_SF in SF.f90'
          endif
          f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
          call init_BC_props(f)
        end subroutine

        subroutine init_BC_Dirichlet_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call init_Dirichlet(f%RF(i)%b); enddo
        end subroutine

        subroutine init_BC_val_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: i
          do i=1,f%s; call init_BCs(f%RF(i),val); enddo
          f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
          call init_BC_props(f)
        end subroutine

        subroutine init_BC_mesh_SF(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,f%s; call init(f%RF(i)%b,m%g(i),f%RF(i)%s); enddo
        end subroutine

        subroutine init_BC_props_SF(f)
          ! THIS IS BROKEN. Technically, all_Neumann should be true for
          ! periodic 1-cell cases, but this fix is not even enough. If 
          ! any boundaries are Dirichlet but stitched together, then 
          ! all_Neumann should also be true. The mesh, after stitching
          ! is required to make a fully comprehensive init_BC_props_SF.
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
        end subroutine

        subroutine volume_SF(u,m)
          ! Computes
          ! 
          !   volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i,j,k,t
          call assign(u,0.0_cp)
          if (u%is_CC) then
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                 (m%g(t)%c(2)%dhn(j))*&
                                 (m%g(t)%c(3)%dhn(k))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          elseif (u%is_Node) then
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                 (m%g(t)%c(2)%dhc(j-1))*&
                                 (m%g(t)%c(3)%dhc(k-1))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          elseif (u%is_Face) then
          select case (u%face)
          case (1);
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                 (m%g(t)%c(2)%dhn(j))*&
                                 (m%g(t)%c(3)%dhn(k))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          case (2);
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                 (m%g(t)%c(2)%dhc(j-1))*&
                                 (m%g(t)%c(3)%dhn(k))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          case (3);
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                 (m%g(t)%c(2)%dhn(j))*&
                                 (m%g(t)%c(3)%dhc(k-1))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          case default; stop 'Error: SF has no face location in volume_SF in ops_aux.f90'
          end select
          elseif (u%is_Edge) then
          select case (u%edge)
          case (1);
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                                 (m%g(t)%c(2)%dhc(j-1))*&
                                 (m%g(t)%c(3)%dhc(k-1))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          case (2);
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                 (m%g(t)%c(2)%dhn(j))*&
                                 (m%g(t)%c(3)%dhc(k-1))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          case (3);
          !$OMP PARALLEL DO SHARED(m)
          do t=1,m%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              u%RF(t)%f(i,j,k) = (m%g(t)%c(1)%dhc(i-1))*&
                                 (m%g(t)%c(2)%dhc(j-1))*&
                                 (m%g(t)%c(3)%dhn(k))
          enddo; enddo; enddo; enddo
          !$OMP END PARALLEL DO
          case default; stop 'Error: SF has no face location in volume_SF in ops_aux.f90'
          end select
          else; stop 'Error: SF has no location in volume_SF in ops_aux.f90'
          endif
          u%vol = sum(u)
        end subroutine

        subroutine multiply_volume_SF(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(SF) :: vol
          call init(vol,f)
          call volume(vol,m)
          call multiply(f,vol)
          call delete(vol)
        end subroutine

       subroutine mean_along_dir_SF(x_mean,x,m,dir)
         implicit none
         type(SF),intent(inout) :: x_mean
         type(SF),intent(in) :: x
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer :: t,i,j,k
         select case (dir)
         case (1)
           !$OMP PARALLEL DO
           do t=1,m%s; do i=1,x%RF(t)%s(1)
             x_mean%RF(t)%f(i,:,:) = plane_mean(x,i,m,t,dir)
           enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO
           do t=1,m%s; do j=1,x%RF(t)%s(2)
             x_mean%RF(t)%f(:,j,:) = plane_mean(x,j,m,t,dir)
           enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO
           do t=1,m%s; do k=1,x%RF(t)%s(3)
             x_mean%RF(t)%f(:,:,k) = plane_mean(x,k,m,t,dir)
           enddo; enddo
           !$OMP END PARALLEL DO
         case default; stop 'Error: dir must = 1,2,3 in mean_along_dir in SF.f90'
         end select
       end subroutine

       subroutine subtract_mean_along_dir_SF(x,m,dir,x_temp)
         implicit none
         type(SF),intent(inout) :: x,x_temp
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         call mean_along_dir(x_temp,x,m,dir)
         call subtract(x,x_temp)
       end subroutine

       function plane_mean(x_CC,p,m,t,dir) result(x_mean)
         ! Computes  L⁻¹ ∫∫ x_CC dA in plane p along direction dir in grid t
         implicit none
         type(SF),intent(in) :: x_CC
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,p,t
         real(cp) :: x_mean,temp
         integer :: i,j,k
         temp = 0.0_cp
         select case (dir)
         case (1)
           !$OMP PARALLEL DO REDUCTION(+:temp)
           do k=2,m%g(t)%c(3)%sc-1; do j=2,m%g(t)%c(2)%sc-1
             temp = temp + x_CC%RF(t)%f(p,j,k)*m%vol(t)%f(p,j,k)/m%g(t)%c(1)%dhn(p)
           enddo; enddo
           !$OMP END PARALLEL DO
         case (2)
           !$OMP PARALLEL DO REDUCTION(+:temp)
           do k=2,m%g(t)%c(3)%sc-1; do i=2,m%g(t)%c(1)%sc-1
             temp = temp + x_CC%RF(t)%f(i,p,k)*m%vol(t)%f(i,p,k)/m%g(t)%c(2)%dhn(p)
           enddo; enddo
           !$OMP END PARALLEL DO
         case (3)
           !$OMP PARALLEL DO REDUCTION(+:temp)
           do j=2,m%g(t)%c(2)%sc-1; do i=2,m%g(t)%c(1)%sc-1
             temp = temp + x_CC%RF(t)%f(i,j,p)*m%vol(t)%f(i,j,p)/m%g(t)%c(3)%dhn(p)
           enddo; enddo
           !$OMP END PARALLEL DO
         case default; stop 'Error: dir must = 1,2,3 in plane_mean in SF.f90'
         end select
         x_mean = temp/m%g(t)%c(dir)%maxRange
       end function

        subroutine print_physical_SF(f)
          implicit none
          type(SF),intent(in) :: f
          integer :: i
          do i=1,f%s; call print_physical(f%RF(i)); enddo
        end subroutine

        subroutine print_BCs_SF(f,name)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: name
          call display_BCs_SF(f,name,6)
        end subroutine

        subroutine export_BCs_SF(f,dir,name)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          integer :: un
          un = new_and_open(dir,name//'_BoundaryConditions')
          call display_BCs_SF(f,name,un)
          call close_and_message(un,dir,name//'_BoundaryConditions')
        end subroutine

        subroutine display_BCs_SF(f,name,un)
          implicit none
          type(SF),intent(in) :: f
          character(len=*),intent(in) :: name
          integer,intent(in) :: un
          integer :: i
          write(un,*) ' ------ BCs for ' // name // ' ------ '
          do i=1,f%s
            call display(f%RF(i)%b,un)
          enddo
          write(un,*) ' ------------------------------------ '
        end subroutine

        ! ****************************************************************
        ! ****************************************************************
        ! ********************* COMPUTATION ROUTINES *********************
        ! ****************************************************************
        ! ****************************************************************

        subroutine assign_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%RF(i),g%RF(i)); enddo
        end subroutine

        subroutine assign_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%RF(i),g); enddo
        end subroutine
        
        subroutine assign_negative_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign_negative(f%RF(i),g%RF(i)); enddo
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%RF(i),g%RF(i)); enddo
        end subroutine

        subroutine add_SF_SF_SF(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,r
          integer :: i
          do i=1,f%s; call add(f%RF(i),g%RF(i),r%RF(i)); enddo
        end subroutine

        subroutine add_SF_SF_SF_SF(f,g,r,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,r,q
          integer :: i
          do i=1,f%s; call add(f%RF(i),g%RF(i),r%RF(i),q%RF(i)); enddo
        end subroutine

        subroutine add_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%RF(i),g); enddo
        end subroutine

        subroutine add_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call add(g2,f%RF(i)); enddo
        end subroutine

        subroutine add_SF_SF9(A,B1,B2,B3,B4,B5,B6,B7,B8,B9)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B1,B2,B3,B4,B5,B6,B7,B8,B9
          integer :: i
          do i=1,A%s; call add(A%RF(i),B1%RF(i),B2%RF(i),B3%RF(i),&
                                       B4%RF(i),B5%RF(i),B6%RF(i),&
                                       B7%RF(i),B8%RF(i),B9%RF(i)); enddo
        end subroutine

       ! ------------------- ADD PRODUCT ------------------------

        subroutine add_product_SF_SF_S(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          real(cp),intent(in) :: r
          integer :: i
          do i=1,f%s; call add_product(f%RF(i),g%RF(i),r); enddo
        end subroutine

        subroutine add_product_SF_SF_SF(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          type(SF),intent(in) :: r
          integer :: i
          do i=1,f%s; call add_product(f%RF(i),g%RF(i),r%RF(i)); enddo
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%RF(i),g%RF(i)); enddo
        end subroutine

        subroutine subtract_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call subtract(f%RF(i),g%RF(i),q%RF(i)); enddo
        end subroutine

        subroutine subtract_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%RF(i),g); enddo
        end subroutine

        subroutine subtract_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call subtract(g2,f%RF(i)); enddo
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%RF(i),g%RF(i)); enddo
        end subroutine

        subroutine multiply_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call multiply(f%RF(i),g%RF(i),q%RF(i)); enddo
        end subroutine

        subroutine multiply_SF_SF_S(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          real(cp),intent(in) :: q
          integer :: i
          do i=1,f%s; call multiply(f%RF(i),g%RF(i),q); enddo
        end subroutine

        subroutine multiply_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%RF(i),g); enddo
        end subroutine

        subroutine multiply_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call multiply(f%RF(i),g2); enddo
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%RF(i),g%RF(i)); enddo
        end subroutine

        subroutine divide_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call divide(f%RF(i),g%RF(i),q%RF(i)); enddo
        end subroutine

        subroutine divide_SF_S_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: q
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%RF(i),g,q%RF(i)); enddo
        end subroutine

        subroutine divide_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%RF(i),g); enddo
        end subroutine

        subroutine divide_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call divide(g2,f%RF(i)); enddo
        end subroutine

      ! ------------------- OTHER ------------------------

        subroutine invert_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call divide(1.0_cp,f%RF(i)); enddo
        end subroutine

        subroutine square_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call square(f%RF(i)); enddo
        end subroutine

        subroutine swap_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f,g,q
          integer :: i
          do i=1,f%s; call swap(f%RF(i),g%RF(i),q%RF(i)); enddo
        end subroutine

        function min_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval((/m,min(f%RF(i))/))
          enddo
        end function

        function max_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval((/m,max(f%RF(i))/))
          enddo
        end function

        function min_pad_SF(f,pad) result(m)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: pad
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval((/m,min(f%RF(i),pad)/))
          enddo
        end function

        function max_pad_SF(f,pad) result(m)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: pad
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval((/m,max(f%RF(i),pad)/))
          enddo
        end function

        function maxabs_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval(abs((/m,maxabs(f%RF(i))/)))
          enddo
        end function

        function maxabsdiff_SF(a,b) result(m)
          implicit none
          type(SF),intent(in) :: a,b
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,a%s
            m = maxval((/m,maxabsdiff(a%RF(i),b%RF(i))/))
          enddo
        end function

        function mean_SF(a) result(m)
          implicit none
          type(SF),intent(in) :: a
          real(cp) :: m
          integer :: i,s
          m = sum(a)
          s = 0
          do i=1,a%s
            s = s + size(a%RF(i))
          enddo
          m = m/real(s,cp)
        end function

        function sum_SF(a) result(m)
          implicit none
          type(SF),intent(in) :: a
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,a%s
            m = m + sum(a%RF(i))
          enddo
        end function

        function sum_SF_pad(a,pad) result(m)
          implicit none
          type(SF),intent(in) :: a
          integer,intent(in) :: pad
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,a%s
            m = m + sum(a%RF(i),pad)
          enddo
        end function

        function dot_product_SF(A,B,temp) result(dot)
          implicit none
          type(SF),intent(in) :: A,B
          type(SF),intent(inout) :: temp
          real(cp) :: dot
          call multiply(temp,A,B)
          dot = sum(temp)
        end function

        subroutine zero_ghost_xmin_xmax_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: t
          do t=1,f%s; call zero_ghost_xmin_xmax(f%RF(t)); enddo
        end subroutine

        subroutine zero_ghost_ymin_ymax_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: t
          do t=1,f%s; call zero_ghost_ymin_ymax(f%RF(t)); enddo
        end subroutine

        subroutine zero_ghost_zmin_zmax_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: t
          do t=1,f%s; call zero_ghost_zmin_zmax(f%RF(t)); enddo
        end subroutine

      end module