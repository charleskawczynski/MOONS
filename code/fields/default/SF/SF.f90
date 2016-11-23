      module SF_mod
        use current_precision_mod
        use IO_tools_mod
        use data_location_mod
        use mesh_mod
        use mesh_domain_mod
        use boundary_conditions_mod
        use GF_mod
        use block_field_mod
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
        public :: sine_waves
        public :: cosine_waves
        public :: random_noise

        public :: plane_sum_x
        public :: plane_sum_y
        public :: plane_sum_z
        public :: boundary_flux

        public :: symmetry_error_x,symmetry_local_x
        public :: symmetry_error_y,symmetry_local_y
        public :: symmetry_error_z,symmetry_local_z

        public :: mirror_about_hmin,mirror_about_hmax

        public :: multiply_volume
        public :: mean_along_dir,subtract_mean_along_dir
        public :: N0_C1_tensor
        public :: C0_N1_tensor

        public :: assign_ghost_XPeriodic
        public :: assign_ghost_N_XPeriodic
        public :: assign_wall_Dirichlet
        public :: multiply_wall_Neumann

        public :: init_BCs,init_BC_Dirichlet,init_BC_props,init_BC_mesh
        public :: dot_product
        public :: cross_product_x
        public :: cross_product_y
        public :: cross_product_z

        public :: restrict
        public :: prolongate
        public :: reset_index_sets

        public :: laplacian_matrix_based
        public :: curl_curl_matrix_based

        ! Monitoring
        public :: print_BCs
        public :: export_BCs

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: invert
        public :: add_product,product_add,swap
        ! Auxiliary
        public :: square,min,max,amin,amax
        public :: mean,sum,abs,insist_amax_lt_tol

        public :: assign_ghost_xmin_xmax
        public :: assign_ghost_ymin_ymax
        public :: assign_ghost_zmin_zmax

        type SF
          integer :: s ! Number of subdomains in domain decomposition
          type(block_field),dimension(:),allocatable :: BF
          logical :: all_neumann = .false. ! If necessary to subtract mean
          integer :: numEl,numPhysEl ! Number of physical elements, number of physical elements
          real(cp) :: vol

          type(data_location) :: DL
          logical,dimension(3) :: CC_along
          logical,dimension(3) :: N_along
          logical :: is_CC,is_Node,is_face,is_edge
          integer :: face = 0 ! Direction of face data
          integer :: edge = 0 ! Direction of edge data
        end type

        interface init;                    module procedure init_SF_copy;                 end interface
        interface init;                    module procedure init_SF_copy_mesh;            end interface
        interface delete;                  module procedure delete_SF;                    end interface
        interface display;                 module procedure display_SF;                   end interface
        interface print;                   module procedure print_SF;                     end interface
        interface export;                  module procedure export_SF;                    end interface
        interface import;                  module procedure import_SF;                    end interface
        interface export;                  module procedure export_wrapper_SF;            end interface
        interface import;                  module procedure import_wrapper_SF;            end interface

        interface init_CC;                 module procedure init_SF_CC;                   end interface
        interface init_Node;               module procedure init_SF_Node;                 end interface
        interface init_Face;               module procedure init_SF_Face;                 end interface
        interface init_Edge;               module procedure init_SF_Edge;                 end interface

        interface init_CC;                 module procedure init_SF_CC_D;                 end interface
        interface init_Node;               module procedure init_SF_Node_D;               end interface
        interface init_Face;               module procedure init_SF_Face_D;               end interface
        interface init_Edge;               module procedure init_SF_Edge_D;               end interface

        interface init_CC;                 module procedure init_SF_CC_assign;            end interface
        interface init_Node;               module procedure init_SF_Node_assign;          end interface
        interface init_Face;               module procedure init_SF_Face_assign;          end interface
        interface init_Edge;               module procedure init_SF_Edge_assign;          end interface

        interface init_BCs;                module procedure init_BCs_SF_SF;               end interface
        interface init_BC_Dirichlet;       module procedure init_BC_Dirichlet_SF;         end interface
        interface init_BCs;                module procedure init_BC_val_SF;               end interface
        interface init_BC_mesh;            module procedure init_BC_mesh_SF;              end interface
        interface init_BC_props;           module procedure init_BC_props_SF;             end interface

        interface print_BCs;               module procedure print_BCs_SF;                 end interface
        interface export_BCs;              module procedure export_BCs_SF;                end interface

        interface multiply_volume;         module procedure multiply_volume_SF;           end interface
        interface mean_along_dir;          module procedure mean_along_dir_SF;            end interface
        interface subtract_mean_along_dir; module procedure subtract_mean_along_dir_SF;   end interface

        interface volume;                  module procedure volume_SF;                    end interface
        interface sine_waves;              module procedure sine_waves_SF;                end interface
        interface cosine_waves;            module procedure cosine_waves_SF;              end interface
        interface random_noise;            module procedure random_noise_SF;              end interface
        interface random_noise;            module procedure random_noise_SF_dir;          end interface

        interface laplacian_matrix_based;  module procedure laplacian_matrix_based_SF_SF; end interface
        interface laplacian_matrix_based;  module procedure laplacian_matrix_based_VF_SF; end interface
        interface curl_curl_matrix_based;  module procedure curl_curl_matrix_based_SF;    end interface

        interface assign_ghost_XPeriodic;  module procedure assign_ghost_XPeriodic_SF;    end interface
        interface assign_ghost_XPeriodic;  module procedure assign_ghost_XPeriodic_SF2;   end interface
        interface assign_ghost_N_XPeriodic;module procedure assign_ghost_N_XPeriodic_SF;  end interface
        interface assign_ghost_N_XPeriodic;module procedure assign_ghost_N_XPeriodic_SF2; end interface
        interface assign_wall_Dirichlet;   module procedure assign_wall_Dirichlet_SF;     end interface
        interface assign_wall_Dirichlet;   module procedure assign_wall_Dirichlet_SF2;    end interface
        interface multiply_wall_Neumann;   module procedure multiply_wall_Neumann_SF;     end interface
        interface multiply_wall_Neumann;   module procedure multiply_wall_Neumann_SF2;    end interface

        interface plane_sum_x;             module procedure plane_sum_x_SF;               end interface
        interface plane_sum_y;             module procedure plane_sum_y_SF;               end interface
        interface plane_sum_z;             module procedure plane_sum_z_SF;               end interface
        interface boundary_flux;           module procedure boundary_flux_SF;             end interface

        interface symmetry_error_x;        module procedure symmetry_error_x_SF;          end interface
        interface symmetry_error_y;        module procedure symmetry_error_y_SF;          end interface
        interface symmetry_error_z;        module procedure symmetry_error_z_SF;          end interface

        interface symmetry_local_x;        module procedure symmetry_local_x_SF;          end interface
        interface symmetry_local_y;        module procedure symmetry_local_y_SF;          end interface
        interface symmetry_local_z;        module procedure symmetry_local_z_SF;          end interface

        interface mirror_about_hmin;       module procedure mirror_about_hmin_SF;         end interface
        interface mirror_about_hmax;       module procedure mirror_about_hmax_SF;         end interface

        interface assign_ghost_xmin_xmax;  module procedure assign_ghost_xmin_xmax_SF;    end interface
        interface assign_ghost_ymin_ymax;  module procedure assign_ghost_ymin_ymax_SF;    end interface
        interface assign_ghost_zmin_zmax;  module procedure assign_ghost_zmin_zmax_SF;    end interface

        interface cross_product_x;         module procedure cross_product_x_SF;           end interface
        interface cross_product_y;         module procedure cross_product_y_SF;           end interface
        interface cross_product_z;         module procedure cross_product_z_SF;           end interface

        interface restrict;                module procedure restrict_SF;                  end interface
        interface restrict;                module procedure restrict_reset_SF;            end interface
        interface restrict;                module procedure restrict_all_reset_SF;        end interface
        interface prolongate;              module procedure prolongate_SF;                end interface
        interface prolongate;              module procedure prolongate_reset_SF;          end interface
        interface prolongate;              module procedure prolongate_all_reset_SF;      end interface
        interface reset_index_sets;        module procedure reset_index_sets_SF;          end interface

        ! COMPUTATION ROUTINES:

        interface assign;                  module procedure assign_SF_S;                  end interface
        interface assign;                  module procedure assign_SF_SF;                 end interface
        interface assign_negative;         module procedure assign_negative_SF_SF;        end interface

        interface add;                     module procedure add_SF_SF;                    end interface
        interface add;                     module procedure add_SF_SF_SF;                 end interface
        interface add;                     module procedure add_SF_SF_SF_SF;              end interface
        interface add;                     module procedure add_SF_S;                     end interface
        interface add;                     module procedure add_S_SF;                     end interface
        interface add;                     module procedure add_SF_SF9;                   end interface

        interface add_product;             module procedure add_product_SF_SF_S;          end interface
        interface add_product;             module procedure add_product_SF_SF_SF;         end interface
        interface product_add;             module procedure product_add_SF_SF_S;          end interface
        interface product_add;             module procedure product_add_SF_SF_SF;         end interface

        interface multiply;                module procedure multiply_SF_SF;               end interface
        interface multiply;                module procedure multiply_SF_SF_SF;            end interface
        interface multiply;                module procedure multiply_SF_SF_S;             end interface
        interface multiply;                module procedure multiply_SF_S;                end interface
        interface multiply;                module procedure multiply_S_SF;                end interface

        interface subtract;                module procedure subtract_SF_SF;               end interface
        interface subtract;                module procedure subtract_SF_SF_SF;            end interface
        interface subtract;                module procedure subtract_SF_S;                end interface
        interface subtract;                module procedure subtract_S_SF;                end interface

        interface divide;                  module procedure divide_SF_SF;                 end interface
        interface divide;                  module procedure divide_SF_SF_SF;              end interface
        interface divide;                  module procedure divide_SF_S_SF;               end interface
        interface divide;                  module procedure divide_SF_S;                  end interface
        interface divide;                  module procedure divide_S_SF;                  end interface

        interface invert;                  module procedure invert_SF;                    end interface
        interface square;                  module procedure square_SF;                    end interface
        interface abs;                     module procedure abs_SF;                       end interface
        interface insist_amax_lt_tol;      module procedure insist_amax_lt_tol_SF;        end interface

        interface swap;                    module procedure swap_SF;                      end interface
        interface min;                     module procedure min_SF;                       end interface
        interface max;                     module procedure max_SF;                       end interface
        interface min;                     module procedure min_pad_SF;                   end interface
        interface max;                     module procedure max_pad_SF;                   end interface
        interface amin;                    module procedure amin_SF;                      end interface
        interface amax;                    module procedure amax_SF;                      end interface
        interface mean;                    module procedure mean_SF;                      end interface
        interface sum;                     module procedure sum_SF;                       end interface
        interface sum;                     module procedure sum_SF_pad;                   end interface
        interface dot_product;             module procedure dot_product_SF;               end interface

      contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

        subroutine init_SF_copy(f,f_in)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: f_in
          integer :: i
          call delete(f)
          allocate(f%BF(f_in%s)); f%s = f_in%s
          call init(f%DL,f_in%DL)
          do i=1,f%s; call init(f%BF(i),f_in%BF(i)); enddo
          f%numEl = f_in%numEl
          f%numPhysEl = f_in%numPhysEl
          f%is_CC = f_in%is_CC
          f%is_node = f_in%is_node
          f%is_face = f_in%is_face
          f%is_edge = f_in%is_edge

          f%face = f_in%face
          f%edge = f_in%edge
          f%CC_along = f_in%CC_along
          f%N_along = f_in%N_along
        end subroutine

        subroutine init_SF_copy_mesh(f,f_in,m)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: f_in
          type(mesh),intent(in) :: m
          type(SF) :: temp
          if (f_in%is_CC) then;       call init_CC(temp,m)
          elseif (f_in%is_Node) then; call init_Node(temp,m)
          elseif (f_in%is_Face) then; call init_Face(temp,m,f_in%face)
          elseif (f_in%is_Edge) then; call init_Edge(temp,m,f_in%edge)
          else; stop 'Error: bad datatype in init_SF_copy_mesh in SF.f90'
          endif
          call init(f,temp)
          call delete(temp)
        end subroutine

        subroutine delete_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          if (allocated(f%BF)) then
            do i=1,f%s; call delete(f%BF(i)); enddo
            deallocate(f%BF)
          endif
          call delete(f%DL)
          f%s = 0
          f%numEl = 0
          f%numPhysEl = 0
        end subroutine

        subroutine display_SF(f,un)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: un
          integer :: i
          call display(f%DL,un)
          do i=1,f%s; call display(f%BF(i),un); enddo
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
          do i=1,f%s; call export(f%BF(i),un); enddo
          call export(f%DL,un)
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
          allocate(f%BF(f%s))
          do i=1,f%s; call import(f%BF(i),un); enddo
          call import(f%DL,un)
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
          f%numEl = f%numEl + f%BF(i)%GF%s(1)*f%BF(i)%GF%s(2)*f%BF(i)%GF%s(3)
          f%numPhysEl = f%numPhysEl + (f%BF(i)%GF%s(1)-2)*(f%BF(i)%GF%s(2)-2)*(f%BF(i)%GF%s(3)-2)
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
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_CC(f%BF(i),m%B(i)); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_CC = .true.
          call init_CC(f%DL)
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_CC_D(f,m,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          if (compare(m,MD%m_R1)) then;     call init_CC(f,MD%m_R2)
          elseif (compare(m,MD%m_R2)) then; call init_CC(f,MD%m_R1)
          else; stop 'Error: case not found in init_SF_CC_D in SF.f90'
          endif
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
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_Face(f%BF(i),m%B(i),dir); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_face = .true.
          f%face = dir
          call init_Face(f%DL,dir)
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_Face_D(f,m,dir,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          type(mesh_domain),intent(in) :: MD
          if (compare(m,MD%m_R1)) then;     call init_Face(f,MD%m_R2,dir)
          elseif (compare(m,MD%m_R2)) then; call init_Face(f,MD%m_R1,dir)
          else; stop 'Error: case not found in init_SF_Face_D in SF.f90'
          endif
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
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_Edge(f%BF(i),m%B(i),dir); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_edge = .true.
          f%edge = dir
          call init_edge(f%DL,dir)
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_Edge_D(f,m,dir,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          type(mesh_domain),intent(in) :: MD
          if (compare(m,MD%m_R1)) then;     call init_Edge(f,MD%m_R2,dir)
          elseif (compare(m,MD%m_R2)) then; call init_Edge(f,MD%m_R1,dir)
          else; stop 'Error: case not found in init_SF_Edge_D in SF.f90'
          endif
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
          allocate(f%BF(m%s)); f%s = m%s
          do i=1,f%s; call init_Node(f%BF(i),m%B(i)); enddo
          call deleteDataLocation(f)
          call computeNumEl(f)
          f%is_node = .true.
          call init_Node(f%DL)
          call init_CC_N_along(f)
        end subroutine

        subroutine init_SF_Node_D(f,m,MD)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          if (compare(m,MD%m_R1)) then;     call init_Node(f,MD%m_R2)
          elseif (compare(m,MD%m_R2)) then; call init_Node(f,MD%m_R1)
          else; stop 'Error: case not found in init_SF_Node_D in SF.f90'
          endif
        end subroutine

        subroutine init_SF_Node_assign(f,m,val)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f,m); call assign(f,val)
        end subroutine

        ! ***********************************************************
        ! *************************** BCS ***************************
        ! ***********************************************************

        subroutine init_BCs_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call init(f%BF(i)%BCs,g%BF(i)%BCs); enddo
        end subroutine

        subroutine init_BC_Dirichlet_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call init_Dirichlet(f%BF(i)%BCs); enddo
        end subroutine

        subroutine init_BC_val_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: i
          do i=1,f%s; call init_BCs(f%BF(i),val); enddo
          f%all_Neumann = all((/(f%BF(i)%BCs%BCL%all_Neumann,i=1,f%s)/))
          call init_BC_props(f)
        end subroutine

        subroutine init_BC_mesh_SF(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,f%s; call init_BCs(f%BF(i),m%B(i),f%DL); enddo
        end subroutine

        subroutine init_BC_props_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s
            call init_BC_props(f%BF(i))
          enddo
        end subroutine

        ! ***********************************************************
        ! ***********************************************************
        ! ***********************************************************

        subroutine volume_SF(u,m) ! Computes: volume(x(i),y(j),z(k)) = dx(i) dy(j) dz(k)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          call assign(u,0.0_cp)
          do i=1,m%s
            call volume(u%BF(i),m%B(i),u%DL)
          enddo
          u%vol = sum(u)
        end subroutine

        subroutine sine_waves_SF(u,m,wavenum,phi)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          real(cp),dimension(3),intent(in) :: wavenum,phi
          integer :: i
          do i=1,m%s; call sine_waves(u%BF(i),m%B(i),wavenum,phi,u%DL); enddo
        end subroutine

        subroutine cosine_waves_SF(u,m,wavenum,phi)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          real(cp),dimension(3),intent(in) :: wavenum,phi
          integer :: i
          do i=1,m%s; call cosine_waves(u%BF(i),m%B(i),wavenum,phi,u%DL); enddo
        end subroutine

        subroutine random_noise_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call random_noise(u%BF(i)); enddo
        end subroutine

        subroutine random_noise_SF_dir(u,dir)
          implicit none
          type(SF),intent(inout) :: u
          integer,intent(in) :: dir
          integer :: i
          do i=1,u%s; call random_noise(u%BF(i),dir); enddo
        end subroutine

        subroutine laplacian_matrix_based_SF_SF(lapU,U,m)
          implicit none
          type(SF),intent(inout) :: lapU
          type(SF),intent(in) :: U
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,m%s; call laplacian_matrix_based(lapU%BF(i),U%BF(i),m%B(i)); enddo
        end subroutine

        subroutine laplacian_matrix_based_VF_SF(lapX,lapY,lapZ,X,Y,Z,m)
          implicit none
          type(SF),intent(inout) :: lapX,lapY,lapZ
          type(SF),intent(in) :: X,Y,Z
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,m%s; call laplacian_matrix_based(lapX%BF(i),lapY%BF(i),lapZ%BF(i),&
                                                  X%BF(i),Y%BF(i),Z%BF(i),&
                                                  m%B(i)); enddo
        end subroutine

        subroutine curl_curl_matrix_based_SF(CX,CY,CZ,X,Y,Z,m)
          implicit none
          type(SF),intent(inout) :: CX,CY,CZ
          type(SF),intent(in) :: X,Y,Z
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,m%s; call curl_curl_matrix_based(CX%BF(i),CY%BF(i),CZ%BF(i),&
                                                  X%BF(i),Y%BF(i),Z%BF(i),&
                                                  m%B(i)); enddo
        end subroutine

        subroutine assign_ghost_XPeriodic_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_ghost_XPeriodic(u%BF(i),val); enddo
        end subroutine
        subroutine assign_ghost_XPeriodic_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_ghost_XPeriodic(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine assign_ghost_N_XPeriodic_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_ghost_N_XPeriodic(u%BF(i),val); enddo
        end subroutine
        subroutine assign_ghost_N_XPeriodic_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_ghost_N_XPeriodic(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine assign_wall_Dirichlet_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call assign_wall_Dirichlet(u%BF(i),val); enddo
        end subroutine
        subroutine assign_wall_Dirichlet_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call assign_wall_Dirichlet(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        subroutine multiply_wall_Neumann_SF(u,val)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          integer :: i
          do i=1,u%s; call multiply_wall_Neumann(u%BF(i),val); enddo
        end subroutine
        subroutine multiply_wall_Neumann_SF2(u,val,u_with_BCs)
          implicit none
          type(SF),intent(inout) :: u
          real(cp),intent(in) :: val
          type(SF),intent(in) :: u_with_BCs
          integer :: i
          do i=1,u%s; call multiply_wall_Neumann(u%BF(i),val,u_with_BCs%BF(i)); enddo
        end subroutine

        function plane_sum_x_SF(u,m,p) result(SP)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: p
          integer :: i
          real(cp) :: SP
          SP = 0.0_cp
          do i=1,m%s; SP = SP + plane_sum_x(u%BF(i),m%B(i),p); enddo
        end function

        function plane_sum_y_SF(u,m,p) result(SP)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: p
          integer :: i
          real(cp) :: SP
          SP = 0.0_cp
          do i=1,m%s; SP = SP + plane_sum_y(u%BF(i),m%B(i),p); enddo
        end function

        function plane_sum_z_SF(u,m,p) result(SP)
          implicit none
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: p
          integer :: i
          real(cp) :: SP
          SP = 0.0_cp
          do i=1,m%s; SP = SP + plane_sum_z(u%BF(i),m%B(i),p); enddo
        end function

        function boundary_flux_SF(x,y,z,m) result(BF)
          implicit none
          type(SF),intent(in) :: x,y,z
          type(mesh),intent(in) :: m
          integer :: i
          real(cp) :: BF
          BF = 0.0_cp
          do i=1,m%s; BF = BF + boundary_flux(x%BF(i),y%BF(i),z%BF(i),m%B(i)); enddo
        end function

        function symmetry_error_x_SF(u) result(SE)
          implicit none
          type(SF),intent(in) :: u
          integer :: i
          real(cp) :: SE
          SE = 0.0_cp
          do i=1,u%s; SE = SE + symmetry_error_x(u%BF(i)); enddo
        end function

        function symmetry_error_y_SF(u) result(SE)
          implicit none
          type(SF),intent(in) :: u
          integer :: i
          real(cp) :: SE
          SE = 0.0_cp
          do i=1,u%s; SE = SE + symmetry_error_y(u%BF(i)); enddo
        end function

        function symmetry_error_z_SF(u) result(SE)
          implicit none
          type(SF),intent(in) :: u
          integer :: i
          real(cp) :: SE
          SE = 0.0_cp
          do i=1,u%s; SE = SE + symmetry_error_z(u%BF(i)); enddo
        end function

        subroutine symmetry_local_x_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call symmetry_local_x(u%BF(i)); enddo
        end subroutine

        subroutine symmetry_local_y_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call symmetry_local_y(u%BF(i)); enddo
        end subroutine

        subroutine symmetry_local_z_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call symmetry_local_z(u%BF(i)); enddo
        end subroutine

        subroutine mirror_about_hmin_SF(u,dir,mirror_sign)
          implicit none
          type(SF),intent(inout) :: u
          integer,intent(in) :: dir
          real(cp),intent(in) :: mirror_sign
          integer :: i
          do i=1,u%s; call mirror_about_hmin(u%BF(i),dir,mirror_sign); enddo
        end subroutine
        subroutine mirror_about_hmax_SF(u,dir,mirror_sign)
          implicit none
          type(SF),intent(inout) :: u
          integer,intent(in) :: dir
          real(cp),intent(in) :: mirror_sign
          integer :: i
          do i=1,u%s; call mirror_about_hmax(u%BF(i),dir,mirror_sign); enddo
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
         ! Computes  L⁻¹ ∫∫ x_CC dA in plane p along direction dir in grid t
         implicit none
         type(SF),intent(inout) :: x_mean
         type(SF),intent(in) :: x
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer :: t
         select case (dir)
         case (1); do t=1,m%s; call mean_along_x(x_mean%BF(t)%GF,x%BF(t)%GF,m%B(t)%g); enddo
         case (2); do t=1,m%s; call mean_along_y(x_mean%BF(t)%GF,x%BF(t)%GF,m%B(t)%g); enddo
         case (3); do t=1,m%s; call mean_along_z(x_mean%BF(t)%GF,x%BF(t)%GF,m%B(t)%g); enddo
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
            call display(f%BF(i)%BCs,un)
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
          do i=1,f%s; call assign(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine assign_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine assign_negative_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign_negative(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_SF_SF(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,r
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g%BF(i)%GF,r%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_SF_SF_SF(f,g,r,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,r,q
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g%BF(i)%GF,r%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine add_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call add(g2,f%BF(i)%GF); enddo
        end subroutine

        subroutine add_SF_SF9(A,B1,B2,B3,B4,B5,B6,B7,B8,B9)
          implicit none
          type(SF),intent(inout) :: A
          type(SF),intent(in) :: B1,B2,B3,B4,B5,B6,B7,B8,B9
          integer :: i
          do i=1,A%s; call add(A%BF(i)%GF,B1%BF(i)%GF,B2%BF(i)%GF,B3%BF(i)%GF,&
                                       B4%BF(i)%GF,B5%BF(i)%GF,B6%BF(i)%GF,&
                                       B7%BF(i)%GF,B8%BF(i)%GF,B9%BF(i)%GF); enddo
        end subroutine

       ! ------------------- ADD PRODUCT ------------------------

        subroutine add_product_SF_SF_S(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          real(cp),intent(in) :: r
          integer :: i
          do i=1,f%s; call add_product(f%BF(i)%GF,g%BF(i)%GF,r); enddo
        end subroutine

        subroutine add_product_SF_SF_SF(f,g,r)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          type(SF),intent(in) :: r
          integer :: i
          do i=1,f%s; call add_product(f%BF(i)%GF,g%BF(i)%GF,r%BF(i)%GF); enddo
        end subroutine

        subroutine product_add_SF_SF_S(f,r,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: r
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call product_add(f%BF(i)%GF,r,g%BF(i)%GF); enddo
        end subroutine

        subroutine product_add_SF_SF_SF(f,r,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: r
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call product_add(f%BF(i)%GF,r%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine subtract_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call subtract(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine subtract_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine subtract_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call subtract(g2,f%BF(i)%GF); enddo
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine multiply_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine multiply_SF_SF_S(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          real(cp),intent(in) :: q
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g%BF(i)%GF,q); enddo
        end subroutine

        subroutine multiply_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine multiply_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call multiply(f%BF(i)%GF,g2); enddo
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g%BF(i)%GF); enddo
        end subroutine

        subroutine divide_SF_SF_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        subroutine divide_SF_S_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: q
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g,q%BF(i)%GF); enddo
        end subroutine

        subroutine divide_SF_S(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call divide(f%BF(i)%GF,g); enddo
        end subroutine

        subroutine divide_S_SF(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call divide(g2,f%BF(i)%GF); enddo
        end subroutine

      ! ------------------- OTHER ------------------------

        subroutine invert_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call divide(1.0_cp,f%BF(i)%GF); enddo
        end subroutine

        subroutine square_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call square(f%BF(i)%GF); enddo
        end subroutine

        subroutine abs_SF(u)
          implicit none
          type(SF),intent(inout) :: u
          integer :: i
          do i=1,u%s; call abs(u%BF(i)); enddo
        end subroutine

        subroutine insist_amax_lt_tol_SF(u,caller)
          implicit none
          type(SF),intent(in) :: u
          character(len=*),intent(in) :: caller
          integer :: i
          do i=1,u%s; call insist_amax_lt_tol(u%BF(i),caller); enddo
        end subroutine

        subroutine swap_SF(f,g,q)
          implicit none
          type(SF),intent(inout) :: f,g,q
          integer :: i
          do i=1,f%s; call swap(f%BF(i)%GF,g%BF(i)%GF,q%BF(i)%GF); enddo
        end subroutine

        function min_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval((/m,min(f%BF(i)%GF)/))
          enddo
        end function

        function max_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval((/m,max(f%BF(i)%GF)/))
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
            m = minval((/m,min(f%BF(i)%GF,pad)/))
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
            m = maxval((/m,max(f%BF(i)%GF,pad)/))
          enddo
        end function

        function amin_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = minval(abs((/m,amin(f%BF(i)%GF)/)))
          enddo
        end function

        function amax_SF(f) result(m)
          implicit none
          type(SF),intent(in) :: f
          real(cp) :: m
          integer :: i
          m = 0.0_cp
          do i=1,f%s
            m = maxval(abs((/m,amax(f%BF(i)%GF)/)))
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
            s = s + size(a%BF(i)%GF)
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
            m = m + sum(a%BF(i)%GF)
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
            m = m + sum(a%BF(i)%GF,pad)
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

        subroutine assign_ghost_xmin_xmax_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: t
          do t=1,f%s; call assign_ghost_xmin_xmax(f%BF(t),val); enddo
        end subroutine
        subroutine assign_ghost_ymin_ymax_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: t
          do t=1,f%s; call assign_ghost_ymin_ymax(f%BF(t),val); enddo
        end subroutine
        subroutine assign_ghost_zmin_zmax_SF(f,val)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: val
          integer :: t
          do t=1,f%s; call assign_ghost_zmin_zmax(f%BF(t),val); enddo
        end subroutine

        subroutine cross_product_x_SF(ACrossB,Ay,Az,By,Bz)
          implicit none
          type(SF),intent(inout) :: ACrossB
          type(SF),intent(in) :: Ay,Az,By,Bz
          integer :: t
          do t=1,ACrossB%s
            call cross_product_x(ACrossB%BF(t),Ay%BF(t),Az%BF(t),By%BF(t),Bz%BF(t))
          enddo
        end subroutine
        subroutine cross_product_y_SF(ACrossB,Ax,Az,Bx,Bz)
          implicit none
          type(SF),intent(inout) :: ACrossB
          type(SF),intent(in) :: Ax,Az,Bx,Bz
          integer :: t
          do t=1,ACrossB%s
            call cross_product_y(ACrossB%BF(t),Ax%BF(t),Az%BF(t),Bx%BF(t),Bz%BF(t))
          enddo
        end subroutine
        subroutine cross_product_z_SF(ACrossB,Ax,Ay,Bx,By)
          implicit none
          type(SF),intent(inout) :: ACrossB
          type(SF),intent(in) :: Ax,Ay,Bx,By
          integer :: t
          do t=1,ACrossB%s
            call cross_product_z(ACrossB%BF(t),Ax%BF(t),Ay%BF(t),Bx%BF(t),By%BF(t))
          enddo
        end subroutine

        subroutine restrict_SF(r,u,m,dir)
          implicit none
          type(SF),intent(inout) :: r
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call restrict(r%BF(t),u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine restrict_reset_SF(u,m,dir)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call restrict(u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine restrict_all_reset_SF(u,m)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,3; call restrict(u,m,i); enddo
        end subroutine

        subroutine prolongate_SF(r,u,m,dir)
          implicit none
          type(SF),intent(inout) :: r
          type(SF),intent(in) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call prolongate(r%BF(t),u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine prolongate_reset_SF(u,m,dir)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: t
          do t=1,m%s; call prolongate(u%BF(t),m%B(t),dir); enddo
        end subroutine

        subroutine prolongate_all_reset_SF(u,m)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,3; call prolongate(u,m,i); enddo
        end subroutine

        subroutine reset_index_sets_SF(u,m)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer :: i
          do i=1,m%s; call reset_index_sets(u%BF(i),m%B(i)); enddo
        end subroutine

      end module