      module VF_mod

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
        use data_location_mod
        use mesh_mod
        use mesh_domain_mod
        use SF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: VF
        public :: init,delete,display,print,export,import ! Essentials

        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: init_Face_compliment
        public :: init_Edge_compliment

        public :: init_BCs
        public :: init_BC_props
        public :: multiply_volume

        public :: volume
        public :: sine_waves
        public :: cosine_waves
        public :: random_noise

        public :: assign_ghost_XPeriodic
        public :: assign_ghost_N_XPeriodic
        public :: assign_wall_Dirichlet
        public :: multiply_wall_Neumann

        public :: symmetry_error_x,symmetry_local_x
        public :: symmetry_error_y,symmetry_local_y
        public :: symmetry_error_z,symmetry_local_z

        public :: dot_product,dot
        public :: cross_product

        public :: restrict
        public :: prolongate

        ! Monitoring
        public :: print_BCs
        public :: export_BCs

        ! Operators
        public :: assign,assign_negative
        public :: add,subtract
        public :: multiply,divide
        public :: add_product
        public :: product_add
        public :: square,invert,abs,insist_amax_lt_tol
        public :: mean,max,amin,amax
        public :: boundary_flux
        ! public :: sum
        public :: assignX,assignY,assignZ

        public :: laplacian_matrix_based
        public :: curl_curl_matrix_based

        public :: is_collocated
        public :: insist_collocated
        public :: get_DL

        type VF
          ! integer :: s = 3  ! number of components
          type(SF) :: x,y,z ! components

          logical :: is_CC,is_Node,is_Face,is_Edge
        end type

        interface init;                    module procedure init_VF_copy_VF;             end interface
        interface init;                    module procedure init_VF_copy_SF;             end interface
        interface init;                    module procedure init_VF_copy_VF_mesh;        end interface
        interface delete;                  module procedure delete_VF;                   end interface
        interface display;                 module procedure display_VF;                  end interface
        interface print;                   module procedure print_VF;                    end interface
        interface export;                  module procedure export_VF;                   end interface
        interface import;                  module procedure import_VF;                   end interface
        interface export;                  module procedure export_VF_wrapper;           end interface
        interface import;                  module procedure import_VF_wrapper;           end interface

        interface init_CC;                 module procedure init_VF_CC;                  end interface
        interface init_Face;               module procedure init_VF_Face;                end interface
        interface init_Edge;               module procedure init_VF_Edge;                end interface
        interface init_Node;               module procedure init_VF_Node;                end interface

        interface init_CC;                 module procedure init_VF_CC_MD;               end interface
        interface init_Face;               module procedure init_VF_Face_MD;             end interface
        interface init_Edge;               module procedure init_VF_Edge_MD;             end interface
        interface init_Node;               module procedure init_VF_Node_MD;             end interface

        interface init_Face_compliment;    module procedure init_VF_Face_compliment;     end interface
        interface init_Edge_compliment;    module procedure init_VF_Edge_compliment;     end interface

        interface init_CC;                 module procedure init_VF_CC_assign;           end interface
        interface init_Face;               module procedure init_VF_Face_assign;         end interface
        interface init_Edge;               module procedure init_VF_Edge_assign;         end interface
        interface init_Node;               module procedure init_VF_Node_assign;         end interface
        interface multiply_volume;         module procedure multiply_volume_VF;          end interface

        interface volume;                  module procedure volume_VF;                   end interface
        interface sine_waves;              module procedure sine_waves_VF;               end interface
        interface cosine_waves;            module procedure cosine_waves_VF;             end interface
        interface random_noise;            module procedure random_noise_VF;             end interface

        interface symmetry_error_x;        module procedure symmetry_error_x_VF;         end interface
        interface symmetry_error_y;        module procedure symmetry_error_y_VF;         end interface
        interface symmetry_error_z;        module procedure symmetry_error_z_VF;         end interface

        interface symmetry_local_x;        module procedure symmetry_local_x_VF;         end interface
        interface symmetry_local_y;        module procedure symmetry_local_y_VF;         end interface
        interface symmetry_local_z;        module procedure symmetry_local_z_VF;         end interface

        interface assign_ghost_XPeriodic;  module procedure assign_ghost_XPeriodic_VF;   end interface
        interface assign_ghost_XPeriodic;  module procedure assign_ghost_XPeriodic_VF2;  end interface
        interface assign_ghost_N_XPeriodic;module procedure assign_ghost_N_XPeriodic_VF; end interface
        interface assign_ghost_N_XPeriodic;module procedure assign_ghost_N_XPeriodic_VF2;end interface
        interface assign_wall_Dirichlet;   module procedure assign_wall_Dirichlet_VF;    end interface
        interface assign_wall_Dirichlet;   module procedure assign_wall_Dirichlet_VF2;   end interface
        interface multiply_wall_Neumann;   module procedure multiply_wall_Neumann_VF;    end interface
        interface multiply_wall_Neumann;   module procedure multiply_wall_Neumann_VF2;   end interface

        interface dot_product;             module procedure dot_product_VF;              end interface
        interface dot;                     module procedure dot_VF_SF;                   end interface

        interface cross_product;           module procedure cross_product_VF;            end interface

        interface restrict;                module procedure restrict_VF;                 end interface
        interface restrict;                module procedure restrict_dir_VF;             end interface
        interface prolongate;              module procedure prolongate_VF;               end interface
        interface prolongate;              module procedure prolongate_dir_VF;           end interface

        interface curl_curl_matrix_based;  module procedure curl_curl_matrix_based_VF;   end interface
        interface Laplacian_matrix_based;  module procedure Laplacian_matrix_based_VF;   end interface

        interface is_collocated;           module procedure is_collocated_VF;            end interface
        interface insist_collocated;       module procedure insist_collocated_VF;        end interface
        interface get_DL;                  module procedure get_DL_VF;                   end interface

        interface print_BCs;               module procedure print_BCs_VF;                end interface
        interface init_BCs;                module procedure init_BCs_VF_VF;              end interface
        interface export_BCs;              module procedure export_BCs_VF;               end interface
        interface init_BC_props;           module procedure init_BC_props_VF;            end interface

        ! COMPUTATION ROUTINES

        interface assignX;                 module procedure assign_VF_VF_X;              end interface
        interface assignY;                 module procedure assign_VF_VF_Y;              end interface
        interface assignZ;                 module procedure assign_VF_VF_Z;              end interface
        interface assignX;                 module procedure assign_VF_S_X;               end interface
        interface assignY;                 module procedure assign_VF_S_Y;               end interface
        interface assignZ;                 module procedure assign_VF_S_Z;               end interface

        interface assign;                  module procedure assign_VF_S;                 end interface
        interface assign;                  module procedure assign_VF_SF;                end interface
        interface assign;                  module procedure assign_VF_VF;                end interface
        interface assign_negative;         module procedure assign_negative_VF_VF;       end interface

        interface add;                     module procedure add_VF_VF;                   end interface
        interface add;                     module procedure add_VF_VF_VF;                end interface
        interface add;                     module procedure add_VF_SF;                   end interface
        interface add;                     module procedure add_SF_VF;                   end interface
        interface add;                     module procedure add_VF_S;                    end interface
        interface add;                     module procedure add_S_VF;                    end interface

        interface add_product;             module procedure add_product_VF_VF_S;         end interface
        interface product_add;             module procedure product_add_VF_VF_S;         end interface

        interface subtract;                module procedure subtract_VF_VF;              end interface
        interface subtract;                module procedure subtract_VF_VF_VF;           end interface
        interface subtract;                module procedure subtract_VF_SF;              end interface
        interface subtract;                module procedure subtract_VF_S;               end interface
        interface subtract;                module procedure subtract_S_VF;               end interface

        interface multiply;                module procedure multiply_VF_VF;              end interface
        interface multiply;                module procedure multiply_VF_VF_VF;           end interface
        interface multiply;                module procedure multiply_VF_VF_SF;           end interface
        interface multiply;                module procedure multiply_VF_VF_S;            end interface
        interface multiply;                module procedure multiply_VF_SF;              end interface
        interface multiply;                module procedure multiply_SF_VF;              end interface
        interface multiply;                module procedure multiply_VF_S;               end interface
        interface multiply;                module procedure multiply_VF_S3;              end interface
        interface multiply;                module procedure multiply_S_VF;               end interface

        interface divide;                  module procedure divide_VF_VF;                end interface
        interface divide;                  module procedure divide_VF_SF;                end interface
        interface divide;                  module procedure divide_VF_S_VF;              end interface
        interface divide;                  module procedure divide_VF_S;                 end interface
        interface divide;                  module procedure divide_S_VF;                 end interface

        interface invert;                  module procedure invert_VF;                   end interface
        interface mean;                    module procedure mean_VF;                     end interface
        interface max;                     module procedure max_VF;                      end interface
        interface amax;                    module procedure amax_VF;                     end interface
        interface amin;                    module procedure amin_VF;                     end interface
        interface boundary_flux;           module procedure boundary_flux_VF;            end interface

        interface square;                  module procedure square_VF;                   end interface
        interface abs;                     module procedure abs_VF;                      end interface
        interface insist_amax_lt_tol;      module procedure insist_amax_lt_tol_VF;       end interface

        contains

        ! **********************************************************
        ! ********************* ESSENTIALS *************************
        ! **********************************************************

        subroutine init_VF_copy_VF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          call init(f1%x,f2%x); call init(f1%y,f2%y); call init(f1%z,f2%z)
          call copy_props_VF(f1,f2)
        end subroutine

        subroutine init_VF_copy_VF_mesh(f1,f2,m)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          type(mesh),intent(in) :: m
          call init(f1%x,f2%x,m); call init(f1%y,f2%y,m); call init(f1%z,f2%z,m)
          call copy_props_VF(f1,f2)
        end subroutine

        subroutine init_VF_copy_SF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
          f1%is_CC = f2%is_CC
          f1%is_Node = f2%is_Node
          f1%is_Face = f2%is_Face
          f1%is_Edge = f2%is_Edge
        end subroutine

        subroutine copy_props_VF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          f1%is_CC = f2%is_CC
          f1%is_Node = f2%is_Node
          f1%is_Face = f2%is_Face
          f1%is_Edge = f2%is_Edge
        end subroutine

        subroutine delete_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call delete(f%x); call delete(f%y); call delete(f%z)
          call delete_logicals(f)
        end subroutine

        subroutine display_VF(f,un)
          implicit none
          type(VF),intent(in) :: f
          integer,intent(in) :: un
          call display(f%x,un); call display(f%y,un); call display(f%z,un)
        end subroutine

        subroutine print_VF(f)
          implicit none
          type(VF),intent(in) :: f
          call display(f,6)
        end subroutine

        subroutine export_VF(f,un)
          implicit none
          type(VF),intent(in) :: f
          integer,intent(in) :: un
          call export(f%x,un); call export(f%y,un); call export(f%z,un)
        end subroutine

        subroutine import_VF(f,un)
          implicit none
          type(VF),intent(inout) :: f
          integer,intent(in) :: un
          call import(f%x,un); call import(f%y,un); call import(f%z,un)
        end subroutine

        subroutine export_VF_wrapper(f,dir,name)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          call export(f%x,dir,name//'_x'); call export(f%y,dir,name//'_y'); call export(f%z,dir,name//'_z')
        end subroutine

        subroutine import_VF_wrapper(f,dir,name)
          implicit none
          type(VF),intent(inout) :: f
          character(len=*),intent(in) :: dir,name
          call import(f%x,dir,name//'_x'); call import(f%y,dir,name//'_y'); call import(f%z,dir,name//'_z')
        end subroutine

        ! **********************************************************
        ! **********************************************************
        ! **********************************************************

        subroutine delete_logicals(f)
          implicit none
          type(VF),intent(inout) :: f
          f%is_CC = .false.
          f%is_Node = .false.
          f%is_Face = .false.
          f%is_Edge = .false.
        end subroutine

        subroutine print_BCs_VF(f,name)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: name
          call print_BCs(f%x,name//'_x')
          call print_BCs(f%y,name//'_y')
          call print_BCs(f%z,name//'_z')
        end subroutine

        subroutine init_BCs_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call init_BCs(f%x,g%x)
          call init_BCs(f%y,g%y)
          call init_BCs(f%z,g%z)
        end subroutine

        subroutine init_BC_props_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call init_BC_props(f%x)
          call init_BC_props(f%y)
          call init_BC_props(f%z)
        end subroutine

        subroutine export_BCs_VF(f,dir,name)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          call export_BCs(f%x,dir,name//'_x')
          call export_BCs(f%y,dir,name//'_y')
          call export_BCs(f%z,dir,name//'_z')
        end subroutine

        function is_collocated_VF(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = is_collocated((/f%x%DL,f%y%DL,f%z%DL/))
        end function

        function get_DL_VF(f) result(DL)
          implicit none
          type(VF),intent(in) :: f
          type(data_location),dimension(3) :: DL
          DL = (/f%x%DL,f%y%DL,f%z%DL/)
        end function

        subroutine insist_collocated_VF(f,caller)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: caller
          if (.not.is_collocated(f)) then
            call print(f%x%DL)
            call print(f%y%DL)
            call print(f%z%DL)
            write(*,*) 'Error: DLs are not collocated in '//caller//' in VF.f90'
            stop 'Done'
          endif
        end subroutine

        subroutine volume_VF(u,m)
          implicit none
          type(VF),intent(inout) :: u
          type(mesh),intent(in) :: m
          call volume(u%x,m); call volume(u%y,m); call volume(u%z,m)
        end subroutine

        subroutine sine_waves_VF(u,m,wavenum,phi)
          implicit none
          type(VF),intent(inout) :: u
          type(mesh),intent(in) :: m
          real(cp),dimension(3),intent(in) :: wavenum,phi
          call sine_waves(u%x,m,wavenum,phi)
          call sine_waves(u%y,m,wavenum,phi)
          call sine_waves(u%z,m,wavenum,phi)
        end subroutine

        subroutine cosine_waves_VF(u,m,wavenum,phi)
          implicit none
          type(VF),intent(inout) :: u
          type(mesh),intent(in) :: m
          real(cp),dimension(3),intent(in) :: wavenum,phi
          call cosine_waves(u%x,m,wavenum,phi)
          call cosine_waves(u%y,m,wavenum,phi)
          call cosine_waves(u%z,m,wavenum,phi)
        end subroutine

        subroutine random_noise_VF(u)
          implicit none
          type(VF),intent(inout) :: u
          call random_noise(u%x); call random_noise(u%y); call random_noise(u%z)
        end subroutine

        subroutine laplacian_matrix_based_VF(lapU,U,m)
          implicit none
          type(VF),intent(inout) :: lapU
          type(VF),intent(in) :: U
          type(mesh),intent(in) :: m
          call laplacian_matrix_based(lapU%x,lapU%y,lapU%z,U%x,U%y,U%z,m)
        end subroutine

        subroutine curl_curl_matrix_based_VF(curl_curlU,U,m)
          implicit none
          type(VF),intent(inout) :: curl_curlU
          type(VF),intent(in) :: U
          type(mesh),intent(in) :: m
          call curl_curl_matrix_based(curl_curlU%x,curl_curlU%y,curl_curlU%z,U%x,U%y,U%z,m)
        end subroutine

        subroutine multiply_volume_VF(u,m)
          implicit none
          type(VF),intent(inout) :: u
          type(mesh),intent(in) :: m
          call multiply_volume(u%x,m); call multiply_volume(u%y,m); call multiply_volume(u%z,m)
        end subroutine

        subroutine init_VF_CC(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_CC(f%x,m); call init_CC(f%y,m); call init_CC(f%z,m)
          call delete_logicals(f); f%is_CC = .true.
        end subroutine

        subroutine init_VF_CC_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_CC(f%x,m,MD); call init_CC(f%y,m,MD); call init_CC(f%z,m,MD)
          call delete_logicals(f); f%is_CC = .true.
        end subroutine

        subroutine init_VF_Edge(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Edge(f%x,m,1); call init_Edge(f%y,m,2); call init_Edge(f%z,m,3)
          call delete_logicals(f); f%is_Edge = .true.
        end subroutine

        subroutine init_VF_Edge_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Edge(f%x,m,1,MD); call init_Edge(f%y,m,2,MD); call init_Edge(f%z,m,3,MD)
          call delete_logicals(f); f%is_Edge = .true.
        end subroutine

        subroutine init_VF_Edge_compliment(f,m,dir)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_Node(f%x,m);  call init_Face(f%y,m,3);call init_Face(f%z,m,2)
          case (2); call init_Face(f%x,m,3);call init_Node(f%y,m);  call init_Face(f%z,m,1)
          case (3); call init_Face(f%x,m,2);call init_Face(f%y,m,1);call init_Node(f%z,m)
          case default; stop 'Error: dir must = 1,2,3 in init_VF_Edge_compliment in VF.f90'
          end select
          call delete_logicals(f)
        end subroutine

        subroutine init_VF_Face(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Face(f%x,m,1); call init_Face(f%y,m,2); call init_Face(f%z,m,3)
          call delete_logicals(f); f%is_Face = .true.
        end subroutine

        subroutine init_VF_Face_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Face(f%x,m,1,MD); call init_Face(f%y,m,2,MD); call init_Face(f%z,m,3,MD)
          call delete_logicals(f); f%is_Face = .true.
        end subroutine

        subroutine init_VF_Face_compliment(f,m,dir)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_CC  (f%x,m);  call init_Edge(f%y,m,3);call init_Edge(f%z,m,2)
          case (2); call init_Edge(f%x,m,3);call init_CC  (f%y,m);  call init_Edge(f%z,m,1)
          case (3); call init_Edge(f%x,m,2);call init_Edge(f%y,m,1);call init_CC  (f%z,m)
          case default; stop 'Error: dir must = 1,2,3 in init_VF_Face_compliment in VF.f90'
          end select
          call delete_logicals(f)
        end subroutine

        subroutine init_VF_Node(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Node(f%x,m); call init_Node(f%y,m); call init_Node(f%z,m)
          call delete_logicals(f); f%is_Node = .true.
        end subroutine

        subroutine init_VF_Node_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Node(f%x,m,MD); call init_Node(f%y,m,MD); call init_Node(f%z,m,MD)
          call delete_logicals(f); f%is_Node = .true.
        end subroutine

        subroutine init_VF_CC_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC(f%x,m,val); call init_CC(f%y,m,val); call init_CC(f%z,m,val)
          call delete_logicals(f); f%is_CC = .true.
        end subroutine

        subroutine init_VF_Edge_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Edge(f%x,m,1,val); call init_Edge(f%y,m,2,val); call init_Edge(f%z,m,3,val)
          call delete_logicals(f); f%is_Edge = .true.
        end subroutine

        subroutine init_VF_Face_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Face(f%x,m,1,val); call init_Face(f%y,m,2,val); call init_Face(f%z,m,3,val)
          call delete_logicals(f); f%is_Face = .true.
        end subroutine

        subroutine init_VF_Node_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f%x,m,val); call init_Node(f%y,m,val); call init_Node(f%z,m,val)
          call delete_logicals(f); f%is_Node = .true.
        end subroutine

        ! ****************************************************************
        ! ****************************************************************
        ! ********************* COMPUTATION ROUTINES *********************
        ! ****************************************************************
        ! ****************************************************************
        ! ----------------- ASSIGN ------------------

        subroutine assign_VF_VF_X(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%x,g%x)
        end subroutine

        subroutine assign_VF_VF_Y(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%y,g%y)
        end subroutine

        subroutine assign_VF_VF_Z(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%z,g%z)
        end subroutine

        subroutine assign_VF_S_X(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%x,g)
        end subroutine

        subroutine assign_VF_S_Y(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%y,g)
        end subroutine

        subroutine assign_VF_S_Z(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%z,g)
        end subroutine

        subroutine assign_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%x,g%x); call assign(f%y,g%y); call assign(f%z,g%z)
        end subroutine

        subroutine assign_VF_SF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call assign(f%x,g); call assign(f%y,g); call assign(f%z,g)
        end subroutine

        subroutine assign_VF_S(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%x,g); call assign(f%y,g); call assign(f%z,g)
        end subroutine

        subroutine assign_negative_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign_negative(f%x,g%x); call assign_negative(f%y,g%y); call assign_negative(f%z,g%z)
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call add(f%x,g%x); call add(f%y,g%y); call add(f%z,g%z)
        end subroutine

        subroutine add_VF_VF_VF(f,g,r)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g,r
          call add(f%x,g%x,r%x); call add(f%y,g%y,r%y); call add(f%z,g%z,r%z)
        end subroutine

        subroutine add_VF_SF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine add_SF_VF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(VF),intent(in) :: g
          call add(f,g%x,g%y,g%z)
        end subroutine

        subroutine add_VF_S(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine add_S_VF(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call add(f%x,g2); call add(f%y,g2); call add(f%z,g2)
        end subroutine

      ! ------------------- ADD PRODUCT ------------------------

        subroutine add_product_VF_VF_S(f,g,r)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          real(cp),intent(in) :: r
          call add_product(f%x,g%x,r)
          call add_product(f%y,g%y,r)
          call add_product(f%z,g%z,r)
        end subroutine
        subroutine product_add_VF_VF_S(f,r,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: r
          type(VF),intent(in) :: g
          call product_add(f%x,r,g%x)
          call product_add(f%y,r,g%y)
          call product_add(f%z,r,g%z)
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call subtract(f%x,g%x); call subtract(f%y,g%y); call subtract(f%z,g%z)
        end subroutine

        subroutine subtract_VF_VF_VF(f,g,q)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g,q
          call subtract(f%x,g%x,q%x); call subtract(f%y,g%y,q%y); call subtract(f%z,g%z,q%z)
        end subroutine

        subroutine subtract_VF_SF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call subtract(f%x,g); call subtract(f%y,g); call subtract(f%z,g)
        end subroutine

        subroutine subtract_VF_S(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call subtract(f%x,g); call subtract(f%y,g); call subtract(f%z,g)
        end subroutine

        subroutine subtract_S_VF(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call subtract(g2,f%x); call subtract(g2,f%y); call subtract(g2,f%z)
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call multiply(f%x,g%x); call multiply(f%y,g%y); call multiply(f%z,g%z)
        end subroutine

        subroutine multiply_VF_VF_VF(f,g,q)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g,q
          call multiply(f%x,g%x,q%x); call multiply(f%y,g%y,q%y); call multiply(f%z,g%z,q%z)
        end subroutine

        subroutine multiply_VF_VF_SF(f,g,q)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          type(SF),intent(in) :: q
          call multiply(f%x,g%x,q); call multiply(f%y,g%y,q); call multiply(f%z,g%z,q)
        end subroutine

        subroutine multiply_VF_VF_S(f,g,q)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          real(cp),intent(in) :: q
          call multiply(f%x,g%x,q); call multiply(f%y,g%y,q); call multiply(f%z,g%z,q)
        end subroutine

        subroutine multiply_VF_SF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine multiply_SF_VF(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
          call multiply(f%x,g2); call multiply(f%y,g2); call multiply(f%z,g2)
        end subroutine

        subroutine multiply_VF_S(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine multiply_VF_S3(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),dimension(3),intent(in) :: g
          call multiply(f%x,g(1)); call multiply(f%y,g(2)); call multiply(f%z,g(3))
        end subroutine

        subroutine multiply_S_VF(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call multiply(f%x,g2); call multiply(f%y,g2); call multiply(f%z,g2)
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call divide(f%x,g%x); call divide(f%y,g%y); call divide(f%z,g%z)
        end subroutine

        subroutine divide_VF_SF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call divide(f%x,g); call divide(f%y,g); call divide(f%z,g)
        end subroutine

        subroutine divide_VF_S_VF(f,g,q)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          type(VF),intent(inout) :: q
          call divide(f%x,g,q%x); call divide(f%y,g,q%y); call divide(f%z,g,q%z)
        end subroutine

        subroutine divide_VF_S(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call divide(f%x,g); call divide(f%y,g); call divide(f%z,g)
        end subroutine

        subroutine divide_S_VF(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call divide(g2,f%x); call divide(g2,f%y); call divide(g2,f%z)
        end subroutine

      ! ------------------- OTHER ------------------------

        subroutine invert_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call invert(f%x); call invert(f%y); call invert(f%z)
        end subroutine

        function mean_VF(f) result (m)
          implicit none
          type(VF),intent(in) :: f
          real(cp) :: m
          m = (sum(f%x) + sum(f%y) + sum(f%z))/(f%x%numEl + f%y%numEl + f%z%numEl)
        end function

        function max_VF(f) result (m)
          implicit none
          type(VF),intent(in) :: f
          real(cp) :: m
          m = maxval((/max(f%x),max(f%y),max(f%z)/))
        end function

        function amin_VF(f) result (m)
          implicit none
          type(VF),intent(in) :: f
          real(cp) :: m
          m = minval((/abs(amin(f%x)),abs(amin(f%y)),abs(amin(f%z))/))
        end function

        function boundary_flux_VF(f,m) result (BF)
          implicit none
          type(VF),intent(in) :: f
          type(mesh),intent(in) :: m
          real(cp) :: BF
          BF = boundary_flux(f%x,f%y,f%z,m)
        end function

        function amax_VF(f) result (m)
          implicit none
          type(VF),intent(in) :: f
          real(cp) :: m
          m = maxval((/abs(amax(f%x)),abs(amax(f%y)),abs(amax(f%z))/))
        end function

        subroutine square_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call square(f%x); call square(f%y); call square(f%z)
        end subroutine

        subroutine abs_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call abs(f%x); call abs(f%y); call abs(f%z)
        end subroutine

        subroutine insist_amax_lt_tol_VF(f,caller)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: caller
          call insist_amax_lt_tol(f%x,caller//'_x')
          call insist_amax_lt_tol(f%y,caller//'_y')
          call insist_amax_lt_tol(f%z,caller//'_z')
        end subroutine

        ! subroutine vectorSum(f,g)
        !   implicit none
        !   type(SF),intent(inout) :: f
        !   type(VF),intent(in) :: g
        !   call sum(f%x,g%x); call sum(f%y,g%y); call sum(f%z,g%z)
        ! end subroutine

        function dot_product_VF(A,B,temp) result(dot)
          implicit none
          type(VF),intent(in) :: A,B
          type(VF),intent(inout) :: temp
          real(cp) :: dot
          call multiply(temp,A,B)
          dot = sum(temp%x) + sum(temp%y) + sum(temp%z)
        end function

        subroutine dot_VF_SF(A,B,C,temp)
          implicit none
          type(SF),intent(inout) :: A
          type(VF),intent(in) :: B,C
          type(VF),intent(inout) :: temp
          call multiply(temp,B,C)
          call add(A,temp%x,temp%y,temp%z)
        end subroutine

        subroutine cross_product_VF(ACrossB,A,B)
          implicit none
          type(VF),intent(inout) :: ACrossB
          type(VF),intent(in) :: A,B
          call cross_product_x(ACrossB%x,A%y,A%z,B%y,B%z)
          call cross_product_y(ACrossB%y,A%x,A%z,B%x,B%z)
          call cross_product_z(ACrossB%z,A%x,A%y,B%x,B%y)
        end subroutine

        subroutine restrict_VF(A,m)
          implicit none
          type(VF),intent(inout) :: A
          type(mesh),intent(in) :: m
          call restrict(A%x,m)
          call restrict(A%y,m)
          call restrict(A%z,m)
        end subroutine

        subroutine restrict_dir_VF(A,m,dir)
          implicit none
          type(VF),intent(inout) :: A
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          call restrict(A%x,m,dir)
          call restrict(A%y,m,dir)
          call restrict(A%z,m,dir)
        end subroutine

        subroutine prolongate_VF(A,m)
          implicit none
          type(VF),intent(inout) :: A
          type(mesh),intent(in) :: m
          call prolongate(A%x,m)
          call prolongate(A%y,m)
          call prolongate(A%z,m)
        end subroutine

        subroutine prolongate_dir_VF(A,m,dir)
          implicit none
          type(VF),intent(inout) :: A
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          call prolongate(A%x,m,dir)
          call prolongate(A%y,m,dir)
          call prolongate(A%z,m,dir)
        end subroutine

        function symmetry_error_x_VF(A) result(SE)
          implicit none
          type(VF),intent(in) :: A
          real(cp) :: SE
          SE = symmetry_error_x(A%x) + symmetry_error_x(A%y) + symmetry_error_x(A%z)
        end function

        function symmetry_error_y_VF(A) result(SE)
          implicit none
          type(VF),intent(in) :: A
          real(cp) :: SE
          SE = symmetry_error_y(A%x) + symmetry_error_y(A%y) + symmetry_error_y(A%z)
        end function

        function symmetry_error_z_VF(A) result(SE)
          implicit none
          type(VF),intent(in) :: A
          real(cp) :: SE
          SE = symmetry_error_z(A%x) + symmetry_error_z(A%y) + symmetry_error_z(A%z)
        end function

        subroutine symmetry_local_x_VF(A)
          implicit none
          type(VF),intent(inout) :: A
          call symmetry_local_x(A%x)
          call symmetry_local_x(A%y)
          call symmetry_local_x(A%z)
        end subroutine

        subroutine symmetry_local_y_VF(A)
          implicit none
          type(VF),intent(inout) :: A
          call symmetry_local_y(A%x)
          call symmetry_local_y(A%y)
          call symmetry_local_y(A%z)
        end subroutine

        subroutine symmetry_local_z_VF(A)
          implicit none
          type(VF),intent(inout) :: A
          call symmetry_local_z(A%x)
          call symmetry_local_z(A%y)
          call symmetry_local_z(A%z)
        end subroutine

        subroutine assign_ghost_XPeriodic_VF(A,val)
          implicit none
          type(VF),intent(inout) :: A
          real(cp),intent(in) :: val
          call assign_ghost_XPeriodic(A%x,val)
          call assign_ghost_XPeriodic(A%y,val)
          call assign_ghost_XPeriodic(A%z,val)
        end subroutine
        subroutine assign_ghost_XPeriodic_VF2(A,val,A_with_BCs)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: A_with_BCs
          real(cp),intent(in) :: val
          call assign_ghost_XPeriodic(A%x,val,A_with_BCs%x)
          call assign_ghost_XPeriodic(A%y,val,A_with_BCs%y)
          call assign_ghost_XPeriodic(A%z,val,A_with_BCs%z)
        end subroutine

        subroutine assign_ghost_N_XPeriodic_VF(A,val)
          implicit none
          type(VF),intent(inout) :: A
          real(cp),intent(in) :: val
          call assign_ghost_N_XPeriodic(A%x,val)
          call assign_ghost_N_XPeriodic(A%y,val)
          call assign_ghost_N_XPeriodic(A%z,val)
        end subroutine
        subroutine assign_ghost_N_XPeriodic_VF2(A,val,A_with_BCs)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: A_with_BCs
          real(cp),intent(in) :: val
          call assign_ghost_N_XPeriodic(A%x,val,A_with_BCs%x)
          call assign_ghost_N_XPeriodic(A%y,val,A_with_BCs%y)
          call assign_ghost_N_XPeriodic(A%z,val,A_with_BCs%z)
        end subroutine

        subroutine assign_wall_Dirichlet_VF(A,val)
          implicit none
          type(VF),intent(inout) :: A
          real(cp),intent(in) :: val
          call assign_wall_Dirichlet(A%x,val)
          call assign_wall_Dirichlet(A%y,val)
          call assign_wall_Dirichlet(A%z,val)
        end subroutine
        subroutine assign_wall_Dirichlet_VF2(A,val,A_with_BCs)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: A_with_BCs
          real(cp),intent(in) :: val
          call assign_wall_Dirichlet(A%x,val,A_with_BCs%x)
          call assign_wall_Dirichlet(A%y,val,A_with_BCs%y)
          call assign_wall_Dirichlet(A%z,val,A_with_BCs%z)
        end subroutine

        subroutine multiply_wall_Neumann_VF(A,val)
          implicit none
          type(VF),intent(inout) :: A
          real(cp),intent(in) :: val
          call multiply_wall_Neumann(A%x,val)
          call multiply_wall_Neumann(A%y,val)
          call multiply_wall_Neumann(A%z,val)
        end subroutine
        subroutine multiply_wall_Neumann_VF2(A,val,A_with_BCs)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: A_with_BCs
          real(cp),intent(in) :: val
          call multiply_wall_Neumann(A%x,val,A_with_BCs%x)
          call multiply_wall_Neumann(A%y,val,A_with_BCs%y)
          call multiply_wall_Neumann(A%z,val,A_with_BCs%z)
        end subroutine


      end module