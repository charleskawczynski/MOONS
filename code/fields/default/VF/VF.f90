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

        public :: init_CC_Edge
        public :: init_Node_Edge

        public :: init_BCs
        public :: init_BC_Dirichlet
        public :: init_BC_props
        public :: multiply_volume

        public :: get_any_Dirichlet
        public :: get_any_Neumann
        public :: get_any_Robin
        public :: get_any_Prescribed

        public :: volume
        public :: sine_waves
        public :: cosine_waves
        public :: random_noise

        public :: assign_BCs
        public :: assign_BC_vals
        public :: update_BC_vals
        public :: assign_Dirichlet_BCs
        public :: assign_Periodic_BCs
        public :: assign_Neumann_BCs
        public :: assign_Neumann_BCs_wall_normal
        public :: multiply_Neumann_BCs
        public :: multiply_BCs_by_nhat
        public :: assign_Robin_BCs
        public :: multiply_Robin_coeff
        public :: multiply_nhat
        public :: assign_ghost_XPeriodic
        public :: assign_ghost_N_XPeriodic
        public :: assign_wall_Dirichlet
        public :: assign_wall_Periodic_single
        public :: multiply_wall_Neumann
        public :: set_prescribed_BCs

        public :: symmetry_error_x,symmetry_local_x
        public :: symmetry_error_y,symmetry_local_y
        public :: symmetry_error_z,symmetry_local_z

        public :: CFL_number

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
        public :: square,square_root,invert,abs,insist_amax_lt_tol
        public :: mean,max,amin,amax
        public :: boundary_flux
        public :: sum
        public :: assignX,assignY,assignZ

        public :: laplacian_matrix_based
        public :: curl_curl_matrix_based

        public :: is_collocated
        public :: get_DL
        public :: is_CC
        public :: is_Node
        public :: is_Face
        public :: is_Edge
        public :: insist_collocated

        type VF
          type(SF) :: x,y,z ! components
        end type

        interface init;                     module procedure init_VF_copy_VF;               end interface
        interface init;                     module procedure init_VF_copy_SF;               end interface
        interface init;                     module procedure init_DL_VF;                    end interface
        interface init;                     module procedure init_VF_copy_VF_mesh;          end interface
        interface delete;                   module procedure delete_VF;                     end interface
        interface display;                  module procedure display_VF;                    end interface
        interface print;                    module procedure print_VF;                      end interface
        interface export;                   module procedure export_VF;                     end interface
        interface import;                   module procedure import_VF;                     end interface
        interface export;                   module procedure export_VF_wrapper;             end interface
        interface import;                   module procedure import_VF_wrapper;             end interface

        interface init_CC;                  module procedure init_CC_VF;                    end interface
        interface init_Face;                module procedure init_Face_VF;                  end interface
        interface init_Edge;                module procedure init_Edge_VF;                  end interface
        interface init_Node;                module procedure init_Node_VF;                  end interface

        interface init_CC;                  module procedure init_CC_VF_MD;                 end interface
        interface init_Face;                module procedure init_Face_VF_MD;               end interface
        interface init_Edge;                module procedure init_Edge_VF_MD;               end interface
        interface init_Node;                module procedure init_Node_VF_MD;               end interface

        interface init_CC_Edge;             module procedure init_CC_Edge_VF;               end interface
        interface init_Node_Edge;           module procedure init_Node_Edge_VF;             end interface

        interface init_CC;                  module procedure init_CC_VF_assign;             end interface
        interface init_Face;                module procedure init_Face_VF_assign;           end interface
        interface init_Edge;                module procedure init_Edge_VF_assign;           end interface
        interface init_Node;                module procedure init_Node_VF_assign;           end interface
        interface multiply_volume;          module procedure multiply_volume_VF;            end interface

        interface volume;                   module procedure volume_VF;                     end interface
        interface sine_waves;               module procedure sine_waves_VF;                 end interface
        interface cosine_waves;             module procedure cosine_waves_VF;               end interface
        interface random_noise;             module procedure random_noise_VF;               end interface

        interface symmetry_error_x;         module procedure symmetry_error_x_VF;           end interface
        interface symmetry_error_y;         module procedure symmetry_error_y_VF;           end interface
        interface symmetry_error_z;         module procedure symmetry_error_z_VF;           end interface

        interface symmetry_local_x;         module procedure symmetry_local_x_VF;           end interface
        interface symmetry_local_y;         module procedure symmetry_local_y_VF;           end interface
        interface symmetry_local_z;         module procedure symmetry_local_z_VF;           end interface

        interface CFL_number;               module procedure CFL_number_VF;                 end interface

        interface assign_BCs;               module procedure assign_BCs_VF;                 end interface
        interface assign_BC_vals;           module procedure assign_BC_vals_VF;             end interface
        interface update_BC_vals;           module procedure update_BC_vals_VF;             end interface
        interface assign_Dirichlet_BCs;     module procedure assign_Dirichlet_BCs_VF;       end interface
        interface assign_Periodic_BCs;      module procedure assign_Periodic_BCs_VF;        end interface
        interface assign_Neumann_BCs;       module procedure assign_Neumann_BCs_faces_VF;   end interface
        interface assign_Neumann_BCs_wall_normal;  module procedure assign_Neumann_BCs_wall_normal_VF;   end interface
        interface multiply_Neumann_BCs;     module procedure multiply_Neumann_BCs_VF;       end interface
        interface multiply_BCs_by_nhat;     module procedure multiply_BCs_by_nhat_VF;       end interface
        interface assign_Robin_BCs;         module procedure assign_Robin_BCs_dir_VF;       end interface
        interface assign_Robin_BCs;         module procedure assign_Robin_BCs_faces_VF;     end interface
        interface multiply_Robin_coeff;     module procedure multiply_Robin_coeff_VF;       end interface
        interface multiply_nhat;            module procedure multiply_nhat_VF;              end interface
        interface assign_ghost_XPeriodic;   module procedure assign_ghost_XPeriodic_VF;     end interface
        interface assign_ghost_XPeriodic;   module procedure assign_ghost_XPeriodic_VF2;    end interface
        interface assign_ghost_N_XPeriodic; module procedure assign_ghost_N_XPeriodic_VF;   end interface
        interface assign_ghost_N_XPeriodic; module procedure assign_ghost_N_XPeriodic_VF2;  end interface
        interface assign_wall_Dirichlet;    module procedure assign_wall_Dirichlet_VF;      end interface
        interface assign_wall_Dirichlet;    module procedure assign_wall_Dirichlet_VF2;     end interface
        interface assign_wall_Periodic_single; module procedure assign_wall_Periodic_single_VF;      end interface
        interface assign_wall_Periodic_single; module procedure assign_wall_Periodic_single_VF2;     end interface
        interface multiply_wall_Neumann;    module procedure multiply_wall_Neumann_VF;      end interface
        interface multiply_wall_Neumann;    module procedure multiply_wall_Neumann_VF2;     end interface
        interface set_prescribed_BCs;       module procedure set_prescribed_BCs_VF;         end interface

        interface dot_product;              module procedure dot_product_VF;                end interface
        interface dot;                      module procedure dot_VF_SF;                     end interface

        interface cross_product;            module procedure cross_product_VF;              end interface

        interface restrict;                 module procedure restrict_VF;                   end interface
        interface restrict;                 module procedure restrict_dir_VF;               end interface
        interface prolongate;               module procedure prolongate_VF;                 end interface
        interface prolongate;               module procedure prolongate_dir_VF;             end interface

        interface curl_curl_matrix_based;   module procedure curl_curl_matrix_based_VF;     end interface
        interface Laplacian_matrix_based;   module procedure Laplacian_matrix_based_VF;     end interface

        interface is_collocated;            module procedure is_collocated_VF_DL;           end interface
        interface insist_collocated;        module procedure insist_collocated_VF;          end interface
        interface get_DL;                   module procedure get_DL_VF;                     end interface
        interface is_CC;                    module procedure is_CC_VF_DL;                   end interface
        interface is_Node;                  module procedure is_Node_VF_DL;                 end interface
        interface is_Face;                  module procedure is_Face_VF_DL;                 end interface
        interface is_Edge;                  module procedure is_Edge_VF_DL;                 end interface

        interface print_BCs;                module procedure print_BCs_VF;                  end interface
        interface init_BCs;                 module procedure init_BCs_VF_VF;                end interface
        interface init_BC_Dirichlet;        module procedure init_BC_Dirichlet_VF;          end interface
        interface export_BCs;               module procedure export_BCs_VF;                 end interface
        interface init_BC_props;            module procedure init_BC_props_VF;              end interface

        interface get_any_Dirichlet;        module procedure get_any_Dirichlet_VF;          end interface
        interface get_any_Neumann;          module procedure get_any_Neumann_VF;            end interface
        interface get_any_Robin;            module procedure get_any_Robin_VF;              end interface
        interface get_any_Prescribed;       module procedure get_any_Prescribed_VF;         end interface

        ! COMPUTATION ROUTINES

        interface assignX;                  module procedure assign_VF_VF_X;                end interface
        interface assignY;                  module procedure assign_VF_VF_Y;                end interface
        interface assignZ;                  module procedure assign_VF_VF_Z;                end interface
        interface assignX;                  module procedure assign_VF_S_X;                 end interface
        interface assignY;                  module procedure assign_VF_S_Y;                 end interface
        interface assignZ;                  module procedure assign_VF_S_Z;                 end interface

        interface assign;                   module procedure assign_VF_S;                   end interface
        interface assign;                   module procedure assign_VF_SF;                  end interface
        interface assign;                   module procedure assign_VF_VF;                  end interface
        interface assign_negative;          module procedure assign_negative_VF_VF;         end interface

        interface add;                      module procedure add_VF_VF;                     end interface
        interface add;                      module procedure add_VF_VF_VF;                  end interface
        interface add;                      module procedure add_VF_SF;                     end interface
        interface add;                      module procedure add_SF_VF;                     end interface
        interface add;                      module procedure add_VF_S;                      end interface
        interface add;                      module procedure add_S_VF;                      end interface

        interface add_product;              module procedure add_product_VF_VF_S;           end interface
        interface product_add;              module procedure product_add_VF_VF_S;           end interface

        interface subtract;                 module procedure subtract_VF_VF;                end interface
        interface subtract;                 module procedure subtract_VF_VF_VF;             end interface
        interface subtract;                 module procedure subtract_VF_SF;                end interface
        interface subtract;                 module procedure subtract_VF_S;                 end interface
        interface subtract;                 module procedure subtract_S_VF;                 end interface

        interface multiply;                 module procedure multiply_VF_VF;                end interface
        interface multiply;                 module procedure multiply_VF_VF_VF;             end interface
        interface multiply;                 module procedure multiply_VF_VF_SF;             end interface
        interface multiply;                 module procedure multiply_VF_VF_S;              end interface
        interface multiply;                 module procedure multiply_VF_SF;                end interface
        interface multiply;                 module procedure multiply_SF_VF;                end interface
        interface multiply;                 module procedure multiply_VF_S;                 end interface
        interface multiply;                 module procedure multiply_VF_S3;                end interface
        interface multiply;                 module procedure multiply_S_VF;                 end interface

        interface divide;                   module procedure divide_VF_VF;                  end interface
        interface divide;                   module procedure divide_VF_SF;                  end interface
        interface divide;                   module procedure divide_VF_S_VF;                end interface
        interface divide;                   module procedure divide_VF_S;                   end interface
        interface divide;                   module procedure divide_S_VF;                   end interface

        interface invert;                   module procedure invert_VF;                     end interface
        interface mean;                     module procedure mean_VF;                       end interface
        interface max;                      module procedure max_VF;                        end interface
        interface amax;                     module procedure amax_VF;                       end interface
        interface amin;                     module procedure amin_VF;                       end interface
        interface sum;                      module procedure sum_pad_VF;                        end interface
        interface boundary_flux;            module procedure boundary_flux_VF;              end interface

        interface square;                   module procedure square_VF;                     end interface
        interface square_root;              module procedure square_root_VF;                end interface
        interface abs;                      module procedure abs_VF;                        end interface
        interface insist_amax_lt_tol;       module procedure insist_amax_lt_tol_VF;         end interface

        contains

        ! **********************************************************
        ! ********************* ESSENTIALS *************************
        ! **********************************************************

        subroutine init_VF_copy_VF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          call init(f1%x,f2%x); call init(f1%y,f2%y); call init(f1%z,f2%z)
        end subroutine

        subroutine init_VF_copy_VF_mesh(f1,f2,m)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          type(mesh),intent(in) :: m
          call init(f1%x,f2%x,m); call init(f1%y,f2%y,m); call init(f1%z,f2%z,m)
        end subroutine

        subroutine init_VF_copy_SF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
        end subroutine

        subroutine init_DL_VF(f,m,DL)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(data_location),dimension(3),intent(in) :: DL
          call init(f%x,m,DL(1))
          call init(f%y,m,DL(2))
          call init(f%z,m,DL(3))
        end subroutine

        subroutine delete_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call delete(f%x); call delete(f%y); call delete(f%z)
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

        subroutine init_BC_Dirichlet_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call init_BC_Dirichlet(f%x)
          call init_BC_Dirichlet(f%y)
          call init_BC_Dirichlet(f%z)
        end subroutine

        subroutine init_BC_props_VF(f,c_w,Robin_coeff)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),dimension(6),intent(in) :: c_w,Robin_coeff
          call init_BC_props(f%x,c_w,Robin_coeff)
          call init_BC_props(f%y,c_w,Robin_coeff)
          call init_BC_props(f%z,c_w,Robin_coeff)
        end subroutine

        function get_any_Dirichlet_VF(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = any((/get_any_Dirichlet(f%x),get_any_Dirichlet(f%y),get_any_Dirichlet(f%z)/))
        end function

        function get_any_Neumann_VF(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = any((/get_any_Neumann(f%x),get_any_Neumann(f%y),get_any_Neumann(f%z)/))
        end function

        function get_any_Robin_VF(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = any((/get_any_Robin(f%x),get_any_Robin(f%y),get_any_Robin(f%z)/))
        end function

        function get_any_Prescribed_VF(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = any((/get_any_Prescribed(f%x),get_any_Prescribed(f%y),get_any_Prescribed(f%z)/))
        end function

        subroutine export_BCs_VF(f,dir,name)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          call export_BCs(f%x,dir,name//'_x')
          call export_BCs(f%y,dir,name//'_y')
          call export_BCs(f%z,dir,name//'_z')
        end subroutine

        function get_DL_VF(f) result(DL)
          implicit none
          type(VF),intent(in) :: f
          type(data_location),dimension(3) :: DL
          DL = (/f%x%DL,f%y%DL,f%z%DL/)
        end function

        function is_Face_VF_DL(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = is_Face_VF(get_DL(f))
        end function

        function is_Edge_VF_DL(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = is_Edge_VF(get_DL(f))
        end function

        function is_CC_VF_DL(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = is_CC_VF(get_DL(f))
        end function

        function is_Node_VF_DL(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          logical :: L
          L = is_Node_VF(get_DL(f))
        end function

        function is_collocated_VF_DL(f) result(L)
          implicit none
          type(VF),intent(in) :: f
          type(data_location),dimension(3) :: DL
          logical :: L
          DL = get_DL(f)
          L = is_collocated_VF(DL)
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

        subroutine init_CC_VF(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_CC(f%x,m); call init_CC(f%y,m); call init_CC(f%z,m)
        end subroutine

        subroutine init_CC_VF_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_CC(f%x,m,MD); call init_CC(f%y,m,MD); call init_CC(f%z,m,MD)
        end subroutine

        subroutine init_Edge_VF(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Edge(f%x,m,1); call init_Edge(f%y,m,2); call init_Edge(f%z,m,3)
        end subroutine

        subroutine init_Edge_VF_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Edge(f%x,m,1,MD); call init_Edge(f%y,m,2,MD); call init_Edge(f%z,m,3,MD)
        end subroutine

        subroutine init_Node_Edge_VF(f,m,dir)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_Node(f%x,m);  call init_Face(f%y,m,3);call init_Face(f%z,m,2)
          case (2); call init_Face(f%x,m,3);call init_Node(f%y,m);  call init_Face(f%z,m,1)
          case (3); call init_Face(f%x,m,2);call init_Face(f%y,m,1);call init_Node(f%z,m)
          case default; stop 'Error: dir must = 1,2,3 in init_Node_Edge_VF in VF.f90'
          end select
        end subroutine

        subroutine init_Face_VF(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Face(f%x,m,1); call init_Face(f%y,m,2); call init_Face(f%z,m,3)
        end subroutine

        subroutine init_Face_VF_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Face(f%x,m,1,MD); call init_Face(f%y,m,2,MD); call init_Face(f%z,m,3,MD)
        end subroutine

        subroutine init_CC_Edge_VF(f,m,dir)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          select case (dir)
          case (1); call init_CC  (f%x,m);  call init_Edge(f%y,m,2);call init_Edge(f%z,m,3)
          case (2); call init_Edge(f%x,m,1);call init_CC  (f%y,m);  call init_Edge(f%z,m,3)
          case (3); call init_Edge(f%x,m,1);call init_Edge(f%y,m,2);call init_CC  (f%z,m)
          case default; stop 'Error: dir must = 1,2,3 in init_Face_compliment_VF in VF.f90'
          end select
        end subroutine

        subroutine init_Node_VF(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Node(f%x,m); call init_Node(f%y,m); call init_Node(f%z,m)
        end subroutine

        subroutine init_Node_VF_MD(f,m,MD)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Node(f%x,m,MD); call init_Node(f%y,m,MD); call init_Node(f%z,m,MD)
        end subroutine

        subroutine init_CC_VF_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC(f%x,m,val); call init_CC(f%y,m,val); call init_CC(f%z,m,val)
        end subroutine

        subroutine init_Edge_VF_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Edge(f%x,m,1,val); call init_Edge(f%y,m,2,val); call init_Edge(f%z,m,3,val)
        end subroutine

        subroutine init_Face_VF_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Face(f%x,m,1,val); call init_Face(f%y,m,2,val); call init_Face(f%z,m,3,val)
        end subroutine

        subroutine init_Node_VF_assign(f,m,val)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f%x,m,val); call init_Node(f%y,m,val); call init_Node(f%z,m,val)
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

        function sum_pad_VF(f,pad) result (m)
          implicit none
          type(VF),intent(in) :: f
          integer,intent(in) :: pad
          real(cp) :: m
          m = sum(f%x,pad)+sum(f%y,pad)+sum(f%z,pad)
        end function

        subroutine boundary_flux_VF(BF,f,m,f_temp)
          implicit none
          real(cp),intent(inout) :: BF
          type(VF),intent(in) :: f
          type(mesh),intent(in) :: m
          type(VF),intent(inout) :: f_temp
          call boundary_flux(BF,f%x,f%y,f%z,m,f_temp%x,f_temp%y,f_temp%z)
        end subroutine

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

        subroutine square_root_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call square_root(f%x); call square_root(f%y); call square_root(f%z)
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

        function CFL_number_VF(A,m,dt) result(CFL)
          implicit none
          type(VF),intent(in) :: A
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: dt
          real(cp) :: CFL
          CFL = CFL_number(A%x,A%y,A%z,m,dt)
        end function

        subroutine assign_BC_vals_VF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_BC_vals(A%x,B%x)
          call assign_BC_vals(A%y,B%y)
          call assign_BC_vals(A%z,B%z)
        end subroutine

        subroutine update_BC_vals_VF(A)
          implicit none
          type(VF),intent(inout) :: A
          call update_BC_vals(A%x)
          call update_BC_vals(A%y)
          call update_BC_vals(A%z)
        end subroutine

        subroutine assign_BCs_VF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_BCs(A%x,B%x)
          call assign_BCs(A%y,B%y)
          call assign_BCs(A%z,B%z)
        end subroutine
        subroutine assign_Dirichlet_BCs_VF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_Dirichlet_BCs(A%x,B%x)
          call assign_Dirichlet_BCs(A%y,B%y)
          call assign_Dirichlet_BCs(A%z,B%z)
        end subroutine
        subroutine assign_Periodic_BCs_VF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_Periodic_BCs(A%x,B%x)
          call assign_Periodic_BCs(A%y,B%y)
          call assign_Periodic_BCs(A%z,B%z)
        end subroutine
        subroutine assign_Neumann_BCs_faces_VF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_Neumann_BCs(A,B%x,1)
          call assign_Neumann_BCs(A,B%y,2)
          call assign_Neumann_BCs(A,B%z,3)
        end subroutine
        subroutine assign_Neumann_BCs_wall_normal_VF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_Neumann_BCs_wall_normal(A,B%x,1)
          call assign_Neumann_BCs_wall_normal(A,B%y,2)
          call assign_Neumann_BCs_wall_normal(A,B%z,3)
        end subroutine
        subroutine multiply_Neumann_BCs_VF(A,scale)
          implicit none
          type(VF),intent(inout) :: A
          real(cp),intent(in) :: scale
          call multiply_Neumann_BCs(A%x,scale)
          call multiply_Neumann_BCs(A%y,scale)
          call multiply_Neumann_BCs(A%z,scale)
        end subroutine
        subroutine multiply_BCs_by_nhat_VF(A)
          implicit none
          type(VF),intent(inout) :: A
          call multiply_BCs_by_nhat(A%x)
          call multiply_BCs_by_nhat(A%y)
          call multiply_BCs_by_nhat(A%z)
        end subroutine
        subroutine assign_Robin_BCs_dir_VF(A,B)
          implicit none
          type(SF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_Robin_BCs(A,B%x,1)
          call assign_Robin_BCs(A,B%y,2)
          call assign_Robin_BCs(A,B%z,3)
        end subroutine
        subroutine assign_Robin_BCs_faces_VF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: B
          call assign_Robin_BCs(A%x,B%x)
          call assign_Robin_BCs(A%y,B%y)
          call assign_Robin_BCs(A%z,B%z)
        end subroutine
        subroutine multiply_Robin_coeff_VF(u)
          implicit none
          type(VF),intent(inout) :: u
          call multiply_Robin_coeff(u%x)
          call multiply_Robin_coeff(u%y)
          call multiply_Robin_coeff(u%z)
        end subroutine
        subroutine multiply_nhat_VF(u,u_with_BCs)
          implicit none
          type(VF),intent(inout) :: u
          type(VF),intent(in) :: u_with_BCs
          call multiply_nhat(u%x,u_with_BCs%x)
          call multiply_nhat(u%y,u_with_BCs%y)
          call multiply_nhat(u%z,u_with_BCs%z)
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

        subroutine assign_wall_Periodic_single_VF(A,val)
          implicit none
          type(VF),intent(inout) :: A
          real(cp),intent(in) :: val
          call assign_wall_Periodic_single(A%x,val)
          call assign_wall_Periodic_single(A%y,val)
          call assign_wall_Periodic_single(A%z,val)
        end subroutine
        subroutine assign_wall_Periodic_single_VF2(A,val,A_with_BCs)
          implicit none
          type(VF),intent(inout) :: A
          type(VF),intent(in) :: A_with_BCs
          real(cp),intent(in) :: val
          call assign_wall_Periodic_single(A%x,val,A_with_BCs%x)
          call assign_wall_Periodic_single(A%y,val,A_with_BCs%y)
          call assign_wall_Periodic_single(A%z,val,A_with_BCs%z)
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

        subroutine set_prescribed_BCs_VF(A)
          implicit none
          type(VF),intent(inout) :: A
          call set_prescribed_BCs(A%x)
          call set_prescribed_BCs(A%y)
          call set_prescribed_BCs(A%z)
        end subroutine

      end module