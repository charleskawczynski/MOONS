      module TF_mod
      ! Tensor fields are a bit difficult to think about.. So here's
      ! a short description as to how they are constructed, used.
      !
      ! TF = [VF_x,VF_y,VF_z]
      !
      ! If TF is staggered, then realize that
      !
      ! call init_Face(TF,m)
      !
      ! Implies
      !
      ! TF = [ VF_x  , VF_y  , VF_z  ]
      !         ||      ||      ||
      !    x [(n,c,c),(n,c,c),(n,c,c)]
      !    y [(c,n,c),(c,n,c),(c,n,c)]
      !    z [(c,c,n),(c,c,n),(c,c,n)]
      !
      ! That is to say that each "component" of the TF is a
      ! staggered vector field depending on initialization.
      !
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

        use IO_tools_mod
        use current_precision_mod
        use data_location_mod
        use mesh_mod
        use mesh_domain_mod
        use SF_mod
        use VF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: TF
        public :: init,delete,export,import

        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: init_CC_Edge
        public :: init_Node_Face
        public :: init_Face_Transpose

        public :: assign_Neumann_BCs
        public :: assign_Neumann_BCs_wall_normal
        public :: multiply_Neumann_BCs

        ! Monitoring
        public :: print

        ! Operators
        public :: assign
        public :: add,add_product,subtract
        public :: multiply,divide
        public :: amax
        public :: square,square_root
        public :: cross_product
        public :: transpose

        public :: restrict
        public :: prolongate
        public :: get_DL
        public :: is_Face
        public :: is_Edge
        public :: is_CC
        public :: is_Node
        public :: is_CC_Edge
        public :: is_collocated
        ! public :: sum
        ! public :: assignX,assignY,assignZ

        type TF
          type(VF) :: x,y,z ! Staggered VF_1 = (xx,xy,xz)
        end type

        interface init;                module procedure init_TF_copy_VF;           end interface
        interface init;                module procedure init_TF_copy_TF;           end interface
        interface init;                module procedure init_TF_copy_TF_mesh;      end interface

        interface init_CC;             module procedure init_CC_TF;                end interface
        interface init_Face;           module procedure init_Face_TF;              end interface
        interface init_Edge;           module procedure init_Edge_TF;              end interface
        interface init_Node;           module procedure init_Node_TF;              end interface

        interface init_CC;             module procedure init_CC_MD_TF;             end interface
        interface init_Face;           module procedure init_Face_MD_TF;           end interface
        interface init_Edge;           module procedure init_Edge_MD_TF;           end interface
        interface init_Node;           module procedure init_Node_MD_TF;           end interface

        interface init_Face_Transpose; module procedure init_Face_Transpose_TF;    end interface
        interface init_Face_Transpose; module procedure init_Face_Transpose_assign_TF;    end interface
        interface init_CC_Edge;        module procedure init_CC_Edge_TF;           end interface
        interface init_Node_Face;      module procedure init_Node_Face_TF;         end interface

        interface init_CC;             module procedure init_CC_assign_TF;         end interface
        interface init_Face;           module procedure init_Face_assign_TF;       end interface
        interface init_Edge;           module procedure init_Edge_assign_TF;       end interface
        interface init_Node;           module procedure init_Node_assign_TF;       end interface

        interface init_CC_Edge;module procedure init_CC_Edge_assign_TF; end interface

        interface assign_Neumann_BCs;  module procedure assign_Neumann_BCs_faces_TF;        end interface
        interface assign_Neumann_BCs_wall_normal;  module procedure assign_Neumann_BCs_wall_normal_TF;  end interface
        interface multiply_Neumann_BCs;module procedure multiply_Neumann_BCs_faces_TF; end interface

        interface delete;              module procedure delete_TF;                 end interface
        interface export;              module procedure export_TF;                 end interface
        interface import;              module procedure import_TF;                 end interface
        interface export;              module procedure export_TF_wrapper;         end interface
        interface import;              module procedure import_TF_wrapper;         end interface
        interface print;               module procedure print_TF;                  end interface

        interface assign;              module procedure assign_TF_S;               end interface
        interface assign;              module procedure assign_TF_VF;              end interface
        interface assign;              module procedure assign_TF_TF;              end interface

        interface add;                 module procedure add_TF_TF;                 end interface
        interface add;                 module procedure add_TF_TF_TF;              end interface
        interface add;                 module procedure add_TF_TF_VF;              end interface
        interface add;                 module procedure add_TF_VF;                 end interface
        interface add;                 module procedure add_VF_TF;                 end interface
        interface add;                 module procedure add_TF_S;                  end interface
        interface add;                 module procedure add_S_TF;                  end interface
        interface add;                 module procedure add_SF_TF;                 end interface
        interface add_product;         module procedure add_product_TF_TF_S;       end interface

        interface subtract;            module procedure subtract_TF_TF;            end interface
        interface subtract;            module procedure subtract_TF_VF;            end interface
        interface subtract;            module procedure subtract_TF_S;             end interface
        interface subtract;            module procedure subtract_S_TF;             end interface

        interface multiply;            module procedure multiply_TF_TF;            end interface
        interface multiply;            module procedure multiply_TF_VF;            end interface
        interface multiply;            module procedure multiply_VF_TF;            end interface
        interface multiply;            module procedure multiply_TF_S;             end interface
        interface multiply;            module procedure multiply_S_TF;             end interface

        interface divide;              module procedure divide_TF_TF;              end interface
        interface divide;              module procedure divide_TF_VF;              end interface
        interface divide;              module procedure divide_TF_S;               end interface
        interface divide;              module procedure divide_S_TF;               end interface

        interface amax;                module procedure amax_TF;                   end interface

        interface square;              module procedure square_TF;                 end interface
        interface square_root;         module procedure square_root_TF;            end interface
        interface cross_product;       module procedure cross_product_TF;          end interface

        interface restrict;            module procedure restrict_TF;               end interface
        interface restrict;            module procedure restrict_dir_TF;           end interface
        interface prolongate;          module procedure prolongate_TF;             end interface
        interface prolongate;          module procedure prolongate_dir_TF;         end interface

        interface get_DL;              module procedure get_DL_TF;                 end interface
        interface is_Face;             module procedure is_Face_VF_DL;             end interface
        interface is_Edge;             module procedure is_Edge_VF_DL;             end interface
        interface is_CC;               module procedure is_CC_VF_DL;               end interface
        interface is_Node;             module procedure is_Node_VF_DL;             end interface
        interface is_CC_Edge;          module procedure is_CC_Edge_VF_DL;          end interface
        interface is_collocated;       module procedure is_collocated_TF_DL;       end interface

        interface transpose;           module procedure transpose_TF_TF;           end interface
        interface transpose;           module procedure transpose_TF_SF;           end interface

        contains

        ! ----------------- ASSIGN ------------------

        subroutine assign_TF_TF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call assign(f%x,g%x); call assign(f%y,g%y); call assign(f%z,g%z)
        end subroutine

        subroutine assign_TF_VF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%x,g); call assign(f%y,g); call assign(f%z,g)
        end subroutine

        subroutine assign_TF_S(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%x,g); call assign(f%y,g); call assign(f%z,g)
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_TF_TF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call add(f%x,g%x); call add(f%y,g%y); call add(f%z,g%z)
        end subroutine

        subroutine add_TF_TF_TF(f,g,r)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g,r
          call add(f%x,g%x,r%x); call add(f%y,g%y,r%y); call add(f%z,g%z,r%z)
        end subroutine

        subroutine add_TF_TF_VF(f,g,r)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          type(VF),intent(in) :: r
          call add(f%x,g%x,r); call add(f%y,g%y,r); call add(f%z,g%z,r)
        end subroutine

        subroutine add_TF_VF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine add_VF_TF(g2,f)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g2
          call add(f%x,g2); call add(f%y,g2); call add(f%z,g2)
        end subroutine

        subroutine add_TF_S(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine add_S_TF(g2,f)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call add(f%x,g2); call add(f%y,g2); call add(f%z,g2)
        end subroutine

        subroutine add_SF_TF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(TF),intent(in) :: g
          call add(f,g%x%x,g%y%x,g%z%x,&
                     g%x%y,g%y%y,g%z%y,&
                     g%x%z,g%y%z,g%z%z)
        end subroutine

        subroutine add_product_TF_TF_S(f,g,c)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          real(cp),intent(in) :: c
          call add_product(f%x,g%x,c)
          call add_product(f%y,g%y,c)
          call add_product(f%z,g%z,c)
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_TF_TF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call subtract(f%x,g%x); call subtract(f%y,g%y); call subtract(f%z,g%z)
        end subroutine

        subroutine subtract_TF_VF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g
          call subtract(f%x,g); call subtract(f%y,g); call subtract(f%z,g)
        end subroutine

        subroutine subtract_TF_S(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call subtract(f%x,g); call subtract(f%y,g); call subtract(f%z,g)
        end subroutine

        subroutine subtract_S_TF(g2,f)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call subtract(g2,f%x); call subtract(g2,f%y); call subtract(g2,f%z)
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_TF_TF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call multiply(f%x,g%x); call multiply(f%y,g%y); call multiply(f%z,g%z)
        end subroutine

        subroutine multiply_TF_VF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine multiply_VF_TF(g2,f)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g2
          call multiply(f%x,g2); call multiply(f%y,g2); call multiply(f%z,g2)
        end subroutine

        subroutine multiply_TF_S(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine multiply_S_TF(g2,f)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call multiply(f%x,g2); call multiply(f%y,g2); call multiply(f%z,g2)
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_TF_TF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call divide(f%x,g%x); call divide(f%y,g%y); call divide(f%z,g%z)
        end subroutine

        subroutine divide_TF_VF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(VF),intent(in) :: g
          call divide(f%x,g); call divide(f%y,g); call divide(f%z,g)
        end subroutine

        subroutine divide_TF_S(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call divide(f%x,g); call divide(f%y,g); call divide(f%z,g)
        end subroutine

        subroutine divide_S_TF(g2,f)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call divide(g2,f%x); call divide(g2,f%y); call divide(g2,f%z)
        end subroutine

      ! ------------------- OTHER ------------------------

        function amax_TF(f) result (m)
          implicit none
          type(TF),intent(in) :: f
          real(cp) :: m
          m = maxval((/amax(f%x),amax(f%y),amax(f%z)/))
        end function

        subroutine square_TF(f)
          implicit none
          type(TF),intent(inout) :: f
          call square(f%x); call square(f%y); call square(f%z)
        end subroutine

        subroutine square_root_TF(f)
          implicit none
          type(TF),intent(inout) :: f
          call square_root(f%x); call square_root(f%y); call square_root(f%z)
        end subroutine

        subroutine cross_product_TF(AcrossB,A,B)
          ! First index refers to vector direction.
          ! Second index refers to vector location.
          !      For example, in A%x%y, the direction of the vector
          !      will be in x, and it will be located on whatever
          !      location is defined by the y-component (y-face for face data
          !      or y-edge for edge data).
          ! Since this is a collocated operation, the second
          ! index should be the same (data should be collocated).
          ! NOTE: The diagonal, xx,yy,zz are not used.
          implicit none
          type(VF),intent(inout) :: AcrossB
          type(TF),intent(in) :: A,B
          call cross_product_x(AcrossB%x,A%y%x,A%z%x,B%y%x,B%z%x)
          call cross_product_y(AcrossB%y,A%x%y,A%z%y,B%x%y,B%z%y)
          call cross_product_z(AcrossB%z,A%x%z,A%y%z,B%x%z,B%y%z)
        end subroutine

        subroutine restrict_TF(A,m)
          implicit none
          type(TF),intent(inout) :: A
          type(mesh),intent(in) :: m
          call restrict(A%x,m)
          call restrict(A%y,m)
          call restrict(A%z,m)
        end subroutine

        subroutine restrict_dir_TF(A,m,dir)
          implicit none
          type(TF),intent(inout) :: A
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          call restrict(A%x,m,dir)
          call restrict(A%y,m,dir)
          call restrict(A%z,m,dir)
        end subroutine

        subroutine prolongate_TF(A,m)
          implicit none
          type(TF),intent(inout) :: A
          type(mesh),intent(in) :: m
          call prolongate(A%x,m)
          call prolongate(A%y,m)
          call prolongate(A%z,m)
        end subroutine

        subroutine prolongate_dir_TF(A,m,dir)
          implicit none
          type(TF),intent(inout) :: A
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          call prolongate(A%x,m,dir)
          call prolongate(A%y,m,dir)
          call prolongate(A%z,m,dir)
        end subroutine

        function get_DL_TF(A) result(DL)
          implicit none
          type(TF),intent(in) :: A
          type(data_location),dimension(9) :: DL
          DL(1:3) = get_DL(A%x)
          DL(4:6) = get_DL(A%y)
          DL(7:9) = get_DL(A%z)
        end function

        function is_Face_VF_DL(f) result(L)
          implicit none
          type(TF),intent(in) :: f
          logical :: L
          L = is_Face_TF(get_DL(f))
        end function

        function is_Edge_VF_DL(f) result(L)
          implicit none
          type(TF),intent(in) :: f
          logical :: L
          L = is_Edge_TF(get_DL(f))
        end function

        function is_CC_VF_DL(f) result(L)
          implicit none
          type(TF),intent(in) :: f
          logical :: L
          L = is_CC_TF(get_DL(f))
        end function

        function is_Node_VF_DL(f) result(L)
          implicit none
          type(TF),intent(in) :: f
          logical :: L
          L = is_Node_TF(get_DL(f))
        end function

        function is_CC_Edge_VF_DL(f) result(L)
          implicit none
          type(TF),intent(in) :: f
          logical :: L
          L = is_CC_edge_TF(get_DL(f))
        end function

        function is_collocated_TF_DL(A) result(L)
          implicit none
          type(TF),intent(in) :: A
          type(data_location),dimension(9) :: DL
          logical :: L
          DL = get_DL(A)
          L = is_collocated_TF(DL)
        end function

        subroutine transpose_TF_SF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(SF),intent(inout) :: g
          call swap(f%x%y,f%y%x,g)
          call swap(f%y%x,f%x%y,g)
          call swap(f%x%z,f%z%x,g)
          call swap(f%z%x,f%x%z,g)
          call swap(f%y%z,f%z%y,g)
          call swap(f%z%y,f%y%z,g)
        end subroutine

        subroutine transpose_TF_TF(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call assign(f%x%x,g%x%x) ! Diagonal (remains the same)
          call assign(f%y%y,g%y%y) ! Diagonal (remains the same)
          call assign(f%z%z,g%z%z) ! Diagonal (remains the same)

          call assign(f%x%y,g%y%x) ! Off diag
          call assign(f%y%x,g%x%y) ! Off diag

          call assign(f%x%z,g%z%x) ! Off diag
          call assign(f%z%x,g%x%z) ! Off diag

          call assign(f%y%z,g%z%y) ! Off diag
          call assign(f%z%y,g%y%z) ! Off diag
        end subroutine

        ! subroutine vectorSum(f,g)
        !   implicit none
        !   type(VF),intent(inout) :: f
        !   type(TF),intent(in) :: g
        !   call sum(f%x,g%x); call sum(f%y,g%y); call sum(f%z,g%z)
        ! end subroutine

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine init_TF_copy_TF(f1,f2)
          implicit none
          type(TF),intent(inout) :: f1
          type(TF),intent(in) :: f2
          call init(f1%x,f2%x); call init(f1%y,f2%y); call init(f1%z,f2%z)
        end subroutine

        subroutine init_TF_copy_TF_mesh(f1,f2,m)
          implicit none
          type(TF),intent(inout) :: f1
          type(TF),intent(in) :: f2
          type(mesh),intent(in) :: m
          call init(f1%x,f2%x,m); call init(f1%y,f2%y,m); call init(f1%z,f2%z,m)
        end subroutine

        subroutine init_TF_copy_VF(f1,f2)
          implicit none
          type(TF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
        end subroutine

        subroutine init_CC_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_CC(f%x,m); call init_CC(f%y,m); call init_CC(f%z,m)
        end subroutine

        subroutine init_CC_MD_TF(f,m,MD)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_CC(f%x,m,MD); call init_CC(f%y,m,MD); call init_CC(f%z,m,MD)
        end subroutine

        subroutine init_Edge_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Edge(f%x,m); call init_Edge(f%y,m); call init_Edge(f%z,m)
        end subroutine

        subroutine init_Edge_MD_TF(f,m,MD)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Edge(f%x,m,MD); call init_Edge(f%y,m,MD); call init_Edge(f%z,m,MD)
        end subroutine

        subroutine init_Node_Face_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Node(f%x%x,m)
          call init_Face(f%x%y,m,3)
          call init_Face(f%x%z,m,2)

          call init_Face(f%y%x,m,3)
          call init_Node(f%y%y,m)
          call init_Face(f%y%z,m,1)

          call init_Face(f%z%x,m,2)
          call init_Face(f%z%y,m,1)
          call init_Node(f%z%z,m)
        end subroutine

        subroutine init_Face_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Face(f%x,m); call init_Face(f%y,m); call init_Face(f%z,m)
        end subroutine

        subroutine init_Face_MD_TF(f,m,MD)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Face(f%x,m,MD); call init_Face(f%y,m,MD); call init_Face(f%z,m,MD)
        end subroutine

        subroutine init_Face_Transpose_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Face(f%x%x,m,1)
          call init_Face(f%x%y,m,1)
          call init_Face(f%x%z,m,1)
          call init_Face(f%y%x,m,2)
          call init_Face(f%y%y,m,2)
          call init_Face(f%y%z,m,2)
          call init_Face(f%z%x,m,3)
          call init_Face(f%z%y,m,3)
          call init_Face(f%z%z,m,3)
        end subroutine

        subroutine init_Face_Transpose_assign_TF(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Face_Transpose(f,m)
          call assign(f,val)
        end subroutine

        subroutine init_CC_Edge_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_CC  (f%x%x,m)
          call init_Edge(f%x%y,m,3)
          call init_Edge(f%x%z,m,2)

          call init_Edge(f%y%x,m,3)
          call init_CC  (f%y%y,m)
          call init_Edge(f%y%z,m,1)

          call init_Edge(f%z%x,m,2)
          call init_Edge(f%z%y,m,1)
          call init_CC  (f%z%z,m)
        end subroutine

        subroutine init_Node_TF(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Node(f%x,m); call init_Node(f%y,m); call init_Node(f%z,m)
        end subroutine

        subroutine init_Node_MD_TF(f,m,MD)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          type(mesh_domain),intent(in) :: MD
          call init_Node(f%x,m,MD); call init_Node(f%y,m,MD); call init_Node(f%z,m,MD)
        end subroutine

        subroutine init_CC_assign_TF(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC(f%x,m,val); call init_CC(f%y,m,val); call init_CC(f%z,m,val)
        end subroutine

        subroutine init_Edge_assign_TF(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Edge(f%x,m,val); call init_Edge(f%y,m,val); call init_Edge(f%z,m,val)
        end subroutine

        subroutine init_Face_assign_TF(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Face(f%x,m,val); call init_Face(f%y,m,val); call init_Face(f%z,m,val)
        end subroutine

        subroutine init_Node_assign_TF(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f%x,m,val); call init_Node(f%y,m,val); call init_Node(f%z,m,val)
        end subroutine

        subroutine init_CC_Edge_assign_TF(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC_Edge(f,m); call assign(f,val)
        end subroutine

        subroutine assign_Neumann_BCs_faces_TF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(TF),intent(in) :: B
          call assign_Neumann_BCs(A%x,B%x)
          call assign_Neumann_BCs(A%y,B%y)
          call assign_Neumann_BCs(A%z,B%z)
        end subroutine

        subroutine assign_Neumann_BCs_wall_normal_TF(A,B)
          implicit none
          type(VF),intent(inout) :: A
          type(TF),intent(in) :: B
          call assign_Neumann_BCs_wall_normal(A%x,B%x)
          call assign_Neumann_BCs_wall_normal(A%y,B%y)
          call assign_Neumann_BCs_wall_normal(A%z,B%z)
        end subroutine

        subroutine multiply_Neumann_BCs_faces_TF(A,scale)
          implicit none
          type(TF),intent(inout) :: A
          real(cp),intent(in) :: scale
          call multiply_Neumann_BCs(A%x,scale)
          call multiply_Neumann_BCs(A%y,scale)
          call multiply_Neumann_BCs(A%z,scale)
        end subroutine

        subroutine delete_TF(f)
          implicit none
          type(TF),intent(inout) :: f
          call delete(f%x); call delete(f%y); call delete(f%z)
        end subroutine

        subroutine print_TF(f)
          implicit none
          type(TF),intent(in) :: f
          call print(f%x); call print(f%y); call print(f%z)
        end subroutine

        subroutine export_TF(f,un)
          implicit none
          type(TF),intent(in) :: f
          integer,intent(in) :: un
          call export(f%x,un)
          call export(f%y,un)
          call export(f%z,un)
        end subroutine

        subroutine import_TF(f,un)
          implicit none
          type(TF),intent(inout) :: f
          integer,intent(in) :: un
          call import(f%x,un)
          call import(f%y,un)
          call import(f%z,un)
        end subroutine

        subroutine export_TF_wrapper(f,dir,name)
          implicit none
          type(TF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          integer :: un
          un = new_and_open(dir,name)
          call export(f,un)
          close(un)
        end subroutine

        subroutine import_TF_wrapper(f,dir,name)
          implicit none
          type(TF),intent(inout) :: f
          character(len=*),intent(in) :: dir,name
          integer :: un
          un = open_to_read(dir,name)
          call import(f,un)
          close(un)
        end subroutine

      end module