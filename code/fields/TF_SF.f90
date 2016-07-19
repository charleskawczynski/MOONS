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

        use current_precision_mod
        use mesh_mod
        use SF_mod
        use VF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: TF
        public :: init,delete
        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        ! Monitoring
        public :: print

        ! Operators
        public :: assign
        public :: add,subtract
        public :: multiply,divide
        public :: square
        public :: transpose
        ! public :: sum
        public :: assignX,assignY,assignZ



        type TF
          integer :: s = 3  ! number of components
          type(SF),dimension(3) :: x ! (xx,xy,xz)
          type(SF),dimension(3) :: y ! (yx,yy,yz)
          type(SF),dimension(3) :: z ! (zx,zy,zz)
          logical :: is_CC,is_Node,is_Face,is_Edge
          logical :: is_CC_F,is_CC_E
        end type

        interface init;          module procedure init_TF_copy_VF;          end interface
        interface init;          module procedure init_TF_copy_TF;          end interface

        interface init_CC;       module procedure init_TF_CC;               end interface
        interface init_Face;     module procedure init_TF_Face;             end interface
        interface init_Edge;     module procedure init_TF_Edge;             end interface
        interface init_Node;     module procedure init_TF_Node;             end interface
        interface init_CC_F;     module procedure init_CC_F_TF;             end interface
        interface init_CC_E;     module procedure init_CC_E_TF;             end interface

        interface init_CC;       module procedure init_TF_CC_assign;        end interface
        interface init_Face;     module procedure init_TF_Face_assign;      end interface
        interface init_Edge;     module procedure init_TF_Edge_assign;      end interface
        interface init_Node;     module procedure init_TF_Node_assign;      end interface

        interface delete;        module procedure delete_TF;                end interface
        interface print;         module procedure print_TF;                 end interface

        interface assignX;       module procedure assign_TF_TF_X;          end interface
        interface assignY;       module procedure assign_TF_TF_Y;          end interface
        interface assignZ;       module procedure assign_TF_TF_Z;          end interface
        interface assignX;       module procedure assign_TF_S_X;           end interface
        interface assignY;       module procedure assign_TF_S_Y;           end interface
        interface assignZ;       module procedure assign_TF_S_Z;           end interface

        interface assign;        module procedure assign_TF_S;              end interface
        interface assign;        module procedure assign_TF_VF;             end interface
        interface assign;        module procedure assign_TF_TF;             end interface

        interface add;           module procedure add_TF_TF;                end interface
        interface add;           module procedure add_TF_TF_TF;             end interface
        interface add;           module procedure add_TF_VF;                end interface
        interface add;           module procedure add_VF_TF;                end interface
        interface add;           module procedure add_TF_S;                 end interface
        interface add;           module procedure add_S_TF;                 end interface
        interface add;           module procedure add_SF_TF;                end interface

        interface subtract;      module procedure subtract_TF_TF;           end interface
        interface subtract;      module procedure subtract_TF_VF;           end interface
        interface subtract;      module procedure subtract_TF_S;            end interface
        interface subtract;      module procedure subtract_S_TF;            end interface

        interface multiply;      module procedure multiply_TF_TF;           end interface
        interface multiply;      module procedure multiply_TF_VF;           end interface
        interface multiply;      module procedure multiply_VF_TF;           end interface
        interface multiply;      module procedure multiply_TF_S;            end interface
        interface multiply;      module procedure multiply_S_TF;            end interface

        interface divide;        module procedure divide_TF_TF;             end interface
        interface divide;        module procedure divide_TF_VF;             end interface
        interface divide;        module procedure divide_TF_S;              end interface
        interface divide;        module procedure divide_S_TF;              end interface

        interface square;        module procedure square_TF;                end interface
        interface transpose;     module procedure transpose_TF_TF;          end interface
        interface transpose;     module procedure transpose_TF_SF;          end interface
        ! interface sum;           module procedure vectorSum;             end interface

        contains

        ! ----------------- ASSIGN ------------------

        subroutine assign_TF_TF_X(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call assign(f%x,g%x)
        end subroutine

        subroutine assign_TF_TF_Y(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call assign(f%y,g%y)
        end subroutine

        subroutine assign_TF_TF_Z(f,g)
          implicit none
          type(TF),intent(inout) :: f
          type(TF),intent(in) :: g
          call assign(f%z,g%z)
        end subroutine

        subroutine assign_TF_S_X(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%x,g)
        end subroutine

        subroutine assign_TF_S_Y(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%y,g)
        end subroutine

        subroutine assign_TF_S_Z(f,g)
          implicit none
          type(TF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%z,g)
        end subroutine

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

        subroutine square_TF(f)
          implicit none
          type(TF),intent(inout) :: f
          call square(f%x); call square(f%y); call square(f%z)
        end subroutine

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
          f1%is_CC = f2%is_CC
          f1%is_Node = f2%is_Node
          f1%is_Face = f2%is_Face
          f1%is_Edge = f2%is_Edge
        end subroutine

        subroutine init_TF_copy_VF(f1,f2)
          implicit none
          type(TF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
          f1%is_CC = f2%is_CC
          f1%is_Node = f2%is_Node
          f1%is_Face = f2%is_Face
          f1%is_Edge = f2%is_Edge
        end subroutine

        subroutine init_TF_CC(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_CC(f%x,m); call init_CC(f%y,m); call init_CC(f%z,m)
          call delete_logicals(f); f%is_CC = .true.
        end subroutine

        subroutine init_TF_Edge(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Edge(f%x,m); call init_Edge(f%y,m); call init_Edge(f%z,m)
          call delete_logicals(f); f%is_Edge = .true.
        end subroutine

        subroutine init_TF_Face(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Face(f%x,m); call init_Face(f%y,m); call init_Face(f%z,m)
          call delete_logicals(f); f%is_Face = .true.
        end subroutine

        subroutine init_TF_Node(f,m)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Node(f%x,m); call init_Node(f%y,m); call init_Node(f%z,m)
          call delete_logicals(f); f%is_Node = .true.
        end subroutine

        subroutine init_TF_CC_assign(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_CC(f%x,m,val); call init_CC(f%y,m,val); call init_CC(f%z,m,val)
        end subroutine

        subroutine init_TF_Edge_assign(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Edge(f%x,m,val); call init_Edge(f%y,m,val); call init_Edge(f%z,m,val)
        end subroutine

        subroutine init_TF_Face_assign(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Face(f%x,m,val); call init_Face(f%y,m,val); call init_Face(f%z,m,val)
        end subroutine

        subroutine init_TF_Node_assign(f,m,val)
          implicit none
          type(TF),intent(inout) :: f
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: val
          call init_Node(f%x,m,val); call init_Node(f%y,m,val); call init_Node(f%z,m,val)
        end subroutine

        subroutine delete_TF(f)
          implicit none
          type(TF),intent(inout) :: f
          call delete(f%x); call delete(f%y); call delete(f%z)
          call delete_logicals(f)
        end subroutine

        subroutine delete_logicals(f)
          implicit none
          type(TF),intent(inout) :: f
          f%is_CC = .false.
          f%is_Node = .false.
          f%is_Face = .false.
          f%is_Edge = .false.
        end subroutine

        subroutine print_TF(f)
          implicit none
          type(TF),intent(in) :: f
          call print(f%x); call print(f%y); call print(f%z)
        end subroutine

      end module