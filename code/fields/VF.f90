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

        use mesh_mod
        use SF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: VF
        public :: init,delete
        ! public :: allocateX,allocateY,allocateZ
        ! Grid initialization
        public :: init_CC
        public :: init_Face
        public :: init_Edge
        public :: init_Node

        public :: dot_product

        ! Monitoring
        public :: print
        public :: print_BCs
        public :: export_BCs

        ! Operators
        public :: assign,assignMinus
        public :: add,subtract
        public :: multiply,divide
        public :: square
        ! public :: sum
        public :: assignX,assignY,assignZ

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

        type VF
          integer :: s = 3  ! number of components
          type(SF) :: x,y,z ! components
        end type

        interface init;         module procedure init_VF_copy_VF;          end interface
        interface init;         module procedure init_VF_copy_SF;          end interface
        interface init_CC;      module procedure init_VF_CC;               end interface
        interface init_Face;    module procedure init_VF_Face;             end interface
        interface init_Edge;    module procedure init_VF_Edge;             end interface
        interface init_Node;    module procedure init_VF_Node;             end interface

        interface dot_product;  module procedure dot_product_VF;           end interface

        interface delete;       module procedure delete_VF;                end interface
        interface print;        module procedure print_VF;                 end interface
        interface print_BCs;    module procedure print_BCs_VF;             end interface
        interface export_BCs;   module procedure export_BCs_VF;            end interface

        interface assignX;      module procedure assign_VF_VF_X;          end interface
        interface assignY;      module procedure assign_VF_VF_Y;          end interface
        interface assignZ;      module procedure assign_VF_VF_Z;          end interface
        interface assignX;      module procedure assign_VF_S_X;           end interface
        interface assignY;      module procedure assign_VF_S_Y;           end interface
        interface assignZ;      module procedure assign_VF_S_Z;           end interface

        interface assign;       module procedure assign_VF_S;              end interface
        interface assign;       module procedure assign_VF_SF;             end interface
        interface assign;       module procedure assign_VF_VF;             end interface
        interface assignMinus;  module procedure assignMinus_VF_VF;        end interface

        interface add;          module procedure add_VF_VF;                end interface
        interface add;          module procedure add_VF_VF_VF;             end interface
        interface add;          module procedure add_VF_SF;                end interface
        interface add;          module procedure add_SF_VF;                end interface
        interface add;          module procedure add_VF_S;                 end interface
        interface add;          module procedure add_S_VF;                 end interface

        interface subtract;     module procedure subtract_VF_VF;           end interface
        interface subtract;     module procedure subtract_VF_VF_VF;        end interface
        interface subtract;     module procedure subtract_VF_SF;           end interface
        interface subtract;     module procedure subtract_VF_S;            end interface
        interface subtract;     module procedure subtract_S_VF;            end interface

        interface multiply;     module procedure multiply_VF_VF;           end interface
        interface multiply;     module procedure multiply_VF_VF_VF;        end interface
        interface multiply;     module procedure multiply_VF_VF_SF;        end interface
        interface multiply;     module procedure multiply_VF_SF;           end interface
        interface multiply;     module procedure multiply_SF_VF;           end interface
        interface multiply;     module procedure multiply_VF_S;            end interface
        interface multiply;     module procedure multiply_VF_S3;           end interface
        interface multiply;     module procedure multiply_S_VF;            end interface

        interface divide;       module procedure divide_VF_VF;             end interface
        interface divide;       module procedure divide_VF_SF;             end interface
        interface divide;       module procedure divide_VF_S_VF;           end interface
        interface divide;       module procedure divide_VF_S;              end interface
        interface divide;       module procedure divide_S_VF;              end interface

        interface square;       module procedure square_VF;                end interface
        ! interface sum;          module procedure vectorSum;             end interface

        contains

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

        subroutine assignMinus_VF_VF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assignMinus(f%x,g%x); call assignMinus(f%y,g%y); call assignMinus(f%z,g%z)
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

        subroutine add_SF_VF(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
          call add(f%x,g2); call add(f%y,g2); call add(f%z,g2)
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

        subroutine square_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call square(f%x); call square(f%y); call square(f%z)
        end subroutine

        ! subroutine vectorSum(f,g)
        !   implicit none
        !   type(SF),intent(inout) :: f
        !   type(VF),intent(in) :: g
        !   call sum(f%x,g%x); call sum(f%y,g%y); call sum(f%z,g%z)
        ! end subroutine

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine init_VF_copy_VF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          call init(f1%x,f2%x); call init(f1%y,f2%y); call init(f1%z,f2%z)
        end subroutine

        subroutine init_VF_copy_SF(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
        end subroutine

        subroutine init_VF_CC(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_CC(f%x,m); call init_CC(f%y,m); call init_CC(f%z,m)
        end subroutine

        subroutine init_VF_Edge(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Edge(f%x,m,1); call init_Edge(f%y,m,2); call init_Edge(f%z,m,3)
        end subroutine

        subroutine init_VF_Face(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Face(f%x,m,1); call init_Face(f%y,m,2); call init_Face(f%z,m,3)
        end subroutine

        subroutine init_VF_Node(f,m)
          implicit none
          type(VF),intent(inout) :: f
          type(mesh),intent(in) :: m
          call init_Node(f%x,m); call init_Node(f%y,m); call init_Node(f%z,m)
        end subroutine

        function dot_product_VF(A,B,temp) result(dot)
          implicit none
          type(VF),intent(in) :: A,B
          type(VF),intent(inout) :: temp
          real(cp) :: dot
          call multiply(temp,A,B)
          dot = sum(temp%x) + sum(temp%y) + sum(temp%z)
        end function

        subroutine delete_VF(f)
          implicit none
          type(VF),intent(inout) :: f
          call delete(f%x); call delete(f%y); call delete(f%z)
        end subroutine

        subroutine print_VF(f)
          implicit none
          type(VF),intent(in) :: f
          call print(f%x); call print(f%y); call print(f%z)
        end subroutine

        subroutine print_BCs_VF(f,name)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: name
          call print_BCs(f%x,name//'_x')
          call print_BCs(f%y,name//'_y')
          call print_BCs(f%z,name//'_z')
        end subroutine

        subroutine export_BCs_VF(f,dir,name)
          implicit none
          type(VF),intent(in) :: f
          character(len=*),intent(in) :: dir,name
          call export_BCs(f%x,dir,name//'_x')
          call export_BCs(f%y,dir,name//'_y')
          call export_BCs(f%z,dir,name//'_z')
        end subroutine

      end module