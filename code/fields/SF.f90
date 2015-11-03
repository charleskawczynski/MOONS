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
        use mesh_mod
        use RF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: SF
        public :: init,delete
        ! Grid initialization
        public :: init_CC
        public :: init_Node
        public :: init_Face
        public :: init_Edge
        public :: CC_along,Node_along

        public :: init_BCs,init_BC_props
        

        ! Monitoring
        public :: print

        ! Operators
        public :: assign,assignMinus
        public :: add,subtract
        public :: multiply,divide
        ! Auxiliary
        public :: square,min,max,maxabs
        public :: maxabsdiff,mean,sum

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

        type SF
          integer :: s ! Number of subdomains in domain decomposition
          type(realField),dimension(:),allocatable :: RF
          logical :: is_CC,is_Node,is_face,is_edge
          logical :: all_neumann ! If necessary to subtract mean
          integer :: face = 0 ! Direction of face data
          integer :: edge = 0 ! Direction of edge data
        end type

        interface init;           module procedure init_SF_copy;           end interface
        interface init_CC;        module procedure init_SF_CC;             end interface
        interface init_Node;      module procedure init_SF_Node;           end interface
        interface init_Face;      module procedure init_SF_Face;           end interface
        interface init_Edge;      module procedure init_SF_Edge;           end interface

        interface init_BCs;       module procedure init_BCs_SF;            end interface
        interface init_BC_props;  module procedure init_BC_props_SF;       end interface

        interface delete;         module procedure delete_SF;              end interface

        interface assign;         module procedure assign_SF_S;            end interface
        interface assign;         module procedure assign_SF_SF;           end interface
        interface assignMinus;    module procedure assignMinus_SF_SF;      end interface

        interface add;            module procedure add_SF_SF;              end interface
        interface add;            module procedure add_SF_SF_SF;           end interface
        interface add;            module procedure add_SF_S;               end interface
        interface add;            module procedure add_S_SF;               end interface

        interface multiply;       module procedure multiply_SF_SF;         end interface
        interface multiply;       module procedure multiply_SF_SF_SF;      end interface
        interface multiply;       module procedure multiply_SF_S;          end interface
        interface multiply;       module procedure multiply_S_SF;          end interface

        interface subtract;       module procedure subtract_SF_SF;         end interface
        interface subtract;       module procedure subtract_SF_SF_SF;      end interface
        interface subtract;       module procedure subtract_SF_S;          end interface
        interface subtract;       module procedure subtract_S_SF;          end interface

        interface divide;         module procedure divide_SF_SF;           end interface
        interface divide;         module procedure divide_SF_SF_SF;        end interface
        interface divide;         module procedure divide_SF_S;            end interface
        interface divide;         module procedure divide_S_SF;            end interface

        interface square;         module procedure square_SF;              end interface
        interface print;          module procedure print_SF;               end interface
        interface min;            module procedure min_SF;                 end interface
        interface max;            module procedure max_SF;                 end interface
        interface min;            module procedure min_pad_SF;             end interface
        interface max;            module procedure max_pad_SF;             end interface
        interface maxabs;         module procedure maxabs_SF;              end interface
        interface maxabsdiff;     module procedure maxabsdiff_SF;          end interface
        interface mean;           module procedure mean_SF;                end interface
        interface sum;            module procedure sum_SF;                 end interface

        interface CC_along;       module procedure CC_along_SF;            end interface
        interface Node_along;     module procedure Node_along_SF;          end interface

      contains

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
        
        subroutine assignMinus_SF_SF(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
          integer :: i
          do i=1,f%s; call assignMinus(f%RF(i),g%RF(i)); enddo
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

        subroutine square_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call square(f%RF(i)); enddo
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

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine deleteDataLocation(a)
          implicit none
          type(SF),intent(inout) :: a
          a%is_CC = .false.
          a%is_node = .false.
          a%is_face = .false.
          a%is_edge = .false.
        end subroutine

        subroutine init_SF_copy(f1,f2)
          implicit none
          type(SF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          integer :: i
          call delete(f1)
          allocate(f1%RF(f2%s)); f1%s = f2%s
          do i=1,f1%s; call init(f1%RF(i),f2%RF(i)); enddo
          f1%is_CC = f2%is_CC
          f1%is_node = f2%is_node
          f1%is_face = f2%is_face
          f1%is_edge = f2%is_edge
        end subroutine

        subroutine init_SF_CC(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          call delete(f)
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_CC(f%RF(i),m%g(i)); enddo
          call deleteDataLocation(f)
          f%is_CC = .true.
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
          f%is_face = .true.
          f%face = dir
        end subroutine

        subroutine init_SF_Edge(f,m,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer,intent(in) :: dir
          integer :: i
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_Edge(f%RF(i),m%g(i),dir); enddo
          call deleteDataLocation(f)
          f%is_edge = .true.
          f%edge = dir
        end subroutine

        subroutine init_SF_Node(f,m)
          implicit none
          type(SF),intent(inout) :: f
          type(mesh),intent(in) :: m
          integer :: i
          allocate(f%RF(m%s)); f%s = m%s
          do i=1,f%s; call init_Node(f%RF(i),m%g(i)); enddo
          call deleteDataLocation(f)
          f%is_node = .true.
        end subroutine

        function CC_along_SF(f,dir) result(TF)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: dir
          logical :: TF
          TF = any((/f%is_CC,f%is_Face.and.(f%face.ne.dir),f%is_Edge.and.(f%edge.eq.dir)/))
        end function

        function Node_along_SF(f,dir) result(TF)
          implicit none
          type(SF),intent(in) :: f
          integer,intent(in) :: dir
          logical :: TF
          TF = any((/f%is_Node,f%is_Face.and.(f%face.eq.dir),f%is_Edge.and.(f%edge.ne.dir)/))
        end function

        subroutine init_BCs_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          if (f%is_CC) then
            do i=1,f%s; call init_BCs(f%RF(i),.true.,.false.); enddo
          elseif (f%is_Node) then
            do i=1,f%s; call init_BCs(f%RF(i),.false.,.true.); enddo
          else
            stop 'Error: no datatype found in init_BCs_SF in SF.f90'
          endif
          f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
          call init_BC_props(f)
        end subroutine

        subroutine init_BC_props_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          f%all_Neumann = all((/(f%RF(i)%b%all_Neumann,i=1,f%s)/))
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
        end subroutine

        subroutine print_SF(f)
          implicit none
          type(SF),intent(in) :: f
          integer :: i
          do i=1,f%s; call print(f%RF(i)); enddo
        end subroutine

      end module