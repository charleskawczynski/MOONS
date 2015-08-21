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
        use grid_mod
        use RF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: SF
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
        ! Auxiliary
        public :: square,max,maxabs
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
          integer :: s = 1 ! Number of subdomains in domain decomposition
          integer,dimension(3) :: stot ! Total number of indexes in each direction (for export)
          type(realField),dimension(1) :: RF
          ! integer :: s ! Number of subdomains in domain decomposition
          ! type(realField),dimension(:),allocatable :: RF
        end type

        interface init;      module procedure init_SF_1;              end interface
        interface init;      module procedure init_SF_2;              end interface
        interface init;      module procedure init_SF_3;              end interface

        interface init_CC;   module procedure init_SF_CC;             end interface
        interface init_Face; module procedure init_SF_Face;           end interface
        interface init_Edge; module procedure init_SF_Edge;           end interface
        interface init_Node; module procedure init_SF_Node;           end interface

        interface delete;    module procedure delete_SF;              end interface

        interface assign;    module procedure assign_SF_S;            end interface
        interface assign;    module procedure assign_SF_SF;           end interface

        interface add;       module procedure add_SF_SF;              end interface
        interface add;       module procedure add_SF_SF_SF;           end interface
        interface add;       module procedure add_SF_S;               end interface
        interface add;       module procedure add_S_SF;               end interface

        interface multiply;  module procedure multiply_SF_SF;         end interface
        interface multiply;  module procedure multiply_SF_SF_SF;      end interface
        interface multiply;  module procedure multiply_SF_S;          end interface
        interface multiply;  module procedure multiply_S_SF;          end interface

        interface subtract;  module procedure subtract_SF_SF;         end interface
        interface subtract;  module procedure subtract_SF_SF_SF;      end interface
        interface subtract;  module procedure subtract_SF_S;          end interface
        interface subtract;  module procedure subtract_S_SF;          end interface

        interface divide;    module procedure divide_SF_SF;           end interface
        interface divide;    module procedure divide_SF_SF_SF;        end interface
        interface divide;    module procedure divide_SF_S;            end interface
        interface divide;    module procedure divide_S_SF;            end interface

        interface square;    module procedure square_SF;              end interface
        interface print;     module procedure print_SF;               end interface
        interface max;       module procedure max_SF;                 end interface
        interface maxabs;    module procedure maxabs_SF;              end interface
        interface maxabsdiff;module procedure maxabsdiff_SF;          end interface
        interface mean;      module procedure mean_SF;                end interface
        interface sum;       module procedure sum_SF;                 end interface

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

        subroutine init_props(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          f%stot = 0; do i=1,f%s; f%stot = f%stot + f%RF(i)%s; enddo
          f%s = 1
        end subroutine

        subroutine init_SF_1(f,Nx,Ny,Nz)
          implicit none
          type(SF),intent(inout) :: f
          integer,intent(in) :: Nx,Ny,Nz
          integer :: i
          do i=1,f%s; call init(f%RF(i),Nx,Ny,Nz); enddo
          call init_props(f)
        end subroutine

        subroutine init_SF_2(f1,f2)
          implicit none
          type(SF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          integer :: i
          do i=1,f1%s; call init(f1%RF(i),f2%RF(i)); enddo
          call init_props(f1)
        end subroutine

        subroutine init_SF_3(f,s)
          implicit none
          type(SF),intent(inout) :: f
          integer,dimension(3),intent(in) :: s
          integer :: i
          do i=1,f%s; call init(f%RF(i),s); enddo
          call init_props(f)
        end subroutine

        subroutine init_SF_CC(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(grid),intent(in) :: g
          integer :: i
          do i=1,f%s; call init_CC(f%RF(i),g); enddo
          call init_props(f)
        end subroutine

        subroutine init_SF_Face(f,g,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          integer :: i
          do i=1,f%s; call init_Face(f%RF(i),g,dir); enddo
          call init_props(f)
        end subroutine

        subroutine init_SF_Edge(f,g,dir)
          implicit none
          type(SF),intent(inout) :: f
          type(grid),intent(in) :: g
          integer,intent(in) :: dir
          integer :: i
          do i=1,f%s; call init_Edge(f%RF(i),g,dir); enddo
          call init_props(f)
        end subroutine

        subroutine init_SF_Node(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(grid),intent(in) :: g
          integer :: i
          do i=1,f%s; call init_Node(f%RF(i),g); enddo
          call init_props(f)
        end subroutine

        subroutine delete_SF(f)
          implicit none
          type(SF),intent(inout) :: f
          integer :: i
          do i=1,f%s; call delete(f%RF(i)); enddo
          call init_props(f)
        end subroutine

        subroutine print_SF(f)
          implicit none
          type(SF),intent(in) :: f
          integer :: i
          do i=1,f%s; call print(f%RF(i)); enddo
        end subroutine

      end module