      module RF_mod
        ! Naming convention: name = operation_type1_type2
        ! 
        !      RF_array = type(RF_array)
        !      R  = real(cp),dimension(:,:,:)
        !      S  = real(cp)
        ! 
        ! Example(1): Adding a scalar to RF_array
        !             name = add_RF_S
        ! Example(2): Subtracting a real field from RF_array
        !             name = subtract_RF_R
        ! Example(3): Subtracting a RF_array from a real field
        !             name = subtract_R_RF
        ! 
        ! NOTES: RF_array stands for 'real field'
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

        ! Available pre-processor directives:
        !         _DEBUG_FIELD_ ! not yet implemented
        !         _PARALLELIZE_RF_

        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: RF_array
        public :: init,delete

        ! Monitoring
        public :: print

        ! Operators
        public :: assign
        public :: add,subtract
        public :: multiply,divide
        public :: square
        ! public :: sum


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

        type RF_array
          integer :: s = 1
          type(realField),dimension(1) :: RF
          ! integer :: s ! Number of subdomains in domain decomposition
          ! type(realField),dimension(:),allocatable :: RF
        end type

        interface init;      module procedure init_RF_1;              end interface
        interface init;      module procedure init_RF_2;              end interface
        interface init;      module procedure init_RF_3;              end interface

        interface delete;    module procedure delete_RF;              end interface

        interface assign;    module procedure assign_RF_S;            end interface
        interface assign;    module procedure assign_RF_RF;           end interface
        interface assign;    module procedure assign_RF_R;            end interface

        interface add;       module procedure add_RF_RF;              end interface
        interface add;       module procedure add_RF_R;               end interface
        interface add;       module procedure add_RF_S;               end interface
        interface add;       module procedure add_S_RF;               end interface

        interface subtract;  module procedure subtract_RF_RF;         end interface
        interface subtract;  module procedure subtract_RF_RF_RF;      end interface
        interface subtract;  module procedure subtract_RF_R_R;        end interface
        interface subtract;  module procedure subtract_RF_R;          end interface
        interface subtract;  module procedure subtract_RF_S;          end interface
        interface subtract;  module procedure subtract_S_RF;          end interface

        interface multiply;  module procedure multiply_RF_RF;         end interface
        interface multiply;  module procedure multiply_RF_RF_RF;      end interface
        interface multiply;  module procedure multiply_RF_S;          end interface
        interface multiply;  module procedure multiply_S_RF;          end interface

        interface divide;    module procedure divide_RF_RF;           end interface
        interface divide;    module procedure divide_RF_RF_RF;        end interface
        interface divide;    module procedure divide_RF_S;            end interface
        interface divide;    module procedure divide_S_RF;            end interface

        interface square;    module procedure square_RF;              end interface
        interface print;     module procedure printRF;                end interface
        ! interface sum;       module procedure sumRF;                  end interface

      contains

        subroutine assign_RF_RF(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%f(i),g%f(i)); enddo
        end subroutine

        subroutine assign_RF_S(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call assign(f%f(i),g); enddo
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine add_RF_RF(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%f(i),g%f(i)); enddo
        end subroutine

        subroutine add_RF_S(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call add(f%f(i),g); enddo
        end subroutine

        subroutine add_S_RF(g2,f)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call add(g2,f%f(i)); enddo
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine subtract_RF_RF(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%f(i),g%f(i)); enddo
        end subroutine

        subroutine subtract_RF_RF_RF(f,g,q)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call subtract(f%f(i),g%f(i),q%f(i)); enddo
        end subroutine

        subroutine subtract_RF_S(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call subtract(f%f(i),g); enddo
        end subroutine

        subroutine subtract_S_RF(g2,f)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call subtract(g2,f%f(i)); enddo
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine multiply_RF_RF(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g%f(i)); enddo
        end subroutine

        subroutine multiply_RF_RF_RF(f,g,q)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g%f(i),q%f(i)); enddo
        end subroutine

        subroutine multiply_RF_S(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g); enddo
        end subroutine

        subroutine multiply_S_RF(g2,f)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g2); enddo
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine divide_RF_RF(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g%f(i)); enddo
        end subroutine

        subroutine divide_RF_RF_RF(f,g,q)
          implicit none
          type(RF_array),intent(inout) :: f
          type(RF_array),intent(in) :: g,q
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g%f(i),q%f(i)); enddo
        end subroutine

        subroutine divide_RF_S(f,g)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g
          integer :: i
          do i=1,f%s; call multiply(f%f(i),g); enddo
        end subroutine

        subroutine divide_S_RF(g2,f)
          implicit none
          type(RF_array),intent(inout) :: f
          real(cp),intent(in) :: g2
          integer :: i
          do i=1,f%s; call multiply(g2,g%f(i)); enddo
        end subroutine

        subroutine square_RF(f)
          implicit none
          type(RF_array),intent(inout) :: f
          integer :: i
          do i=1,f%s; call square(f%f(i)); enddo
        end subroutine

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine init_RF_1(f,Nx,Ny,Nz)
          implicit none
          type(RF_array),intent(inout) :: f
          integer,intent(in) :: Nx,Ny,Nz
          integer :: i
          do i=1,f%s; call init(f%f(i),Nx,Ny,Nz); enddo
        end subroutine

        subroutine init_RF_2(f1,f2)
          implicit none
          type(RF_array),intent(inout) :: f1
          type(RF_array),intent(in) :: f2
          integer :: i
          do i=1,f%s; call init(f1%f(i),f2%f(i)); enddo
        end subroutine

        subroutine init_RF_3(f,s)
          implicit none
          type(RF_array),intent(inout) :: f
          integer,dimension(3),intent(in) :: s
          integer :: i
          do i=1,f%s; call init(f%f(i),s); enddo
        end subroutine

        subroutine delete_RF(f)
          implicit none
          type(RF_array),intent(inout) :: f
          integer :: i
          do i=1,f%s; call delete(f%f(i)); enddo
        end subroutine

        subroutine printRF(f)
          implicit none
          type(RF_array),intent(in) :: f
          integer :: i
          do i=1,f%s; call print(f%f(i)); enddo
        end subroutine

      end module