      module SF_mod
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
        !         _PARALLELIZE_SF_

        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: SF
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

        type SF
          integer,dimension(3) :: s
          real(cp),dimension(:,:,:),allocatable :: phi
        end type

        interface init;      module procedure init1;                  end interface
        interface init;      module procedure init2;                  end interface
        interface init;      module procedure init3;                  end interface

        interface delete;    module procedure deleteSF;               end interface

        interface assign;    module procedure scalarAssign;           end interface
        interface assign;    module procedure fieldFieldAssign;       end interface
        interface assign;    module procedure fieldRealAssign;        end interface

        interface add;       module procedure fieldFieldAdd;          end interface
        interface add;       module procedure fieldRealAdd;           end interface
        interface add;       module procedure fieldScalarAdd;         end interface
        interface add;       module procedure SFAdd;                  end interface

        interface subtract;  module procedure fieldFieldSubtract;     end interface
        interface subtract;  module procedure fieldFieldSubtract2;    end interface
        interface subtract;  module procedure fieldRRSubtract;        end interface
        interface subtract;  module procedure SF_RFSubtract;          end interface
        interface subtract;  module procedure fieldScalarSubtract;    end interface
        interface subtract;  module procedure SFSubtract;             end interface

        interface multiply;  module procedure fieldFieldMultiply;     end interface
        interface multiply;  module procedure fieldFieldMultiply2;    end interface
        interface multiply;  module procedure fieldScalarMultiply;    end interface
        interface multiply;  module procedure SFMultiply;             end interface

        interface divide;    module procedure fieldFieldDivide;       end interface
        interface divide;    module procedure fieldFieldDivide2;      end interface
        interface divide;    module procedure fieldScalarDivide;      end interface
        interface divide;    module procedure SFDivide;               end interface

        interface square;    module procedure squareSF;               end interface
        interface print;     module procedure printSF;                end interface
        ! interface sum;       module procedure sumSF;                  end interface

      contains

        subroutine fieldFieldAssign(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g%phi
#endif
        end subroutine

        subroutine fieldRealAssign(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),dimension(:,:,:),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g
#endif
        end subroutine

        subroutine scalarAssign(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g
#endif
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine fieldFieldAdd(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) + g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi + g%phi
#endif
        end subroutine

        subroutine fieldRealAdd(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),dimension(:,:,:),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) + g(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi + g
#endif
        end subroutine

        subroutine fieldScalarAdd(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) + g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi + g
#endif
        end subroutine
        subroutine SFAdd(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) + g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi + g2
#endif
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine fieldFieldSubtract(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) - g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi - g%phi
#endif
        end subroutine

        subroutine fieldFieldSubtract2(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g%phi(i,j,k) - q%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g%phi - q%phi
#endif
        end subroutine

        subroutine fieldRRSubtract(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),dimension(:,:,:),intent(in) :: g,q
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g(i,j,k) - q(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g - q
#endif
        end subroutine

        subroutine fieldScalarSubtract(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) - g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi - g
#endif
        end subroutine

        subroutine SF_RFSubtract(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),dimension(:,:,:),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) - g(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi - g
#endif
        end subroutine
        subroutine SFSubtract(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g2 - f%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g2 - f%phi
#endif
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine fieldFieldMultiply(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) * g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi * g%phi
#endif
        end subroutine

        subroutine fieldFieldMultiply2(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g%phi(i,j,k) * q%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g%phi * q%phi
#endif
        end subroutine

        subroutine fieldScalarMultiply(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) * g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi * g
#endif
        end subroutine
        subroutine SFMultiply(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) * g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi * g2
#endif
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine fieldFieldDivide(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) / g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi / g%phi
#endif
        end subroutine

        subroutine fieldFieldDivide2(f,g,q)
          implicit none
          type(SF),intent(inout) :: f
          type(SF),intent(in) :: g,q
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g%phi(i,j,k) / q%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g%phi / q%phi
#endif
        end subroutine

        subroutine fieldScalarDivide(f,g)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) / g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi / g
#endif
        end subroutine
        subroutine SFDivide(g2,f)
          implicit none
          type(SF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g2 / f%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g2 / f%phi
#endif
        end subroutine

        subroutine squareSF(f)
          implicit none
          type(SF),intent(inout) :: f
#ifdef _PARALLELIZE_SF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = f%phi(i,j,k) * f%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = f%phi*f%phi
#endif
        end subroutine

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine init1(field,Nx,Ny,Nz)
          implicit none
          type(SF),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%phi)) deallocate(field%phi)
          allocate(field%phi(Nx,Ny,Nz))
          field%s = shape(field%phi)
        end subroutine

        subroutine init2(field1,field2)
          implicit none
          type(SF),intent(inout) :: field1
          type(SF),intent(in) :: field2
          integer,dimension(3) :: s
          s = shape(field2%phi)
          if (allocated(field1%phi)) deallocate(field1%phi)
          allocate(field1%phi(s(1),s(2),s(3)))
          field1%s = shape(field1%phi)
        end subroutine

        subroutine init3(field,s)
          implicit none
          type(SF),intent(inout) :: field
          integer,dimension(3),intent(in) :: s
          if (allocated(field%phi)) deallocate(field%phi)
          allocate(field%phi(s(1),s(2),s(3)))
          field%s = shape(field%phi)
        end subroutine

        subroutine deleteSF(field)
          implicit none
          type(SF),intent(inout) :: field
          if (allocated(field%phi)) deallocate(field%phi)
          field%s = 0
        end subroutine

        subroutine printSF(field)
          implicit none
          type(SF),intent(in) :: field
          integer :: i,j,k
          if (allocated(field%phi))   then
            write(*,*) 'shape(phi) = ',field%s
            do i=1,field%s(1)
              do j=1,field%s(2)
                do k=1,field%s(3)
                  write(*,'(A4,I1,A,I1,A,I1,A4,1F15.6)') 'phi(',i,',',j,',',k,') = ',field%phi(i,j,k)
                enddo
              enddo
            enddo
          endif
        end subroutine

      end module