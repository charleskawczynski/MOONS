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

        ! Available pre-processor directives:
        !         _PARALLELIZE_VF_

        use SF_mod
        implicit none
        private

        ! Initialization / Deletion (allocate/deallocate)
        public :: VF
        public :: init,delete
        public :: allocateX,allocateY,allocateZ

        ! Monitoring
        public :: print

        ! Operators
        public :: assign
        public :: add,subtract
        public :: multiply,divide
        public :: sum,square

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
          integer,dimension(3) :: sx,sy,sz
          real(cp),dimension(:,:,:),allocatable :: x,y,z
        end type

        interface init;     module procedure initVFField1;          end interface
        interface init;     module procedure initVFField2;          end interface
        interface init;     module procedure initVFField3;          end interface
        interface init;     module procedure initVFField4;          end interface
        interface init;     module procedure initVFField5;          end interface

        interface delete;   module procedure deleteVF;              end interface
        interface print;    module procedure printVF;               end interface

        interface assign;   module procedure vectorScalarAssign;    end interface
        interface assign;   module procedure VFAssign;              end interface
        interface assign;   module procedure vectorVectorAssign;    end interface

        interface add;      module procedure vectorVectorAdd;       end interface
        interface add;      module procedure vectorVectorAdd2;      end interface
        interface add;      module procedure VFAdd;                 end interface
        interface add;      module procedure fieldVectorAdd;        end interface
        interface add;      module procedure vectorScalarAdd;       end interface
        interface add;      module procedure scalarVectorAdd;       end interface

        interface subtract; module procedure vectorVectorSubtract;  end interface
        interface subtract; module procedure VFSubtract;            end interface
        interface subtract; module procedure fieldVectorSubtract;   end interface
        interface subtract; module procedure vectorScalarSubtract;  end interface
        interface subtract; module procedure scalarVectorSubtract;  end interface

        interface multiply; module procedure vectorVectorMultiply;  end interface
        interface multiply; module procedure VFMultiply;            end interface
        interface multiply; module procedure fieldVectorMultiply;   end interface
        interface multiply; module procedure vectorScalarMultiply;  end interface
        interface multiply; module procedure scalarVectorMultiply;  end interface

        interface divide;   module procedure vectorVectorDivide;    end interface
        interface divide;   module procedure VFDivide;              end interface
        interface divide;   module procedure fieldVectorDivide;     end interface
        interface divide;   module procedure vectorScalarDivide;    end interface
        interface divide;   module procedure scalarVectorDivide;    end interface

        interface square;   module procedure vectorVectorSquare;    end interface
        interface sum;      module procedure vectorSum;             end interface

        contains

        ! ----------------- ASSIGN ------------------

        subroutine vectorVectorAssign(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g%x
          f%y = g%y
          f%z = g%z
#endif
        end subroutine

        subroutine VFAssign(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g%phi
          f%y = g%phi
          f%z = g%phi
#endif
        end subroutine


        subroutine vectorScalarAssign(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g
          f%y = g
          f%z = g
#endif
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine vectorVectorAdd(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) + g%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) + g%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) + g%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x + g%x
          f%y = f%y + g%y
          f%z = f%z + g%z
#endif
        end subroutine

        subroutine vectorVectorAdd2(f,g,r)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g,r
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g%x(i,j,k) + r%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g%y(i,j,k) + r%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g%z(i,j,k) + r%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g%x + r%x
          f%y = g%y + r%y
          f%z = g%z + r%z
#endif
        end subroutine

        subroutine VFAdd(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) + g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) + g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) + g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x + g%phi
          f%y = f%y + g%phi
          f%z = f%z + g%phi
#endif
        end subroutine
        subroutine fieldVectorAdd(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) + g2%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) + g2%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) + g2%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x + g2%phi
          f%y = f%y + g2%phi
          f%z = f%z + g2%phi
#endif
        end subroutine

        subroutine vectorScalarAdd(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) + g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) + g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) + g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x + g
          f%y = f%y + g
          f%z = f%z + g
#endif
        end subroutine
        subroutine scalarVectorAdd(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) + g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) + g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) + g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x + g2
          f%y = f%y + g2
          f%z = f%z + g2
#endif
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine vectorVectorSubtract(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) - g%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) - g%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) - g%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x - g%x
          f%y = f%y - g%y
          f%z = f%z - g%z
#endif
        end subroutine

        subroutine VFSubtract(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) - g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) - g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) - g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x - g%phi
          f%y = f%y - g%phi
          f%z = f%z - g%phi
#endif
        end subroutine
        subroutine fieldVectorSubtract(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g2%phi(i,j,k) - f%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g2%phi(i,j,k) - f%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g2%phi(i,j,k) - f%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g2%phi - f%x
          f%y = g2%phi - f%y
          f%z = g2%phi - f%z
#endif
        end subroutine

        subroutine vectorScalarSubtract(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) - g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) - g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) - g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x - g
          f%y = f%y - g
          f%z = f%z - g
#endif
        end subroutine
        subroutine scalarVectorSubtract(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g2 - f%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g2 - f%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g2 - f%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g2 - f%x
          f%y = g2 - f%y
          f%z = g2 - f%z
#endif
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine vectorVectorMultiply(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) * g%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) * g%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) * g%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x * g%x
          f%y = f%y * g%y
          f%z = f%z * g%z
#endif
        end subroutine

        subroutine VFMultiply(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) * g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) * g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) * g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x * g%phi
          f%y = f%y * g%phi
          f%z = f%z * g%phi
#endif
        end subroutine
        subroutine fieldVectorMultiply(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) * g2%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) * g2%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) * g2%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x * g2%phi
          f%y = f%y * g2%phi
          f%z = f%z * g2%phi
#endif
        end subroutine

        subroutine vectorScalarMultiply(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) * g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) * g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) * g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x * g
          f%y = f%y * g
          f%z = f%z * g
#endif
        end subroutine
        subroutine scalarVectorMultiply(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) * g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) * g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) * g2
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x * g2
          f%y = f%y * g2
          f%z = f%z * g2
#endif
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine vectorVectorDivide(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) / g%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) / g%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) / g%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x / g%x
          f%y = f%y / g%y
          f%z = f%z / g%z
#endif
        end subroutine

        subroutine VFDivide(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) / g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) / g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) / g%phi(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x / g%phi
          f%y = f%y / g%phi
          f%z = f%z / g%phi
#endif
        end subroutine
        subroutine fieldVectorDivide(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g2%phi(i,j,k) / f%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g2%phi(i,j,k) / f%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g2%phi(i,j,k) / f%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g2%phi / f%x
          f%y = g2%phi / f%y
          f%z = g2%phi / f%z
#endif
        end subroutine

        subroutine vectorScalarDivide(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k) / g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k) / g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k) / g
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x / g
          f%y = f%y / g
          f%z = f%z / g
#endif
        end subroutine
        subroutine scalarVectorDivide(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = g2 / f%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = g2 / f%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = g2 / f%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = g2 / f%x
          f%y = g2 / f%y
          f%z = g2 / f%z
#endif
        end subroutine

        subroutine vectorVectorSquare(f)
          implicit none
          type(VF),intent(inout) :: f
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%sx(3)
            do j=1,f%sx(2)
              do i=1,f%sx(1)
                f%x(i,j,k) = f%x(i,j,k)*f%x(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sy(3)
            do j=1,f%sy(2)
              do i=1,f%sy(1)
                f%y(i,j,k) = f%y(i,j,k)*f%y(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
          !$OMP PARALLEL DO
          do k=1,f%sz(3)
            do j=1,f%sz(2)
              do i=1,f%sz(1)
                f%z(i,j,k) = f%z(i,j,k)*f%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%x = f%x*f%x
          f%y = f%y*f%y
          f%z = f%z*f%z
#endif
        end subroutine

        subroutine vectorSum(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(VF),intent(in) :: g
#ifdef _PARALLELIZE_VF_
          integer :: i,j,k
          !$OMP PARALLEL DO
          do k=1,f%s(3)
            do j=1,f%s(2)
              do i=1,f%s(1)
                f%phi(i,j,k) = g%x(i,j,k) + g%y(i,j,k) + g%z(i,j,k)
              enddo
            enddo
          enddo
          !$OMP END PARALLEL DO
#else
          f%phi = g%x + g%y + g%z
#endif
        end subroutine

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine initVFField1(field,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: field
          integer,dimension(3),intent(in) :: Nx,Ny,Nz
          call allocateX(field,Nx(1),Nx(2),Nx(3))
          call allocateY(field,Ny(1),Ny(2),Ny(3))
          call allocateZ(field,Nz(1),Nz(2),Nz(3))
        end subroutine

        subroutine initVFField2(field,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          call allocateX(field,Nx,Ny,Nz)
          call allocateY(field,Nx,Ny,Nz)
          call allocateZ(field,Nx,Ny,Nz)
        end subroutine

        subroutine initVFField3(VF1,VF2)
          implicit none
          type(VF),intent(inout) :: VF1
          type(VF),intent(in) :: VF2
          call allocateX(VF1,VF2%sx(1),VF2%sx(2),VF2%sx(3))
          call allocateY(VF1,VF2%sy(1),VF2%sy(2),VF2%sy(3))
          call allocateZ(VF1,VF2%sz(1),VF2%sz(2),VF2%sz(3))
        end subroutine

        subroutine initVFField4(SF1,SF2)
          implicit none
          type(VF),intent(inout) :: SF1
          type(SF),intent(in) :: SF2
          call allocateX(SF1,SF2%s(1),SF2%s(2),SF2%s(3))
          call allocateY(SF1,SF2%s(1),SF2%s(2),SF2%s(3))
          call allocateZ(SF1,SF2%s(1),SF2%s(2),SF2%s(3))
        end subroutine

        subroutine initVFField5(f1,s)
          implicit none
          type(VF),intent(inout) :: f1
          integer,dimension(3),intent(in) :: s
          call allocateX(f1,s(1),s(2),s(3))
          call allocateY(f1,s(1),s(2),s(3))
          call allocateZ(f1,s(1),s(2),s(3))
        end subroutine

        subroutine allocateX(field,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%x)) deallocate(field%x)
          allocate(field%x(Nx,Ny,Nz)); field%sx = shape(field%x)
        end subroutine

        subroutine allocateY(field,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%y)) deallocate(field%y)
          allocate(field%y(Nx,Ny,Nz)); field%sy = shape(field%y)
        end subroutine

        subroutine allocateZ(field,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%z)) deallocate(field%z)
          allocate(field%z(Nx,Ny,Nz)); field%sz = shape(field%z)
        end subroutine

        subroutine deleteVF(field)
          implicit none
          type(VF),intent(inout) :: field
          if (allocated(field%x)) deallocate(field%x)
          if (allocated(field%y)) deallocate(field%y)
          if (allocated(field%z)) deallocate(field%z)
          field%sx = 0; field%sy = 0; field%sz = 0
        end subroutine

        subroutine printVF(field)
          implicit none
          type(VF),intent(in) :: field
          integer :: i,j,k
          if (allocated(field%x)) write(*,*) 'shape(x) = ',field%sx
          if (allocated(field%y)) write(*,*) 'shape(y) = ',field%sy
          if (allocated(field%z)) write(*,*) 'shape(z) = ',field%sz

          if (allocated(field%x))   then
            do i=1,field%sx(1)
              do j=1,field%sx(2)
                do k=1,field%sx(3)
                  write(*,'(A2,I1,A,I1,A,I1,A4,1F15.6)') 'x(',i,',',j,',',k,') = ',field%x(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (allocated(field%y))   then
            do i=1,field%sy(1)
              do j=1,field%sy(2)
                do k=1,field%sy(3)
                  write(*,'(A2,I1,A,I1,A,I1,A4,1F15.6)') 'y(',i,',',j,',',k,') = ',field%y(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (allocated(field%z))   then
            do i=1,field%sz(1)
              do j=1,field%sz(2)
                do k=1,field%sz(3)
                  write(*,'(A2,I1,A,I1,A,I1,A4,1F15.6)') 'z(',i,',',j,',',k,') = ',field%z(i,j,k)
                enddo
              enddo
            enddo
          endif
        end subroutine

      end module