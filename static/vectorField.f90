      module vectorField_mod

        ! Rules:
        ! a = a + b => call add(a,b)
        ! a = a - b => call subtract(a,b)
        ! a = a * b => call multiply(a,b)
        ! a = a / b => call divide(a,b)

        ! Available pre-processor directives:
        !         _DEBUG_VECTOR_
        !         _PARALLELIZE_VECTOR_FIELD_

        use scalarField_mod
        implicit none
        private

        public :: vectorField
        public :: allocateVectorField
        public :: allocateX,allocateY,allocateZ

        ! Optimized for readability:
        public :: assignment(=)                 ! Causes segfault on hoffman
        public :: operator(+),operator(-)       ! Causes segfault on hoffman
        public :: operator(*),operator(/)       ! Causes segfault on hoffman

        ! Optimized for speed:
        public :: assign,delete
        public :: add,subtract
        public :: multiply,divide

        public :: checkVectorField
        public :: printVectorField

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

        type vectorField
          integer,dimension(3) :: sx,sy,sz
          real(cp),dimension(:,:,:),allocatable :: x,y,z
        end type

#ifdef _DEBUG_VECTOR_
      interface compare
        module procedure compareVectorVector
        module procedure compareVectorField
      end interface
#endif

      interface allocateVectorField
        module procedure allocateVectorField1
        module procedure allocateVectorField2
      end interface

      interface delete
        module procedure deallocateVectorField
      end interface

      ! Operators (optimized for readability)

      interface assignment (=)
        module procedure vectorScalarAssignOp
        module procedure vectorFieldAssignOp
        module procedure vectorVectorAssignOp
      end interface

      interface operator (+)
        module procedure vectorVectorAddOp
        module procedure vectorScalarAddOp
        module procedure scalarVectorAddOp
        module procedure vectorFieldAddOp
        module procedure fieldVectorAddOp
      end interface

      interface operator (-)
        module procedure vectorVectorSubtractOp
        module procedure vectorScalarSubtractOp
        module procedure scalarVectorSubtractOp
        module procedure vectorFieldSubtractOp
        module procedure fieldVectorSubtractOp
      end interface

      interface operator (*)
        module procedure vectorVectirMultiplyOp
        module procedure vectorScalarMultiplyOp
        module procedure scalarVectorMultiplyOp
        module procedure vectorFieldMultiplyOp
        module procedure fieldVectorMultiplyOp
      end interface

      interface operator (/)
        module procedure vectorVectorDivideOp
        module procedure vectorScalarDivideOp
        module procedure scalarVectorDivideOp
        module procedure vectorFieldDivideOp
        module procedure fieldVectorDivideOp
      end interface

      ! Operators (optimized for speed)

      interface assign
        module procedure vectorScalarAssign
        module procedure vectorFieldAssign
        module procedure vectorVectorAssign
      end interface

      interface add
        module procedure vectorVectorAdd
        module procedure vectorFieldAdd
        module procedure fieldVectorAdd
        module procedure vectorScalarAdd
        module procedure scalarVectorAdd
      end interface

      interface subtract
        module procedure vectorVectorSubtract
        module procedure vectorFieldSubtract
        module procedure fieldVectorSubtract
        module procedure vectorScalarSubtract
        module procedure scalarVectorSubtract
      end interface

      interface multiply
        module procedure vectorVectorMultiply
        module procedure vectorFieldMultiply
        module procedure fieldVectorMultiply
        module procedure vectorScalarMultiply
        module procedure scalarVectorMultiply
      end interface

      interface divide
        module procedure vectorVectorDivide
        module procedure vectorFieldDivide
        module procedure fieldVectorDivide
        module procedure vectorScalarDivide
        module procedure scalarVectorDivide
      end interface

      contains

#ifdef _DEBUG_VECTOR_
        function cmp(a,b) result(TFall)
          implicit none
          integer,dimension(3),intent(in) :: a,b
          logical,dimension(3) :: TF
          logical :: TFall
          TF(1)=a(1).eq.b(1); TF(2)=a(2).eq.b(2); TF(3)=a(3).eq.b(3)
          TFall=all(TF)
        end function

        subroutine compareVectorVector(f,g,name)
          implicit none
          type(vectorField),intent(in) :: f,g
          character(len=*),intent(in),optional :: name
          logical,dimension(3) :: TF
          TF(1) = .not.cmp(f%sx,g%sx)
          TF(2) = .not.cmp(f%sy,g%sy)
          TF(3) = .not.cmp(f%sz,g%sz)
          if(any(TF)) then
            if (present(name)) then
                  write(*,*) 'Vector-vector bad size inside ',name
            else; write(*,*) 'Vector-vector bad size (unknown operator)'
            endif
            write(*,*) 'f(x,y,z) = ',f%sx,f%sy,f%sz
            write(*,*) 'g(x,y,z) = ',g%sx,g%sy,g%sz
            stop
            write(*,*) '---------------------'
          endif
        end subroutine

        subroutine compareVectorField(f,g,name)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          character(len=*),intent(in),optional :: name
          logical,dimension(3) :: TF
          TF(1) = .not.cmp(f%sx,g%s)
          TF(2) = .not.cmp(f%sy,g%s)
          TF(3) = .not.cmp(f%sz,g%s)
          if(any(TF)) then
            if (present(name)) then
                  write(*,*) 'Vector-field bad size inside ',name
            else; write(*,*) 'Vector-field bad size (unknown operator)'
            endif
            write(*,*) 'x,y,z,phi = ',f%sx,f%sy,f%sz,g%s
            stop
            write(*,*) '---------------------'
          endif
        end subroutine
#endif

        ! ***************** OPERATORS OPTIMIZED FOR READABILITY ************
        ! ------------------- ASSIGN ------------------------

        subroutine vectorScalarAssignOp(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g
          f%x = g
          f%y = g
          f%z = g
        end subroutine

        subroutine vectorVectorAssignOp(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(vectorField),intent(in) :: g
          f%x = g%x
          f%y = g%y
          f%z = g%z
          f%sx = g%sx
          f%sy = g%sy
          f%sz = g%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VV assign')
#endif
        end subroutine

        subroutine vectorFieldAssignOp(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g
          f%x = g%phi
          f%y = g%phi
          f%z = g%phi
          f%sx = g%s
          f%sy = g%s
          f%sz = g%s
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VF assign')
#endif
        end subroutine

      ! ------------------- ADD ------------------------

        function vectorVectorAddOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f,g
          type(vectorField) :: q
          q%x = f%x + g%x
          q%y = f%y + g%y
          q%z = f%z + g%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VV add')
#endif
        end function

        function vectorScalarAddOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x + g
          q%y = f%y + g
          q%z = f%z + g
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function
        function scalarVectorAddOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x + g
          q%y = f%y + g
          q%z = f%z + g
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function

        function vectorFieldAddOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x + g%phi
          q%y = f%y + g%phi
          q%z = f%z + g%phi
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function
        function fieldVectorAddOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x + g%phi
          q%y = f%y + g%phi
          q%z = f%z + g%phi
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function

      ! ------------------- SUBTRACT ------------------------

        function vectorVectorSubtractOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f,g
          type(vectorField) :: q
          q%x = f%x - g%x
          q%y = f%y - g%y
          q%z = f%z - g%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function

        function vectorScalarSubtractOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x - g
          q%y = f%y - g
          q%z = f%z - g
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function
        function scalarVectorSubtractOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = g - f%x
          q%y = g - f%y
          q%z = g - f%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function

        function vectorFieldSubtractOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x - g%phi
          q%y = f%y - g%phi
          q%z = f%z - g%phi
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function
        function fieldVectorSubtractOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = g%phi - f%x
          q%y = g%phi - f%y
          q%z = g%phi - f%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function

      ! ------------------- MULTIPLY ------------------------

        function vectorVectirMultiplyOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f,g
          type(vectorField) :: q
          q%x = f%x * g%x
          q%y = f%y * g%y
          q%z = f%z * g%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function

        function vectorScalarMultiplyOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x * g
          q%y = f%y * g
          q%z = f%z * g
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function
        function scalarVectorMultiplyOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x * g
          q%y = f%y * g
          q%z = f%z * g
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function

        function vectorFieldMultiplyOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x * g%phi
          q%y = f%y * g%phi
          q%z = f%z * g%phi
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function
        function fieldVectorMultiplyOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x * g%phi
          q%y = f%y * g%phi
          q%z = f%z * g%phi
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function

      ! ------------------- DIVIDE ------------------------

        function vectorVectorDivideOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f,g
          type(vectorField) :: q
          q%x = f%x / g%x
          q%y = f%y / g%y
          q%z = f%z / g%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end function

        function vectorScalarDivideOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x / g
          q%y = f%y / g
          q%z = f%z / g
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function
        function scalarVectorDivideOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          real(cp),intent(in) :: g
          type(vectorField) :: q
          q%x = g / f%x
          q%y = g / f%y
          q%z = g / f%z
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
        end function

        function vectorFieldDivideOp(f,g) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = f%x / g%phi
          q%y = f%y / g%phi
          q%z = f%z / g%phi
          q%sx = f%sx
          q%sy = f%sy
          q%sz = f%sz
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VF divide')
#endif
        end function
        function fieldVectorDivideOp(g,f) result(q)
          implicit none
          type(vectorField),intent(in) :: f
          type(scalarField),intent(in) :: g
          type(vectorField) :: q
          q%x = g%phi / f%x
          q%y = g%phi / f%y
          q%z = g%phi / f%z
          q%sx = g%s
          q%sy = g%s
          q%sz = g%s
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'FV divide')
#endif
        end function


        ! ***************** OPERATORS OPTIMIZED FOR SPEED ************
        ! ----------------- ASSIGN ------------------

        subroutine vectorVectorAssign(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(vectorField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VV assign')
#endif
        end subroutine

        subroutine vectorFieldAssign(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VF assign')
#endif
        end subroutine


        subroutine vectorScalarAssign(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          type(vectorField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VV add')
#endif
        end subroutine

        subroutine vectorFieldAdd(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end subroutine
        subroutine fieldVectorAdd(g2,f)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g2)
#endif
        end subroutine

        subroutine vectorScalarAdd(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          type(vectorField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end subroutine

        subroutine vectorFieldSubtract(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end subroutine
        subroutine fieldVectorSubtract(g2,f)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g2)
#endif
        end subroutine

        subroutine vectorScalarSubtract(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          type(vectorField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end subroutine

        subroutine vectorFieldMultiply(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end subroutine
        subroutine fieldVectorMultiply(g2,f)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g2)
#endif
        end subroutine

        subroutine vectorScalarMultiply(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          type(vectorField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g)
#endif
        end subroutine

        subroutine vectorFieldDivide(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g,'VF divide')
#endif
        end subroutine
        subroutine fieldVectorDivide(g2,f)
          implicit none
          type(vectorField),intent(inout) :: f
          type(scalarField),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
#ifdef _DEBUG_VECTOR_
          call compare(f,g2,'FV divide')
#endif
        end subroutine

        subroutine vectorScalarDivide(f,g)
          implicit none
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g
#ifdef _PARALLELIZE_VECTOR_FIELD_
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
          type(vectorField),intent(inout) :: f
          real(cp),intent(in) :: g2
#ifdef _PARALLELIZE_VECTOR_FIELD_
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

      ! ------------------- ALLOCATE / DEALLOCATE --------------------


        subroutine allocateVectorField1(field,Nx,Ny,Nz)
          implicit none
          type(vectorField),intent(inout) :: field
          integer,dimension(3),intent(in) :: Nx,Ny,Nz
          call allocateX(field,Nx(1),Nx(2),Nx(3))
          call allocateY(field,Ny(1),Ny(2),Ny(3))
          call allocateZ(field,Nz(1),Nz(2),Nz(3))
        end subroutine

        subroutine allocateVectorField2(field,Nx,Ny,Nz)
          implicit none
          type(vectorField),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          call allocateX(field,Nx,Ny,Nz)
          call allocateY(field,Nx,Ny,Nz)
          call allocateZ(field,Nx,Ny,Nz)
        end subroutine

        subroutine allocateX(field,Nx,Ny,Nz)
          implicit none
          type(vectorField),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%x)) deallocate(field%x)
          allocate(field%x(Nx,Ny,Nz)); field%sx = shape(field%x)
        end subroutine

        subroutine allocateY(field,Nx,Ny,Nz)
          implicit none
          type(vectorField),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%y)) deallocate(field%y)
          allocate(field%y(Nx,Ny,Nz)); field%sy = shape(field%y)
        end subroutine

        subroutine allocateZ(field,Nx,Ny,Nz)
          implicit none
          type(vectorField),intent(inout) :: field
          integer,intent(in) :: Nx,Ny,Nz
          if (allocated(field%z)) deallocate(field%z)
          allocate(field%z(Nx,Ny,Nz)); field%sz = shape(field%z)
        end subroutine

        subroutine deallocateVectorField(field)
          implicit none
          type(vectorField),intent(inout) :: field
          if (allocated(field%x)) deallocate(field%x)
          if (allocated(field%y)) deallocate(field%y)
          if (allocated(field%z)) deallocate(field%z)
          field%sx = 0; field%sy = 0; field%sz = 0
        end subroutine

        subroutine checkVectorField(field,name)
          implicit none
          type(vectorField),intent(in) :: field
          character(len=*),intent(in) :: name
          write(*,*) 'Vector info for: ',name
          if (allocated(field%x)) then
                write(*,*) 'x allocated, sx = ',field%sx
          else; write(*,*) 'x NOT allocated'
          endif
          if (allocated(field%y)) then
                write(*,*) 'y allocated, sy = ',field%sy
          else; write(*,*) 'y NOT allocated'
          endif
          if (allocated(field%z)) then
                write(*,*) 'z allocated, sz = ',field%sz
          else; write(*,*) 'z NOT allocated'
          endif
        end subroutine

        subroutine printVectorField(field)
          implicit none
          type(vectorField),intent(in) :: field
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