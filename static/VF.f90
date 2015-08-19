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
          type(SF) :: x,y,z
        end type

        interface init;     module procedure initVFField1;          end interface
        interface init;     module procedure initVFField2;          end interface
        interface init;     module procedure initVFField3;          end interface
        interface init;     module procedure initVFField4;          end interface
        interface init;     module procedure initVFField5;          end interface

        interface delete;   module procedure deleteVF;              end interface
        interface print;    module procedure printVF;               end interface

        interface assignX;   module procedure assignXVF;            end interface
        interface assignY;   module procedure assignYVF;            end interface
        interface assignZ;   module procedure assignZVF;            end interface
        interface assignX;   module procedure assignXVFS;           end interface
        interface assignY;   module procedure assignYVFS;           end interface
        interface assignZ;   module procedure assignZVFS;           end interface

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

        subroutine assignXVF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%x,g%x)
        end subroutine

        subroutine assignYVF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%y,g%y)
        end subroutine

        subroutine assignZVF(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%z,g%z)
        end subroutine

        subroutine assignXVFS(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%x,g)
        end subroutine

        subroutine assignYVFS(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%y,g)
        end subroutine

        subroutine assignZVFS(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%z,g)
        end subroutine

        subroutine vectorVectorAssign(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call assign(f%x,g%x); call assign(f%y,g%y); call assign(f%z,g%z)
        end subroutine

        subroutine VFAssign(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call assign(f%x,g); call assign(f%y,g); call assign(f%z,g)
        end subroutine

        subroutine vectorScalarAssign(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call assign(f%x,g); call assign(f%y,g); call assign(f%z,g)
        end subroutine

      ! ------------------- ADD ------------------------

        subroutine vectorVectorAdd(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call add(f%x,g%x); call add(f%y,g%y); call add(f%z,g%z)
        end subroutine

        subroutine vectorVectorAdd2(f,g,r)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g,r
          call add(f%x,g%x,r%x); call add(f%y,g%y,r%y); call add(f%z,g%z,r%z)
        end subroutine

        subroutine VFAdd(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine fieldVectorAdd(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
          call add(f%x,g2); call add(f%y,g2); call add(f%z,g2)
        end subroutine

        subroutine vectorScalarAdd(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine scalarVectorAdd(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call add(f%x,g2); call add(f%y,g2); call add(f%z,g2)
        end subroutine

      ! ------------------- SUBTRACT ------------------------

        subroutine vectorVectorSubtract(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine VFSubtract(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call add(f%x,g); call add(f%y,g); call add(f%z,g)
        end subroutine

        subroutine fieldVectorSubtract(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
          call subtract(g2,f%x); call subtract(g2,f%y); call subtract(g2,f%z)
        end subroutine

        subroutine vectorScalarSubtract(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call subtract(f%x,g); call subtract(f%y,g); call subtract(f%z,g)
        end subroutine

        subroutine scalarVectorSubtract(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call subtract(g2,f%x); call subtract(g2,f%y); call subtract(g2,f%z)
        end subroutine

      ! ------------------- MULTIPLY ------------------------

        subroutine vectorVectorMultiply(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine VFMultiply(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine fieldVectorMultiply(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
          call multiply(f%x,g2); call multiply(f%y,g2); call multiply(f%z,g2)
        end subroutine

        subroutine vectorScalarMultiply(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call multiply(f%x,g); call multiply(f%y,g); call multiply(f%z,g)
        end subroutine

        subroutine scalarVectorMultiply(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call multiply(f%x,g2); call multiply(f%y,g2); call multiply(f%z,g2)
        end subroutine

      ! ------------------- DIVIDE ------------------------

        subroutine vectorVectorDivide(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(VF),intent(in) :: g
          call divide(f%x,g%x); call divide(f%y,g%y); call divide(f%z,g%z)
        end subroutine

        subroutine VFDivide(f,g)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g
          call divide(f%x,g); call divide(f%y,g); call divide(f%z,g)
        end subroutine

        subroutine fieldVectorDivide(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          type(SF),intent(in) :: g2
          call divide(g2,f%x); call divide(g2,f%y); call divide(g2,f%z)
        end subroutine

        subroutine vectorScalarDivide(f,g)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g
          call divide(f%x,g); call divide(f%y,g); call divide(f%z,g)
        end subroutine

        subroutine scalarVectorDivide(g2,f)
          implicit none
          type(VF),intent(inout) :: f
          real(cp),intent(in) :: g2
          call divide(g2,f%x); call divide(g2,f%y); call divide(g2,f%z)
        end subroutine

        subroutine vectorVectorSquare(f)
          implicit none
          type(VF),intent(inout) :: f
          call square(f%x); call square(f%y); call square(f%z)
        end subroutine

        subroutine vectorSum(f,g)
          implicit none
          type(SF),intent(inout) :: f
          type(VF),intent(in) :: g
          call sum(f%x,g%x); call sum(f%y,g%y); call sum(f%z,g%z)
        end subroutine

      ! ------------------- ALLOCATE / DEALLOCATE --------------------

        subroutine initVFField1(f,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: f
          integer,dimension(3),intent(in) :: Nx,Ny,Nz
          call init(f%x,Nx(1),Nx(2),Nx(3))
          call init(f%y,Ny(1),Ny(2),Ny(3))
          call init(f%z,Nz(1),Nz(2),Nz(3))
        end subroutine

        subroutine initVFField2(f,Nx,Ny,Nz)
          implicit none
          type(VF),intent(inout) :: f
          integer,intent(in) :: Nx,Ny,Nz
          call init(f%x,Nx,Nx,Nx); call init(f%y,Ny,Ny,Ny); call init(f%z,Nz,Nz,Nz)
        end subroutine

        subroutine initVFField3(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(VF),intent(in) :: f2
          call init(f1%x,f2%x); call init(f1%y,f2%y); call init(f1%z,f2%z)
        end subroutine

        subroutine initVFField4(f1,f2)
          implicit none
          type(VF),intent(inout) :: f1
          type(SF),intent(in) :: f2
          call init(f1%x,f2); call init(f1%y,f2); call init(f1%z,f2)
        end subroutine

        subroutine initVFField5(f1,s)
          implicit none
          type(VF),intent(inout) :: f1
          integer,dimension(3),intent(in) :: s
          call init(f1%x,s); call init(f1%y,s); call init(f1%z,s)
        end subroutine

        subroutine deleteVF(f)
          implicit none
          type(VF),intent(inout) :: f
          call delete(f%x); call delete(f%y); call delete(f%z)
        end subroutine

        subroutine printVF(f)
          implicit none
          type(VF),intent(in) :: f
          call print(f%x); call print(f%y); call print(f%z)
        end subroutine

      end module