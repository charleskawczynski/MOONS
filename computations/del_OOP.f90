      module del_mod
      ! call del(dfdh,f,g,n,dir,diffType,pad) returns an nth-derivative of the
      ! scalar field, f, along direction dir (1,2,3) which corresponds to (x,y,z).
      ! 
      ! 
      ! call del(dfdh,f,g,n,dir,diffType,pad,del%add)
      ! 
      ! dfdh = dfdh + d/dh (f)
      ! dfdh = dfdh - d/dh (f)
      ! dfdh = dfdh * d/dh (f)
      ! dfdh = dfdh / d/dh (f)
      ! 
      ! INPUT:
      !     f            = f(x,y,z)
      !     g            = grid (g%c(1,2,3)%dhn, g%c(1,2,3)%dhc)
      !     n            = nth derivative (n=1,2 supported)
      !     dir          = direction along which to take the derivative (1,2,3)
      !     diffType     = (1,2,3,4) derivative type, refer to myDiff for more details
      !     pad          = (1,0) = (exclude,include) boundary calc along derivative direction
      !                    |0000000|     |-------|
      !                    |-------|  ,  |-------| Look at del for implementation details  
      !                    |0000000|     |-------|
      !
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      !
      ! CharlieKawczynski@gmail.com
      ! 5/15/2014

      use grid_mod
      implicit none

      private
      public :: del ! call del(dfdh,f,g,n,dir,diffType,pad)
      public :: add,subtract,multiply,divide

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type assign;     end type
      type add;        end type
      type subtract;   end type
      type multiply;   end type
      type divide;     end type

      interface collocated;  module procedure collocatedAssign;    end interface
      interface collocated;  module procedure collocatedAdd;       end interface
      interface collocated;  module procedure collocatedSubtract;  end interface
      interface collocated;  module procedure collocatedMultiply;  end interface
      interface collocated;  module procedure collocatedDivide;    end interface

      interface staggered;   module procedure staggeredAssign;     end interface
      interface staggered;   module procedure staggeredAdd;        end interface
      interface staggered;   module procedure staggeredSubtract;   end interface
      interface staggered;   module procedure staggeredMultiply;   end interface
      interface staggered;   module procedure staggeredDivide;     end interface

      interface add;         module procedure delTypeAdd;              end interface
      interface subtract;    module procedure delTypeSubtract;         end interface
      interface multiply;    module procedure delTypeMultiply;         end interface
      interface divide;      module procedure delTypeDivide;           end interface

      interface del;         module procedure delAdd;              end interface
      interface del;         module procedure delSubtract;         end interface
      interface del;         module procedure delMultiply;         end interface
      interface del;         module procedure delDivide;           end interface

      contains

      function delTypeAdd()      result(r); type(add) :: r;       
      end function
      function delTypeSubtract() result(r); type(subtract) :: r;  
      end function
      function delTypeMultiply() result(r); type(multiply) :: r;  
      end function
      function delTypeDivide()   result(r); type(divide) :: r;    
      end function

      subroutine myDiff(dfdh,f,dhc,dhn,n,diffType,s,genType)
        implicit none
        real(cp),dimension(:),intent(in) :: f
        real(cp),dimension(:),intent(in) :: dhc,dhn
        integer,intent(in) :: n,diffType,s,genType
        real(cp),dimension(:),intent(inout) :: dfdh
        type(add) :: ad
        type(subtract) :: sub
        type(multiply) :: mult
        type(divide) :: div

        select case(genType)
        case (1)
        select case (diffType)
        case (1); call collocated(dfdh,f,dhc,dhn,n,s,ad)    ! Collocated CellCenter derivative
        case (2); call collocated(dfdh,f,dhn,dhc,n,s,ad)    ! Collocated Node derivative
        case (3); call staggered(dfdh,f,dhc,s,0,ad)         ! Cell centered staggered derivative
        case (4); call staggered(dfdh,f,dhn,s,1,ad)         ! Node centered staggered derivative
        end select
        case (2)
        select case (diffType)
        case (1); call collocated(dfdh,f,dhc,dhn,n,s,sub)    ! Collocated CellCenter derivative
        case (2); call collocated(dfdh,f,dhn,dhc,n,s,sub)    ! Collocated Node derivative
        case (3); call staggered(dfdh,f,dhc,s,0,sub)         ! Cell centered staggered derivative
        case (4); call staggered(dfdh,f,dhn,s,1,sub)         ! Node centered staggered derivative
        end select
        case (3)
        select case (diffType)
        case (1); call collocated(dfdh,f,dhc,dhn,n,s,mult)    ! Collocated CellCenter derivative
        case (2); call collocated(dfdh,f,dhn,dhc,n,s,mult)    ! Collocated Node derivative
        case (3); call staggered(dfdh,f,dhc,s,0,mult)         ! Cell centered staggered derivative
        case (4); call staggered(dfdh,f,dhn,s,1,mult)         ! Node centered staggered derivative
        end select
        case (4)
        select case (diffType)
        case (1); call collocated(dfdh,f,dhc,dhn,n,s,div)    ! Collocated CellCenter derivative
        case (2); call collocated(dfdh,f,dhn,dhc,n,s,div)    ! Collocated Node derivative
        case (3); call staggered(dfdh,f,dhc,s,0,div)         ! Cell centered staggered derivative
        case (4); call staggered(dfdh,f,dhn,s,1,div)         ! Node centered staggered derivative
        end select
        case default
          stop 'Error: genType must = 1,2,3,4 in diff.'
        end select
      end subroutine

      ! ********************** STAGGERED DERIVATIVES *************************

      function staggeredFunc(f,dh,s,gt) result(dfdh)
        implicit none                                   ! f    lives on CC
        real(cp),dimension(:),intent(in) :: f,dh        ! dfdh lives on N
        integer,intent(in) :: s,gt                      ! The entire array dfdh is defined here
        real(cp),dimension(s-1+2*gt) :: dfdh            ! gridType (0,1) = (f on CC, f on N)
        integer :: i                                    !               (dfdh on N , dfdh on CC)
        dfdh(1+gt:s-1+gt) = (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end function

      subroutine staggeredAssign(dfdh,f,dh,s,gt,opType); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        type(assign),intent(in) :: opType
        dfdh(1+gt:s-1+gt) = staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredAdd(dfdh,f,dh,s,gt,opType); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        type(add),intent(in) :: opType
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) + staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredSubtract(dfdh,f,dh,s,gt,opType); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        type(subtract),intent(in) :: opType
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) - staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredMultiply(dfdh,f,dh,s,gt,opType); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        type(multiply),intent(in) :: opType
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) * staggeredFunc(f,dh,s,gt)
      end subroutine

      subroutine staggeredDivide(dfdh,f,dh,s,gt,opType); implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s,gt
        type(divide),intent(in) :: opType
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) / staggeredFunc(f,dh,s,gt)
      end subroutine

      ! ********************** COLLOCATED DERIVATIVES *************************

      function collocatedFunc(f,dhp,dhd,n,s) result(dfdh)
        implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        integer,intent(in) :: n,s
        real(cp),dimension(s) :: dfdh
        integer :: i,j,k
        real(cp) :: alpha,beta
        select case (n)
        case (1); k = -1; j = 1 ! Interior
          do i=2,s-1
            alpha = -dhp(i-1); beta = dhp(i)
            dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                       f(i+k)*beta/alpha + &
                       f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo
          i = 1; k = 1; j = 2; alpha = dhp(1); beta = dhp(1) + dhp(2) ! Forward difference
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                     f(i+k)*beta/alpha + &
                     f(i+j)*(-alpha/beta))/(beta-alpha)
          i = s; k = -1; j = -2; alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2)) ! Backward difference
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                     f(i+k)*beta/alpha + &
                     f(i+j)*(-alpha/beta))/(beta-alpha)
        case (2) ! Interior
          do i=2,s-1
            dfdh(i) = ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
          enddo
        case default
        write(*,*) 'Error: nth-derivative must = 1,2 in collocatedFunc.';stop
        end select
      end function

      subroutine collocatedAssign(dfdh,f,dhp,dhd,n,s,opType); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(assign),intent(in) :: opType
        dfdh = collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedAdd(dfdh,f,dhp,dhd,n,s,opType); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(add),intent(in) :: opType
        dfdh = dfdh + collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedSubtract(dfdh,f,dhp,dhd,n,s,opType); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(subtract),intent(in) :: opType
        dfdh = dfdh - collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedMultiply(dfdh,f,dhp,dhd,n,s,opType); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(multiply),intent(in) :: opType
        dfdh = dfdh * collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine collocatedDivide(dfdh,f,dhp,dhd,n,s,opType); implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(divide),intent(in) :: opType
        dfdh = dfdh / collocatedFunc(f,dhp,dhd,n,s)
      end subroutine

      subroutine delGen(dfdh,f,g,nth,dir,diffType,pad,genType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad,genType
        integer,dimension(3) :: s
        integer :: i,j,k
        s = shape(f)
        select case (dir)
        case (1) !$OMP PARALLEL DO
                 do k=1+pad,s(3)-pad; do j=1+pad,s(2)-pad
                     call myDiff(dfdh(:,j,k),f(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(1),genType)
                 enddo; enddo !$OMP END PARALLEL DO
        case (2) !$OMP PARALLEL DO
                 do k=1+pad,s(3)-pad; do i=1+pad,s(1)-pad
                     call myDiff(dfdh(i,:,k),f(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(2),genType)
                 enddo; enddo !$OMP END PARALLEL DO
        case (3) !$OMP PARALLEL DO
                 do j=1+pad,s(2)-pad; do i=1+pad,s(1)-pad
                     call myDiff(dfdh(i,j,:),f(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(3),genType)
                 enddo; enddo !$OMP END PARALLEL DO
        end select
      end subroutine

      ! ******************* OPERATOR TYPES ****************************

      subroutine delDefault(dfdh,f,g,nth,dir,diffType,pad)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        call delGen(dfdh,f,g,nth,dir,diffType,pad,1)
      end subroutine

      subroutine delAssign(dfdh,f,g,nth,dir,diffType,pad,opType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        type(assign),intent(in) :: opType
        call delGen(dfdh,f,g,nth,dir,diffType,pad,1)
      end subroutine

      subroutine delAdd(dfdh,f,g,nth,dir,diffType,pad,opType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        type(add),intent(in) :: opType
        call delGen(dfdh,f,g,nth,dir,diffType,pad,2)
      end subroutine

      subroutine delSubtract(dfdh,f,g,nth,dir,diffType,pad,opType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        type(subtract),intent(in) :: opType
        call delGen(dfdh,f,g,nth,dir,diffType,pad,3)
      end subroutine

      subroutine delMultiply(dfdh,f,g,nth,dir,diffType,pad,opType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        type(multiply),intent(in) :: opType
        call delGen(dfdh,f,g,nth,dir,diffType,pad,4)
      end subroutine

      subroutine delDivide(dfdh,f,g,nth,dir,diffType,pad,opType)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        type(divide),intent(in) :: opType
        call delGen(dfdh,f,g,nth,dir,diffType,pad,5)
      end subroutine

      end module