      module del_mod
      ! call del(dfdh,f,g,n,dir,diffType,pad) returns an nth-derivative of the
      ! scalar field, f, along direction dir (1,2,3) which corresponds to (x,y,z).
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

      interface Add;         module procedure delAdd;              end interface
      interface Subtract;    module procedure delSubtract;         end interface
      interface Multiply;    module procedure delMultiply;         end interface
      interface Divide;      module procedure delDivide;           end interface

      contains

      function delAdd()      result(r); type(Add) :: r;       
      end function
      function delSubtract() result(r); type(Subtract) :: r;  
      end function
      function delMultiply() result(r); type(Multiply) :: r;  
      end function
      function delDivide()   result(r); type(Divide) :: r;    
      end function


      subroutine myDiff(dfdh,f,dhc,dhn,n,diffType,s,opType)
        implicit none
        real(cp),dimension(:),intent(in) :: f
        real(cp),dimension(:),intent(in) :: dhc,dhn
        integer,intent(in) :: n,diffType,s
        real(cp),dimension(:),intent(inout) :: dfdh

        select case (diffType)
        case (1); call collocated(dfdh,f,dhc,dhn,n,s,opType)    ! Collocated CellCenter derivative
        case (2); call collocated(dfdh,f,dhn,dhc,n,s,opType)    ! Collocated Node derivative
        
        case (3); call staggered(dfdh,f,dhc,s,0,opType)         ! Cell centered staggered derivative
        case (4); call staggered(dfdh,f,dhn,s,1,opType)         ! Node centered staggered derivative
        end select
      end subroutine

      subroutine staggeredAdd(dfdh,f,dh,s,gt,opType)
        ! f    lives on CC
        ! dfdh lives on N
        ! The entire array dfdh is defined here
        implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s
        integer,intent(in) :: gt ! gridType (0,1) = (f on CC, f on N)
        type(add),intent(in) :: opType
        integer :: i
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) + (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end subroutine

      subroutine staggeredSubtract(dfdh,f,dh,s,gt,opType)
        ! f    lives on CC
        ! dfdh lives on N
        ! The entire array dfdh is defined here
        implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s
        integer,intent(in) :: gt ! gridType (0,1) = (f on CC, f on N)
        type(subtract),intent(in) :: opType
        integer :: i
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) - (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end subroutine

      subroutine staggeredMultiply(dfdh,f,dh,s,gt,opType)
        ! f    lives on CC
        ! dfdh lives on N
        ! The entire array dfdh is defined here
        implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s
        integer,intent(in) :: gt ! gridType (0,1) = (f on CC, f on N)
        type(multiply),intent(in) :: opType
        integer :: i
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) * (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end subroutine

      subroutine staggeredDivide(dfdh,f,dh,s,gt,opType)
        ! f    lives on CC
        ! dfdh lives on N
        ! The entire array dfdh is defined here
        implicit none
        real(cp),dimension(:),intent(in) :: f,dh
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: s
        integer,intent(in) :: gt ! gridType (0,1) = (f on CC, f on N)
        type(divide),intent(in) :: opType
        integer :: i
        dfdh(1+gt:s-1+gt) = dfdh(1+gt:s-1+gt) / (/((f(i+1)-f(i))/dh(i),i=1,s-1)/)
      end subroutine

      subroutine collocatedAdd(dfdh,f,dhp,dhd,n,s,opType)
        implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(add),intent(in) :: opType
        integer :: i,j,k
        real(cp) :: alpha,beta
        select case (n)
        case (1); k = -1; j = 1 ! Interior
          do i=2,s-1
            alpha = -dhp(i-1); beta = dhp(i)
            dfdh(i) = dfdh(i) + (f( i )*(alpha/beta-beta/alpha) +&
                                 f(i+k)*beta/alpha + &
                                 f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo
          i = 1; k = 1; j = 2; alpha = dhp(1); beta = dhp(1) + dhp(2) ! Forward difference
          dfdh(i) = dfdh(i) + (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
          i = s; k = -1; j = -2; alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2)) ! Backward difference
          dfdh(i) = dfdh(i) + (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
        case (2) ! Interior
          do i=2,s-1
            dfdh(i) = dfdh(i) + ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
          enddo
        end select
      end subroutine

      subroutine collocatedSubtract(dfdh,f,dhp,dhd,n,s,opType)
        implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(subtract),intent(in) :: opType
        integer :: i,j,k
        real(cp) :: alpha,beta
        select case (n)
        case (1); k = -1; j = 1 ! Interior
          do i=2,s-1
            alpha = -dhp(i-1); beta = dhp(i)
            dfdh(i) = dfdh(i) - (f( i )*(alpha/beta-beta/alpha) +&
                                 f(i+k)*beta/alpha + &
                                 f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo
          i = 1; k = 1; j = 2; alpha = dhp(1); beta = dhp(1) + dhp(2) ! Forward difference
          dfdh(i) = dfdh(i) - (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
          i = s; k = -1; j = -2; alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2)) ! Backward difference
          dfdh(i) = dfdh(i) - (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
        case (2) ! Interior
          do i=2,s-1
            dfdh(i) = dfdh(i) - ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
          enddo
        end select
      end subroutine

      subroutine collocatedMultiply(dfdh,f,dhp,dhd,n,s,opType)
        implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(multiply),intent(in) :: opType
        integer :: i,j,k
        real(cp) :: alpha,beta
        select case (n)
        case (1); k = -1; j = 1 ! Interior
          do i=2,s-1
            alpha = -dhp(i-1); beta = dhp(i)
            dfdh(i) = dfdh(i) * (f( i )*(alpha/beta-beta/alpha) +&
                                 f(i+k)*beta/alpha + &
                                 f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo
          i = 1; k = 1; j = 2; alpha = dhp(1); beta = dhp(1) + dhp(2) ! Forward difference
          dfdh(i) = dfdh(i) * (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
          i = s; k = -1; j = -2; alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2)) ! Backward difference
          dfdh(i) = dfdh(i) * (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
        case (2) ! Interior
          do i=2,s-1
            dfdh(i) = dfdh(i) * ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
          enddo
        end select
      end subroutine

      subroutine collocatedDivide(dfdh,f,dhp,dhd,n,s,opType)
        implicit none
        real(cp),intent(in),dimension(:) :: f,dhp,dhd
        real(cp),dimension(:),intent(inout) :: dfdh
        integer,intent(in) :: n,s
        type(divide),intent(in) :: opType
        integer :: i,j,k
        real(cp) :: alpha,beta
        select case (n)
        case (1); k = -1; j = 1 ! Interior
          do i=2,s-1
            alpha = -dhp(i-1); beta = dhp(i)
            dfdh(i) = dfdh(i) / (f( i )*(alpha/beta-beta/alpha) +&
                                 f(i+k)*beta/alpha + &
                                 f(i+j)*(-alpha/beta))/(beta-alpha)
          enddo
          i = 1; k = 1; j = 2; alpha = dhp(1); beta = dhp(1) + dhp(2) ! Forward difference
          dfdh(i) = dfdh(i) / (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
          i = s; k = -1; j = -2; alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2)) ! Backward difference
          dfdh(i) = dfdh(i) / (f( i )*(alpha/beta-beta/alpha) +&
                               f(i+k)*beta/alpha + &
                               f(i+j)*(-alpha/beta))/(beta-alpha)
        case (2) ! Interior
          do i=2,s-1
            dfdh(i) = dfdh(i) / ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1)
          enddo
        end select
      end subroutine

      subroutine del(dfdh,f,g,nth,dir,diffType,pad,opType)
        ! The order in the loops have been changed for better memory caching.
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: dfdh
        real(cp),dimension(:,:,:),intent(in) :: f
        type(grid),intent(in) :: g
        integer,intent(in) :: nth,dir,diffType,pad
        logical,intent(in),optional :: addTo
        integer,dimension(3) :: s
        integer :: i,j,k

        s = shape(f)

        if (.not.present(addTo)) then
          dfdh = real(0.0,cp) ! Zero result first
        endif
        
        select case (dir)
        case (1)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do j=1+pad,s(2)-pad
              call myDiff(dfdh(:,j,k),f(:,j,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(1))
            enddo
          enddo
          !$OMP END PARALLEL DO
        case (2)
          !$OMP PARALLEL DO
          do k=1+pad,s(3)-pad
            do i=1+pad,s(1)-pad
              call myDiff(dfdh(i,:,k),f(i,:,k),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(2))
            enddo
          enddo
          !$OMP END PARALLEL DO
        case (3)
          !$OMP PARALLEL DO
          do j=1+pad,s(2)-pad
            do i=1+pad,s(1)-pad
              call myDiff(dfdh(i,j,:),f(i,j,:),g%c(dir)%dhc,g%c(dir)%dhn,nth,diffType,s(3))
            enddo
          enddo
          !$OMP END PARALLEL DO
        end select
      end subroutine

      end module