      module coordinates_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
      use triDiag_mod
      use IO_tools_mod
      implicit none

#ifdef _SINGLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
      integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
      integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: coordinates
      public :: init,delete
      public :: print,export
      public :: addToFile
      public :: init_stencils
      public :: stitch_stencils ! For stitching multi-domains

      ! For multi-grid
      public :: restrict

#ifdef _DEBUG_COORDINATES_
      public :: checkCoordinates
#endif

      type coordinates
        integer :: N                                    ! Number of cells
        real(cp) :: hmin,hmax                           ! Min/Max value of domain
        real(cp) :: dhMin,dhMax,maxRange                ! Smallest spatial step/Maximum range
        integer :: sn,sc                                ! size of hn/hc
        real(cp),dimension(:),allocatable :: hn         ! Cell corner coordinates
        real(cp),dimension(:),allocatable :: hc         ! Cell center coordinates
        real(cp),dimension(:),allocatable :: dhn        ! Difference in cell corner coordinates
        real(cp),dimension(:),allocatable :: dhc        ! Difference in cell center coordinates
        type(triDiag) :: stagCC2N,stagN2CC
        type(triDiag) :: lapCC,lapN
        type(triDiag) :: colCC,colN
        ! Stencils for splitting methods (jacobi/SOR)
        type(triDiag) :: D_CC2N,U_CC2N
        type(triDiag) :: D_N2CC,U_N2CC
        ! type(triDiag) :: F_N,B_N     ! Forward and Backward Node based stencils for applying Neumann BCs
        ! type(triDiag) :: F_CC,B_CC   ! Forward and Backward  CC  based stencils for applying Neumann BCs
        real(cp),dimension(:),allocatable :: alpha,beta ! Interpolation coefficients
      end type

      interface init;              module procedure initCoordinates;        end interface
      interface init;              module procedure initCopy;               end interface
      interface delete;            module procedure deleteCoordinates;      end interface

      interface print;             module procedure printCoordinates;       end interface
      interface export;            module procedure exportCoordinates;      end interface
      interface addToFile;         module procedure addToFileCoordinates;   end interface

      interface restrict;          module procedure restrictCoordinates;    end interface
      interface init_stencils;     module procedure init_stencils_c;        end interface

      interface stitch_stencils;    module procedure stitch_stencils_c;     end interface
      
      contains

      ! *****************************************************************
      ! ************************ INIT / DELETE **************************
      ! *****************************************************************

      subroutine deleteCoordinates(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (allocated(c%hn)) deallocate(c%hn)
        if (allocated(c%hc)) deallocate(c%hc)
        if (allocated(c%dhn)) deallocate(c%dhn)
        if (allocated(c%dhc)) deallocate(c%dhc)
        if (allocated(c%alpha)) deallocate(c%alpha)
        if (allocated(c%beta)) deallocate(c%beta)
      end subroutine

      subroutine initCoordinates(c,h,gridType)
        implicit none
        type(coordinates),intent(inout) :: c
         real(cp),dimension(:),intent(in) :: h
        integer,intent(in) :: gridType
        select case (gridType)
        case (1); call initCellCenter(c,h)
        case (2); call initNodes(c,h)
        end select
      end subroutine

      subroutine initCopy(c,d)
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates),intent(in) :: d
        call delete(c)

        allocate(c%alpha(size(d%alpha))); c%alpha = d%alpha
        allocate(c%beta(size(d%beta))); c%beta = d%beta
        allocate(c%hn(size(d%hn))); c%hn = d%hn
        allocate(c%hc(size(d%hc))); c%hc = d%hc
        allocate(c%dhn(size(d%dhn))); c%dhn = d%dhn
        allocate(c%dhc(size(d%dhc))); c%dhc = d%dhc

        call init(c%stagCC2N,d%stagCC2N)
        call init(c%stagN2CC,d%stagN2CC)
        call init(c%lapCC,d%lapCC)
        call init(c%lapN,d%lapN)
        call init(c%colCC,d%colCC)
        call init(c%colN,d%colN)
        call init(c%D_CC2N,d%D_CC2N)
        call init(c%U_CC2N,d%U_CC2N)
        call init(c%D_N2CC,d%D_N2CC)
        call init(c%U_N2CC,d%U_N2CC)

        c%sn = d%sn
        c%sc = d%sc
        call initProps(c)
      end subroutine

      subroutine initNodes(c,hn)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),intent(in) :: hn
        integer :: i
        call delete(c)
        ! Node grid
        c%sn = size(hn)
        allocate(c%hn(c%sn))
        allocate(c%dhn(c%sn-1))
        c%hn = hn
        c%dhn = (/(hn(i+1)-hn(i),i=1,c%sn-1)/)

        ! Cell center grid
        c%sc = c%sn-1
        allocate(c%hc(c%sc))
        allocate(c%dhc(c%sc-1))

        c%hc = (/ ((hn(i+1)+hn(i))/2.0_cp,i=1,c%sn-1) /)
        c%dhc = (/(c%hc(i+1)-c%hc(i),i=1,c%sc-1)/)

        ! Additional information
        call initProps(c)
      end subroutine

      subroutine initCellCenter(c,hc)
        ! hc must includes the ghost cell.
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),intent(in) :: hc
        integer :: i
        call delete(c)
        ! Node grid
        stop 'Coordinates are being init. via cell centers!'

        ! Cell center grid
        c%sc = size(hc)
        allocate(c%hc(c%sc))
        allocate(c%dhc(c%sc-1))
        c%hc = hc
        c%dhc = (/(c%hc(i+1)-c%hc(i),i=1,c%sc-1)/)

        c%sn = c%sc-1
        allocate(c%hn(c%sn))
        allocate(c%dhn(c%sn-1))
        c%hn(1) = c%hc(1) + c%dhc(1)/2.0_cp

        do i = 1,c%sn-1
         c%hn(i+1) = 2.0_cp*c%hc(i+1) - c%hn(i)
        enddo

        ! Differences
        c%dhn = (/(c%hn(i+1)-c%hn(i),i=1,c%sn-1)/)

        ! Additional information
        call initProps(c)
      end subroutine

      subroutine initProps(c)
        implicit none
        type(coordinates),intent(inout) :: c
         ! Additional information
         c%dhMin = minval(c%dhn)
         c%dhMax = maxval(c%dhn)
         c%hmin = c%hn(2)
         c%hmax = c%hn(c%sn-1) ! To account for ghost node
         c%maxRange = c%hmax-c%hmin
         c%N = size(c%hc)-2
      end subroutine

      ! *****************************************************************
      ! ***************************** IO ********************************
      ! *****************************************************************

      subroutine exportCoordinates(c,dir,name,u)
        implicit none
        type(coordinates), intent(in) :: c
        character(len=*),intent(in) :: dir,name
        integer,intent(in),optional :: u
        integer :: newU
        if (present(u)) then
          call addToFile(c,u)
        else
          newU = newAndOpen(dir,name)
          call addToFile(c,newU)
          close(newU)
        endif
      end subroutine

      subroutine printCoordinates(c)
        implicit none
        type(coordinates),intent(in) :: c
        call addToFile(c,6)
      end subroutine

      subroutine addToFileCoordinates(c,u)
        implicit none
        type(coordinates), intent(in) :: c
        integer,intent(in) :: u
        write(u,*) '------- coordinates -------'
        write(u,*) 'sn = ',c%sn
        write(u,*) 'sc = ',c%sc
        write(u,*) 'hn = ',c%hn
        write(u,*) 'hc = ',c%hc
        write(u,*) 'dhn = ',c%dhn
        write(u,*) 'dhc = ',c%dhc
      end subroutine

      ! *****************************************************************
      ! ********************** DERIVATIVE STENCILS **********************
      ! *****************************************************************

      ! ********************** NODE DATA STENCILS ***********************

      subroutine stencil_stagCC2N(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: D,U,dh
        integer :: i,s
        s = c%sc
        allocate(dh(s-1)); dh = c%dhc
        allocate(D(s-1)); allocate(U(s-1))
        D = 0.0_cp; U = 0.0_cp
        D = -(/(1.0_cp/dh(i),i=1,s-1)/)
        U =  (/(1.0_cp/dh(i),i=1,s-1)/)
        call initD(c%stagCC2N,D)
        call initU(c%stagCC2N,U)
        call initL(c%stagCC2N,U*0.0_cp)
        deallocate(D,U,dh)
      end subroutine

      subroutine stencil_colN(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U,dh
        integer :: i,s
        s = c%sn
        allocate(dh(s-1)); dh = c%dhn
        allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
        L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
        i = 2 ! Front
        L(1) = -(dh(i+1)+2.0_cp*dh(i))/(dh(i)*(dh(i+1)+dh(i)))
        D(1) = (dh(i+1)+dh(i))/(dh(i+1)*dh(i))
        U(1) = -dh(i)/(dh(i+1)*(dh(i+1)+dh(i)))
        ! Interior
        L(2:s-3) = -(/( (dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=3,s-2 )/)
        D(2:s-3) =  (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))  ,i=3,s-2 )/)
        U(2:s-3) =  (/( (dh(i-1)/(dh( i )*(dh(i-1)+dh(i)))) ,i=3,s-2 )/)
        i = s-1 ! Back
        L(s-2) = dh(i-1)/(dh(i-2)*(dh(i-1)+dh(i-2)))
        D(s-2) = -(dh(i-1)+dh(i-2))/(dh(i-1)*dh(i-2))
        U(s-2) = (2.0_cp*dh(i-1)+dh(i-2))/(dh(i-1)*(dh(i-1)+dh(i-2)))
        call init(c%colN,L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      subroutine stencil_lapN(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U,dh
        integer :: i,s
        s = c%sn
        allocate(dh(s-1)); dh = c%dhn
        allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
        L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
        ! Front
        i = 2
        L(1) =  2.0_cp/(dh( i )*(dh(i+1)+dh(i)))
        D(1) = -2.0_cp/(dh(i+1)*dh(i))
        U(1) =  2.0_cp/(dh(i+1)*(dh(i+1)+dh(i)))
        ! Interior
        L(2:s-3) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
        D(2:s-3) =  (/(-2.0_cp/(dh(i-1)*dh(i))           ,i=3,s-2 )/)
        U(2:s-3) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
        ! Back
        i = s-1
        L(s-2) =  2.0_cp/(dh(i-2)*(dh(i-1)+dh(i-2)))
        D(s-2) = -2.0_cp/(dh(i-1)*dh(i-2))
        U(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i-2)))
        call init(c%lapN,L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      ! *********************** CC DATA STENCILS ************************

      subroutine stencil_stagN2CC(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: D,U,dh
        integer :: i,s
        s = c%sn
        allocate(dh(s-1)); dh = c%dhn
        allocate(D(s-1)); allocate(U(s-1))
        D = 0.0_cp; U = 0.0_cp
        D = -(/(1.0_cp/dh(i),i=1,s-1)/)
        U =  (/(1.0_cp/dh(i),i=1,s-1)/)
        call initD(c%stagN2CC,D)
        call initU(c%stagN2CC,U)
        call initL(c%stagN2CC,U*0.0_cp)
        deallocate(D,U,dh)
      end subroutine

      subroutine stencil_colCC(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sc
        allocate(dh(s-1)); dh = c%dhc
        allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
        L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
        ! Front, dh(i-1) => 0.5 dh(i-1)
        i = 2
        L(1) = (-(dh(i))/((0.5_cp*dh(i-1))*((0.5_cp*dh(i-1))+dh(i))))
        D(1) = ((-(0.5_cp*dh(i-1))+dh(i))/((0.5_cp*dh(i-1))*dh(i)))
        U(1) = ((0.5_cp*dh(i-1))/(dh(i)*((0.5_cp*dh(i-1))+dh(i))))
        ! Interior
        L(2:s-3) = (/( (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=3,s-2 )/)
        D(2:s-3) = (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))   ,i=3,s-2 )/)
        U(2:s-3) = (/( (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))    ,i=3,s-2 )/)
        ! Back, dh(i) => 0.5 dh(i)
        i = s-1
        L(s-2) = (-0.5_cp*dh(i)/(dh(i-1)*(dh(i-1)+(0.5_cp*dh(i)))))
        D(s-2) = ((-dh(i-1)+(0.5_cp*dh(i)))/(dh(i-1)*(0.5_cp*dh(i))))
        U(s-2) = (dh(i-1)/((0.5_cp*dh(i))*(dh(i-1)+(0.5_cp*dh(i)))))
        call init(c%colCC,L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      subroutine stencil_lapCC(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sc
        allocate(dh(s-1)); dh = c%dhc
        allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
        L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
        ! Front, dh(i-1) => 0.5 dh(i-1)
        i = 2
        L(1) =  2.0_cp/((0.5_cp*dh(i-1))*((0.5_cp*dh(i-1))+dh(i)))
        D(1) = -2.0_cp/((0.5_cp*dh(i-1))*dh(i))
        U(1) =  2.0_cp/(dh(i)*((0.5_cp*dh(i-1))+dh(i)))
        ! Interior
        L(2:s-3) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
        D(2:s-3) = -(/( 2.0_cp/(dh(i-1)*dh(i))           ,i=3,s-2 )/)
        U(2:s-3) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=3,s-2 )/)
        ! Back, dh(i) => 0.5 dh(i)
        i = s-1
        L(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+(0.5_cp*dh(i))))
        D(s-2) = -2.0_cp/(dh(i-1)*(0.5_cp*dh(i)))
        U(s-2) =  2.0_cp/((0.5_cp*dh(i))*(dh(i-1)+(0.5_cp*dh(i))))
        call init(c%lapCC,L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      ! *****************************************************************
      ! ************************ STITCH STENCILS ************************
      ! *****************************************************************

      subroutine stitch_stencils_c(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        call stitch_lapCC(c,hmin,hmax)
        call stitch_colCC(c,hmin,hmax)
        call stitch_lapN(c,hmin,hmax)
        call stitch_colN(c,hmin,hmax)
      end subroutine

      subroutine stitch_lapCC(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sc; allocate(dh(s-1)); dh = c%dhc
        if (hmin) then; i = 2
          c%lapCC%L(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%lapCC%D(1) = -2.0_cp/(dh(i-1)*dh(i))
          c%lapCC%U(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        if (hmax) then; i = s-1
          c%lapCC%L(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%lapCC%D(s-2) = -2.0_cp/(dh(i-1)*dh(i))
          c%lapCC%U(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        deallocate(dh)
      end subroutine

      subroutine stitch_colCC(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        s = c%sc; allocate(dh(s-1)); dh = c%dhc
        if (hmin) then; i = 2
          c%colCC%L(1) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colCC%D(1) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colCC%U(1) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
        endif
        if (hmax) then; i = s-1
          c%colCC%L(s-2) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colCC%D(s-2) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colCC%U(s-2) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
        endif
        deallocate(dh)
      end subroutine

      subroutine stitch_lapN(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        s = c%sn; allocate(dh(s-1)); dh = c%dhn
        if (hmin) then; i = 2
          c%lapN%L(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%lapN%D(1) = -2.0_cp/(dh(i-1)*dh(i))
          c%lapN%U(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        if (hmax) then; i = s-1
          c%lapN%L(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%lapN%D(s-2) = -2.0_cp/(dh(i-1)*dh(i))
          c%lapN%U(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        deallocate(dh)
      end subroutine

      subroutine stitch_colN(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sn; allocate(dh(s-1)); dh = c%dhn
        if (hmin) then; i = 2
          c%colN%L(1) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colN%D(1) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colN%U(1) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
        endif
        if (hmax) then; i = s-1
          c%colN%L(s-2) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colN%D(s-2) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colN%U(s-2) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
        endif
        deallocate(dh)
      end subroutine


      ! *****************************************************************
      ! ************************ SPLIT STENCILS *************************
      ! *****************************************************************

      subroutine init_D_CC2N(c)
        implicit none
        type(coordinates),intent(inout) :: c
        call init(c%D_CC2N,c%stagCC2N); c%D_CC2N%L = 0.0_cp; c%D_CC2N%U = 0.0_cp
      end subroutine

      subroutine init_D_N2CC(c)
        implicit none
        type(coordinates),intent(inout) :: c
        call init(c%D_N2CC,c%stagN2CC); c%D_N2CC%L = 0.0_cp; c%D_N2CC%U = 0.0_cp
      end subroutine

      subroutine init_U_CC2N(c)
        implicit none
        type(coordinates),intent(inout) :: c
        call init(c%U_CC2N,c%stagCC2N); c%U_CC2N%L = 0.0_cp; c%U_CC2N%D = 0.0_cp
      end subroutine

      subroutine init_U_N2CC(c)
        implicit none
        type(coordinates),intent(inout) :: c
        call init(c%U_N2CC,c%stagN2CC); c%U_N2CC%L = 0.0_cp; c%U_N2CC%D = 0.0_cp
      end subroutine

      ! *****************************************************************
      ! ********** NEUMANN BOUNDARY CONDITION COEFFICIENTS **************
      ! *****************************************************************

      ! subroutine BC_N(c)
      !   implicit none
      !   type(coordinates),intent(inout) :: c
      !   real(cp) :: L,D,U
      !   integer :: i,s
      !   s = c%sn
      !   i = 1 ! Front
      !   L = 1.0_cp/c%colN%L(i)       ! Coefficient of f'_{boundary}
      !   D =-c%colN%D(i)/c%colN%L(i)  ! Coefficient of f_{first interior}
      !   U =-c%colN%U(i)/c%colN%L(i)  ! Coefficient of f_{second interior}
      !   call init(c%F_N,L,D,U)
      !   i = s-2 ! Back
      !   L = 1.0_cp/c%colN%U(i)       ! Coefficient of f'_{boundary}
      !   D =-c%colN%D(i)/c%colN%U(i)  ! Coefficient of f_{first interior}
      !   U =-c%colN%L(i)/c%colN%U(i)  ! Coefficient of f_{second interior}
      !   call init(c%B_N,L,D,U)
      ! end subroutine


      ! *****************************************************************
      ! ****************** INTERPOLATION COEFFICIENTS *******************
      ! *****************************************************************

      subroutine init_interpStencil(c)
        implicit none
        type(coordinates),intent(inout) :: c
        integer :: t
        allocate(c%alpha(c%sc-1))
        allocate(c%beta(c%sc-1))
        do t=1,size(c%alpha)
          c%alpha(t) = (c%hn(t+1) - c%hc(t))/(c%hc(t+1) - c%hc(t))
          c%beta(t) = 1.0_cp - c%alpha(t)
        enddo
      end subroutine

      ! *****************************************************************
      ! ***************** RESTRICTION (FOR MULTIGRID) *******************
      ! *****************************************************************

      subroutine restrictCoordinates(r,c)
        ! Restriction for bad cases should only be allowed ~1 time
        ! Multiple restrictions of this type can make the cells
        ! blow up in size and overlap.
        ! Consider making a flag to only allow 1 time or
        ! use warnings to the user about the grid
        implicit none
        type(coordinates),intent(inout) :: r
        type(coordinates),intent(in) :: c
        integer :: i
        if (c%sc.gt.3) then
          ! if (mod(c%sc,2).eq.0) call init(r,(/(c%hn(2*i),i=1,c%sc/2)/),2) ! good case
          if (mod(c%sc,2).eq.0) then
            call init(r,(/(c%hn(2*i),i=1,c%sc/2)/),2)
            call addGhostNodes(r)
          endif

          if (mod(c%sc,2).ne.0) call init(r,(/c%hc(1),(/(c%hc(2*i),i=1,(c%sc-1)/2)/),c%hc(c%sc)/),1) ! bad case
        else; call init(r,c) ! return c
        endif
      end subroutine

      ! *****************************************************************
      ! ************************** AUXILIARY ****************************
      ! *****************************************************************

      subroutine init_stencils_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        ! Interpolation stencils
        call init_interpStencil(c)

        ! Derivative stencils
        call stencil_lapCC(c)
        call stencil_lapN(c)
        call stencil_stagCC2N(c)
        call stencil_stagN2CC(c)
        call stencil_colCC(c)
        call stencil_colN(c)

        ! Splitting method stencils:
        call init_D_CC2N(c)
        call init_D_N2CC(c)
        call init_U_CC2N(c)
        call init_U_N2CC(c)
        ! call check(c%lapCC)
        ! call check(c%lapN)
        ! call check(c%stagCC2N)
        ! call check(c%stagN2CC)
        ! call check(c%colCC)
        ! call check(c%colN)
        ! stop 'Done'
      end subroutine

      subroutine addGhostNodes(c)
        implicit none
        type(coordinates),intent(inout) :: c
        call init(c,(/c%hn(1)-c%dhn(1),c%hn,c%hn(c%sn)+(c%dhn(c%sn-1))/),2)
      end subroutine

#ifdef _DEBUG_COORDINATES_

      subroutine checkCoordinates(c)
        implicit none
        type(coordinates),intent(in) :: c
        write(*,*) 'Starting to check coordinates'
        write(*,*) 'step 0'; call check_consecutive(c)
        write(*,*) 'step 1'; call check_stencilSymmetry(c,c%stagCC2N,'stagCC2N')
        write(*,*) 'step 2'; call check_stencilSymmetry(c,c%stagN2CC,'stagN2CC')
        write(*,*) 'step 3'; call check_stencilSymmetry(c,c%lapCC,'lapCC')
        write(*,*) 'step 4'; call check_stencilSymmetry(c,c%lapN,'lapN')
        write(*,*) 'step 5'; call check_stencilSymmetry(c,c%colCC,'colCC')
        write(*,*) 'step 6'; call check_stencilSymmetry(c,c%colN,'colN')
        write(*,*) 'done'
      end subroutine

      subroutine check_consecutive(c)
        implicit none
        type(coordinates),intent(in) :: c
        integer :: i
        real(cp) :: tol
        ! Check if consectutive
        do i=1,c%sn-1
          if (c%hn(i+1)-c%hn(i).lt.c%dhMin/2.0_cp) then
             write(*,*) 'i,dh',i,c%hn(i+1)-c%hn(i)
             write(*,*) 'hn = ',c%hn
             stop 'Error: coordinates are not consecutive.'
          endif
        enddo
        ! Check if cell centeres are in cell center
        tol = c%dhMin*1.0_cp**(-10.0_cp)
        do i=1,c%sn-1
          if (abs((c%hc(i)-c%hn(i))-(c%hn(i+1)-c%hc(i))).gt.tol) then
             write(*,*) 'Cell centers are not centered'
             write(*,*) 'i = ',i
             write(*,*) 'hn = ',c%hn
             write(*,*) 'hc = ',c%hc
             stop 'Error: cell centeres are not in cell centers.'
          endif
        enddo
      end subroutine

      subroutine check_stencilSymmetry(c,T,name)
        implicit none
        type(coordinates),intent(in) :: c
        type(triDiag),intent(in) :: T
        character(len=*),intent(in) :: name
        integer :: i
        real(cp) :: tol,temp
        tol = c%dhMin*1.0_cp**(-15.0_cp)
        ! Check L
        do i=1,T%sL
          if (allocated(T%L)) then
           temp = T%L(i) - T%L(T%s-i+1)
           ! write(*,*) 'temp(L(',i,')) = ',temp
           if (abs(temp).gt.tol) then
             write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
             write(*,*) 'for L on stencil '//name
             write(*,*) 'T%L = ',T%L
             stop 'Done'
           endif
          endif
        enddo
        ! Check D
        do i=1,T%sD
          if (allocated(T%D)) then
          temp = T%D(i) - T%D(T%s-i+1)
          ! write(*,*) 'temp(D(',i,')) = ',temp
          if (abs(temp).gt.tol) then
            write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
            write(*,*) 'for D on stencil '//name
            write(*,*) 'T%D = ',T%D
            stop 'Done'
          endif
          endif
        enddo
        ! Check U
        do i=1,T%sU
          if (allocated(T%U)) then
          temp = T%U(i) - T%U(T%s-i+1)
          ! write(*,*) 'temp(U(',i,')) = ',temp
          if (abs(temp).gt.tol) then
            write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
            write(*,*) 'for U on stencil '//name
            write(*,*) 'T%U = ',T%U
            stop 'Done'
          endif
          endif
        enddo
      end subroutine

#endif
      end module