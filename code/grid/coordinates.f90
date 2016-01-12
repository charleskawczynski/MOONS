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

      ! For stitching multi-domains, only
      ! after coordinates has been defined
      public :: stitch_stencils

      ! For multi-grid
      public :: restrict

#ifdef _DEBUG_COORDINATES_
      public :: checkCoordinates
#endif

      type coordinates
        ! Stencils for del (ops_del.f90)
        type(triDiag) :: stagCC2N,stagN2CC
        type(triDiag),dimension(2) :: colCC,colN
        type(triDiag),dimension(2) :: colCC_centered
         ! Properties
        real(cp),dimension(:),allocatable :: alpha,beta ! Interpolation coefficients
        integer :: N                                    ! Number of cells
        real(cp) :: hmin,hmax                           ! Min/Max value of domain
        real(cp) :: dhMin,dhMax,maxRange                ! Smallest spatial step/Maximum range
        integer :: sn,sc                                ! size of hn/hc
        real(cp),dimension(:),allocatable :: hn         ! Cell corner coordinates
        real(cp),dimension(:),allocatable :: hc         ! Cell center coordinates
        real(cp),dimension(:),allocatable :: dhn        ! Difference in cell corner coordinates
        real(cp),dimension(:),allocatable :: dhc        ! Difference in cell center coordinates
        logical :: defined = .false.
        logical :: stencils_defined = .false.
      end type

      interface init;              module procedure initCoordinates;        end interface
      interface init;              module procedure initCopy;               end interface
      interface delete;            module procedure deleteCoordinates;      end interface

      interface print;             module procedure printCoordinates;       end interface
      interface export;            module procedure export_c;               end interface

      interface restrict;          module procedure restrictCoordinates;    end interface

      interface stitch_stencils;   module procedure stitch_stencils_c;      end interface
      interface init_stencils;     module procedure init_stencils_c;        end interface ! Private
      
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
        call delete(c%stagCC2N); call delete(c%stagN2CC)
        call delete(c%colCC(1)); call delete(c%colN(1))
        call delete(c%colCC(2)); call delete(c%colN(2))
        c%defined = .false.
        c%stencils_defined = .false.
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
        call init(c%colCC(1),d%colCC(1))
        call init(c%colCC(2),d%colCC(2))
        call init(c%colN(1),d%colN(1))
        call init(c%colN(2),d%colN(2))
        call init(c%colCC_centered(1),d%colCC_centered(1))
        call init(c%colCC_centered(2),d%colCC_centered(2))

        c%sn = d%sn
        c%sc = d%sc
        c%defined = d%defined
        c%stencils_defined = d%stencils_defined
        call initProps(c)
      end subroutine

      subroutine initCoordinates(c,hn,sn)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: sn
        real(cp),dimension(sn),intent(in) :: hn
        integer :: i
        call delete(c)
        ! Node grid
        if (.not.(size(hn).gt.0)) stop 'Error: hn not allocated in initCoordinates in coordinates.f90'
        if (.not.(size(hn).eq.sn)) stop 'Error: sn.ne.size(hn) in initCoordinates in coordinates.f90'
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
        call init_stencils(c)
        c%defined = .true.
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

      ! subroutine exportCoordinates(c,dir,name,u)
      !   implicit none
      !   type(coordinates), intent(in) :: c
      !   character(len=*),intent(in) :: dir,name
      !   integer,intent(in),optional :: u
      !   integer :: newU
      !   if (present(u)) then
      !     call addToFile(c,u)
      !   else
      !     newU = newAndOpen(dir,name)
      !     call addToFile(c,newU)
      !     close(newU)
      !   endif
      ! end subroutine

      ! subroutine exportCoordinates(c,dir,name,u)
      !   implicit none
      !   type(coordinates), intent(in) :: c
      !   character(len=*),intent(in) :: dir,name
      !   integer,intent(in),optional :: u
      !   integer :: newU
      !   if (present(u)) then
      !     call addToFile(c,u)
      !   else
      !     newU = newAndOpen(dir,name)
      !     call addToFile(c,newU)
      !     close(newU)
      !   endif
      ! end subroutine

      subroutine printCoordinates(c)
        implicit none
        type(coordinates),intent(in) :: c
        call export(c,6)
      end subroutine

      subroutine export_c(c,un)
        implicit none
        type(coordinates), intent(in) :: c
        integer,intent(in) :: un
        write(un,*) ' ---------------- coordinates'
        write(un,*) 'sn = ',c%sn
        write(un,*) 'sc = ',c%sc
        ! write(un,*) 'hn = ',c%hn
        ! write(un,*) 'hc = ',c%hc
        ! write(un,*) 'dhn = ',c%dhn
        ! write(un,*) 'dhc = ',c%dhc
        ! write(un,*) 'alpha = ',c%alpha
        ! write(un,*) 'beta = ',c%beta
        ! write(*,*) 'stagCC2N: '; call print(c%stagCC2N); write(*,*) 'stagN2CC:';call print(c%stagN2CC)
        ! write(*,*) 'colCC(1): '; call print(c%colCC(1)); write(*,*) 'colN(1):';call print(c%colN(1))
        ! write(*,*) 'colCC(2): '; call print(c%colCC(2)); write(*,*) 'colN(2):';call print(c%colN(2))
        ! write(*,*) 'D_CC2N: '; call print(c%D_CC2N); write(*,*) 'U_CC2N:';call print(c%U_CC2N)
        ! write(*,*) 'D_N2CC: '; call print(c%D_N2CC); write(*,*) 'U_N2CC:';call print(c%U_N2CC)
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

      subroutine stencil_colN_1(c)
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
        call init(c%colN(1),L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      subroutine stencil_colN_2(c)
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
        call init(c%colN(2),L,D,U)
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

      subroutine stencil_colCC_1(c)
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
        call init(c%colCC(1),L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      subroutine stencil_colCC_1_centered(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sc
        allocate(dh(s-1)); dh = c%dhc
        allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
        L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
        ! Interior
        L(1:s-2) = (/( (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=2,s-1 )/)
        D(1:s-2) = (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))   ,i=2,s-1 )/)
        U(1:s-2) = (/( (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))    ,i=2,s-1 )/)
        call init(c%colCC_centered(1),L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      subroutine stencil_colCC_2(c)
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
        call init(c%colCC(2),L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      subroutine stencil_colCC_2_centered(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sc
        allocate(dh(s-1)); dh = c%dhc
        allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
        L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
        ! Interior
        L(1:s-2) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=2,s-1 )/)
        D(1:s-2) = -(/( 2.0_cp/(dh(i-1)*dh(i))           ,i=2,s-1 )/)
        U(1:s-2) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=2,s-1 )/)
        call init(c%colCC_centered(2),L,D,U)
        deallocate(L,D,U,dh)
      end subroutine

      ! *****************************************************************
      ! ************************ STITCH STENCILS ************************
      ! *****************************************************************

      subroutine stitch_stencils_c(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        if (.not.c%defined) then
          stop 'Error: coordinates not defined in stitch_stencils_c in coordinates.f90'
        endif
        if (.not.c%stencils_defined) then
          stop 'Error: coordinate stencils not defined in stitch_stencils_c in coordinates.f90'
        endif
        call stitch_colCC_1(c,hmin,hmax)
        call stitch_colCC_2(c,hmin,hmax)
        call stitch_colN_1(c,hmin,hmax)
        call stitch_colN_2(c,hmin,hmax)
      end subroutine

      subroutine stitch_colCC_2(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sc; allocate(dh(s-1)); dh = c%dhc
        if (hmin) then; i = 2
          c%colCC(2)%L(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%colCC(2)%D(1) = -2.0_cp/(dh(i-1)*dh(i))
          c%colCC(2)%U(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        if (hmax) then; i = s-1
          c%colCC(2)%L(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%colCC(2)%D(s-2) = -2.0_cp/(dh(i-1)*dh(i))
          c%colCC(2)%U(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        deallocate(dh)
      end subroutine

      subroutine stitch_colCC_1(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        s = c%sc; allocate(dh(s-1)); dh = c%dhc
        if (hmin) then; i = 2
          c%colCC(1)%L(1) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colCC(1)%D(1) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colCC(1)%U(1) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
        endif
        if (hmax) then; i = s-1
          c%colCC(1)%L(s-2) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colCC(1)%D(s-2) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colCC(1)%U(s-2) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
        endif
        deallocate(dh)
      end subroutine

      subroutine stitch_colN_2(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        s = c%sn; allocate(dh(s-1)); dh = c%dhn
        if (hmin) then; i = 2
          c%colN(2)%L(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%colN(2)%D(1) = -2.0_cp/(dh(i-1)*dh(i))
          c%colN(2)%U(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        if (hmax) then; i = s-1
          c%colN(2)%L(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
          c%colN(2)%D(s-2) = -2.0_cp/(dh(i-1)*dh(i))
          c%colN(2)%U(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
        endif
        deallocate(dh)
      end subroutine

      subroutine stitch_colN_1(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        s = c%sn; allocate(dh(s-1)); dh = c%dhn
        if (hmin) then; i = 2
          c%colN(1)%L(1) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colN(1)%D(1) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colN(1)%U(1) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
        endif
        if (hmax) then; i = s-1
          c%colN(1)%L(s-2) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
          c%colN(1)%D(s-2) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
          c%colN(1)%U(s-2) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
        endif
        deallocate(dh)
      end subroutine


      ! *****************************************************************
      ! ****************** INTERPOLATION COEFFICIENTS *******************
      ! *****************************************************************

      subroutine init_interpStencil(c)
        implicit none
        type(coordinates),intent(inout) :: c
        integer :: t
        if (allocated(c%alpha)) deallocate(c%alpha)
        if (allocated(c%beta)) deallocate(c%beta)
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
          if (mod(c%sc,2).eq.0) then
            call init(r,(/(c%hn(2*i),i=1,c%sc/2)/),c%sc/2)
            call addGhostNodes(r)
            else; stop 'Error: coordinates must be even in restrictCoordinates in coordinates.f90'
          endif
        else; call init(r,c) ! return c
        endif
      end subroutine

      ! *****************************************************************
      ! ************************** AUXILIARY ****************************
      ! *****************************************************************

      subroutine init_stencils_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (c%sc.gt.2) then
          ! Interpolation stencils
          call init_interpStencil(c)

          ! Derivative stencils
          call stencil_colCC_1(c)
          call stencil_colCC_2(c)
          call stencil_colCC_1_centered(c)
          call stencil_colCC_2_centered(c)
          call stencil_colN_1(c)
          call stencil_colN_2(c)
          call stencil_stagCC2N(c)
          call stencil_stagN2CC(c)
          
          ! call check(c%lapCC)
          ! call check(c%lapN)
          ! call check(c%stagCC2N)
          ! call check(c%stagN2CC)
          ! call check(c%colCC)
          ! call check(c%colN)
          ! stop 'Done'
          c%stencils_defined = .true.
        endif
      end subroutine

      subroutine addGhostNodes(c)
        implicit none
        type(coordinates),intent(inout) :: c
        call init(c,(/c%hn(1)-c%dhn(1),c%hn,c%hn(c%sn)+(c%dhn(c%sn-1))/),c%sn+2)
      end subroutine

#ifdef _DEBUG_COORDINATES_

      subroutine checkCoordinates(c)
        implicit none
        type(coordinates),intent(in) :: c
        write(*,*) 'Starting to check coordinates'
        call check(c%colCC(1))
        call check(c%colCC(2))
        call check(c%colCC_centered(1))
        call check(c%colCC_centered(2))
        call check(c%colN(1))
        call check(c%colN(2))
        call check(c%stagCC2N)
        call check(c%stagN2CC)
        write(*,*) 'step 0'; call check_consecutive(c)
        write(*,*) 'step 1'; call check_stencilSymmetry(c,c%stagCC2N,'stagCC2N')
        write(*,*) 'step 2'; call check_stencilSymmetry(c,c%stagN2CC,'stagN2CC')
        write(*,*) 'step 3'; call check_stencilSymmetry(c,c%colCC(1),'colCC(1)')
        write(*,*) 'step 4'; call check_stencilSymmetry(c,c%colCC(2),'colCC(2)')
        write(*,*) 'step 5'; call check_stencilSymmetry(c,c%colCC_centered(1),'colCC_centered(1)')
        write(*,*) 'step 6'; call check_stencilSymmetry(c,c%colCC_centered(2),'colCC_centered(2)')
        write(*,*) 'step 7'; call check_stencilSymmetry(c,c%colN(1),'colN(1)')
        write(*,*) 'step 8'; call check_stencilSymmetry(c,c%colN(2),'colN(2)')
        write(*,*) 'Done checking coordinates'
        ! stop 'Done'
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
        ! do i=1,T%sL
        !   if (allocated(T%L)) then
        !    temp = abs(T%L(i)) - abs(T%L(T%s-i+1))
        !    ! write(*,*) 'temp(L(',i,')) = ',temp
        !    if (abs(temp).gt.tol) then
        !      write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
        !      write(*,*) 'for L on stencil '//name
        !      write(*,*) 'T%L = ',T%L
        !      write(*,*) 'error = ',abs(temp)
        !      stop 'Done'
        !    endif
        !   endif
        ! enddo
        ! Check LU
        ! if (allocated(T%L).and.(allocated(T%U))) then
        !   if (T%sL.ne.T%sU) stop 'Error: T%sL must = T%sU in check_stencilSymmetry in coordinates.f90'
        !   do i=1,T%sL
        !      temp = abs(T%L(i)) - abs(T%U(T%s-i+1))
        !      if (abs(temp).gt.tol) then
        !        write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
        !        write(*,*) 'for LU on stencil '//name
        !        write(*,*) 'T%L = ',T%L
        !        write(*,*) 'T%U = ',T%U
        !        write(*,*) 'error = ',abs(temp)
        !        stop 'Done'
        !      endif
        !   enddo
        ! endif
        ! Check D
        do i=1,T%sD
          if (allocated(T%D)) then
          temp = abs(T%D(i)) - abs(T%D(T%s-i+1))
          ! write(*,*) 'temp(D(',i,')) = ',temp
          if (abs(temp).gt.tol) then
            write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
            write(*,*) 'for D on stencil '//name
            write(*,*) 'T%D = ',T%D
            write(*,*) 'error = ',abs(temp)
            stop 'Done'
          endif
          endif
        enddo
        ! Check U
        ! do i=1,T%sU
        !   if (allocated(T%U)) then
        !   temp = abs(T%U(i)) - abs(T%U(T%s-i+1))
        !   ! write(*,*) 'temp(U(',i,')) = ',temp
        !   if (abs(temp).gt.tol) then
        !     write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
        !     write(*,*) 'for U on stencil '//name
        !     write(*,*) 'T%U = ',T%U
        !      write(*,*) 'error = ',abs(temp)
        !     stop 'Done'
        !   endif
        !   endif
        ! enddo
      end subroutine

#endif
      end module