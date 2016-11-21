      module coordinates_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
      use current_precision_mod
      use array_mod
      use sparse_mod
      use derivative_stencils_mod
      ! use interp_stencils_mod
      use IO_tools_mod
      implicit none

      private
      public :: coordinates
      public :: init,delete,display,print,export,import ! Essentials

      ! For stitching multi-domains, only
      ! after coordinates has been defined
      public :: stitch_stencils

      ! For multi-grid
      public :: restrict
      public :: prolongate

      public :: pop,snip
      public :: mirror_about_hmin
      public :: mirror_about_hmax

      ! For getting surface / edge corner
      public :: get_GI
      public :: get_boundary

#ifdef _DEBUG_COORDINATES_
      public :: checkCoordinates
#endif

      type coordinates
        type(sparse) :: stagCC2N,stagN2CC               ! Derivative coefficients
        type(sparse),dimension(2) :: colCC,colN         ! Derivative coefficients
        type(sparse),dimension(2) :: colCC_centered     ! Derivative coefficients
        type(sparse) :: theta                           ! Interpolation coefficients
        real(cp),dimension(:),allocatable :: hn         ! Cell corner coordinates
        real(cp),dimension(:),allocatable :: hc         ! Cell center coordinates
        real(cp),dimension(:),allocatable :: dhn        ! Difference in cell corner coordinates
        real(cp),dimension(:),allocatable :: dhc        ! Difference in cell center coordinates
        real(cp) :: hmin,hmax                           ! Min/Max value of domain
        real(cp) :: amin,amax                           ! absolute Min/Max value of domain (including ghost)
        real(cp) :: dhMin,dhMax,maxRange                ! Smallest spatial step/Maximum range
        real(cp) :: dhn_e,dhc_e                         ! dhn(end),dhc(end)
        real(cp) :: hn_e,hc_e                           ! hn(end),hc(end)
        integer :: N                                    ! Number of cells
        integer :: sn,sc                                ! size of hn/hc
        logical :: defined = .false.
        logical :: stencils_defined = .false.
        logical,dimension(2) :: stencils_modified = .false.
      end type

      interface init;              module procedure initCoordinates;        end interface
      interface init;              module procedure initCopy;               end interface
      interface delete;            module procedure deleteCoordinates;      end interface
      interface display;           module procedure display_coordinates;    end interface
      interface print;             module procedure printCoordinates;       end interface
      interface export;            module procedure export_c;               end interface
      interface import;            module procedure import_c;               end interface

      interface restrict;          module procedure restrict_c;             end interface
      interface restrict;          module procedure restrict_reset_c;       end interface
      interface prolongate;        module procedure prolongate_c;           end interface
      interface prolongate;        module procedure prolongate_reset_c;     end interface

      interface mirror_about_hmin; module procedure mirror_about_hmin_c;    end interface
      interface mirror_about_hmax; module procedure mirror_about_hmax_c;    end interface

      interface stitch_stencils;   module procedure stitch_stencils_c;      end interface
      interface init_stencils;     module procedure init_stencils_c;        end interface ! Private

      interface pop;               module procedure pop_coordinates;        end interface
      interface snip;              module procedure snip_coordinates;       end interface

      interface get_GI;            module procedure get_GI_c;               end interface
      interface get_boundary;      module procedure get_boundary_c;         end interface

      contains

      ! **********************************************************
      ! ********************* ESSENTIALS *************************
      ! **********************************************************

      subroutine initCoordinates(c,hn,sn)
        ! Here is a picture how coordinates are initialized for different cases:
        !
        !
        ! ----------------------------- 1) 1 or more interior cells  + 2 ghost (typical):
        !
        !         hn(1)     hn(2)     hn(3)     hn(4)
        !          |-----------------------------|
        !          |    .    |    .    |    .    |
        !          |-----------------------------|
        !          |   hc(1) |   hc(2) |   hc(3) |
        !        amin       hmin      hmax      amax
        !
        ! ----------------------------- 2) 0 interior cells + 2 ghost:
        ! * This case does need special initialization because it is a special case of 1).
        !
        !              hn(1)     hn(2)     hn(3)
        !               |-------------------|
        !               |    .    |    .    |
        !               |-------------------|
        !               |   hc(1) |   hc(2) |
        !             amin    hmin,hmax      amax
        !
        ! ----------------------------- 3) 0 interior cells + 1 ghost:
        !
        !                 hn(1)        hn(2)
        !                  |-------------|
        !                  |      .      |
        !                  |-------------|
        !                  |     hc(1)   |
        !              hmin,amin     hmax,amax
        !
        ! ----------------------------- 4) 0 interior cells + 0 ghost (infinitely thin plane):
        !
        !                    hn(1),hc(1)
        !                         |
        !                         .
        !                         |
        !                         |
        !                hmin,amin,hmax,amax
        !
        ! -----------------------------
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: sn
        real(cp),dimension(sn),intent(in) :: hn
        integer :: i
        call delete(c)
        if (.not.(size(hn).gt.0)) stop 'Error: hn not allocated in initCoordinates in coordinates.f90'
        if (.not.(size(hn).eq.sn)) stop 'Error: sn.ne.size(hn) in initCoordinates in coordinates.f90'

        ! Typical init
        c%sn = size(hn)
        if (c%sn.gt.2) then
          ! Node grid
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
        elseif (c%sn.eq.2) then
          ! Node grid
          allocate(c%hn(c%sn))
          allocate(c%dhn(c%sn-1))
          c%hn = hn
          c%dhn = (/(hn(i+1)-hn(i),i=1,c%sn-1)/)
          ! Cell center grid
          c%sc = c%sn-1
          allocate(c%hc(c%sc))
          allocate(c%dhc(1))
          c%hc = (/ ((hn(i+1)+hn(i))/2.0_cp,i=1,c%sn-1) /)
          c%dhc = 0.0_cp
        elseif (c%sn.eq.1) then
          ! Node grid
          allocate(c%hn(c%sn))
          allocate(c%dhn(1))
          c%hn = hn
          c%dhn = 0.0_cp
          ! Cell center grid
          c%sc = 1
          allocate(c%hc(c%sc))
          allocate(c%dhc(1))
          c%hc = c%hn
          c%dhc = 0.0_cp
        endif
        ! Additional information
        call initProps(c)
        call init_stencils(c)
        c%stencils_modified = .false.
        c%defined = .true.
      end subroutine

      subroutine initCopy(c,d)
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates),intent(in) :: d
        integer :: i
        call delete(c)
        if (.not.d%defined) stop 'Error: trying to copy undefined coordinate in coordinates.f90'

        call insist_allocated(d%theta,'initCopy coordinates')
        if (.not.allocated(d%hn))    stop 'Error: d%hn    not allocated in coordinates.f90'
        if (.not.allocated(d%hc))    stop 'Error: d%hc    not allocated in coordinates.f90'
        if (.not.allocated(d%dhn))   stop 'Error: d%dhn   not allocated in coordinates.f90'
        if (.not.allocated(d%dhc))   stop 'Error: d%dhc   not allocated in coordinates.f90'
        allocate(c%hn(size(d%hn)));       c%hn    = d%hn
        allocate(c%hc(size(d%hc)));       c%hc    = d%hc
        allocate(c%dhn(size(d%dhn)));     c%dhn   = d%dhn
        allocate(c%dhc(size(d%dhc)));     c%dhc   = d%dhc

        call init(c%theta,d%theta)
        call init(c%stagCC2N,d%stagCC2N)
        call init(c%stagN2CC,d%stagN2CC)
        do i=1,2; call init(c%colCC(i),d%colCC(i)); enddo
        do i=1,2; call init(c%colCC_centered(i),d%colCC_centered(i)); enddo
        do i=1,2; call init(c%colN(i),d%colN(i)); enddo

        c%hc_e = d%hc_e
        c%hn_e = d%hn_e
        c%dhc_e = d%dhc_e
        c%dhn_e = d%dhn_e
        c%sn = d%sn
        c%sc = d%sc
        c%defined = d%defined
        c%stencils_defined = d%stencils_defined
        c%stencils_modified = d%stencils_modified
        call initProps(c)
      end subroutine

      subroutine deleteCoordinates(c)
        implicit none
        type(coordinates),intent(inout) :: c
        integer :: i
        if (allocated(c%hn)) deallocate(c%hn)
        if (allocated(c%hc)) deallocate(c%hc)
        if (allocated(c%dhn)) deallocate(c%dhn)
        if (allocated(c%dhc)) deallocate(c%dhc)
        call delete(c%theta)
        call delete(c%stagCC2N)
        call delete(c%stagN2CC)
        do i=1,2; call delete(c%colCC_centered(i)); enddo
        do i=1,2; call delete(c%colN(i)); enddo
        do i=1,2; call delete(c%colCC(i)); enddo
        c%dhc_e = 0.0_cp
        c%dhn_e = 0.0_cp
        c%defined = .false.
        c%stencils_defined = .false.
      end subroutine

      subroutine display_coordinates(c,un)
        implicit none
        type(coordinates),intent(in) :: c
        integer,intent(in) :: un
        write(un,*) ' ---------------- coordinates'
        write(un,*) 'sc,sn = ',c%sc,c%sn
        write(un,*) 'hmin,hmax = ',c%hmin,c%hmax
        write(un,*) 'amin,amax = ',c%amin,c%amax
        write(un,*) 'hn = ',c%hn
        ! write(*,*) 'stagCC2N: '; call print(c%stagCC2N); write(*,*) 'stagN2CC:';call print(c%stagN2CC)
        ! write(*,*) 'colCC(1): '; call print(c%colCC(1)); write(*,*) 'colN(1):';call print(c%colN(1))
        ! write(*,*) 'colCC(2): '; call print(c%colCC(2)); write(*,*) 'colN(2):';call print(c%colN(2))
        ! write(*,*) 'D_CC2N: '; call print(c%D_CC2N); write(*,*) 'U_CC2N:';call print(c%U_CC2N)
        ! write(*,*) 'D_N2CC: '; call print(c%D_N2CC); write(*,*) 'U_N2CC:';call print(c%U_N2CC)
      end subroutine

      subroutine printCoordinates(c)
        implicit none
        type(coordinates),intent(in) :: c
        call display(c,6)
      end subroutine

      subroutine export_c(c,un)
        implicit none
        type(coordinates),intent(in) :: c
        integer,intent(in) :: un
        write(un,*) ' ---------------- coordinates'
        write(un,*) 'sn = ';  write(un,*) c%sn
        write(un,*) 'hn = ';  write(un,*) c%hn
      end subroutine

      subroutine import_c(c,un)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: un
        real(cp),dimension(:),allocatable :: hn
        integer :: sn
        call delete(c)
        read(un,*)
        read(un,*); read(un,*) sn; allocate(hn(sn))
        read(un,*); read(un,*) hn; call init(c,hn,sn)
        deallocate(hn)
      end subroutine

      ! **********************************************************
      ! **********************************************************
      ! **********************************************************

      subroutine initProps(c)
        implicit none
        type(coordinates),intent(inout) :: c
         ! Additional information
         c%dhMin = minval(c%dhn)
         c%dhMax = maxval(c%dhn)
         c%amin = c%hn(1)
         c%amax = c%hn(c%sn)
         c%hn_e = c%hn(c%sn)
         c%hc_e = c%hc(c%sc)
         if (c%sn.gt.2) then ! Typical init
           c%hmin = c%hn(2)
           c%hmax = c%hn(c%sn-1) ! To account for ghost node
           c%maxRange = c%hmax-c%hmin
           c%N = size(c%hc)-2
           c%dhc_e = c%dhc(c%sc-1)
           c%dhn_e = c%dhn(c%sn-1)
         elseif (c%sn.eq.2) then
           c%hmin = c%hn(1)
           c%hmax = c%hn(2)
           c%maxRange = c%hmax-c%hmin
           c%N = 0
           c%dhc_e = c%dhc(1)
           c%dhn_e = c%dhn(1)
         elseif (c%sn.eq.1) then
           c%hmin = c%hn(1)
           c%hmax = c%hn(1)
           c%maxRange = 0.0_cp
           c%N = 0
           c%dhc_e = c%dhc(1)
           c%dhn_e = c%dhn(1)
         endif
      end subroutine

      ! *****************************************************************
      ! ********************** DERIVATIVE STENCILS **********************
      ! *****************************************************************

      ! ********************** NODE DATA STENCILS ***********************

      subroutine stencil_stagCC2N(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U,dh
        integer :: i,s
        if (c%sc.gt.1) then
          s = c%sc
          allocate(L(1),D(s-1),U(s-1),dh(s-1)); dh = c%dhc
          L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
          D = -(/(1.0_cp/dh(i),i=1,s-1)/)
          U =  (/(1.0_cp/dh(i),i=1,s-1)/)
          call init(c%stagCC2N,L,D,U,1,s-1,s-1)
          deallocate(L,D,U,dh)
        else; call init(c%stagCC2N,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end subroutine

      subroutine stencil_colN_1(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U,dh
        integer :: i,s
        if (c%sn.gt.3) then
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
          call init(c%colN(1),L,D,U,s-2)
          deallocate(L,D,U,dh)
        else; call init(c%colN(1),(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
        c%stencils_modified = .false.
      end subroutine

      subroutine stencil_colN_2(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U,dh
        integer :: i,s
        if (c%sn.gt.3) then
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
          call init(c%colN(2),L,D,U,s-2)
          deallocate(L,D,U,dh)
        else; call init(c%colN(2),(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
        c%stencils_modified = .false.
      end subroutine

      ! *********************** CC DATA STENCILS ************************

      subroutine stencil_stagN2CC(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U,dh
        integer :: i,s
        if (c%sn.gt.1) then
          s = c%sn
          allocate(L(1),D(s-1),U(s-1),dh(s-1)); dh = c%dhn
          L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
          D = -(/(1.0_cp/dh(i),i=1,s-1)/)
          U =  (/(1.0_cp/dh(i),i=1,s-1)/)
          call init(c%stagN2CC,L,D,U,1,s-1,s-1)
          deallocate(L,D,U,dh)
        else; call init(c%stagN2CC,(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end subroutine

      subroutine stencil_colCC_1(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sc.gt.2) then
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
          call init(c%colCC(1),L,D,U,s-2)
          deallocate(L,D,U,dh)
        else; call init(c%colCC(1),(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
        c%stencils_modified = .false.
      end subroutine

      subroutine stencil_colCC_1_centered(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sc.gt.2) then
          s = c%sc
          allocate(dh(s-1)); dh = c%dhc
          allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
          L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
          ! Interior
          L(1:s-2) = (/( (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))   ,i=2,s-1 )/)
          D(1:s-2) = (/( ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))   ,i=2,s-1 )/)
          U(1:s-2) = (/( (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))    ,i=2,s-1 )/)
          call init(c%colCC_centered(1),L,D,U,s-2)
          deallocate(L,D,U,dh)
        else; call init(c%colCC_centered(1),(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
      end subroutine

      subroutine stencil_colCC_2(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sc.gt.2) then
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
          call init(c%colCC(2),L,D,U,s-2)
          deallocate(L,D,U,dh)
        else; call init(c%colCC(2),(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
        c%stencils_modified = .false.
      end subroutine

      subroutine stencil_colCC_2_centered(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: L,D,U
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sc.gt.2) then
          s = c%sc
          allocate(dh(s-1)); dh = c%dhc
          allocate(L(s-2)); allocate(D(s-2)); allocate(U(s-2))
          L = 0.0_cp; D = 0.0_cp; U = 0.0_cp
          ! Interior
          L(1:s-2) =  (/( 2.0_cp/(dh(i-1)*(dh(i-1)+dh(i))) ,i=2,s-1 )/)
          D(1:s-2) = -(/( 2.0_cp/(dh(i-1)*dh(i))           ,i=2,s-1 )/)
          U(1:s-2) =  (/( 2.0_cp/(dh( i )*(dh(i-1)+dh(i))) ,i=2,s-1 )/)
          call init(c%colCC_centered(2),L,D,U,s-2)
          deallocate(L,D,U,dh)
        else; call init(c%colCC_centered(2),(/0.0_cp/),(/0.0_cp/),(/0.0_cp/),1)
        endif
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
        c%stencils_modified(1) = hmin
        c%stencils_modified(2) = hmax
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
        if (c%sc.gt.2) then
          s = c%sc; allocate(dh(s-1)); dh = c%dhc
          if (hmin) then; i = 2
            c%colCC(2)%L%f(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colCC(2)%D%f(1) = -2.0_cp/(dh(i-1)*dh(i))
            c%colCC(2)%U%f(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          if (hmax) then; i = s-1
            c%colCC(2)%L%f(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colCC(2)%D%f(s-2) = -2.0_cp/(dh(i-1)*dh(i))
            c%colCC(2)%U%f(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine stitch_colCC_1(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        if (c%sc.gt.2) then
          s = c%sc; allocate(dh(s-1)); dh = c%dhc
          if (hmin) then; i = 2
            c%colCC(1)%L%f(1) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colCC(1)%D%f(1) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colCC(1)%U%f(1) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
          endif
          if (hmax) then; i = s-1
            c%colCC(1)%L%f(s-2) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colCC(1)%D%f(s-2) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colCC(1)%U%f(s-2) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine stitch_colN_2(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        if (c%sn.gt.2) then
          s = c%sn; allocate(dh(s-1)); dh = c%dhn
          if (hmin) then; i = 2
            c%colN(2)%L%f(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colN(2)%D%f(1) = -2.0_cp/(dh(i-1)*dh(i))
            c%colN(2)%U%f(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          if (hmax) then; i = s-1
            c%colN(2)%L%f(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colN(2)%D%f(s-2) = -2.0_cp/(dh(i-1)*dh(i))
            c%colN(2)%U%f(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine stitch_colN_1(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sn.gt.2) then
          s = c%sn; allocate(dh(s-1)); dh = c%dhn
          if (hmin) then; i = 2
            c%colN(1)%L%f(1) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colN(1)%D%f(1) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colN(1)%U%f(1) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
          endif
          if (hmax) then; i = s-1
            c%colN(1)%L%f(s-2) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colN(1)%D%f(s-2) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colN(1)%U%f(s-2) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine


      ! *****************************************************************
      ! ****************** INTERPOLATION COEFFICIENTS *******************
      ! *****************************************************************

      subroutine init_interpStencil(c)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(2) :: v
        integer :: t
        call delete(c%theta)
        if (c%sc.gt.1) then
          call init(c%theta,c%sc-1)
          do t=1,c%theta%D%N
            v(1) = (c%hn(t+1) - c%hc(t))/(c%hc(t+1) - c%hc(t))
            call init(c%theta%D,v(1),t)
            v(2) = 1.0_cp - c%theta%D%f(t)
            call init(c%theta%U,v(2),t)
          enddo
          call init_L(c%theta,(/0.0_cp/),1)
        elseif (c%sc.eq.1) then
          call init_D(c%theta,(/0.0_cp/),1)
          call init_U(c%theta,(/1.0_cp/),1)
          call init_L(c%theta,(/0.0_cp/),1)
        else; stop 'Error: c%sc must > 1 in init_interpStencil in coordinates.f90'
        endif
      end subroutine

      ! *****************************************************************
      ! ***************** RESTRICTION (FOR MULTIGRID) *******************
      ! *****************************************************************

      subroutine restrict_c(r,c)
        ! Restriction for bad cases should only be allowed ~1 time
        ! Multiple restrictions of this type can make the cells
        ! blow up in size and overlap.
        ! Consider making a flag to only allow 1 time or
        ! use warnings to the user about the grid
        implicit none
        type(coordinates),intent(inout) :: r
        type(coordinates),intent(in) :: c
        integer :: i
        call init(r,c)
        if (c%sc.gt.3) then
          if (mod(c%sc,2).eq.0) then
            call init(r,(/(c%hn(2*i),i=1,c%sc/2)/),c%sc/2)
            call addGhostNodes(r)
            else; stop 'Error: coordinates must be even in restrictCoordinates in coordinates.f90'
          endif
        else; call init(r,c) ! return c
        endif
      end subroutine

      subroutine restrict_reset_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        call restrict(temp,c)
        call init(c,temp)
        call delete(temp)
      end subroutine

      ! *****************************************************************
      ! ****************** PROLONGATE (FOR MULTIGRID) *******************
      ! *****************************************************************

      subroutine prolongate_c(p,c)
        implicit none
        type(coordinates),intent(inout) :: p
        type(coordinates),intent(in) :: c
        type(array),dimension(3) :: a
        integer :: i
        call init(p,c)
            if (c%sn.eq.1) then ! Can't prolongate without interior!
        elseif (c%sn.eq.2) then
        elseif (c%sn.eq.3) then
        elseif (c%sn.gt.3) then ! typical case
          call init(a(1),c%hn(2:c%sn-1),c%sn-2)
          call init(a(2),c%hc(2:c%sc-1),c%sc-2)
          call init(a(3),a(1)%N+a(2)%N)
          ! call init(a(3),2*(a(1)%N-1)-1)
          do i=1,a(1)%N; a(3)%f(2*i-1) = a(1)%f(i); enddo
          do i=1,a(2)%N; a(3)%f(2*i)   = a(2)%f(i); enddo
          call init(p,a(3)%f,a(3)%N)
          call addGhostNodes(p)
          do i=1,3; call delete(a(i)); enddo
        endif
      end subroutine

      subroutine prolongate_reset_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        call prolongate(temp,c)
        call init(c,temp)
        call delete(temp)
      end subroutine

      ! *****************************************************************
      ! ******************** MIRROR (FOR SYMMETRY) **********************
      ! *****************************************************************

      subroutine mirror_about_hmin_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(array) :: a,m
        if (c%sn.eq.1) then ! Can't mirror without interior!
        elseif (c%sn.eq.2) then
        elseif (c%sn.eq.3) then
        elseif (c%sn.gt.3) then ! typical case
          call init(m,c%hn(2:c%sn),c%sn-1)
          call multiply(m,-1.0_cp)
          call add(m,2.0_cp*c%hn(2))
          call reverse(m)
          call init(a,(/m%f,c%hn(3:c%sn)/),2*c%sn-1)
          call init(c,a%f,a%N)
          call delete(a)
          call delete(m)
        endif
      end subroutine

      subroutine mirror_about_hmax_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(array) :: a,m
        if (c%sn.eq.1) then ! Can't mirror without interior!
        elseif (c%sn.eq.2) then
        elseif (c%sn.eq.3) then
        elseif (c%sn.gt.3) then ! typical case
          call init(m,c%hn(1:c%sn-1),c%sn-1)
          call multiply(m,-1.0_cp)
          call add(m,2.0_cp*c%hn(c%sn-1))
          call reverse(m)
          call init(a,(/c%hn(1:c%sn-2),m%f/),2*c%sn-3)
          call init(c,a%f,a%N)
          call delete(a)
          call delete(m)
        endif
      end subroutine

      ! *****************************************************************
      ! ********************* POP / SNIP ROUTINES ***********************
      ! *****************************************************************

      subroutine snip_coordinates(c) ! Removes the first index from the coordinates
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        if (c%sn.eq.1) stop 'Error: no nodes to snip in snip_coordinates in coordinates.f90'
        call init(temp,c%hn(2:c%sn-1),c%sn-1)
        call init(c,temp%hn,temp%sn)
        call delete(temp)
      end subroutine

      subroutine pop_coordinates(c) ! Removes the last index from the coordinates
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        if (c%sn.eq.1) stop 'Error: no nodes to snip in pop_coordinates in coordinates.f90'
        call init(temp,c%hn(1:c%sn-1),c%sn-1)
        call init(c,temp%hn,temp%sn)
        call delete(temp)
      end subroutine

      ! *****************************************************************
      ! ************* GET GHOST / BOUNDARY / FIRST INTERIOR *************
      ! *****************************************************************

      subroutine get_GI_c(c,dir)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: dir
        integer :: i,s
        if ((dir.ne.1).and.(dir.ne.-1)) stop 'Error: dir must = 1,-1 in get_GI_c in coordinates.f90'
        s = c%sn
        if (s.gt.3) then ! 3 or more nodes, remove all but boundary surface
        if (dir.eq.-1) then; do i=1,s-3; call pop(c);  enddo; endif
        if (dir.eq. 1) then; do i=1,s-3; call snip(c); enddo; endif
        elseif ((s.eq.3).or.(s.eq.2).or.(s.eq.1)) then
        else; stop 'Error: bad case in get_GI_c in coordinates.f90'
        endif
      end subroutine

      subroutine get_boundary_c(c,dir)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: dir
        integer :: i,s
        if ((dir.ne.1).and.(dir.ne.-1)) stop 'Error: dir must = 1,-1 in get_boundary_c in coordinates.f90'
        s = c%sn
        if (s.gt.2) then ! 3 or more nodes, remove all but boundary surface
        if (dir.eq.-1) then; do i=1,s-2; call pop(c);  enddo; call snip(c); endif
        if (dir.eq. 1) then; do i=1,s-2; call snip(c); enddo; call pop(c);  endif
        elseif ((s.eq.2).or.(s.eq.1)) then ! single cell, cannot choose which node to remove
        else; stop 'Error: bad case in get_boundary_c in coordinates.f90'
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
        call stencil_colCC_1(c)
        call stencil_colCC_2(c)
        call stencil_colCC_1_centered(c)
        call stencil_colCC_2_centered(c)
        call stencil_colN_1(c)
        call stencil_colN_2(c)
        call stencil_stagCC2N(c)
        call stencil_stagN2CC(c)

        ! call init(c%theta,interpolation_stencil(c%hc,c%hn,c%sc,c%sn))
        ! call init(c%stagCC2N,staggered_CC2N(c%dhc,c%sc))
        ! call init(c%stagN2CC,staggered_N2CC(c%dhn,c%sn))
        ! call init(c%colCC_centered(1),collocated_CC_1_centered(c%dhc,c%sc))
        ! call init(c%colCC_centered(2),collocated_CC_2_centered(c%dhc,c%sc))
        ! call init(c%colCC(1),collocated_CC_1(c%dhc,c%sc))
        ! call init(c%colCC(2),collocated_CC_2(c%dhc,c%sc))
        ! call init(c%colN(1),collocated_Node_1(c%dhn,c%sn))
        ! call init(c%colN(2),collocated_Node_2(c%dhn,c%sn))

        ! call check(c%stagCC2N)
        ! call check(c%stagN2CC)
        ! call check(c%colCC)
        ! call check(c%colN)
        ! stop 'Done'
        c%stencils_defined = .true.
      end subroutine

      subroutine addGhostNodes(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (c%sn.gt.1) then
        call init(c,(/c%hn(1)-c%dhn(1),c%hn,c%hn(c%sn)+(c%dhn(c%sn-1))/),c%sn+2)
        else
          write(*,*) 'Trying to add ghost point to a single point'
          stop 'program stopped in coordinates.f90'
        endif
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
        if (c%sn.gt.1) then
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
        else
          write(*,*) 'Warning: cannot check consecutive coordinates for a single point'
          stop 'program stopped in coordinates.f90'
        endif
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