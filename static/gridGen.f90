       module gridGenerator_mod
       use myIO_mod
       use coordinates_mod
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

       integer,parameter :: preDefinedGrid = 0
                                           ! 0 User defined
                                           ! 1 Lid Driven Cavity
                                           ! 2 Lid Driven Cavity
                                           ! 3 Duct

       private
       public :: gridGen
       public :: init,delete
       public :: print,export
       private :: checkGrid

       type gridGen
         private
         type(grid) :: g_mom,g_ind
         real(cp),dimension(3) :: hmin,hmax
         real(cp),dimension(3) :: twtop,twbot
         real(cp),dimension(3) :: alphai,betai
         real(cp),dimension(3) :: alphaw,betaw
       end type

       interface init;       module procedure initGridGen;         end interface
       interface delete;     module procedure deleteGridGen;       end interface

       interface print;      module procedure printGridGen;        end interface
       interface export;     module procedure exportGridGen;       end interface
       
       contains

       subroutine initGridGen(g_mom,g_ind,Re,Ha)
         implicit none
         type(grid),intent(inout) :: g_mom,g_ind
         real(cp),intent(in) :: Re,Ha
         real(cp),dimension(3) :: twtop,twbot
         real(cp),dimension(3) :: hmin,hmax
         real(cp),dimension(3) :: alphai,betai
         real(cp),dimension(3) :: alphaw,betaw

         ! **************** USER DEFINED GRIDDATA ********************

         ! Geometry:
         hmin(1) = -one; hmax(1) = one ! for x
         hmin(2) = -one; hmax(2) = one ! for y
         hmin(3) = -one; hmax(3) = one ! for z

         ! Wall thickness:
         twtop = 0.1d0
         twtop(2) = 0.0d0
         twbot = 0.1d0

         ! Grid stretching:
         !       alpha = 0      stretching at y=0 only
         !       alpha = 0.5    stretching at y=0 and y=h
         !       beta ~ (1.0 - 1.0/Ha)**(-0.5)
         !       beta ~ (1.0 - Re**(-0.5))**(-0.5) ! (flat plate)

         call robertsGridBL(betai,one/Re,hmin,hmax)
         call robertsGridBL(betai,one/Ha,hmin,hmax)

         ! Interior
         alphai = oneHalf ! for xyz
         betai = 1.004d0

         ! Wall
         alphaw = zero  ! for xyz
         betaw = betai ! for xyz


         ! **************** BENCHMARK DEFINED GRIDDATA ********************
         ! Geometry for benchmarks
         select case (benchmarkCase)
         case (1); hmin = zero; hmax = one ! for xyz
         case (50); hmin = -oneHalf; hmax = oneHalf ! for xyz
         case (51); hmin = -oneHalf; hmax = oneHalf ! for xyz

         case (100); hmin = zero; hmax = one ! for xyz
         case (101); hmin = zero; hmax = one ! for xyz

         case (102); hmin = -one; hmax = one ! for xyz
         case (103); hmin = -one; hmax = one ! for xyz
         case (104); hmin = -one; hmax = one ! for xyz
         case (105); hmin = -one; hmax = one ! for xyz
         case (106); hmin = -one; hmax = one ! for xyz
         case (107); hmin = -one; hmax = one ! for xyz
         case (108); hmin = -one; hmax = one ! for xyz
         case (109); hmin = -one; hmax = one ! for xyz

         case (200); hmin = -one; hmax = one
         hmin(1) = zero; hmax(1) = 20.0d0
         case (201); hmin = -one; hmax = one
         hmin(1) = zero; hmax(1) = 20.0d0
         case (202); hmin = -one; hmax = one
         hmin(1) = zero; hmax(1) = 40.0d0

         case (300); hmin = -one; hmax = one ! for xyz
         case (301); hmin = -one; hmax = one ! for xyz
         case default
           write(*,*) 'Incorrect benchmarkCase in setGriddata';stop
         end select

         ! Grid stretching for benchmarks
         select case (benchmarkCase)
         case (1); betai = 1000.0d0 ! Should be uniform
         case (50); betai = 1.05d0
         case (51); betai = 1.05d0

         case (100); betai = 1000.0d0
         case (101); betai = 1.01d0 
         case (102); betai = 100.0d0
         case (103); betai = 1.004d0
         case (104); betai = 1.0002d0

         case (105); betai = 1.0d6
         case (106); betai = 1.05d0
         case (107); betai = 1.03d0
         case (108); betai = 1.01d0

         case (109); betai = 1.04d0

         case (200); betai = 100.0d0
         case (201); betai = 1.01d0; betai(1) = 1000.0d0
         case (202); betai = 1.001d0; betai(1) = 1000.0d0

         case (300); betai = 1000.0d0
         case (301); betai = 1000.0d0
         case default
           write(*,*) 'Incorrect benchmarkCase in setGriddata';stop
         end select

         ! tw for benchmarks:
         select case (benchmarkCase)
         case (1); twtop = 0.0d0;      twbot = 0.0d0
         case (50); twtop = 0.0d0;      twbot = 0.0d0
         case (51); twtop = 0.0d0;      twbot = 0.0d0

         case (100); twtop = 0.0d0;      twbot = 0.0d0
         case (101); twtop = 0.0d0;      twbot = 0.0d0
         case (102); twtop = 0.467d0;    twbot = 0.467d0
         case (103); twtop = 0.01539d0;  twbot = 0.01539d0
         case (104); twtop = 0.0005d0;   twbot = 0.0005d0

         case (105); twtop = 0.48888d0;   twbot = 0.48888d0
                     twtop(2) = 0.0d0
         case (106); twtop = 0.48888d0;   twbot = 0.48888d0
                     twtop(2) = 0.0d0
         case (107); twtop = 0.48888d0;   twbot = 0.48888d0
                     twtop(2) = 0.0d0
         case (108); twtop = 0.48888d0;   twbot = 0.48888d0
                     twtop(2) = 0.0d0

         case (109); twtop = 0.4d0;    twbot = 0.4d0
                     twtop(2) = 0.01d0;  twbot(2) = 0.01d0

         case (200); twtop = zero;     twbot = zero
         case (201); twtop = 0.1d0;    twbot = 0.1d0
                     twtop(1) = zero;  twbot(1) = zero
         case (202); twtop = 0.1d0;    twbot = 0.1d0
                     twtop(1) = zero;  twbot(1) = zero

         case (300); twtop = 0.0d0;      twbot = 0.0d0
         case (301); twtop = 0.0d0;      twbot = 0.0d0
         case default
           write(*,*) 'Incorrect benchmarkCase in setGriddata';stop
         end select

         if (benchmarkCase.ne.0) then
           alphai = oneHalf; alphaw = zero; betaw = betai
         endif

         ! --------------------- GENERATE GRID -----------------------
         dir = 1
         call gridGen(hn,hni,hmin(dir),hmax(dir),Ni(dir),&
          alphai(dir),betai(dir),twtop(dir),twbot(dir),&
          Nwtop(dir),Nwbot(dir),alphaw(dir),betaw(dir))
         call init(g_mom,hni,dir)
         call init(g_ind,hn,dir)
         dir = 2
         call gridGen(hn,hni,hmin(dir),hmax(dir),Ni(dir),&
          alphai(dir),betai(dir),twtop(dir),twbot(dir),&
          Nwtop(dir),Nwbot(dir),alphaw(dir),betaw(dir))
         call init(g_mom,hni,dir)
         call init(g_ind,hn,dir)
         dir = 3
         call gridGen(hn,hni,hmin(dir),hmax(dir),Ni(dir),&
          alphai(dir),betai(dir),twtop(dir),twbot(dir),&
          Nwtop(dir),Nwbot(dir),alphaw(dir),betaw(dir))
         call init(g_mom,hni,dir)
         call init(g_ind,hn,dir)

         call reComputeTw(this)
         call setSmallestSpatialStep(this)
         call setLargestRange(this)
       end subroutine

       subroutine setGrid(this,hmin,hmax,alphai,betai,twtop,twbot,alphaw,betaw,dir)
         implicit none
         type(griddata),intent(inout) :: this
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),dimension(3),intent(in) :: alphai,betai
         real(cp),dimension(3),intent(in) :: twtop
         real(cp),dimension(3),intent(in) :: twbot
         real(cp),dimension(3),intent(in) :: alphaw
         real(cp),dimension(3),intent(inout) :: betaw
         integer,intent(in) :: dir ! direction

         real(cp),dimension(:),allocatable :: hn,hc
         real(cp),dimension(:),allocatable :: dhn,dhc

         real(cp),dimension(:),allocatable :: hni,hci
         real(cp),dimension(:),allocatable :: dhni,dhci

         ! --------------------- ALLOCATE GRID -----------------------
         ! Interior
         allocate(hni(Ni(dir)+1))
         allocate(hci(Ni(dir)+2))
         allocate(dhni(Ni(dir)))
         allocate(dhci(Ni(dir)+1))

         ! Total
         allocate(hn(Ni(dir)+Nwtop(dir)+Nwbot(dir)+1))
         allocate(hc(Ni(dir)+Nwtop(dir)+Nwbot(dir)+2))
         allocate(dhn(Ni(dir)+Nwtop(dir)+Nwbot(dir)))
         allocate(dhc(Ni(dir)+Nwtop(dir)+Nwbot(dir)+1))

         ! --------------------- GERNATRE GRID -----------------------
         call gridGen(hn,hni,hmin(dir),hmax(dir),Ni(dir),&
          alphai(dir),betai(dir),twtop(dir),twbot(dir),&
          Nwtop(dir),Nwbot(dir),alphaw(dir),betaw(dir))

         call getHcAndDh(hn,hc,dhn,dhc)
         call getHcAndDh(hni,hci,dhni,dhci)

         ! -------------------- STORE COORDINATES --------------------
         select case (dir)
         case (1)
           call setX(this%xyz_n,hn);     call setX(this%xyz_cc,hc)
           call setX(this%xyz_ni,hni);   call setX(this%xyz_cci,hci)

           call setX(this%dxyz_n,dhn);   call setX(this%dxyz_cc,dhc)
           call setX(this%dxyz_ni,dhni); call setX(this%dxyz_cci,dhci)
         case (2)
           call setY(this%xyz_n,hn);     call setY(this%xyz_cc,hc)
           call setY(this%xyz_ni,hni);   call setY(this%xyz_cci,hci)

           call setY(this%dxyz_n,dhn);   call setY(this%dxyz_cc,dhc)
           call setY(this%dxyz_ni,dhni); call setY(this%dxyz_cci,dhci)
         case (3)
           call setZ(this%xyz_n,hn);     call setZ(this%xyz_cc,hc)
           call setZ(this%xyz_ni,hni);   call setZ(this%xyz_cci,hci)

           call setZ(this%dxyz_n,dhn);   call setZ(this%dxyz_cc,dhc)
           call setZ(this%dxyz_ni,dhni); call setZ(this%dxyz_cci,dhci)
         end select
         ! ------------------ STORE DATA ------------------
         this%hmin(dir) = hmin(dir)
         this%hmax(dir) = hmax(dir)
         this%N(dir) = N(dir)
         this%Ni(dir) = Ni(dir)
         this%Nwtop(dir) = Nwtop(dir)
         this%Nwbot(dir) = Nwbot(dir)
         this%twtop(dir) = twtop(dir)
         this%twbot(dir) = twbot(dir)
         this%betai(dir) = betai(dir)
         this%betaw(dir) = betaw(dir)
         this%alphai(dir) = alphai(dir)
         this%alphaw(dir) = alphaw(dir)
         if (printGrid) then
           write(*,*) 'dir',dir
           write(*,*) 'hni = '
           call printH(hni)
           write(*,*) 'hn = '
           call printH(hn)
           write(*,*) 'hci = '
           call printH(hci)
           write(*,*) 'hc = '
           call printH(hc)
           call myPause()
         endif

         deallocate(hn,hc)
         deallocate(dhn,dhc)
         deallocate(hni,hci)
         deallocate(dhni,dhci)
       end subroutine

       subroutine gridGen(hn,hni,hmin,hmax,Ni,alpha,beta,&
         twtop,twbot,Nwtop,Nwbot,alphaw,betaw)
         ! gridGen creates the total domain grid as well as the 
         ! interior grid at the nodes/faces. Note that this is a 
         ! directionless routine, so only the top, bottom and interior
         ! cells are addressed.
         ! 
         ! The idea behind the following approach was that I wanted 
         ! the following inputs:
         !      Ni (number of interior cells)
         !      hmax,hmin (dimensions of interior boundaries)
         !      tw (wall thickness)
         !      Nw (number of cells in the wall)
         !      betai (stretching factor)
         ! for a lid driven cavity geometry. I did not want to have to
         ! manually adjust betaw so that the interior and exterior
         ! grids were similar in size. Instead, I estimate beta (in
         ! in the Robert's grid stretching reference) and use this
         ! stretching for the given wall thickness and number of cells 
         ! in the wall.
         ! 
         ! 
         ! Here's a picture...
         ! 
         !                    Real boundary
         !                         |
         !       |--o--|--o--|--o--|--o--| . . . interior
         !          wall        |  |  |
         !                      |     |
         !                      |     |
         !                      c1    c2
         ! 
         ! Cells c1 and c2 are EXACTLY the same size, the remaining cells
         ! in the wall use a stretching that is estimated from the first cell
         ! size and the beta required to ensure that the wall thickness is exact.
         ! 
         ! NOTE: There may be problems if the density of cells in the wall
         ! is greater than the interior cells near the boundary. The least 
         ! stretching there is means the grid is uniform, in case which case
         ! it better match the density of the interior cells.
         ! 
         ! Fixes / Improvements:
         !    If betaw is calculated to be large, try calculating
         !    betaw using stretching in the other direction (small)
         !    grid near wall, and large grid near interface, this way
         !    beta errors can always be avoided.
         ! 
         implicit none
         real(cp),dimension(:),intent(inout) :: hn,hni
         real(cp),intent(in) :: hmin,hmax,alpha,beta,twtop,twbot,alphaw
         real(cp),intent(inout) :: betaw
         integer,intent(in) :: Ni,Nwtop,Nwbot
         real(cp) :: dh,betawtemp
         integer :: N
         real(cp),dimension(:),allocatable  :: hnw1,hnw2
         N = Ni + Nwtop + Nwbot
         ! **************** Interior fluid domain **********************
         ! |----wall-----|----interior-------|------wall-------|
         !                       ^
         !                       |
         if (nonUniformGridFluid) then
           hni = robertsGrid(hmin,hmax,Ni,alpha,beta)
         else
           hni = linspace1(hmin,hmax,Ni)
         endif

         ! ****************** Wall bottom domain ************************
         ! |----wall-----|----interior-------|------wall-------|
         !       ^
         !       |
         if (Nwbot.gt.0) then
           allocate(hnw1(Nwbot+1))
           if (nonUniformGridWall) then
             if (autoMatchBetas) then
               dh = hni(2)-hni(1)
               ! This will estimate betaw
               call estimateBetaWRecursive(betawtemp,beta,alphaw,Nwbot,hmin-twbot,hmin,dh)
               betaw = betawtemp
             endif
             !          hnw1(1)                   hnw1(Nwbot+1)
             !             |                           |
             !    hnw1 =   |----------|-------|---|--|-|
             hnw1 = robertsGrid(hmin-twbot,hmin,Nwbot,alphaw,betaw)
             hnw1(Nwbot) = hni(1) - dh ! This automatches betas exactly
           else
             dh = hni(2)-hni(1)
             hnw1 = uniformGrid1(hmin,dh,-1)
             call reverseIndex(hnw1)
           endif
         endif

         ! ****************** Wall top domain ************************
         ! |----wall-----|----interior-------|------wall-------|
         !                                           ^
         !                                           |
         if (Nwtop.gt.0) then ! wall + domain
           allocate(hnw2(Nwtop+1))
           if (nonUniformGridWall) then
             if (autoMatchBetas) then
               dh = hni(Ni+1)-hni(Ni)
               ! This will estimate betaw
               call estimateBetaWRecursive(betawtemp,beta,alphaw,Nwtop,hmax,hmax+twtop,dh)
               betaw = betawtemp
             endif
             !         hnw2(Nwtop+1)                hnw2(1)
             !             |                           |
             !    hnw2 =   |-|--|---|-------|----------|    (Indexes need to be reversed!)
             call robertsGrid(hnw2,hmax+twtop,hmax,Nwtop,alphaw,betaw)
             hnw2(Nwtop) = hni(Ni+1) + dh ! This automatches betas exactly
             call reverseIndex(hnw2)
           else
             dh = hni(Ni+1)-hni(Ni)
             hnw2 = uniformGrid1(hmax,dh,1)
           endif
         endif

         ! Combine all together
         if ((Nwbot.gt.0).and.(Nwtop.gt.0)) then
           hn = (/hnw1(1:Nwbot),hni,hnw2(2:Nwtop+1)/)
           deallocate(hnw1,hnw2)
         elseif (Nwbot.gt.0) then
           hn = (/hnw1(1:Nwbot),hni/)
           deallocate(hnw1)
         elseif (Nwtop.gt.0) then
           hn = (/hni,hnw2(2:Nwtop+1)/)
           deallocate(hnw2)
         else
           hn = hni
         endif
       end subroutine

       function robertsGrid(hmin,hmax,N,alpha,beta) result(hn)
         ! This function returns the coordinates and differences of a Robert's 
         ! stretching function as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al.
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         !      alpha    = 0      stretching at y=h only
         !      alpha    = 0.5    stretching at y=0 and y=hmax
         !      beta     = 1.0 <= beta <= infinity = stretching factor
         !               = larger -> less stretching
         ! 
         ! Here is a picture illustration for alpha = 0:
         ! 
         !                                y=h
         !                                 |
         !     |----------|-------|---|--|-|
         !     |------> y
         ! 
         ! Note that this must be used in reverse for the lid driven
         ! cavity geometry for the 'front' and 'back' walls.
         ! 
         ! NOTE: I have abused notation a bit to provide consistent notation
         ! with the reference as well as generalize the returned grid
         ! so that it need not start at y=0.
         ! 
         implicit none
         real(cp),intent(in) :: hmin,hmax,alpha,beta
         integer,intent(in) :: N
         ! Locals
         real(cp),dimension(N+1) :: hn,hnbar
         real(cp) :: dh ! Uniform dh
         integer :: i
         real(cp) :: a,b,g ! alpha,beta,gamma

         a = alpha; b = beta
         g = (b+one)/(b-one)
         ! Where N is the number of cells in the wall
         dh = (hmax - hmin)/dble(N)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+dble(i-1)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( ((b+two*a)*(g)**((hnbar(i)-a)/(one-a))-&
         b+two*a)/((two*a+one)*(one+&
         g**((hnbar(i)-a)/(one-a)))),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
       end function

       subroutine linspace1(hn,hmin,hmax,N)
         ! This routine returns a uniform grid from
         ! hmin to hmax using N+1 points.
         ! 
         ! NOTE: hmin and hmax are included in the result.
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         implicit none
         real(cp),dimension(:),intent(inout) :: hn
         real(cp),intent(in) :: hmin,hmax
         integer,intent(in) :: N
         integer :: i
         real(cp) :: dh
         dh = (hmax - hmin)/dble(N)
         hn = (/(hmin+dble(i-1)*dh,i=1,N+1)/)
       end subroutine

       subroutine uniformGrid1(hn,hstart,dh,dir)
         ! This routine returns a uniform grid beginning
         ! from hstart with uniform step size dh.
         ! The size of the segment depends on the size
         ! of hn. dir is the positive or negative direction.
         ! 
         ! NOTE: hstart is included in the result.
         ! 
         ! INPUT:
         !      hstart   = start value
         !      dh       = step size
         !      N        = N points in segment
         !      dir      = (1,-1)
         implicit none
         real(cp),dimension(:),intent(inout) :: hn
         real(cp),intent(in) :: hstart,dh
         integer,intent(in) :: dir
         integer :: s,i
         s = size(hn)
         ! Total coordinates (uniform)
         hn = (/(hstart+dble(dir)*dble(i-1)*dh,i=1,s)/)
       end subroutine

       subroutine robertsGridBL(beta,delta,hmin,hmax)
         ! robertsGridBL returns the beta for a given boundary laryer
         ! as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al.
         ! 
         ! INPUT:
         !      hmin     = wall boundary (minimum value)
         !      hmax     = wall boundary (maximum value)
         !      delta    = thickness of boundary layer
         implicit none
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),intent(in) :: delta
         real(cp),dimension(3),intent(inout) :: beta
         integer :: i
         do i=1,3
           beta(i) = (one - delta/(hmax(i)-hmin(i)))**(-oneHalf)
         enddo
       end subroutine

       subroutine estimateBetaW(betaw,betai,betamin,betamax,&
         dbeta,alphaw,Nw,hmin,hmax,dh)
         implicit none
         real(cp),intent(inout) :: betaw,betamin,betamax,dbeta
         integer,intent(in) :: Nw
         real(cp),intent(in) :: betai,dh,alphaw,hmax,hmin
         real(cp),dimension(:),allocatable :: root,beta,gammaN,gammaNm1
         real(cp) :: dhstar
         integer :: i,N
         integer,dimension(1) :: hstar
         N = 10000 ! Ten thousand
         allocate(beta(N),root(N),gammaN(N),gammaNm1(N))
         ! Define beta range
         if (betai.lt.one) then
           write(*,*) 'betai must be <1.';stop
         endif
         dbeta = (betamax-betamin)/dble(N)
         beta = (/(betamin + dble(i)*dbeta,i=1,N)/)
         ! prep root
         dhstar = one/(dble(Nw))
         gammaN = ((beta+one)/(beta-one))**((one-alphaw)/(one-alphaw))
         gammaNm1 = ((beta+one)/(beta-one))**((one-dhstar-alphaw)/(one-alphaw))

         do i=1,N
           root(i) = ((beta(i)+two*alphaw)*gammaN(i)-beta(i)+&
            two*alphaw)/((two*alphaw+one)*(one+gammaN(i))) -&
           ((beta(i)+two*alphaw)*gammaNm1(i)-beta(i)+&
            two*alphaw)/((two*alphaw+one)*(one+gammaNm1(i))) -&
           dh/(hmax-hmin)
         enddo
         ! solve root
         hstar = minloc(abs(root),dim=1)
         betaw = beta(hstar(1))
         deallocate(beta,root,gammaN,gammaNm1)
       end subroutine

       subroutine estimateBetaWRecursive(betaw,betai,alphaw,Nw,hmin,hmax,dh)
         implicit none
         real(cp),intent(inout) :: betaw
         integer,intent(in) :: Nw
         real(cp),intent(in) :: betai,dh,alphaw,hmax,hmin
         real(cp) :: betamin,betamax,dbeta
         integer :: i
         ! Initial range
         betamin = one + (betai - one)/1000.0d0
         betamax = one + (betai - one)*1000.0d0
         betaw = betai
         ! Central search
         do i=1,30
           call estimateBetaW(betaw,betai,betamin,betamax,dbeta,alphaw,Nw,hmin,hmax,dh)
           betamin = betaw - two*dbeta
           betamax = betaw + two*dbeta
         enddo
       end subroutine

! ---------------------- CHECK GRID ROUTINES -------------------------------

       subroutine checkAllPointsConsecutive(gd)
         implicit none
         type(griddata),intent(in) :: gd
         call checkPtsConsecDirless(gd,1)
         call checkPtsConsecDirless(gd,2)
         call checkPtsConsecDirless(gd,3)
       end subroutine

       subroutine checkPtsConsecDirless(gd,dir)
         implicit none
         type(griddata),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: hn
         real(cp) :: temp,tol
         integer :: Ndir,i,j
         integer,dimension(3) :: N
         call getN(gd,N)
         Ndir = N(dir)
         allocate(hn(Ndir+1))
         select case (dir)
         case (1); call getXn(gd,hn)
         case (2); call getYn(gd,hn)
         case (3); call getZn(gd,hn)
         end select
         tol = abs(gd%dhMin/two) ! tol must be positive

         do i=1,Ndir
           temp = hn(i+1)-hn(i)
           if (temp.lt.tol) then
              write(*,*) 'hn is not consecutivein direction',dir
              write(*,*) 'tol',tol
              write(*,*) 'i,temp',i,temp
              write(*,*) 'Ndir',Ndir
              write(*,*) 'hn = '
              do j=1,Ndir
                if (j.eq.i) then
                  write(*,*) hn(j),'***'
                else
                  write(*,*) hn(j)
                endif
              enddo
              stop
           endif
         enddo

         deallocate(hn)
       end subroutine

       subroutine checkTwIsAcceptable(gd)
         ! checkWallThicknessIsAcceptable checks if
         ! the wall thickness is acceptable for a given
         ! number of nodes in the wall and the cell 
         ! density of the interior grid. 
         ! 
         ! If the first  exterior cell is larger than 
         ! 50% of the wall thickness, the simulation 
         ! is terminated. This means that there must 
         ! be at least two cells in the wall for a 
         ! uniform grid.
         ! 
         implicit none
         type(griddata),intent(in) :: gd
         call checkTwDirectionless(gd,1)
         call checkTwDirectionless(gd,2)
         call checkTwDirectionless(gd,3)
       end subroutine

       subroutine checkTwDirectionless(gd,dir)
         ! checkTwDirectionless checks if
         ! the wall thickness is acceptable for a given
         ! number of nodes in the wall and the cell 
         ! density of the interior grid. 
         ! 
         ! If ANY exterior cells are larger than 
         ! 'frac' of the wall thickness, the simulation 
         ! is terminated. This means that there must 
         ! be at least two cells in the wall for a 
         ! uniform grid.
         ! 
         ! If this error is triggered, make sure that
         ! nonUniformGridFluid is defined correctly.
         ! If it is, try increasing the density of 
         ! cells in the wall

         type(griddata),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: hn
         real(cp) :: temp,frac
         integer :: Ndir,i
         integer,dimension(3) :: N
         call getN(gd,N)
         Ndir = N(dir)
         allocate(hn(Ndir+1))
         select case (dir)
         case (1); call getXn(gd,hn)
         case (2); call getYn(gd,hn)
         case (3); call getZn(gd,hn)
         end select

         frac = 0.5d0

         ! Check Bottom
         if (gd%twbot(dir).gt.zero) then
           ! temp = size of first interior (fluid) cell
           do i=2,Nin1(dir)+1
             temp = hn(i) - hn(i-1)
             if (temp.gt.frac*gd%twbot(dir)) then
               write(*,*) 'A cell in the BOTTOM wall along direction',dir
               write(*,*) 'is too large compared to the wall thickness.'
               write(*,*) 'twbot(dir) = ',gd%twbot(dir)
               write(*,*) 'frac = ',frac
               write(*,*) 'hn(i) - hn(i-1) = ',hn(i) - hn(i-1)
               if (.not.overrideGeometryWarnings) then
                   stop
               endif
             endif
           enddo
         endif

         ! Check Top
         if (gd%twtop(dir).gt.zero) then
           do i=Nin2(dir),N(dir)
             temp = hn(i+1) - hn(i)
             if (temp.gt.frac*gd%twtop(dir)) then
               write(*,*) 'A cell in the TOP wall along direction',dir
               write(*,*) 'is too large compared to the wall thickness.'
               write(*,*) 'twbot(dir) = ',gd%twtop(dir)
               write(*,*) 'frac = ',frac
               write(*,*) 'hn(i+1) - hn(i) = ',hn(i+1) - hn(i)
               if (.not.overrideGeometryWarnings) then
                   stop
               endif
             endif
           enddo
         endif

         deallocate(hn)
       end subroutine

       subroutine checkCellCentersAreCentered(gd)
         ! This routine makes sure that the cell center
         ! grid is in fact located at the center of the cell.
         ! That is
         !     hc(i+1)-hn(i) = hn(i+1)-hc(i+1)
         ! for all i.
         ! The tolerance used is 10^(-6)*dhmin
         ! 
         implicit none
         type(griddata),intent(in) :: gd
         call checkCCAreCCDirectionless(gd,1)
         call checkCCAreCCDirectionless(gd,2)
         call checkCCAreCCDirectionless(gd,3)
       end subroutine

       subroutine checkCCAreCCDirectionless(gd,dir)
         ! This routine makes sure that the cell center
         ! grid is in fact located at the center of the cell.
         ! That is
         !     hc(i+1)-hn(i) = hn(i+1)-hc(i+1)
         ! for all i for INTERIOR cell centers.
         ! The tolerance used is 10^(-6)*dhmin
         ! It is assumed that hc(1) and hc(Ndir+2)
         ! are correctly located.

         implicit none
         type(griddata),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: hn,hc
         integer :: Ndir,i
         integer,dimension(3) :: N
         real(cp) :: tol,temp1,temp2

         call getN(gd,N)
         Ndir = N(dir)
         allocate(hn(Ndir+1),hc(Ndir+2))
         select case (dir)
         case (1); call getXn(gd,hn); call getXcc(gd,hc)
         case (2); call getYn(gd,hn); call getYcc(gd,hc)
         case (3); call getZn(gd,hn); call getZcc(gd,hc)
         end select

         tol = gd%dhMin*10.0d0**(-6.0d0)
         do i=1,Ndir
           temp1 = hc(i+1)-hn(i)
           temp2 = hn(i+1)-hc(i+1)
           if (abs(temp1-temp2).gt.tol) then
              write(*,*) 'Cell centers are not centered in direction',dir
              write(*,*) 'i,temp1,temp2',i,temp1,temp2
              write(*,*) 'Ndir',Ndir
              write(*,*) 'hn = '
              call printH(hn)
              write(*,*) 'hc = '
              call printH(hc)
              stop
           endif
         enddo

         deallocate(hn,hc)
       end subroutine

       subroutine checkIfTwAndNw(gd)
         ! This routine makes sure that if the wall thickness
         ! is zero, then the number of cells in the wall
         ! should be zero.
         implicit none
         type(griddata),intent(in) :: gd
         call checkIfTwAndNwDirectionless(gd,1)
         call checkIfTwAndNwDirectionless(gd,2)
         call checkIfTwAndNwDirectionless(gd,3)
       end subroutine

       subroutine checkUniformGrid(gd)
         ! This routine checks
         ! If 
         !   nonUniformGridFluid = nonUniformGridWall = .false.
         ! That 
         !   dh_i = dh_i-1 for all i
         implicit none
         type(griddata),intent(in) :: gd
         if (.not.(nonUniformGridFluid.or.nonUniformGridWall)) then
           call checkUniformGridDirectionless(gd,1)
           call checkUniformGridDirectionless(gd,2)
           call checkUniformGridDirectionless(gd,3)
         endif
       end subroutine

       subroutine checkUniformGridDirectionless(gd,dir)
         implicit none
         type(griddata),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: hn
         real(cp),dimension(:),allocatable :: hc
         integer,dimension(3) :: N
         integer :: i,Ndir
         real(cp) :: tol,temp1,temp2

         call getN(gd,N)
         Ndir = N(dir)
         allocate(hn(Ndir+1),hc(Ndir+2))
         select case (dir)
         case (1); call getXn(gd,hn); call getXcc(gd,hc)
         case (2); call getYn(gd,hn); call getYcc(gd,hc)
         case (3); call getZn(gd,hn); call getZcc(gd,hc)
         end select

         ! Node grid
         tol = getDhMin(gd)/10000.0d0
         temp1 = hn(2)-hn(1)
         do i=1,Ndir
           temp2 = hn(i+1)-hn(i)
           if (abs(temp1-temp2).gt.tol) then
              write(*,*) 'Uniform grid is not uniform along direction',dir
              write(*,*) 'i,temp1,temp2',i,temp1,temp2
              write(*,*) 'Ndir',Ndir
              write(*,*) 'hn = '
              call printH(hn)
              stop
           endif
         enddo

         ! Cell center grid
         temp1 = hc(2)-hc(1)
         do i=1,Ndir
           temp2 = hc(i+1)-hc(i)
           if (abs(temp1-temp2).gt.tol) then
              write(*,*) 'Uniform grid is not uniform along direction',dir
              write(*,*) 'i,temp1,temp2',i,temp1,temp2
              write(*,*) 'Ndir',Ndir
              write(*,*) 'hc = '
              call printH(hc)
              stop
           endif
         enddo
       end subroutine

       subroutine checkIfTwAndNwDirectionless(gd,dir)
         ! This routine checks two things:
         ! 
         !   1) If the wall thickness is zero, 
         !      then the number of cells in the
         !      wall should be zero.
         ! 
         !   2) If the wall thickness is nonzero, 
         !      then the number of cells in the
         !      wall should be greater than 1 in
         !      order for the for hn to reach tw.
         ! 
         implicit none
         type(griddata),intent(in) :: gd
         integer,intent(in) :: dir
         real(cp) :: tol

         tol = gd%dhMin*10.0d0**(-6.0d0)
         ! ---------------- zero tw
         ! Check Bottom
         if ((gd%twbot(dir).lt.tol).and.(Nwbot(dir).ne.0)) then
           write(*,*) 'The BOTTOM wall thickness is specified as zero'
           write(*,*) 'but Nw is not zero along direction',dir
           stop
         endif

         ! Check Top
         if ((gd%twtop(dir).lt.tol).and.(Nwtop(dir).ne.0)) then
           write(*,*) 'The TOP wall thickness is specified as zero'
           write(*,*) 'but Nw is not zero along direction',dir
           stop
         endif
         
         ! ---------------- finite tw
         ! Check Bottom
         if ((gd%twbot(dir).gt.tol).and.(Nwbot(dir).lt.2)) then
           write(*,*) 'The BOTTOM wall thickness is larger than zero'
           write(*,*) 'but Nw is less than 2 along direction',dir
           stop
         endif

         ! Check Top
         if ((gd%twtop(dir).lt.tol).and.(Nwtop(dir).ne.0)) then
           write(*,*) 'The TOP wall thickness is larger than zero'
           write(*,*) 'but Nw is less than 2 along direction',dir
           stop
         endif
       end subroutine

       subroutine reComputeTwDirectionless(gd,dir)
         ! This routine computes the wall thickness (if necessary)
         ! when the wall nodes are uniformly spaced. This may override
         ! the input wall thickness.
         ! 
         ! This routine assumes that the wall thickness is the number
         ! of cells in the wall times the size of the first interior
         ! (fluid) cell.
         implicit none
         type(griddata),intent(inout) :: gd
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: hni
         integer :: Ndir
         integer,dimension(3) :: Ni

         call getNi(gd,Ni)
         Ndir = Ni(dir)
         allocate(hni(Ndir+1))
         select case (dir)
         case (1); call getXn(gd,hni)
         case (2); call getYn(gd,hni)
         case (3); call getZn(gd,hni)
         end select

         if (Nwtop(dir).gt.0) then
           gd%twtop = dble(Nwtop(dir))*(hni(Ndir+1)-hni(Ndir))
         endif

         if (Nwbot(dir).gt.0) then
           gd%twbot = dble(Nwbot(dir))*(hni(2)-hni(1))
         endif

         deallocate(hni)
       end subroutine

       subroutine reComputeTw(gd)
         ! This routine computes the wall thickness (if necessary)
         ! when the wall nodes are uniformly spaced. This may override
         ! the input wall thickness.
         implicit none
         type(griddata),intent(inout) :: gd
         call reComputeTwDirectionless(gd,1)
         call reComputeTwDirectionless(gd,2)
         call reComputeTwDirectionless(gd,3)
       end subroutine

       subroutine checkGrid(gd)
         implicit none
         type(griddata),intent(in) :: gd
         call checkAllPointsConsecutive(gd)    ! Seem to protect correctly
         call checkIfTwAndNw(gd)               ! Seem to protect correctly
         call checkTwIsAcceptable(gd)          ! Seem to protect correctly
         call checkCellCentersAreCentered(gd)  ! Seem to protect correctly
         call checkUniformGrid(gd)             ! Need to check if protects
       end subroutine

! -------------------- PRINT / WRITE ROUTINES ---------------------------------

       subroutine exportGriddata(this,dir)
         implicit none
         type(griddata), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: newU

         newU = newAndOpen(dir//'parameters/','Griddata')
         call writeGriddataToFileOrScreen(this,newU)
         call closeAndMessage(newU,'Griddata',dir)
       end subroutine

       subroutine printGriddata(this)
         implicit none
         type(griddata), intent(in) :: this
         call writeGriddataToFileOrScreen(this,6)
       end subroutine

       subroutine printH(h)
         implicit none
         real(cp),dimension(:), intent(in) :: h
         integer s,i
         s = size(h)
         do i=1,s
          write(*,*) h(i)
         enddo
       end subroutine

       subroutine writeGriddataToFileOrScreen(this,u)
         implicit none
         type(griddata), intent(in) :: this
         integer,intent(in) :: u
         write(u,*) '----------------GRIDDATA--------------'
         write(u,'(A8,'//xyzfmt//')')  ' hmin = ',this%hmin
         write(u,'(A8,'//xyzfmt//')')  ' hmax = ',this%hmax
         if (nonUniformGridWall) then
           write(u,'(A9,'//xyzfmt//')')  ' twtop = ',this%twtop
           write(u,'(A9,'//xyzfmt//')')  ' twbot = ',this%twbot
         else
           write(u,'(A22,'//xyzfmt//')')  ' twtop (overridden) = ',this%twtop
           write(u,'(A22,'//xyzfmt//')')  ' twbot (overridden) = ',this%twbot
         endif
         write(u,'(A6,'//xyzifmt//')') ' Ni = ',this%Ni
         write(u,'(A9,'//xyzifmt//')') ' Nwtop = ',this%Nwtop
         write(u,'(A9,'//xyzifmt//')') ' Nwbot = ',this%Nwbot
         write(u,'(A5,'//xyzifmt//')') ' N = ',this%N
         write(u,'(A16,2'//hfmt//')')  ' dhiMin,dhMin = ',this%dhiMin,this%dhMin
         ! Stretching information:
         if (nonUniformGridFluid.or.nonUniformGridWall) then
           write(u,'(A10,'//xyzfmt//')') ' alphai = ',this%alphai
           write(u,'(A10,'//xyzfmt//')') ' alphaw = ',this%alphaw
           write(u,'(A9,'//xyzfmt//')')  ' betai = ',this%betai
           write(u,'(A9,'//xyzfmt//')')  ' betaw = ',this%betaw
         else
           write(u,'(A23,L1)')  ' nonUniformGridFluid = ',nonUniformGridFluid
           write(u,'(A22,L1)')  ' nonUniformGridWall = ',nonUniformGridWall
         endif
       end subroutine

       end module