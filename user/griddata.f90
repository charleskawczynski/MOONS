       module griddata_mod
       use simParams_mod
       use IO_tools_mod
       use grid_mod
       use gridGen_mod
       use gridGenTools_mod
       use coordinates_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       public :: griddata
       public :: delete

       public :: init

       public :: printGriddata
       public :: exportGriddata
       public :: checkGrid


       public :: Nin1,Nin2
       public :: Nici1,Nici2
       public :: Nice1,Nice2

       public :: xyzfmt,hfmt

       ! Export/print format. May need to adjust # in 3F_#_ if betaw is large
       character(len=5),parameter :: hfmt = 'F15.6'
       character(len=6),parameter :: xyzfmt = '3F15.6'
       character(len=3),parameter :: xyzifmt = '3I5'

       ! ********************* NUMBER OF CELLS ***********************
       ! In this section, the number of cells in the (x,y,z)=(1,2,3) directions
       ! are specified in the fluid domain (Ni) and in the walls (Nwtop,Nwbot).
       ! The INDEX OF CELLS section should not be modified.
       ! 
       ! NOTES:
       !           Making the number of interior cells even results 
       !           in a odd number of nodes along each direction. So, 
       !           if u(0,0,0) is desired, then choose Ni to be even.
       !           Wall (interface is included in interior)
       ! 
       ! CAUTION:
       !           Careful, if too many cells are in the wall, 
       !           then the beta's (stretching factor) may not 
       !           match because a uniform grid in the walls may 
       !           be too fine compared to the interior. This
       !           should be checked for in griddata and terminate
       !           execution if it happens.
       ! 
       ! *********************** USER DEFINED ************************
       ! integer,dimension(3),parameter :: Ni = 1, Nwtop = 0, Nwbot = 0
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = 0, Nwbot = 0

       ! integer,dimension(3),parameter :: Ni = (/46,46,46/) ! Number of cells in fluid
       ! integer,dimension(3),parameter :: Nwtop = (/11,11,11/) ! Number of cells in wall
       ! integer,dimension(3),parameter :: Nwbot = (/11,11,11/) ! Number of cells in wall

       ! ********************* BENCHMARK CASES ***********************

       ! --------------------- VERIFICATION CASES --------------------
       ! benchmarkCase 1
       ! integer,dimension(3),parameter :: Ni = (/101,101,41/), Nwtop = 0, Nwbot = 0
       ! benchmarkCase 50 (Re=2000)
       ! integer,dimension(3),parameter :: Ni = 105, Nwtop = 0, Nwbot = 0
       ! benchmarkCase 51 (Re=3200)
       ! integer,dimension(3),parameter :: Ni = 105, Nwtop = 0, Nwbot = 0
       ! 
       ! ---------------------- VALIDATION CASES ---------------------

       ! benchmarkCase = 100
       ! integer,dimension(3),parameter :: Ni = (/67,67,27/), Nwtop = 0, Nwbot = 0
       ! benchmarkCase = 101
       ! integer,dimension(3),parameter :: Ni = 52, Nwtop = 0, Nwbot = 0
       ! integer,dimension(3),parameter :: Ni = 64, Nwtop = 0, Nwbot = 0
       ! integer,dimension(3),parameter :: Ni = (/101,101,41/), Nwtop = 0, Nwbot = 0

       ! benchmarkCase = 102
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = 11, Nwbot = 11
       ! integer,dimension(3),parameter :: Ni = 48, Nwtop = 8, Nwbot = 8 ! For multigrid
       ! integer,dimension(3),parameter :: Ni = 84, Nwtop = 20, Nwbot = 20
       ! benchmarkCase = 103
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = 11, Nwbot = 11
       ! benchmarkCase = 104
       ! integer,dimension(3),parameter :: Ni = 51, Nwtop = 5, Nwbot = 5

       ! benchmarkCase = 105
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = (/11,0,11/), Nwbot = 11
       ! benchmarkCase = 106
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = (/11,0,11/), Nwbot = 11
       ! benchmarkCase = 107
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = (/11,0,11/), Nwbot = 11
       ! benchmarkCase = 108
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = (/11,0,11/), Nwbot = 11

       ! benchmarkCase = 109
       ! integer,dimension(3),parameter :: Ni = 45, Nwtop = (/11,2,11/), Nwbot = (/11,2,11/)

       ! benchmarkCase = 200
       ! integer,dimension(3),parameter :: Ni = (/2**7,2**5,2**5/)+1, Nwtop = 0, Nwbot = 0
       ! benchmarkCase = 201
       ! integer,dimension(3),parameter :: Ni = (/101,32,32/), Nwtop = (/0,5,5/), Nwbot = (/0,5,5/)
       ! benchmarkCase = 202
       ! integer,dimension(3),parameter :: Ni = (/181,47,47/), Nwtop = (/0,5,5/), Nwbot = (/0,5,5/)

       ! benchmarkCase = 250
       ! integer,dimension(3),parameter :: Ni = (/200,51,56/), Nwtop = (/0,5,5/), Nwbot = (/0,5,5/)
       ! integer,dimension(3),parameter :: Ni = (/30,10,10/), Nwtop = (/0,5,5/), Nwbot = (/0,5,5/)

       ! benchmarkCase = 300
       ! integer,dimension(3),parameter :: Ni = 51, Nwtop = 0, Nwbot = 0
       ! benchmarkCase = 301
       ! integer,dimension(3),parameter :: Ni = 101, Nwtop = 0, Nwbot = 0

       ! benchmarkCase = 1001
       ! integer,dimension(3),parameter :: Ni = 52, Nwtop = (/8,0,8/), Nwbot = 8 ! Ha = 10,100,1000
       ! benchmarkCase = 1002
       ! integer,dimension(3),parameter :: Ni = (/65,45,45/), Nwtop = 0, Nwbot = 0     ! Insulating
       ! integer,dimension(3),parameter :: Ni = (/65,45,45/), Nwtop = (/0,5,0/), Nwbot = (/0,5,0/)     ! Conducting
       ! benchmarkCase = 1003
       integer,dimension(3),parameter :: Ni = (/75,45,45/), Nwtop = 11, Nwbot = 11
       ! benchmarkCase = 1004
       ! integer,dimension(3),parameter :: Ni = 35, Nwtop = 0, Nwbot = 0


       ! ********************* INDEX OF CELLS ************************ (DO NOT CHANGE)
       integer,dimension(3),parameter :: N        = Ni+Nwtop+Nwbot    ! Total number of cells (excluding ghost)

       ! These indexes are used in three places:
       !       initSigmaMu.f90 (to init sigma/mu)
       !       inductionSolver.f90 (in embedVelocity
       !       inductionSolver.f90 (in computeJCrossB)
       integer,dimension(3),parameter :: Nin1     = Nwbot+2           ! Node of min wall (excluding ghost)
       integer,dimension(3),parameter :: Nin2     = N-Nwtop+2         ! Node of max wall (excluding ghost)

       integer,dimension(3),parameter :: Nice1    = Nwbot+2           ! CC of min wall (excluding ghost)
       integer,dimension(3),parameter :: Nice2    = N-Nwtop+1         ! CC of max wall (excluding ghost)
       integer,dimension(3),parameter :: Nici1    = Nwbot+1           ! CC of min wall (including ghost)
       integer,dimension(3),parameter :: Nici2    = N-Nwtop+2         ! CC of max wall (including ghost)

       type griddata
         private ! figure out a way to still do this
         ! Number of cells
         integer,dimension(3) :: N
         integer,dimension(3) :: Nwtop
         integer,dimension(3) :: Nwbot
         integer,dimension(3) :: Ni

         ! Wall thicknesses
         real(cp),dimension(3) :: twtop
         real(cp),dimension(3) :: twbot

         ! Fluid domain boundaries
         real(cp),dimension(3) :: hmin
         real(cp),dimension(3) :: hmax

         ! Spatial discretization
         real(cp),dimension(3) :: dh
         real(cp),dimension(3) :: alphai,betai
         real(cp),dimension(3) :: alphaw,betaw

         ! Coordinates
         type(grid) :: hi,ht
       end type

       interface delete;       module procedure deleteGriddata;      end interface
       interface init;         module procedure initGriddata;        end interface
       
       contains

       subroutine deleteGriddata(this)
         implicit none
         type(griddata),intent(inout) :: this
         call delete(this%hi)
         call delete(this%ht)
         write(*,*) 'Griddata object deleted'
       end subroutine

       subroutine initGriddata(this,g_mom,g_ind,Re,Ha)
         implicit none
         type(griddata),intent(out) :: this
         type(grid),intent(inout) :: g_mom,g_ind
         real(cp),intent(in) :: Re,Ha
         real(cp),dimension(3) :: twtop,twbot
         real(cp),dimension(3) :: hmin,hmax
         real(cp),dimension(3) :: alphai,betai
         real(cp),dimension(3) :: alphaw,betaw
         real(cp),dimension(3) :: betawBot,betawTop
         real(cp) :: tau,y_c,dh,dh1,dh2
         integer :: i,j,N_cells_uniform
         type(gridGenerator) :: gg

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
         call robertsGridBL(betai,real(2.0)/Ha,hmin,hmax)

         ! write(*,*) 'betai = ',betai
         ! stop

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
         hmin(1) = zero; hmax(1) = 40.0d0
         case (201); hmin = -one; hmax = one
         hmin(1) = zero; hmax(1) = 20.0d0
         case (202); hmin = -one; hmax = one
         hmin(1) = zero; hmax(1) = 40.0d0

         case (250); hmin = -one; hmax = one
         hmin(1) = zero; hmax(1) = real(25.0,cp)

         case (300); hmin = -one; hmax = one ! for xyz
         case (301); hmin = -one; hmax = one ! for xyz

         case (1001); hmin = -one; hmax = one ! for xyz

         case (1002); hmin = -one; hmax = one ! for xyz
         hmin(1) = real(0.0,cp); hmax(1) = real(10.0,cp)

         case (1003); hmin = -one; hmax = one ! for xyz
         hmin(1) = real(-10.0,cp); hmax(1) = real(10.0,cp)

         case (1004); hmin = zero; hmax = one ! for xyz
         case default
           write(*,*) 'Incorrect benchmarkCase in initGriddata';stop
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

         case (200); betai = 1.05d0; betai(1) = 1000.0d0
         case (201); betai = 1.01d0; betai(1) = 1000.0d0
         case (202); betai = 1.001d0; betai(1) = 1000.0d0

         ! case (250); betai = 1.00008d0; betai(1) = 1000.0d0
         ! case (250); betai = 1.0001d0; betai(1) = 1000.0d0
         case (250); betai = 1.001d0; betai(1) = 1000.0d0

         case (300); betai = 1000.0d0
         case (301); betai = 1000.0d0

         ! case (1001); betai = 1.025d0 ! Ha = 10
         ! case (1001); betai = 1.005d0 ! Ha = 100
         case (1001); betai = 1.0005d0 ! Ha = 1000

         case (1002); betai = 1.01d0
         ! call robertsGridBL(betai,real(2.0,cp)/Ha,hmin,hmax)

         betai = robertsBL(real(2.0,cp)/Ha,hmax-hmin)
         betai(1) = 10000.0d0

         case (1003); betai = 1.04d0
                      betai(1) = 1.004d0

         case (1004); betai = 100.0d0

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

         case (250); twtop = real(0.142394,cp); twbot = real(0.142394,cp)
                     twtop(1) = real(0.0,cp);  twbot(1) = real(0.0,cp)

         case (300); twtop = 0.0d0;      twbot = 0.0d0
         case (301); twtop = 0.0d0;      twbot = 0.0d0



         case (1001); twtop = 0.1d0;   twbot = 0.1d0
                     twtop(2) = 0.0d0

         case (1002); twtop = 0.0d0;   twbot = 0.0d0
!          case (1002); twtop = 0.0d0;  twbot = 0.0d0
!                       twtop(2) = 0.01d0;  twbot(2) = 0.01d0

         case (1003); twtop = 0.1d0;   twbot = 0.1d0

         case (1004); twtop = 0.0d0;   twbot = 0.0d0

         case default
           stop 'Error: Incorrect benchmarkCase in setGriddata'
         end select

         if (benchmarkCase.ne.0) then
           alphai = oneHalf; alphaw = zero; betaw = betai
         endif

         ! --------------------- GERNATRE GRID -----------------------
         call setGrid(this,hmin,hmax,alphai,betai,twtop,twbot,alphaw,betaw,1) ! x
         call setGrid(this,hmin,hmax,alphai,betai,twtop,twbot,alphaw,betaw,2) ! y
         call setGrid(this,hmin,hmax,alphai,betai,twtop,twbot,alphaw,betaw,3) ! z

         if (.not.nonUniformGridWall) then
           call reComputeTw(this)
         endif

         ! *****************************************************************
         ! *****************************************************************
         ! *************************** DUCT ********************************
         ! *****************************************************************
         ! *****************************************************************
         ! Duct flow for Sergey's fringe
         tau = real(2.0,cp); y_c = real(1.5,cp) ! y_c should match Bshift in sergey's fringe
         ! call init(gg,(/cluster(hmin(1),(hmax(1)-hmin(1))/2.0,Ni(1)/2,y_c,tau)/),1)
         ! call pop(gg,1)
         ! call app(gg,(/cluster((hmax(1)-hmin(1))/2.0,hmax(1),Ni(1)/2,hmax(1)-y_c-(hmax(1)-hmin(1))/2.0,tau)/),1)
         ! call applyGhost(gg,1)
         ! call init(g_mom,gg%g%c(1)%hn,1,2)
         ! call init(g_ind,gg%g%c(1)%hn,1,2)
         ! ! yz grid
         ! do i=2,3
         !   if (nonUniformGridFluid) then
         !         call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
         !   else; call init(gg,(/uniform(hmin(i),hmax(i),Ni(i))/),i)
         !   endif
         !   call applyGhost(gg,i)
         !   call init(g_mom,gg%g%c(i)%hn,i,2)
         !   call init(g_ind,gg%g%c(i)%hn,i,2)
         ! enddo
         ! call delete(gg)


         ! *****************************************************************
         ! *****************************************************************
         ! ************************** 3D CAVITY ****************************
         ! *****************************************************************
         ! *****************************************************************

         ! 3D Cavity (interior)
         ! do i=1,3
         !   if (nonUniformGridFluid) then
         !         call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
         !   else; call init(gg,(/uniform(hmin(i),hmax(i),Ni(i))/),i)
         !   endif
         !   call applyGhost(gg,i)
         !   call init(g_mom,gg%g%c(i)%hn,i,2)
         ! enddo

         ! if (autoMatchBetas) then
         !   do i=1,3
         !     dh = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
         !     call estimateBetaWRecursive(betawBot(i),betai(i),alphaw(i),Nwbot(i),hmin(i)-twbot(i),hmin(i),dh)
         !     dh = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)
         !     call estimateBetaWRecursive(betawTop(i),betai(i),alphaw(i),Nwtop(i),hmax(i),hmax(i)+twtop(i),dh)
         !   enddo
         ! else
         !   betawTop = betaw; betawBot = betaw
         ! endif

         ! 3D Cavity (add walls in all directions)
         ! do i=1,3
         !   call snip(gg,i); call pop(gg,i) ! Remove ghost nodes
         ! 
         !   if (Nwbot(i).gt.0) then
         !     if (nonUniformGridWall) then
         !       call snip(gg,i)
         !       call prep(gg,(/robertsRight(hmin(i)-twbot(i),hmin(i),Nwbot(i),betaw(i))/),i)
         !     else
         !       dh = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
         !       call snip(gg,i)
         !       call prep(gg,(/uniformLeft(hmin(i),dh,Nwbot(i))/),i)
         !     endif
         !   endif
         !   if (Nwtop(i).gt.0) then
         !     if (nonUniformGridWall) then
         !       call pop(gg,i)
         !       call app(gg,(/robertsLeft(hmax(i),hmax(i)+twtop(i),Nwtop(i),betaw(i))/),i)
         !     else
         !       dh = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)
         !       call pop(gg,i)
         !       call app(gg,(/uniformRight(hmax(i),dh,Nwtop(i))/),i)
         !     endif
         !   endif
         ! 
         !   call applyGhost(gg,i) ! re-apply ghosts
         !   call init(g_ind,gg%g%c(i)%hn,i,2)
         ! enddo
         ! call delete(gg)


         ! *****************************************************************
         ! *****************************************************************
         ! ****************** 3D CAVITY - UNIFORM BL ***********************
         ! *****************************************************************
         ! *****************************************************************

!          N_cells_uniform = 5
!          ! 3D Cavity (interior)
!          do i=1,3
!            call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
!            dh1 = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
!            dh2 = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)

!            if ((i.eq.2).or.(i.eq.3)) then
!              betai(i) = (betai(i)-real(1.0,cp))/real(3.5,cp)+real(1.0,cp)
!              call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
!            else
!              call init(gg,(/robertsBoth(hmin(i),hmax(i),Ni(i),betai(i))/),i)
!            endif

!            do j=1,N_cells_uniform+1
!              call snip(gg,i)
!            enddo
!            call prep(gg,(/uniformRight(hmin(i),dh1,N_cells_uniform)/),i)

!            do j=1,N_cells_uniform+1
!              call pop(gg,i)
!            enddo
!            call app(gg,(/uniformLeft(hmax(i),dh2,N_cells_uniform)/),i)

!            call applyGhost(gg,i)
!            call init(g_mom,gg%g%c(i)%hn,i,2)
!          enddo

         ! if (autoMatchBetas) then
         !   do i=1,3
         !     dh = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
         !     call estimateBetaWRecursive(betawBot(i),betai(i),alphaw(i),Nwbot(i),hmin(i)-twbot(i),hmin(i),dh)
         !     dh = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)
         !     call estimateBetaWRecursive(betawTop(i),betai(i),alphaw(i),Nwtop(i),hmax(i),hmax(i)+twtop(i),dh)
         !   enddo
         ! else
         !   betawTop = betaw; betawBot = betaw
         ! endif

         ! 3D Cavity (add walls in all directions)
!          do i=1,3
!            call snip(gg,i); call pop(gg,i) ! Remove ghost nodes
         
!            if (Nwbot(i).gt.0) then
!              if (nonUniformGridWall) then
!                call snip(gg,i)
!                call prep(gg,(/robertsRight(hmin(i)-twbot(i),hmin(i),Nwbot(i),betaw(i))/),i)
!              else
!                dh = gg%g%c(i)%hn(2)-gg%g%c(i)%hn(1)
!                call snip(gg,i)
!                call prep(gg,(/uniformLeft(hmin(i),dh,Nwbot(i))/),i)
!              endif
!            endif
!            if (Nwtop(i).gt.0) then
!              if (nonUniformGridWall) then
!                call pop(gg,i)
!                call app(gg,(/robertsLeft(hmax(i),hmax(i)+twtop(i),Nwtop(i),betaw(i))/),i)
!              else
!                dh = gg%g%c(i)%hn(gg%g%c(i)%sn)-gg%g%c(i)%hn(gg%g%c(i)%sn-1)
!                call pop(gg,i)
!                call app(gg,(/uniformRight(hmax(i),dh,Nwtop(i))/),i)
!              endif
!            endif
         
!            call applyGhost(gg,i) ! re-apply ghosts
!            call init(g_ind,gg%g%c(i)%hn,i,2)
!          enddo
!          call delete(gg)


         call init(g_ind,this%ht%c(1)%hn,1,2)
         call init(g_ind,this%ht%c(2)%hn,2,2)
         call init(g_ind,this%ht%c(3)%hn,3,2)
         call init(g_mom,this%hi%c(1)%hn,1,2)
         call init(g_mom,this%hi%c(2)%hn,2,2)
         call init(g_mom,this%hi%c(3)%hn,3,2)

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

         real(cp),dimension(:),allocatable :: hni,hn
         real(cp),dimension(:),allocatable :: hniTemp,hnTemp

         ! --------------------- ALLOCATE GRID -----------------------
         ! Interior
         allocate(hni(Ni(dir)+1))
         allocate(hniTemp(Ni(dir)+1+2))

         ! Total
         allocate(hn(Ni(dir)+Nwtop(dir)+Nwbot(dir)+1))
         allocate(hnTemp(Ni(dir)+Nwtop(dir)+Nwbot(dir)+1+2))
         ! --------------------- GERNATRE GRID -----------------------
         call gridGen(hn,hni,hmin(dir),hmax(dir),Ni(dir),&
          alphai(dir),betai(dir),twtop(dir),twbot(dir),&
          Nwtop(dir),Nwbot(dir),alphaw(dir),betaw(dir))

         ! -------------------- STORE COORDINATES --------------------
         hniTemp(2:size(hniTemp)-1) = hni
         hnTemp(2:size(hnTemp)-1) = hn

         call defineGhostPoints(hniTemp)
         call defineGhostPoints(hnTemp)

         call init(this%hi,hniTemp,dir,2)
         call init(this%ht,hnTemp,dir,2)
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

         deallocate(hni,hn)
         deallocate(hniTemp,hnTemp)
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
           call robertsGrid2(hni,hmin,hmax,Ni,alpha,beta)
         else
           call linspace1(hni,hmin,hmax,Ni)
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
             call robertsGrid2(hnw1,hmin-twbot,hmin,Nwbot,alphaw,betaw)
             hnw1(Nwbot) = hni(1) - dh ! This automatches betas exactly
           else
             dh = hni(2)-hni(1)
             call uniformGrid1(hnw1,hmin,dh,-1)
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
             call robertsGrid2(hnw2,hmax+twtop,hmax,Nwtop,alphaw,betaw)
             hnw2(Nwtop) = hni(Ni+1) + dh ! This automatches betas exactly
             call reverseIndex(hnw2)
           else
             dh = hni(Ni+1)-hni(Ni)
             call uniformGrid1(hnw2,hmax,dh,1)
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
         dbeta = (betamax-betamin)/real(N,cp)
         beta = (/(betamin + real(i,cp)*dbeta,i=1,N)/)
         ! prep root
         dhstar = one/(real(Nw,cp))
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
         real(cp) :: frac
         integer :: Ndir
         ! integer :: i
         ! real(cp) :: temp

         Ndir = gd%N(dir)
         allocate(hn(Ndir+1))
         hn = gd%ht%c(dir)%hn

         frac = 0.5d0

         ! Check Bottom
         if (gd%twbot(dir).gt.zero) then
           ! temp = size of first interior (fluid) cell
           ! do i=2,Nin1(dir)+1
           !   temp = hn(i) - hn(i-1)
           !   if (temp.gt.frac*gd%twbot(dir)) then
           !     write(*,*) 'A cell in the BOTTOM wall along direction',dir
           !     write(*,*) 'is too large compared to the wall thickness.'
           !     write(*,*) 'twbot(dir) = ',gd%twbot(dir)
           !     write(*,*) 'frac = ',frac
           !     write(*,*) 'hn(i) - hn(i-1) = ',hn(i) - hn(i-1)
           !     if (.not.overrideGeometryWarnings) then
           !         stop
           !     endif
           !   endif
           ! enddo
         endif

         ! Check Top
         if (gd%twtop(dir).gt.zero) then
           ! do i=Nin2(dir),N(dir)
           !   temp = hn(i+1) - hn(i)
           !   if (temp.gt.frac*gd%twtop(dir)) then
           !     write(*,*) 'A cell in the TOP wall along direction',dir
           !     write(*,*) 'is too large compared to the wall thickness.'
           !     write(*,*) 'twbot(dir) = ',gd%twtop(dir)
           !     write(*,*) 'frac = ',frac
           !     write(*,*) 'hn(i+1) - hn(i) = ',hn(i+1) - hn(i)
           !     if (.not.overrideGeometryWarnings) then
           !         stop
           !     endif
           !   endif
           ! enddo
         endif

         deallocate(hn)
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
         integer :: i,Ndir
         real(cp) :: tol,temp1,temp2

         Ndir = gd%N(dir)
         allocate(hn(Ndir+1),hc(Ndir+2))

         hn = gd%ht%c(dir)%hn
         hc = gd%ht%c(dir)%hc

         ! Node grid
         tol = gd%ht%dhMin/10000.0d0
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

         tol = gd%ht%dhMin*10.0d0**(-6.0d0)
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
           write(*,*) 'twbot = ',gd%twbot(dir)
           write(*,*) 'Nwbot = ',Nwbot(dir)
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

         Ndir = gd%Ni(dir)
         allocate(hni(Ndir+1))

         hni = gd%hi%c(dir)%hn

         if (Nwtop(dir).gt.0) then
           gd%twtop = real(Nwtop(dir),cp)*(hni(Ndir+1)-hni(Ndir))
         endif

         if (Nwbot(dir).gt.0) then
           gd%twbot = real(Nwbot(dir),cp)*(hni(2)-hni(1))
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
         call checkIfTwAndNw(gd)               ! Seem to protect correctly
         call checkTwIsAcceptable(gd)          ! Seem to protect correctly
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
         write(u,'(A16,2'//hfmt//')')  ' dhiMin,dhMin = ',this%hi%dhMin,this%ht%dhMin
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