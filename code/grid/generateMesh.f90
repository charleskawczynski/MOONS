       module generateMesh_mod
       use grid_mod
       use mesh_mod
       use generateGrid_mod
       use gridGen_mod
       use gridGenTools_mod
       use matchGridStretching_mod
       use simParams_mod
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
       real(cp),parameter :: PI = 3.141592653589793238460_cp

       public :: makeGrids
       public :: getBMCParams

       contains

       subroutine makeGrids(g_mom,g_ind,Ni,Nwtop,Nwbot)
         implicit none
         type(grid),intent(inout) :: g_mom,g_ind
         integer,dimension(3),intent(in) :: Ni,Nwtop,Nwbot
         ! call cavity3D_withWalls(g_mom,g_ind,Ni,Nwtop,Nwbot)
         call makeGrids_BCs(g_mom,g_ind,Ni,Nwtop,Nwbot)
       end subroutine

       subroutine cavity3D_withWalls(g_mom,g_ind,Ni,Nwtop,Nwbot)
         implicit none
         type(grid),intent(inout) :: g_mom,g_ind
         integer,dimension(3),intent(in) :: Ni,Nwtop,Nwbot
         real(cp),dimension(3) :: hmin,hmax,betai
         real(cp),dimension(3) :: twtop,twbot
         ! type(grid) :: temp
         call getBMCParams(hmin,hmax,betai,twtop,twbot)
         ! call cavity3D_uniform(g_mom,hmin,hmax,Ni)
         ! call extend_uniform(g_ind,g_mom,Nwtop,Nwbot) ! Extend to wall
         call cavity3D_nonUniform(g_mom,hmin,hmax,Ni,betai)
         call extend_nonuniform_both_safe(g_ind,g_mom,twtop,twbot,Nwtop,Nwbot) ! Extend to wall
         ! call checkGrid(g_mom)
         ! call checkGrid(g_ind)
       end subroutine

       subroutine makeGrids_BCs(g_mom,g_ind,Ni,Nwtop,Nwbot)
         implicit none
         type(grid),intent(inout) :: g_mom,g_ind
         integer,dimension(3),intent(in) :: Ni,Nwtop,Nwbot
         real(cp),dimension(3) :: hmin,hmax,betai
         real(cp),dimension(3) :: twtop,twbot
         integer,dimension(3) :: Nvactop,Nvacbot
         real(cp),dimension(3) :: tvactop,tvacbot
         type(grid) :: temp
         integer :: vacDist
         vacDist = 1
         select case (vacDist)
         case (1) ! Pseudo-vacuum
                  Nvactop = 10
                  Nvacbot = Nvactop
                  Nvactop(2) = Nvactop(2) + 6
                  tvactop = 3.5_cp
                  tvacbot = tvactop
                  tvactop(2) = tvactop(2) + 0.5_cp
         case (2) ! Intermediate
                  Nvactop = 8
                  Nvacbot = Nvactop
                  Nvactop(2) = Nvactop(2) + 6
                  tvactop = 1.75_cp
                  tvacbot = tvactop
                  tvactop(2) = tvactop(2) + 0.25_cp
         case (3) ! Far (B = 0)
                  Nvactop = 0
                  Nvacbot = Nvactop
                  tvactop = 0.0_cp
                  tvacbot = tvactop
         case default
         stop 'Error: vacDist must = 1,2,3 in makeGrids in generateGrids.f90'
         end select
         call getBMCParams(hmin,hmax,betai,twtop,twbot)
         call cavity3D_nonUniform(g_mom,hmin,hmax,Ni,betai)
         ! call cavity3D_uniform(g_mom,hmin,hmax,Ni)
         call extend_nonuniform_both_safe(g_ind,g_mom,twtop,twbot,Nwtop,Nwbot) ! Extend to wall
         call init(temp,g_ind)
         call extend_nonuniform(g_ind,temp,tvactop,tvacbot,Nvactop,Nvacbot) ! Extend to vacuum
         call delete(temp)
       end subroutine

       subroutine getBMCParams(hmin,hmax,betai,twtop,twbot)
         implicit none
         real(cp),dimension(3),intent(inout) :: hmin,hmax
         real(cp),dimension(3),intent(inout) :: betai
         real(cp),dimension(3),intent(inout) :: twtop,twbot

         ! **************** BENCHMARK DEFINED GRIDDATA ********************
         ! Geometry for benchmarks
         select case (benchmarkCase)
         case (1); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (2); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (3); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         hmin(1) = 0.0_cp; hmax(1) = 30.0_cp
         case (4); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         hmin(1) = -0.5_cp; hmax(1) = 0.5_cp

         case (50); hmin = -0.5_cp; hmax = 0.5_cp ! for xyz
         case (51); hmin = -0.5_cp; hmax = 0.5_cp ! for xyz

         case (100); hmin = 0.0_cp; hmax = 1.0_cp ! for xyz
         case (101); hmin = 0.0_cp; hmax = 1.0_cp ! for xyz

         case (102); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (103); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (104); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (105); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (106); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (107); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (108); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (109); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz

         case (200); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 40.0_cp
         case (201); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 20.0_cp
         case (202); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 40.0_cp

         case (250); hmin = -1.0_cp; hmax = 1.0_cp
         hmin(1) = 0.0_cp; hmax(1) = 25.0_cp

         case (300); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         case (301); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz

         case (1001); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz

         case (1002); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         hmin(1) = 0.0_cp; hmax(1) = 10.0_cp

         case (1003); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         ! hmin(1) = -0.5_cp; hmax(1) = 0.5_cp
         hmin(3) = -0.5_cp; hmax(3) = 0.5_cp
         ! hmin(1) = real(-10.0,cp); hmax(1) = 10.0_cp
         ! hmin(1) = real(-5.0,cp); hmax(1) = real(5.0,cp)

         case (1004); hmin = 0.0_cp; hmax = 1.0_cp ! for xyz

         case (1005); hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         hmin(1) = real(-10.0,cp); hmax(1) = 10.0_cp

         ! case (1006); hmin = -real(0.25,cp); hmax = real(0.25,cp) ! Isolated Eddy
         ! hmin(3) = -0.5_cp; hmax(3) = 0.5_cp          ! Isolated Eddy

         case (1006); hmin = -0.5_cp; hmax = 0.5_cp ! Single Eddy
         ! hmin(1) = -1.0_cp; hmax(1) = 1.0_cp ! Extend for true periodicity (2 eddies)
         hmin(1) = -2.0_cp; hmax(1) = 2.0_cp ! Extend for true periodicity (4 eddies)

         case (1007); hmin = -2.0_cp; hmax = 2.0_cp ! Parker - Cylinder
         hmin(3) = -0.5_cp; hmax(3) = 0.5_cp

         case (1008) ! Bandaru
         hmin(1) = 0.0_cp; hmax(1) = 2.0_cp*PI
         hmin(2) = -0.5_cp; hmax(2) = 0.5_cp
         hmin(3) = -1.0_cp; hmax(3) = 1.0_cp
         case (1009) ! Kawczynski
         hmin = -0.5_cp; hmax = 0.5_cp ! for xyz
         case (1010) ! Kawczynski
         hmin = -1.0_cp; hmax = 1.0_cp ! for xyz

         case (1011) ! Kawczynski
         hmin = -1.0_cp; hmax = 1.0_cp ! for xyz
         hmin(1) = -0.5_cp; hmax(1) = 0.5_cp ! for xyz

         case (1012) ! Pattison
         hmin = 0.0_cp; hmax = 1.0_cp

         case (1013) ! LDC using FFT
         hmin = 0.0_cp; hmax = 1.0_cp

         case default
           write(*,*) 'Incorrect benchmarkCase in initGriddata';stop
         end select

         ! Grid stretching for benchmarks
         select case (benchmarkCase)
         case (1); betai = 1000.0_cp
         case (2); betai = 100.0_cp
         case (3); betai = 1000.0_cp
         case (4); betai = 1000.0_cp

         case (50); betai = 1.05_cp
         case (51); betai = 1.05_cp

         case (100); betai = 1000.0_cp
         case (101); betai = 1.01_cp 
         case (102); betai = 100.0_cp
         case (103); betai = 1.004_cp
         case (104); betai = 1.0002_cp

         case (105); betai = 1.0d6
         case (106); betai = 1.05_cp
         case (107); betai = 1.03_cp
         case (108); betai = 1.01_cp

         case (109); betai = 1.04_cp

         case (200); betai = 1.05_cp; betai(1) = 1000.0_cp
         case (201); betai = 1.01_cp; betai(1) = 1000.0_cp
         case (202); betai = 1.001_cp; betai(1) = 1000.0_cp

         ! case (250); betai = 1.00008_cp; betai(1) = 1000.0_cp
         ! case (250); betai = 1.0001_cp; betai(1) = 1000.0_cp
         case (250); betai = 1.001_cp; betai(1) = 1000.0_cp

         case (300); betai = 1000.0_cp
         case (301); betai = 1000.0_cp

         ! case (1001); betai = 1.025_cp ! Ha = 10
         ! case (1001); betai = 1.005_cp ! Ha = 100
         case (1001); betai = 1.0005_cp ! Ha = 1000

         case (1002); betai = 1.01_cp
         ! betai = hartmannBL(Ha,hmin,hmax)

         ! betai = hartmannBL(Ha,hmin,hmax)
         betai(1) = 10000.0_cp

         case (1003); ! betai = 1.04_cp
                      ! betai(1) = 1.004_cp
                      betai = 1.1_cp
                      betai(1) = 100000.0_cp
                      betai = 100000.0_cp

         case (1004); betai = 100.0_cp

         case (1005); betai = 1.04_cp
                      betai(1) = 1.004_cp
         case (1006); betai = 100000_cp
         case (1007); betai = 100000_cp
         case (1008); betai = 100000_cp
         betai(3) = 1.1_cp

         case (1009); betai = 100000_cp
         case (1010)
         betai = hartmannBL(20.0_cp,hmin,hmax)
         case (1011)
         ! betai = hartmannBL(Ha,hmin,hmax)
         betai(1) = 10000.0_cp
         case (1012); betai = 1.01_cp
         case (1013); betai = 1.01_cp

         case default
           write(*,*) 'Incorrect benchmarkCase in setGriddata';stop
         end select

         ! tw for benchmarks:
         select case (benchmarkCase)
         case (1); twtop = 0.0_cp;      twbot = 0.0_cp
         case (2); twtop = 0.488888_cp;      twbot = 0.488888_cp
         case (3); twtop = 0.0_cp;      twbot = 0.0_cp
         case (4); twtop = 0.488888_cp;      twbot = 0.488888_cp
                   twtop(1) = 0.0_cp;        twbot(1) = 0.0_cp
                   ! twtop = 0.0_cp;        twbot = 0.0_cp

         case (50); twtop = 0.0_cp;      twbot = 0.0_cp
         case (51); twtop = 0.0_cp;      twbot = 0.0_cp

         case (100); twtop = 0.0_cp;      twbot = 0.0_cp
         case (101); twtop = 0.0_cp;      twbot = 0.0_cp
         case (102); twtop = 0.467_cp;    twbot = 0.467_cp
         case (103); twtop = 0.01539_cp;  twbot = 0.01539_cp
         case (104); twtop = 0.0005_cp;   twbot = 0.0005_cp

         case (105); twtop = 0.48888_cp;   twbot = 0.48888_cp
                     twtop(2) = 0.0_cp
         case (106); twtop = 0.48888_cp;   twbot = 0.48888_cp
                     twtop(2) = 0.0_cp
         case (107); twtop = 0.48888_cp;   twbot = 0.48888_cp
                     twtop(2) = 0.0_cp
         case (108); twtop = 0.48888_cp;   twbot = 0.48888_cp
                     twtop(2) = 0.0_cp

         case (109); twtop = 0.4_cp;    twbot = 0.4_cp
                     twtop(2) = 0.01_cp;  twbot(2) = 0.01_cp

         case (200); twtop = 0.0_cp;     twbot = 0.0_cp
         case (201); twtop = 0.1_cp;    twbot = 0.1_cp
                     twtop(1) = 0.0_cp;  twbot(1) = 0.0_cp
         case (202); twtop = 0.1_cp;    twbot = 0.1_cp
                     twtop(1) = 0.0_cp;  twbot(1) = 0.0_cp

         case (250); twtop = 0.142394_cp; twbot = 0.142394_cp
                     twtop(1) = 0.0_cp;  twbot(1) = 0.0_cp

         case (300); twtop = 0.0_cp;      twbot = 0.0_cp
         case (301); twtop = 0.0_cp;      twbot = 0.0_cp


         case (1001); twtop = 0.1_cp;   twbot = 0.1_cp
                     twtop(2) = 0.0_cp

         case (1002); twtop = 0.0_cp;   twbot = 0.0_cp
         ! case (1002); twtop = 0.0_cp;  twbot = 0.0_cp
         !              twtop(2) = 0.01_cp;  twbot(2) = 0.01_cp

         case (1003); twtop = 0.1_cp;   twbot = 0.1_cp
         ! twtop(1) = 0.0_cp;  twbot(1) = 0.0_cp
         twtop = 0.0_cp;  twbot = 0.0_cp

         case (1004); twtop = 0.0_cp;   twbot = 0.0_cp

         case (1005); twtop = 0.0_cp;   twbot = 0.0_cp

         ! case (1006); twtop = 0.25_cp;   twbot = 0.25_cp ! Isolated Eddy
         ! twtop(3) = 0.0_cp;   twbot(3) = 0.0_cp          ! Isolated Eddy
         case (1006); twtop = 0.0_cp;   twbot = 0.0_cp     ! Single Eddy
         ! twtop(2) = 0.1_cp;   twbot(2) = 0.1_cp          ! Single Eddy

         case (1007); twtop = 0.0_cp;   twbot = 0.0_cp     ! Parker - Cylinder

         case (1008); twtop = 0.0_cp;   twbot = 0.0_cp     ! Bandaru

         case (1009); twtop = 0.0_cp;   twbot = 0.0_cp     ! Kawczynski
         ! case (1010); twtop = 5.0_cp;   twbot = 5.0_cp     ! Kawczynski for sigma* = 0.01
         ! case (1010); twtop = (/0.5_cp,0.0_cp,0.5_cp/); twbot = 0.5_cp  ! Kawczynski for sigma* = 0.001
         case (1010); twtop = (/0.05_cp,0.0_cp,0.05_cp/); twbot = 0.05_cp  ! Kawczynski for sigma* = 0.001

         case (1011); twtop = 0.0_cp;   twbot = 0.0_cp     ! Kawczynski
         case (1012); twtop = 0.0_cp;   twbot = 0.0_cp     ! Pattison
         case (1013); twtop = 0.0_cp;  twbot = 0.0_cp    ! LDC using FFT

         case default
           stop 'Error: Incorrect benchmarkCase in setGriddata'
         end select
       end subroutine

       end module