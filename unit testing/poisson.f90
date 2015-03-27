       module modelProblem_mod
       use grid_mod
       use BCs_mod
       use applyBCs_mod
       use delOps_mod
       implicit none
       private

       public :: get_ModelProblem

       ! integer,parameter :: modelProblem = 1

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: PI = real(3.14159265358979,cp)

       contains

       subroutine get_ModelProblem(g,f,u_bcs,u_exact)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: u_exact,f
         type(BCs),intent(inout) :: u_bcs
         integer :: i,j,k
         real(cp),dimension(3) :: p
         integer,dimension(3) :: s
         integer :: modelProblem,MP
         s = shape(f)

         modelProblem = 4
         ! 1 = Dirichlet, (N data)  , performance(SOR,ADI,MG) = (perfect,perfect,)
         ! 2 = Dirichlet, (CC data) , performance(SOR,ADI,MG) = (perfect,perfect,)
         ! 3 = Neumann  , (N data)  , performance(SOR,ADI,MG) = (perfect,perfect,)
         ! 4 = Neumann  , (CC data) , performance(SOR,ADI,MG) = (perfect,perfect,)

         ! perfect = reaches machine accuracy

         MP = modelProblem

         select case(MP)
         case (1); call setAllZero(u_bcs,s(1),s(2),s(3),1)
         if (g%c(1)%sn.ne.s(1)) stop 'Mismatch in grid and function in get_ModelProblem.'
         case (2); call setAllZero(u_bcs,s(1),s(2),s(3),2)
         if (g%c(1)%sc.ne.s(1)) stop 'Mismatch in grid and function in get_ModelProblem.'
         case (3); call setAllZero(u_bcs,s(1),s(2),s(3),4) ! 3 = O(dh^2), 4 = O(dh)
         if (g%c(1)%sn.ne.s(1)) stop 'Mismatch in grid and function in get_ModelProblem.'
         case (4); call setAllZero(u_bcs,s(1),s(2),s(3),5)
         if (g%c(1)%sc.ne.s(1)) stop 'Mismatch in grid and function in get_ModelProblem.'
         case default; stop 'MP must = 1,2,3,4 in get_ModelProblem.'
         end select
         call setGrid(u_bcs,g)
         call checkBCs(u_bcs)

         select case (MP)
         case (1,2); p = (/real(3.0,cp),real(3.0,cp),real(3.0,cp)/)
         case (3,4); p = (/real(2.0,cp),real(2.0,cp),real(2.0,cp)/)
         end select

         select case (MP)
         case (1)
         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u_exact(i,j,k) = dsin(p(1)*PI*g%c(1)%hn(i))*&
                          dsin(p(2)*PI*g%c(2)%hn(j))*&
                          dsin(p(3)*PI*g%c(3)%hn(k))
         enddo;enddo;enddo
         case (2)
         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u_exact(i,j,k) = dsin(p(1)*PI*g%c(1)%hc(i))*&
                          dsin(p(2)*PI*g%c(2)%hc(j))*&
                          dsin(p(3)*PI*g%c(3)%hc(k))
         enddo;enddo;enddo
         case (3)
         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u_exact(i,j,k) = dcos(p(1)*PI*g%c(1)%hn(i))*&
                          dcos(p(2)*PI*g%c(2)%hn(j))*&
                          dcos(p(3)*PI*g%c(3)%hn(k))
         enddo;enddo;enddo
         case (4)
         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u_exact(i,j,k) = dcos(p(1)*PI*g%c(1)%hc(i))*&
                          dcos(p(2)*PI*g%c(2)%hc(j))*&
                          dcos(p(3)*PI*g%c(3)%hc(k))
         enddo;enddo;enddo
         end select

         if (allNeumann(u_bcs)) then
           ! Necessary BEFORE mean is subtracted
           call applyAllBCs(u_bcs,u_exact,g)
         endif

         u_exact = u_exact - sum(u_exact)/(max(1,size(u_exact)))

         if (.not.allNeumann(u_bcs)) then
           ! Necessary AFTER mean is subtracted
           call applyAllBCs(u_bcs,u_exact,g)
         endif

         if (g%c(1)%sc.ne.s(1)) then
              call CC2CCLap(f,u_exact,g)
         else;call myNodeLap(f,u_exact,g)
         endif

         ! Important notes:

         if (g%c(1)%sn.eq.s(1)) then
           call applyAllBCs(u_bcs,f,g) ! physical boundary must be set for Dirichlet problems
                                       ! for node data (two values are defined)
         endif
         call zeroGhostPoints(f)        ! Necessary for BOTH Dirichlet problems AND Neumann

       end subroutine

       end module

       program poisson_3D
       use simParams_mod
       use myIO_mod
       use myTime_mod
       use grid_mod
       use myError_mod
       use delOps_mod
       use BCs_mod
       use applyBCs_mod
       use mySOR_mod
       use myADI_mod
       use myMG_mod
       use gridGen_mod
       use gridGenTools_mod
       use solverSettings_mod
       use myPoisson_mod

       use modelProblem_mod

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
       type(grid) :: g
       integer,dimension(3),parameter :: N = 2**5 ! Number of cells
       real(cp),dimension(3) :: hmin,hmax
       integer,dimension(3) :: s
       integer :: i,Nt
       real(cp),dimension(3) :: dh
       type(myADI) :: ADI
       type(mySOR) :: SOR
       type(multiGrid),dimension(4) :: MG
       type(BCs) :: u_bcs
       character(len=3) :: name
       type(myError) :: norm_res,norm_e
       type(solverSettings) :: ss
       type(myTime) :: time
       character(len=4) :: dir = 'out\'
       type(gridGenerator) :: gg
       ! Field quantities
       real(cp),dimension(:,:,:),allocatable :: u,u_exact,f,lapU,e,R

       write(*,*) 'Number of cells = ',N

       hmin = real(0.0,cp); hmax = real(1.0,cp)
       dh = (hmax-hmin)/real(N,cp)
       call init(gg,(/uniform(hmin(1),hmax(1),N(1))/),1)
       call init(gg,(/uniform(hmin(2),hmax(2),N(2))/),2)
       call init(gg,(/uniform(hmin(3),hmax(3),N(3))/),3)
       call applyGhost(gg,1)
       call applyGhost(gg,2)
       call applyGhost(gg,3)
       call init(g,gg%g%c(1)%hn,1,2) ! 2 means node values given
       call init(g,gg%g%c(2)%hn,2,2) ! 2 means node values given
       call init(g,gg%g%c(3)%hn,3,2) ! 2 means node values given
       call export(g,dir,'g_base')

       call init(ss)
       call setName(ss,'Lap(u) = f          ')
       call setAlpha(ADI,real(1.0,cp))

       ! *************************************************************
       ! ****************** PARAMETERS TO DEFINE *********************
       ! *************************************************************
       ! s = g%c(1)%sn ! Determines wether cell corner or cell center data is used
       s = g%c(1)%sc ! Determines wether cell corner or cell center data is used
       ! *************************************************************
       ! *************************************************************
       ! *************************************************************

       write(*,*) 'System shape = ',s

       allocate(e(s(1),s(2),s(3)))
       allocate(u(s(1),s(2),s(3)))
       allocate(R(s(1),s(2),s(3)))
       allocate(u_exact(s(1),s(2),s(3)))
       allocate(f(s(1),s(2),s(3)))
       allocate(lapU(s(1),s(2),s(3)))

       call get_ModelProblem(g,f,u_bcs,u_exact)
       write(*,*) 'Model problem finished!'
       ! call init(mg,u_exact,f,u_bcs,g,ss,1,.true.)
       ! call testRP(mg,dir)

       if (s(1).eq.g%c(1)%sc) then
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,u_exact,dir,'u_exact')
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,f,dir,'f')
       else
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,u_exact,dir,'u_exact')
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,f,dir,'f')
       endif

       name = 'ADI'
       if (allNeumann(u_bcs)) then
         call setMaxIterations(ss,3000) ! Neumann
       else
         call setMaxIterations(ss,1000) ! Dirichlet
       endif
       u = real(0.0,cp) ! Initial guess
       call myPoisson(ADI,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
       if (g%c(1)%sc.ne.s(1)) then
            call CC2CCLap(lapU,u,g)
       else;call myNodeLap(lapU,u,g)
       endif
       write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
       write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
       e = u - u_exact
       R = lapU - f
       call zeroGhostPoints(R)
       call compute(norm_e,u_exact,u)
       call print(norm_e,'u_'//name//' vs u_exact')
       if (s(1).eq.g%c(1)%sc) then
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,R,dir,'R_'//name)
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,u,dir,'u_'//name)
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,e,dir,'e_'//name)
       else
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,R,dir,'R_'//name)
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,u,dir,'u_'//name)
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,e,dir,'e_'//name)
       endif

       if (s(1).eq.g%c(1)%sn) then
         name = 'MMG'
         if (allNeumann(u_bcs)) then
           call setMaxIterations(ss,500) ! Neumann
         else
           call setMaxIterations(ss,50) ! Dirichlet
         endif
         u = real(0.0,cp) ! Initial guess
         call myPoisson(MG,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
         if (g%c(1)%sc.ne.s(1)) then
              call CC2CCLap(lapU,u,g)
         else;call myNodeLap(lapU,u,g)
         endif
         write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         e = u - u_exact
         R = lapU - f
         call zeroGhostPoints(R)
         call compute(norm_e,u_exact,u)
         call print(norm_e,'u_'//name//' vs u_exact')
         if (s(1).eq.g%c(1)%sc) then
           call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,R,dir,'R_'//name)
           call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,u,dir,'u_'//name)
           call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,e,dir,'e_'//name)
         else
           call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,R,dir,'R_'//name)
           call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,u,dir,'u_'//name)
           call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,e,dir,'e_'//name)
         endif
       endif

       name = 'SOR'
       if (allNeumann(u_bcs)) then
         call setMaxIterations(ss,7000) ! Neumann
       else
         call setMaxIterations(ss,4000) ! Dirichlet
       endif
       u = real(0.0,cp) ! Initial guess
       call myPoisson(SOR,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
       if (g%c(1)%sc.ne.s(1)) then
            call CC2CCLap(lapU,u,g)
       else;call myNodeLap(lapU,u,g)
       endif
       write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
       write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
       e = u - u_exact
       R = lapU - f
       call zeroGhostPoints(R)
       call compute(norm_e,u_exact,u)
       call print(norm_e,'u_'//name//' vs u_exact')
       if (s(1).eq.g%c(1)%sc) then
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,R,dir,'R_'//name)
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,u,dir,'u_'//name)
         call writeToFile(g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,e,dir,'e_'//name)
       else
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,R,dir,'R_'//name)
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,u,dir,'u_'//name)
         call writeToFile(g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,e,dir,'e_'//name)
       endif

       call delete(g)
       deallocate(e,u,f,u_exact,lapU,R)
       end program
