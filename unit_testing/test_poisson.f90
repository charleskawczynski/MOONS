       module modelProblem_mod
       use grid_mod
       use BCs_mod
       use applyBCs_mod
       use ops_discrete_mod
       use ops_aux_mod
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
         integer :: modelProblem,MP,faceDir
         s = shape(f)

         modelProblem = 1
         ! 1 = Dirichlet, (N data)
         ! 2 = Dirichlet, (CC data)
         ! 3 = Neumann  , (N data)
         ! 4 = Neumann  , (CC data)
         faceDir = 1
         ! 5 = Dirichlet, (face data)
         ! 6 = Neumann  , (face data)

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
         case (5);
           call setAllZero(u_bcs,s(1),s(2),s(3),2)
           select case (faceDir)
           case(1); call setXMinType(u_bcs,1); call setXMaxType(u_bcs,1)
           case(2); call setYMinType(u_bcs,1); call setYMaxType(u_bcs,1)
           case(3); call setZMinType(u_bcs,1); call setZMaxType(u_bcs,1)
           case default
             stop 'Error: faceDir must = 1,2,3 in get_ModelProblem'
           end select
         
         case (6);
           call setAllZero(u_bcs,s(1),s(2),s(3),5)
           select case (faceDir)
           case(1); call setXMinType(u_bcs,4); call setXMaxType(u_bcs,4)
           case(2); call setYMinType(u_bcs,4); call setYMaxType(u_bcs,4)
           case(3); call setZMinType(u_bcs,4); call setZMaxType(u_bcs,4)
           case default
             stop 'Error: faceDir must = 1,2,3 in get_ModelProblem'
           end select
         
         case default; stop 'MP must = 1,2,3,4 in get_ModelProblem.'
         end select
         call setGrid(u_bcs,g)
         call checkBCs(u_bcs)

         select case (MP)
         case (1,2); p = (/real(3.0,cp),real(3.0,cp),real(3.0,cp)/)
         case (3,4); p = (/real(2.0,cp),real(2.0,cp),real(2.0,cp)/)
         case (5,6); p = (/real(2.0,cp),real(2.0,cp),real(2.0,cp)/)
         end select

         select case (MP)
         case (1)
         do k = 1,g%c(3)%sn; do j = 1,g%c(2)%sn; do i = 1,g%c(1)%sn
         u_exact(i,j,k) = dsin(p(1)*PI*g%c(1)%hn(i))*&
                          dsin(p(2)*PI*g%c(2)%hn(j))*&
                          dsin(p(3)*PI*g%c(3)%hn(k))
         enddo;enddo;enddo
         case (2)
         do k = 1,g%c(3)%sc; do j = 1,g%c(2)%sc; do i = 1,g%c(1)%sc
         u_exact(i,j,k) = dsin(p(1)*PI*g%c(1)%hc(i))*&
                          dsin(p(2)*PI*g%c(2)%hc(j))*&
                          dsin(p(3)*PI*g%c(3)%hc(k))
         enddo;enddo;enddo
         case (3)
         do k = 1,g%c(3)%sn; do j = 1,g%c(2)%sn; do i = 1,g%c(1)%sn
         u_exact(i,j,k) = dcos(p(1)*PI*g%c(1)%hn(i))*&
                          dcos(p(2)*PI*g%c(2)%hn(j))*&
                          dcos(p(3)*PI*g%c(3)%hn(k))
         enddo;enddo;enddo
         case (4)
         do k = 1,g%c(3)%sc; do j = 1,g%c(2)%sc; do i = 1,g%c(1)%sc
         u_exact(i,j,k) = dcos(p(1)*PI*g%c(1)%hc(i))*&
                          dcos(p(2)*PI*g%c(2)%hc(j))*&
                          dcos(p(3)*PI*g%c(3)%hc(k))
         enddo;enddo;enddo
         case (5)
         do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
         u_exact(i,j,k) = dcos(p(1)*PI*g%c(1)%hc(i))*&
                          dcos(p(2)*PI*g%c(2)%hc(j))*&
                          dcos(p(3)*PI*g%c(3)%hc(k))
         enddo;enddo;enddo
         case (6)
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

         call lap(f,u_exact,g)

         ! Important notes:

         select case (MP)
         case (1:4)
           if (g%c(1)%sn.eq.s(1)) then
             call applyAllBCs(u_bcs,f,g) ! physical boundary must be set for Dirichlet problems
                                         ! for node data (two values are defined)
           endif
         case (5,6)
           call applyAllBCs(u_bcs,f,g) ! physical boundary must be set for Dirichlet problems
         end select

         call zeroGhostPoints(f)        ! Necessary for BOTH Dirichlet problems AND Neumann

       end subroutine

       end module

       module test_poisson_mod
       use simParams_mod
       use IO_tools_mod
       use IO_scalarFields_mod
       use myTime_mod
       use grid_mod
       use myError_mod
       use ops_discrete_mod
       use ops_aux_mod
       use BCs_mod
       use applyBCs_mod

       use gridGen_mod
       use gridGenTools_mod
       use solverSettings_mod

       use myJacobi_mod
       use mySOR_mod
       use myADI_mod
       use myMG_mod
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

       contains

       subroutine test_poisson(dir)
         implicit none
         character(len=4) :: dir
         type(grid) :: g
         integer,dimension(3),parameter :: N = 2**5 ! Number of cells
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: s
         real(cp),dimension(3) :: dh
         type(myJacobi) :: Jacobi
         type(mySOR) :: SOR
         type(myADI) :: ADI
         type(multiGrid),dimension(3) :: MG
         type(BCs) :: u_bcs
         character(len=3) :: name
         type(myError) :: norm_res,norm_e
         type(solverSettings) :: ss
         type(gridGenerator) :: gg
         logical :: stretchGrid
         integer :: i,dataLoc,faceDir
         real(cp) :: tau,y_c
         ! Field quantities
         real(cp),dimension(:,:,:),allocatable :: u,u_exact,f,lapU,e,R

         write(*,*) 'Number of cells = ',N

         ! *************************************************************
         ! ****************** PARAMETERS TO DEFINE *********************
         ! *************************************************************
         stretchGrid = .false.
         dataLoc = 1 ! (1,2,3) = (n,cc,face)
         faceDir = 1 ! Only used if dataLoc = 3
         ! *************************************************************
         ! *************************************************************
         ! *************************************************************

         if (stretchGrid) then
           tau = real(5.0,cp); y_c = real(0.5,cp) ! y_c should match Bshift in sergey's fringe
           do i=1,3
             hmin = real(0.0,cp); hmax = real(1.0,cp)
             call init(gg,(/cluster(hmin(i),hmax(i),N(i),y_c,tau)/),i)
             call applyGhost(gg,i)
           enddo
         else
           do i=1,3
             hmin = real(0.0,cp); hmax = real(1.0,cp)
             dh = (hmax-hmin)/real(N,cp)
             call init(gg,(/uniform(hmin(i),hmax(i),N(i))/),i)
             call applyGhost(gg,i)
           enddo
         endif
         do i=1,3
           call init(g,gg%g%c(i)%hn,i,2) ! 2 means node values given
         enddo

         call export(g,dir,'g_base')
         ! stop 'Grid exported'

         call init(ss)
         call setName(ss,'Lap(u) = f          ')
         call setAlpha(ADI,real(1.0,cp))

         ! *************************************************************
         ! ****************** PARAMETERS TO DEFINE *********************
         ! *************************************************************
         select case (dataLoc)
         case (1); s = (/(g%c(i)%sn,i=1,3)/) ! Determines if N vs CC data is used
         case (2); s = (/(g%c(i)%sc,i=1,3)/) ! Determines if N vs CC data is used
         case (3); s = (/(g%c(i)%sc,i=1,3)/) ! Determines if N vs CC data is used
                   select case (faceDir)
                   case (1); s(1) = g%c(1)%sn
                   case (2); s(2) = g%c(2)%sn
                   case (3); s(3) = g%c(3)%sn
                   case default
                   stop 'Error: faceDir must = 1,2,3 in poisson.f90'
                   end select
         case default
         stop 'Error: dataLoc must = 1,2,3 in poisson.f90'
         end select
         call setDt(ADI,real(0.001,cp))
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

         call init(MG,s,u_bcs,g,ss,.true.)

         call writeToFile(g,u_exact,dir,'u_exact')
         call writeToFile(g,f,dir,'f')


         name = 'MMG' ! Uses SOR, so must be first
         if (allNeumann(u_bcs)) then
           ! call setMaxIterations(ss,500) ! Neumann
           call setMaxIterations(ss,300) ! Neumann
         else
           call setMaxIterations(ss,100) ! Dirichlet
         endif
         u = real(0.0,cp) ! Initial guess
         call myPoisson(MG,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
         call lap(lapU,u,g)
         write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         e = u - u_exact
         R = lapU - f
         call zeroGhostPoints(R)
         call compute(norm_e,u_exact,u)
         call print(norm_e,'u_'//name//' vs u_exact')
         call writeToFile(g,R,dir,'R_'//name)
         call writeToFile(g,u,dir,'u_'//name)
         call writeToFile(g,e,dir,'e_'//name)

         name = 'Jac'
         if (allNeumann(u_bcs)) then
           ! call setMaxIterations(ss,3000) ! Neumann, Node data
           call setMaxIterations(ss,5000) ! Neumann, CC data
         else
           call setMaxIterations(ss,4000) ! Dirichlet
         endif
         u = real(0.0,cp) ! Initial guess
         call myPoisson(Jacobi,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
         call lap(lapU,u,g)
         write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         e = u - u_exact
         R = lapU - f
         call zeroGhostPoints(R)
         call compute(norm_e,u_exact,u)
         call print(norm_e,'u_'//name//' vs u_exact')
         call writeToFile(g,R,dir,'R_'//name)
         call writeToFile(g,u,dir,'u_'//name)
         call writeToFile(g,e,dir,'e_'//name)

         name = 'SOR'
         if (allNeumann(u_bcs)) then
           ! call setMaxIterations(ss,3000) ! Neumann, Node data
           call setMaxIterations(ss,5000) ! Neumann, CC data
         else
           call setMaxIterations(ss,4000) ! Dirichlet
         endif
         u = real(0.0,cp) ! Initial guess
         call myPoisson(SOR,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
         call lap(lapU,u,g)
         write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         e = u - u_exact
         R = lapU - f
         call zeroGhostPoints(R)
         call compute(norm_e,u_exact,u)
         call print(norm_e,'u_'//name//' vs u_exact')
         call writeToFile(g,R,dir,'R_'//name)
         call writeToFile(g,u,dir,'u_'//name)
         call writeToFile(g,e,dir,'e_'//name)

         name = 'ADI'
         if (allNeumann(u_bcs)) then
           call setMaxIterations(ss,1000) ! Neumann
         else
           ! call setMaxIterations(ss,100) ! Dirichlet, testing
           ! call setMaxIterations(ss,500) ! Dirichlet, multi-scale timestep
           call setMaxIterations(ss,1000) ! Dirichlet, fixed timestep
         endif
         u = real(0.0,cp) ! Initial guess
         call myPoisson(ADI,u,f,u_bcs,g,ss,norm_res,.true.) ! 2 = cell corner data
         call lap(lapU,u,g)
         write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         e = u - u_exact
         R = lapU - f
         call zeroGhostPoints(R)
         call compute(norm_e,u_exact,u)
         call print(norm_e,'u_'//name//' vs u_exact')
         call writeToFile(g,R,dir,'R_'//name)
         call writeToFile(g,u,dir,'u_'//name)
         call writeToFile(g,e,dir,'e_'//name)

         call delete(g)
         call delete(MG)
         deallocate(e,u,f,u_exact,lapU,R)

       end subroutine

       end module
