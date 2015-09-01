       module modelProblem_mod
       use grid_mod
       use BCs_mod
       use SF_mod
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

       real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)

       contains

       subroutine defineBCs(u_bcs,g,s,bctype)
         implicit none
         type(BCs),intent(inout) :: u_bcs
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: bctype
         type(grid),intent(in) :: g
         integer :: i,WC,WI ! Wall coincident / Wall incoincident

         ! bctype = 1 ! Dirichlet
         !          2 ! Neumann

         select case (bctype)
         case (1); WC = 1; WI = 2
         case (2); WC = 4; WI = 5
         case default
         stop 'Error: bctype must = 1,2 in poisson.f90'
         end select

         if ( all( (/ (g%c(i)%sn.eq.s(i),i=1,3) /) ) ) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
         elseif (all((/(g%c(i)%sc.eq.s(i),i=1,3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setZminType(u_bcs,7)
           call setZmaxType(u_bcs,7)

         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setXminType(u_bcs,WC); call setXmaxType(u_bcs,WC)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setYminType(u_bcs,1); call setYmaxType(u_bcs,1)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WI)
           call setZminType(u_bcs,WC); call setZmaxType(u_bcs,WC)

         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
           call setXminType(u_bcs,WI); call setXmaxType(u_bcs,WI)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
           call setYminType(u_bcs,WI); call setYmaxType(u_bcs,WI)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call setAllZero(u_bcs,s(1),s(2),s(3),WC)
           call setZminType(u_bcs,WI); call setZmaxType(u_bcs,WI)
         else
          stop 'Error: Bad sizes in defineBCs in poisson.f90'
         endif

         call setGrid(u_bcs,g)
         call checkBCs(u_bcs)
       end subroutine

       subroutine defineFunction(u,x,y,z,bctype)
         implicit none
         type(SF),intent(inout) :: u
         real(cp),dimension(:),intent(in) :: x,y,z
         integer,intent(in) :: bctype
         integer :: i,j,k
         real(cp),dimension(3) :: p
         integer,dimension(3) :: s
         s = u%RF(1)%s

         select case (bctype)
         case (1); p = (/3.0_cp,3.0_cp,3.0_cp/)
         case (2); p = (/2.0_cp,2.0_cp,2.0_cp/)
         case default
         stop 'Error: bctype must = 1,2 in defineFunction in poisson.f90'
         end select

         if (bctype.eq.2) then
           do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
           u%RF(1)%f(i,j,k) = cos(p(1)*PI*x(i))*&
                      cos(p(2)*PI*y(j))*&
                      cos(p(3)*PI*z(k))
           enddo;enddo;enddo
!            do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
!            u%RF(1)%f(i,j,k) = cos(p(1)*PI*x(i))*&
!                               cos(p(2)*PI*y(j))
!            enddo;enddo;enddo
         elseif (bctype.eq.1) then
           do k = 1,s(3); do j = 1,s(2); do i = 1,s(1)
           u%RF(1)%f(i,j,k) = sin(p(1)*PI*x(i))*&
                              sin(p(2)*PI*y(j))*&
                              sin(p(3)*PI*z(k))
           enddo;enddo;enddo
         endif

         ! call zeroGhostPoints(u)        ! Necessary for BOTH Dirichlet problems AND Neumann
       end subroutine

       subroutine get_ModelProblem(g,f,u,u_exact)
         implicit none
         type(grid),intent(in) :: g
         type(SF),intent(inout) :: u,u_exact,f
         integer :: i
         integer,dimension(3) :: s
         integer :: bctype
         s = f%RF(1)%s

         ! bctype = 1 ! Dirichlet
         bctype = 2 ! Neumann
         call defineBCs(u%RF(1)%b,g,s,bctype)
         call defineBCs(u_exact%RF(1)%b,g,s,bctype)

         ! Node data
         if (all((/(g%c(i)%sn.eq.s(i),i=1,3)/))) then
           call defineFunction(u_exact,g%c(1)%hn,g%c(2)%hn,g%c(3)%hn,bctype)

           ! CC data
         elseif (all((/(g%c(i)%sc.eq.s(i),i=1,3)/))) then
           call defineFunction(u_exact,g%c(1)%hc,g%c(2)%hc,g%c(3)%hc,bctype)

           ! Face data
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call defineFunction(u_exact,g%c(1)%hn,g%c(2)%hc,g%c(3)%hc,bctype)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call defineFunction(u_exact,g%c(1)%hc,g%c(2)%hn,g%c(3)%hc,bctype)
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call defineFunction(u_exact,g%c(1)%hc,g%c(2)%hc,g%c(3)%hn,bctype)

           ! Edge data
         elseif (all((/g%c(1)%sc.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call defineFunction(u_exact,g%c(1)%hc,g%c(2)%hn,g%c(3)%hn,bctype)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sc.eq.s(2),g%c(3)%sn.eq.s(3)/))) then
           call defineFunction(u_exact,g%c(1)%hn,g%c(2)%hc,g%c(3)%hn,bctype)
         elseif (all((/g%c(1)%sn.eq.s(1),g%c(2)%sn.eq.s(2),g%c(3)%sc.eq.s(3)/))) then
           call defineFunction(u_exact,g%c(1)%hn,g%c(2)%hn,g%c(3)%hc,bctype)
         else
          stop 'Error: Bad sizes in defineBCs in poisson.f90'
         endif

         if (getAllNeumann(u%RF(1)%b)) then
           ! Necessary BEFORE mean is subtracted
           call applyAllBCs(u_exact,g)
         endif

         ! u_exact = u_exact - sum(u_exact)/(max(1,size(u_exact)))

         if (.not.getAllNeumann(u%RF(1)%b)) then
           ! Necessary AFTER mean is subtracted
           call applyAllBCs(u_exact,g)
         endif

         call lap(f,u_exact,g)
         ! call applyAllBCs(f,g)
         call applyAllBCs(u_exact,g)

         ! Important notes:

         if (g%c(1)%sn.eq.s(1)) then
           call applyAllBCs(f,g) ! physical boundary must be set for Dirichlet problems
                                       ! for node data (two values are defined)
         endif
         ! call zeroGhostPoints(f)        ! Necessary for BOTH Dirichlet problems AND Neumann
       end subroutine

       end module

       module test_poisson_mod
       use simParams_mod
       use IO_SF_mod
       use myTime_mod
       use grid_mod
       use norms_mod
       use ops_discrete_mod
       use BCs_mod
       use applyBCs_mod
       ! use PSE_mod
       ! use jacobi_mod
       use SOR_mod
       use SF_mod
       ! use ADI_mod
       ! use MG_mod
       use gridGen_mod
       use gridGenTools_mod
       use solverSettings_mod
       use poisson_mod
       use ops_aux_mod
       use dct_mod
       use idct_mod
       use fft_mod
       use FFT_poisson_mod

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
         character(len=*),intent(in) :: dir

         type(grid) :: g
         ! integer,dimension(3) :: N = (/3,4,1/) ! Number of cells
         ! integer,dimension(3) :: N = (/5,5,1/) ! Number of cells
         ! integer,dimension(3) :: N = (/40,40,1/) ! Number of cells
         integer,dimension(3) :: N = (/40,40,40/) ! Number of cells
         real(cp),dimension(3) :: hmin,hmax
         integer,dimension(3) :: s
         integer :: i,dataType
         real(cp),dimension(3) :: dh
         ! type(PseudoTimeSolver) :: PSE
         type(SORSolver) :: SOR
         type(CGSolver) :: CG
         ! type(myADI) :: ADI
         ! type(FFTSolver) :: FT
         ! type(Jacobi) :: JAC
         ! type(multiGrid),dimension(4) :: MG
         ! type(BCs) :: u_bcs
         logical,dimension(6) :: TF ! PseudoTime,jacobi,SOR,ADI,MG,FFT
         character(len=3) :: name
         type(norms) :: norm_res,norm_e
         type(solverSettings) :: ss
         type(gridGenerator) :: gg
         ! complex(cp),dimension(10) :: t
         ! real(cp),dimension(10) :: tR
         ! Field quantities
         type(SF) :: u,u_exact,f,lapU,e,R

         write(*,*) 'Number of cells = ',N
         TF = .false.
         ! TF(4) = .true.
         TF(3) = .true.
         TF(6) = .true.
         hmin = 0.0_cp; hmax = 1.0_cp
         hmin(3) = -0.5_cp; hmax(3) = 0.5_cp
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
         call init_stencils(g)
         call export(g,dir,'g_base')

         call init(ss)
         call setName(ss,'Lap(u) = f          ')

         write(*,*) 'dh = ',g%c(1)%dhn(1)
         write(*,*) 'xrange = ',g%c(1)%dhc(1),g%c(1)%hc(g%c(1)%sc)
         write(*,*) 'yrange = ',g%c(2)%dhc(1),g%c(2)%hc(g%c(2)%sc)
         ! *************************************************************
         ! ****************** PARAMETERS TO DEFINE *********************
         ! *************************************************************
         dataType = 1
         select case (dataType)
         case (1); s = (/(g%c(i)%sc,i=1,3)/) ! Cell centered data
         case (2); s = (/(g%c(i)%sn,i=1,3)/) ! Node data
         case (3); s = (/g%c(1)%sn,g%c(2)%sc,g%c(3)%sc/) ! Face data
                   s = (/g%c(1)%sc,g%c(2)%sn,g%c(3)%sc/) ! Face data
                   s = (/g%c(1)%sc,g%c(2)%sc,g%c(3)%sn/) ! Face data
         case (4); s = (/g%c(1)%sc,g%c(2)%sn,g%c(3)%sn/) ! Edge data
                   s = (/g%c(1)%sn,g%c(2)%sc,g%c(3)%sn/) ! Edge data
                   s = (/g%c(1)%sn,g%c(2)%sn,g%c(3)%sc/) ! Edge data
         case default
         stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         end select
         ! *************************************************************
         ! *************************************************************
         ! *************************************************************

         call init_CC(e,g)
         call init_CC(u,g)
         call init_CC(R,g)
         call init_CC(u_exact,g)
         call init_CC(f,g)
         call init_CC(lapU,g)

         call get_ModelProblem(g,f,u,u_exact)
         write(*,*) 'Model problem finished!'

         call export_1C_SF(g,u_exact,dir,'u_exact',0)
         call export_1C_SF(g,f,dir,'f',0)


         ! write(*,*) 'x = ',g%c(1)%hc
         ! write(*,*) 'y = ',g%c(2)%hc
         write(*,*) 'System shape = ',s
         ! write(*,*) 'All neumann = ',allNeumann(u_bcs)
         ! stop 'Done'

         ! *************************************************************
         ! *************************************************************
         ! *************************************************************

         ! if (TF(5)) then ! FFT
         !   name = 'FFT'
         !   u = 0.0_cp ! Initial guess
         !   call poisson(FT,u,f,u_bcs,g,ss,norm_res,.true.,3)
         !   call lap(lapU,u,g)
         !   write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         !   write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         !   call subtract(e,u,u_exact)
         !   call subtract(R,lapU,f)
         !   call zeroGhostPoints(R)
         !   call compute(norm_e,u_exact,u)
         !   call print(norm_e,'u_'//name//' vs u_exact')
         !   call writeToFile(g,R,dir,'R_'//name)
         !   call writeToFile(g,u,dir,'u_'//name)
         !   call writeToFile(g,e,dir,'e_'//name)
         ! endif

         ! if (TF(4)) then ! ADI
         !   call setAlpha(ADI,1.0_cp)
         !   if (allNeumann(u_bcs)) then; call setDt(ADI,0.01_cp) ! Neumann
         !   else;                        call setDt(ADI,0.001_cp)  ! Dirichlet
         !   endif
         !   name = 'ADI'
         !   if (allNeumann(u_bcs)) then
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,5000) ! Cell centered data
         !     case (2); call setMaxIterations(ss,4500) ! Node data
         !     case (3); call setMaxIterations(ss,5000) ! Face data
         !     case (4); call setMaxIterations(ss,5000) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   else
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,1500) ! Cell centered data
         !     case (2); call setMaxIterations(ss,1500) ! Node data
         !     case (3); call setMaxIterations(ss,1500) ! Face data
         !     case (4); call setMaxIterations(ss,1500) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   endif
         !   call assign(u,0.0_cp)
         !   call poisson(ADI,u,f,u_bcs,g,ss,norm_res,.true.)
         !   call lap(lapU,u,g)
         !   write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         !   write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         !   call subtract(e,u,u_exact)
         !   call subtract(R,lapU,f)
         !   call zeroGhostPoints(R)
         !   call compute(norm_e,u_exact,u)
         !   call print(norm_e,'u_'//name//' vs u_exact')
         !   call writeToFile(g,R,dir,'R_'//name)
         !   call writeToFile(g,u,dir,'u_'//name)
         !   call writeToFile(g,e,dir,'e_'//name)
         !   call init(mg,s,u_bcs,g,ss,.true.)
         !   call setIterationsPerLevel(mg,5)
         !   call setIterationsAtMaxLevel(mg,100)
         ! endif

         ! if (TF(5)) then ! MG
         !   name = 'MMG'
         !   if (allNeumann(u_bcs)) then
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,200) ! Cell centered data
         !     case (2); call setMaxIterations(ss,200) ! Node data
         !     case (3); call setMaxIterations(ss,200) ! Face data
         !     case (4); call setMaxIterations(ss,200) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   else
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,80) ! Cell centered data
         !     case (2); call setMaxIterations(ss,80) ! Node data
         !     case (3); call setMaxIterations(ss,80) ! Face data
         !     case (4); call setMaxIterations(ss,80) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   endif
         !   call assign(u,0.0_cp)
         !   call poisson(MG,u,f,u_bcs,g,ss,norm_res,.true.)
         !   call lap(lapU,u,g)
         !   write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         !   write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         !   call subtract(e,u,u_exact)
         !   call subtract(R,lapU,f)
         !   call zeroGhostPoints(R)
         !   call compute(norm_e,u_exact,u)
         !   call print(norm_e,'u_'//name//' vs u_exact')
         !   call writeToFile(g,R,dir,'R_'//name)
         !   call writeToFile(g,u,dir,'u_'//name)
         !   call writeToFile(g,e,dir,'e_'//name)
         ! endif

         if (TF(3)) then ! SOR
           name = 'SOR'
           if (getAllNeumann(u%RF(1)%b)) then
             select case (dataType)
             case (1); call setMaxIterations(ss,5000) ! Cell centered data
             case (2); call setMaxIterations(ss,2500) ! Node data
             case (3); call setMaxIterations(ss,5000) ! Face data
             case (4); call setMaxIterations(ss,5000) ! Edge data
             case default
             stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
             end select
           else
             select case (dataType)
             case (1); call setMaxIterations(ss,3500) ! Cell centered data
             case (2); call setMaxIterations(ss,3500) ! Node data
             case (3); call setMaxIterations(ss,3500) ! Face data
             case (4); call setMaxIterations(ss,3500) ! Edge data
             case default
             stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
             end select
           endif
           call assign(u,0.0_cp)
           call init(SOR,u%RF(1)%s,g)
           call poisson(SOR,u,f,g,ss,norm_res,.true.)
           call delete(SOR)
           call lap(lapU,u,g)
           call subtract(e,u,u_exact)
           call subtract(R,lapU,f)
           call zeroGhostPoints(R)
           call export_1C_SF(g,R,dir,'R_'//name,0)
           call export_1C_SF(g,u,dir,'u_'//name,0)
           call export_1C_SF(g,e,dir,'e_'//name,0)
           call subtract(u,u_exact)
           call compute(norm_e,u,g)
           call print(norm_e,'u_'//name//' vs u_exact')
         endif

         ! if (TF(2)) then ! JAC
         !   name = 'JAC'
         !   if (allNeumann(u_bcs)) then
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,2500) ! Cell centered data
         !     case (2); call setMaxIterations(ss,2500) ! Node data
         !     case (3); call setMaxIterations(ss,2500) ! Face data
         !     case (4); call setMaxIterations(ss,2500) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   else
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,7000) ! Cell centered data
         !     case (2); call setMaxIterations(ss,7000) ! Node data
         !     case (3); call setMaxIterations(ss,7000) ! Face data
         !     case (4); call setMaxIterations(ss,7000) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   endif
         !   call assign(u,0.0_cp)
         !   call poisson(JAC,u,f,u_bcs,g,ss,norm_res,.true.)
         !   call lap(lapU,u,g)
         !   write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         !   write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         !   call subtract(e,u,u_exact)
         !   call subtract(R,lapU,f)
         !   call zeroGhostPoints(R)
         !   call compute(norm_e,u_exact,u)
         !   call print(norm_e,'u_'//name//' vs u_exact')
         !   call writeToFile(g,R,dir,'R_'//name)
         !   call writeToFile(g,u,dir,'u_'//name)
         !   call writeToFile(g,e,dir,'e_'//name)
         ! endif

         ! if (TF(1)) then ! PSE
         !   name = 'PSE'
         !   call setTimeStep(PSE,0.0001_cp)
         !   if (allNeumann(u_bcs)) then
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,2500) ! Cell centered data
         !     case (2); call setMaxIterations(ss,2500) ! Node data
         !     case (3); call setMaxIterations(ss,2500) ! Face data
         !     case (4); call setMaxIterations(ss,2500) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   else
         !     select case (dataType)
         !     case (1); call setMaxIterations(ss,10000) ! Cell centered data
         !     case (2); call setMaxIterations(ss,7000) ! Node data
         !     case (3); call setMaxIterations(ss,7000) ! Face data
         !     case (4); call setMaxIterations(ss,7000) ! Edge data
         !     case default
         !     stop 'Error: dataType must = 1,2,3,4 in poisson.f90'
         !     end select
         !   endif
         !   call assign(u,0.0_cp)
         !   call poisson(PSE,u,f,u_bcs,g,ss,norm_res,.true.)
         !   call lap(lapU,u,g)
         !   write(*,*)  'mean u for '//name//' = ', abs(sum(u)/max(1,size(u)))
         !   write(*,*)  'mean u_exact for '//name//' = ', abs(sum(u_exact)/max(1,size(u_exact)))
         !   call subtract(e,u,u_exact)
         !   call subtract(R,lapU,f)
         !   call zeroGhostPoints(R)
         !   call compute(norm_e,u_exact,u)
         !   call print(norm_e,'u_'//name//' vs u_exact')
         !   call writeToFile(g,R,dir,'R_'//name)
         !   call writeToFile(g,u,dir,'u_'//name)
         !   call writeToFile(g,e,dir,'e_'//name)
         ! endif

         call delete(g)
         call delete(e)
         call delete(u)
         call delete(f)
         call delete(u_exact)
         call delete(lapU)
         call delete(R)
       end subroutine

       end module
