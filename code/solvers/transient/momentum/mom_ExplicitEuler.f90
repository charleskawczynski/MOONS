       module mom_explicitEuler_mod
       use simParams_mod
       use BCs_mod
       use grid_mod
       use SF_mod
       use VF_mod
       use TF_mod

       use norms_mod
       use del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod

       use applyBCs_mod

       use solverSettings_mod

       ! use jacobi_mod
       use FFT_poisson_mod
       use SOR_mod
       ! use ADI_mod
       ! use MG_mod
       use poisson_mod
       
       implicit none
       private
       
       public :: mom_explicitEuler,init,delete,solve

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type mom_explicitEuler
         ! Tensor Fields
         type(TF) :: U_E
         ! Vector Fields
         type(VF) :: U,Ustar
         type(VF) :: U_CC
         type(VF) :: temp_F
         type(VF) :: temp_E1,temp_E2
         type(SF) :: p,divU,temp_CC
         type(SF) :: Fo_grid,Co_grid,Re_grid
         type(SF) :: KE_adv,KE_diff,KE_pres,KE_transient,KE_jCrossB

         type(solverSettings) :: ss_mom,ss_ppe,ss_ADI
         ! type(multiGrid),dimension(3) :: MG
         ! type(jacobi) :: Jacobi_p
         type(SORSolver) :: SOR_p
         type(FFTSolver) :: FFT_p
         ! type(myADI) :: ADI_p,ADI_u
         type(probe) :: KU_energy

         ! Residuals
         type(norms) :: err_PPE,err_DivU,err_ADI
         type(norms),dimension(5) :: e_KE_terms

         ! Time step, Reynolds number, grid
         integer :: nstep,NmaxPPE
         real(cp) :: dTime,t
         real(cp) :: Re,Ha,Gr,Fr
         type(grid) :: g
         real(cp) :: L_eta,U_eta,t_eta ! Kolmogorov Scales

         ! Transient probes
         type(aveProbe) :: u_center,v_center,w_center
         type(errorProbe) :: transient_ppe,transient_divU
         type(avePlaneErrorProbe) :: u_symmetry
       end type

       interface init;                module procedure initmom_explicitEuler;               end interface
       interface setPiGroups;         module procedure setPiGroupsmom_explicitEuler;        end interface
       interface delete;              module procedure deletemom_explicitEuler;             end interface
       interface solve;               module procedure solvemom_explicitEulerEquation;      end interface
       interface export;              module procedure mom_explicitEulerExport;             end interface
       interface exportRaw;           module procedure mom_explicitEulerExportRaw;          end interface
       interface exportTransient;     module procedure mom_explicitEulerExportTransient;    end interface
       interface exportTransientFull; module procedure mom_explicitEulerExportTransientFull;end interface
       interface printExportBCs;      module procedure printExportmom_explicitEulerBCs;     end interface
       interface computeDivergence;   module procedure computeDivergencemom_explicitEuler;  end interface

       interface ZWCB;                module procedure ZWCB_RF;                    end interface
       interface ZWCB;                module procedure ZWCB_SF;                    end interface
       interface ZWCB;                module procedure ZWCB_VF;                    end interface

       interface setDTime;            module procedure setDTimemom_explicitEuler;           end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initmom_explicitEuler(mom,g,dir)
         implicit none
         type(mom_explicitEuler),intent(inout) :: mom
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing mom_explicitEuler:'

         mom%g = g

         ! Tensor Fields
         call init_Edge(mom%U_E,g)

         ! Vector Fields
         call init_Face(mom%U,g)

         call init_Face(mom%Ustar,g)
         call init_Face(mom%Unm1,g)
         call init_Face(mom%temp_F,g)

         call init_Edge(mom%temp_E1,g)
         call init_Edge(mom%temp_E2,g)

         ! Scalar Fields
         call init_CC(mom%p,g)
         call init_CC(mom%divU,g)
         call init_CC(mom%U_CC,g)
         call init_CC(mom%temp_CC,g)
         call init_CC(mom%Fo_grid,g)
         call init_CC(mom%Co_grid,g)
         call init_CC(mom%Re_grid,g)

         call init_CC(mom%KE_adv,g)
         call init_CC(mom%KE_diff,g)
         call init_CC(mom%KE_pres,g)
         call init_CC(mom%KE_transient,g)
         call init_CC(mom%KE_jCrossB,g)

         write(*,*) '     Fields allocated'
         ! Initialize U-field, P-field and all BCs
         call initUBCs(mom%U,mom%p,g)
         write(*,*) '     BCs initialized'

         ! Use mom%g later, for no just g
         call initUfield(mom%U,g,dir)
         call initPfield(mom%p,g,dir)
         write(*,*) '     Field initialized'

         write(*,*) '     BCs sizes set'

         if (solvemom_explicitEuler) call applyAllBCs(mom%U,g)
         if (solvemom_explicitEuler) call applyAllBCs(mom%p,g)
         write(*,*) '     BCs applied'

         call init(mom%err_DivU)
         call init(mom%err_PPE)

         call init(mom%u_center,dir//'Ufield/','transient_u',&
         .not.restartU,mom%U%x%RF(1)%s,(mom%U%x%RF(1)%s+1)/2,g,1)

         call init(mom%v_center,dir//'Ufield/','transient_v',&
         .not.restartU,mom%U%y%RF(1)%s,(mom%U%y%RF(1)%s+1)/2,g,2)

         call init(mom%w_center,dir//'Ufield/','transient_w',&
         .not.restartU,mom%U%z%RF(1)%s,(mom%U%z%RF(1)%s+1)/2,g,3)

         call init(mom%transient_divU,dir//'Ufield/','transient_divU',.not.restartU)
         call init(mom%transient_ppe,dir//'Ufield/','transient_ppe',.not.restartU)
         write(*,*) '     mom_explicitEuler probes initialized'

         call init(mom%u_symmetry,dir//'Ufield/','u_symmetry',&
         .not.restartU,mom%U%z%RF(1)%s,(mom%U%z%RF(1)%s+1)/2,g,3)
         write(*,*) '     mom_explicitEuler probes initialized'

         call export(mom%u_center)
         call export(mom%v_center)
         call export(mom%w_center)
         call export(mom%transient_ppe)
         call export(mom%transient_divU)
         call export(mom%u_symmetry)

         write(*,*) '     mom_explicitEuler probes initialized'


         ! Initialize interior solvers
         call init(mom%SOR_p,mom%p%RF(1)%s,mom%g)
         write(*,*) '     mom_explicitEuler SOR initialized'

         ! Initialize solver settings
         call init(mom%ss_ppe)
         call setName(mom%ss_ppe,'pressure poisson    ')
         call setMaxIterations(mom%ss_ppe,mom%NmaxPPE)
         ! call setSubtractMean(mom%ss_ppe)

         ! Init ADI ss
         ! call init(mom%ss_ADI)
         ! call setName(mom%ss_ADI,'mom_explicitEuler ADI        ')

         ! Init Multigrid solver
         ! call init(mom%MG,mom%p%s,mom%p_bcs,mom%g,mom%ss_ppe,.false.)
         ! call setMaxIterations(mom%ss_ADI,1) ! Not needed since apply() is used.

         ! call setMinTolerance(mom%ss_ppe,real(1.0**(-6.0),cp))
         ! call setMixedConditions(mom%ss_ppe)
         if (restartU) then
         call readLastStepFromFile(mom%nstep,dir//'parameters/','n_mom')
         else; mom%nstep = 0
         endif
         call init(mom%KU_energy,dir//'Ufield\','KU',.not.restartU)

         call mom_explicitEulerInfo(mom,newAndOpen(dir//'parameters/','info_mom'))
         mom%t = 0.0_cp
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine deletemom_explicitEuler(mom)
         implicit none
         type(mom_explicitEuler),intent(inout) :: mom
         call delete(mom%U_E)
         call delete(mom%U)
         call delete(mom%Unm1)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%p)
         call delete(mom%temp_CC)

         call delete(mom%divU)
         call delete(mom%U_CC)
         call delete(mom%Fo_grid)
         call delete(mom%Co_grid)
         call delete(mom%Re_grid)

         call delete(mom%KE_adv)
         call delete(mom%KE_diff)
         call delete(mom%KE_pres)
         call delete(mom%KE_transient)
         call delete(mom%KE_jCrossB)

         ! call delete(mom%u_center);
         call delete(mom%transient_ppe)
         call delete(mom%transient_divU);
         ! call delete(mom%u_symmetry)
         call delete(mom%temp_E1)
         call delete(mom%temp_E2)
         call delete(mom%g)

         call delete(mom%SOR_p)
         ! call delete(mom%MG)
         write(*,*) 'mom_explicitEuler object deleted'
       end subroutine

       subroutine explicitEuler(mom,F,g,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: Re,dt
         dt = mom%dTime
         Re = mom%Re

         ! Advection Terms -----------------------------------------
         select case (advectiveUFormulation) ! Explicit Euler
         case (1); call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,g)
         case (2); call faceAdvectNew(mom%temp_F,mom%U,mom%U,g)
         end select

         ! Ustar = -TempVF
         call assignMinus(mom%Ustar,mom%temp_F)

         ! Laplacian Terms -----------------------------------------
         call lap(mom%temp_F,mom%U,g)
         call divide(mom%temp_F,Re)
         call add(mom%Ustar,mom%temp_F)

         ! Source Terms (e.g. N j x B) -----------------------------
         call add(mom%Ustar,F)

         ! Zero wall coincident forcing (may be bad for neumann BCs)
         call ZWCB(mom%Ustar,g)

         ! Solve with explicit Euler --------------------
         ! Ustar = U + dt*Ustar
         call multiply(mom%Ustar,dt)
         call add(mom%Ustar,mom%U)

         ! Pressure Correction -------------------------------------
         call div(mom%temp_CC,mom%Ustar,g)
         ! Temp = Temp/dt
         call divide(mom%temp_CC,dt) ! O(dt) pressure treatment
         call zeroGhostPoints(mom%temp_CC)

         ! Solve lap(p) = div(U)/dt
         call poisson(mom%SOR_p,mom%p,mom%temp_CC,g,&
          mom%ss_ppe,mom%err_PPE,getExportErrors(ss_MHD))

         call grad(mom%temp_F,mom%p,g)
         ! call addMeanPressureGrad(mom%temp_F,real(52.0833,cp),1) ! Shercliff Flow
         ! call addMeanPressureGrad(mom%temp_F,real(1.0,cp),1) ! Bandaru

         ! U = Ustar - dt*dp/dx
         call multiply(mom%temp_F,dt)
         call subtract(mom%U,mom%Ustar,mom%temp_F)

         ! Apply BCs
         call applyAllBCs(mom%U,g)
       end subroutine

       subroutine addMeanPressureGrad(f,mpg,dir)
         implicit none
         type(VF),intent(inout) :: f
         real(cp),intent(in) :: mpg
         integer,intent(in) :: dir
         select case (dir)
         case (1); call subtract(f%x,mpg)
         case (2); call subtract(f%y,mpg)
         case (3); call subtract(f%z,mpg)
         case default
         stop 'Error: dir must = 1,2,3 in addMeanPressureGrad in mom_explicitEulerSolver.f90'
         end select
       end subroutine

       subroutine ZWCB_RF(f,s,g,dir)
         ! dir = zero wall coincident boundaries on...
         !       0: all faces
         !       1: x_min / x_max faces
         !       2: y_min / y_max faces
         !       3: z_min / z_max faces
         !      -1: all but x_min / x_max faces
         !      -2: all but y_min / y_max faces
         !      -3: all but z_min / z_max faces
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         integer,dimension(3),intent(in) :: s
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         select case (dir)
         case (0)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
             f(2,:,:) = 0.0_cp; f(s(1)-1,:,:) = 0.0_cp
           elseif (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
             f(:,2,:) = 0.0_cp; f(:,s(2)-1,:) = 0.0_cp
           elseif (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
             f(:,:,2) = 0.0_cp; f(:,:,s(3)-1) = 0.0_cp
           endif
         case (1)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
             f(2,:,:) = 0.0_cp; f(s(1)-1,:,:) = 0.0_cp
           endif
         case (2)
           if (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
             f(:,2,:) = 0.0_cp; f(:,s(2)-1,:) = 0.0_cp
           endif
         case (3)
           if (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
             f(:,:,2) = 0.0_cp; f(:,:,s(3)-1) = 0.0_cp
           endif
         case (-1)
           if (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
             f(:,2,:) = 0.0_cp; f(:,s(2)-1,:) = 0.0_cp
           endif
           if (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
             f(:,:,2) = 0.0_cp; f(:,:,s(3)-1) = 0.0_cp
           endif
         case (-2)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
             f(2,:,:) = 0.0_cp; f(s(1)-1,:,:) = 0.0_cp
           endif
           if (s(3).eq.g%c(3)%sn) then
             f(:,:,1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
             f(:,:,2) = 0.0_cp; f(:,:,s(3)-1) = 0.0_cp
           endif
         case (-3)
           if (s(1).eq.g%c(1)%sn) then
             f(1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
             f(2,:,:) = 0.0_cp; f(s(1)-1,:,:) = 0.0_cp
           endif
           if (s(2).eq.g%c(2)%sn) then
             f(:,1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
             f(:,2,:) = 0.0_cp; f(:,s(2)-1,:) = 0.0_cp
           endif
         case default
           stop 'Error: dir must = 0,1,2,3 in zeroWallCoincidentBoundaries'
         end select
       end subroutine

       subroutine ZWCB_SF(f,g)
         implicit none
         type(SF),intent(inout) :: f
         type(grid),intent(in) :: g
         integer :: i
         do i=1,f%s
           call ZWCB(f%RF(i)%f,f%RF(i)%s,g,0)
         enddo
       end subroutine

       subroutine ZWCB_VF(f,g)
         implicit none
         type(VF),intent(inout) :: f
         type(grid),intent(in) :: g
         call ZWCB(f%x,g); call ZWCB(f%y,g); call ZWCB(f%z,g)
       end subroutine

!        subroutine ZWCB_general(f,s,face)
!          ! face = zero wall coincident boundaries on...
!          !       1: x_min
!          !       2: x_max
!          !       3: y_min
!          !       4: y_max
!          !       5: z_min
!          !       6: z_max
!          implicit none
!          real(cp),dimension(:,:,:),intent(inout) :: f
!          integer,dimension(3),intent(in) :: s
!          integer,intent(in) :: face
!          select case (face)
!          case (1); f(2,:,:)      = 0.0_cp; f(1,:,:)    = 0.0_cp
!          case (2); f(s(1)-1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
!          case (3); f(:,2,:)      = 0.0_cp; f(:,1,:)    = 0.0_cp
!          case (4); f(:,s(2)-1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
!          case (5); f(:,:,2)      = 0.0_cp; f(:,:,1)    = 0.0_cp
!          case (6); f(:,:,s(3)-1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
!          case default
!            stop 'Error: face must = 1-6 in ZWCB'
!          end select
!        end subroutine

!        subroutine ZWCB_allDirichlet(f,s,face)
!          ! face = zero wall coincident boundaries on...
!          !       1: x_min,x_max
!          !       2: y_min,y_max
!          !       3: z_min,z_max
!          implicit none
!          real(cp),dimension(:,:,:),intent(inout) :: f
!          integer,dimension(3),intent(in) :: s
!          integer,intent(in) :: face
!          select case (face)
!          case (1); f(2,:,:)      = 0.0_cp; f(1,:,:)    = 0.0_cp
!                    f(s(1)-1,:,:) = 0.0_cp; f(s(1),:,:) = 0.0_cp
!          case (3); f(:,2,:)      = 0.0_cp; f(:,1,:)    = 0.0_cp
!                    f(:,s(2)-1,:) = 0.0_cp; f(:,s(2),:) = 0.0_cp
!          case (5); f(:,:,2)      = 0.0_cp; f(:,:,1)    = 0.0_cp
!                    f(:,:,s(3)-1) = 0.0_cp; f(:,:,s(3)) = 0.0_cp
!          case default
!            stop 'Error: face must = 1,2,3 in ZWCB_allDirichlet'
!          end select
!        end subroutine

!        subroutine ZWCB_VF(f) ! For all Dirichlet BCs
!          implicit none
!          type(VF),intent(inout) :: f
!          call ZWCB(f%x,f%sx,1)
!          call ZWCB(f%y,f%sy,2)
!          call ZWCB(f%z,f%sz,3)
!        end subroutine

!        subroutine ZWCB_VF_general(f,g,U_bcs) ! General
!          implicit none
!          type(VF),intent(inout) :: f
!          type(grid),intent(in) :: g
!          type(vectorBCs),intent(in) :: U_bcs
!          logical,dimension(3) :: TFall
!          TFall(1) = getAllDirichlet(U_bcs%x)
!          TFall(2) = getAllDirichlet(U_bcs%y)
!          TFall(3) = getAllDirichlet(U_bcs%z)
!          if (all(TFall)) then ! Prescribed velocity (i.e. no slip: viscosity doesn't play a role.)
!            call ZWCB_allDirichlet(f%x,f%sx,1)
!            call ZWCB_allDirichlet(f%y,f%sy,2)
!            call ZWCB_allDirichlet(f%z,f%sz,3)
!          else ! Potentially Neumann (d^2 u_normal / dx_tangent is not necessarily zero)
!            if (any((U_bcs%x%xminType).eq.(/1,2/))) call ZWCB_general(f%x,f%sx,1)
!            if (any((U_bcs%x%xmaxType).eq.(/1,2/))) call ZWCB_general(f%x,f%sx,2)

!            if (any((U_bcs%y%yminType).eq.(/1,2/))) call ZWCB_general(f%y,f%sy,3)
!            if (any((U_bcs%y%ymaxType).eq.(/1,2/))) call ZWCB_general(f%y,f%sy,4)

!            if (any((U_bcs%z%zminType).eq.(/1,2/))) call ZWCB_general(f%z,f%sz,5)
!            if (any((U_bcs%z%zmaxType).eq.(/1,2/))) call ZWCB_general(f%z,f%sz,6)
!          endif
!          call ZWCB_short(f%x,f%sx,g,1)
!          call ZWCB_short(f%y,f%sy,g,2)
!          call ZWCB_short(f%z,f%sz,g,3)
!        end subroutine

       end module