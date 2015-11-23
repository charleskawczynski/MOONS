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
         type(VF) :: U,Ustar,temp_F
         type(VF) :: U_CC
         type(VF) :: temp_E1,temp_E2
         type(SF) :: p,temp_CC

         type(solverSettings) :: ss_mom,ss_ppe
         type(SORSolver) :: SOR_p

         ! Time step, Reynolds number, grid
         integer :: nstep,NmaxPPE
         real(cp) :: dTime,t
         real(cp) :: Re,Ha

         type(monitor) :: m
       end type

       interface init;                module procedure initmom_explicitEuler;               end interface
       interface delete;              module procedure deletemom_explicitEuler;             end interface
       interface solve;               module procedure solvemom_explicitEulerEquation;      end interface

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

         call init_Edge(mom%U_E,g)

         call init_Face(mom%U,g)

         call init_Face(mom%Ustar,g)
         call init_Face(mom%temp_F,g)

         call init_Edge(mom%temp_E1,g)
         call init_Edge(mom%temp_E2,g)

         call init_CC(mom%p,g)
         call init_CC(mom%U_CC,g)
         call init_CC(mom%temp_CC,g)

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

         ! Initialize solver settings
         call init(mom%ss_ppe)
         call setName(mom%ss_ppe,'pressure poisson    ')
         call setMaxIterations(mom%ss_ppe,mom%NmaxPPE)

       end subroutine

       subroutine deletemom_explicitEuler(mom)
         implicit none
         type(mom_explicitEuler),intent(inout) :: mom
         call delete(mom%U)
         call delete(mom%U_E)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%U_CC)
         call delete(mom%temp_E1)
         call delete(mom%temp_E2)
         call delete(mom%g)
         call delete(mom%SOR_p)
         write(*,*) 'mom_explicitEuler object deleted'
       end subroutine

       subroutine explicitEuler(mom,F,g,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: Re,dt
         dt = mom%dTime; Re = mom%Re

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

       end module