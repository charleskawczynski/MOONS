       module inductionSolver_mod
       use simParams_mod
       use IO_tools_mod
       use IO_auxiliary_mod
       use export_SF_mod
       use export_VF_mod
       use myTime_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use IO_SF_mod
       use IO_VF_mod

       use init_BBCs_mod
       use init_Bfield_mod
       use init_Sigma_mod
       use ops_embedExtract_mod

       use grid_mod
       use norms_mod
       use del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use ops_physics_mod
       use applyBCs_mod
       use solverSettings_mod

       use ind_methods_mod
       use monitor_mod

       implicit none

       private
       public :: induction,init,delete,solve

       public :: export
       public :: computeAddJCrossB,computeJCrossB
       public :: computeDivergence
       public :: embedVelocity

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type induction
         type(ind_CTM) :: CTM
         type(monitor) :: mB,mJ
         real(cp) :: dTime,Rem,Ha
         integer :: NmaxB
       end type

       interface init;                 module procedure initInduction;                 end interface
       interface setPiGroups;          module procedure setPiGroupsInduction;          end interface
       interface delete;               module procedure deleteInduction;               end interface
       interface solve;                module procedure inductionSolver;               end interface
       interface printExportBCs;       module procedure inductionPrintExportBCs;       end interface
       interface export;               module procedure inductionExport;               end interface
       interface exportRaw;            module procedure inductionExportRaw;            end interface
       interface exportTransient;      module procedure inductionExportTransient;      end interface
       interface exportTransientFull;  module procedure inductionExportTransientFull;  end interface
       interface computeDivergence;    module procedure computeDivergenceInduction;    end interface
       interface exportMaterial;       module procedure inductionExportMaterial;       end interface

       interface setDTime;             module procedure setDTimeInduction;             end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initInduction(ind,g,SD,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         character(len=*),intent(in) :: dir
         type(SF) :: sigma
         write(*,*) 'Initializing induction:'

         ind%g = g
         ind%SD = SD

         write(*,*) '     Fields allocated'

         call initBBCs(ind%B,g);
         write(*,*) '     BCs initialized'

         call initBfield(ind%B,ind%B0,g,dir)
         write(*,*) '     B-field initialized'

         call applyAllBCs(ind%B,g)
         write(*,*) '     BCs applied'

         call init_CC(sigma,g)
         call initSigma(sigma,ind%SD,g)
         call export_1C_SF(g,sigma,dir//'material/','sigmac',0)
         call init(ind%CTM,g,sigma)
         call delete(sigma)

         write(*,*) '     sig / solver initialized'


         call init(ind%mB,g,ind%B,dir//'Bfield/','B',restartB)
         call init(ind%mJ,g,ind%J,dir//'Jfield/','J',restartB)
         write(*,*) '     B/J monitors initialized'

         ! Initialize solver settings
         call init(ind%ss_ind)
         call setName(ind%ss_ind,'SS B equation       ')
         call setMaxIterations(ind%ss_ind,ind%NmaxB)
         write(*,*) '     Solver settings for B initialized'

         ! ********** SET CLEANING PROCEDURE SOLVER SETTINGS *************
         call init(ind%ss_cleanB)
         call setName(ind%ss_cleanB,'cleaning B          ')
         call setMaxIterations(ind%ss_cleanB,ind%NmaxCleanB)
         write(*,*) '     Solver settings for cleaning initialized'

         ! Initialize multigrid
         ! if (cleanB) call init(ind%MG,ind%phi%s,ind%phi_bcs,ind%g,ind%ss_cleanB,.false.)
         call inductionInfo(ind,newAndOpen(dir//'parameters/','info_ind'))

         if (restartB) then
         call readLastStepFromFile(ind%nstep,dir//'parameters/','n_ind')
         else; ind%nstep = 0
         endif
         ind%t = 0.0_cp
         ind%omega = 1.0_cp
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteInduction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%CTM)
         call delete(ind%mB)
         call delete(ind%mJ)
         call delete(ind%g)
         write(*,*) 'Induction object deleted'
       end subroutine

       subroutine setDTimeInduction(ind,dt)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: dt
         ind%dTime = dt
       end subroutine

       subroutine setNmaxB(ind,NmaxB)
         implicit none
         type(induction),intent(inout) :: ind
         integer,intent(in) :: NmaxB
         ind%NmaxB = NmaxB
       end subroutine

       subroutine setPiGroupsInduction(ind,Ha,Rem)
         implicit none
         type(induction),intent(inout) :: ind
         real(cp),intent(in) :: Ha,Rem
         ind%Ha = Ha; ind%Rem = Rem
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine inductionPrintExportBCs(ind,dir)
         implicit none
         type(induction),intent(in) :: ind
         character(len=*),intent(in) :: dir
         ! if (solveInduction) call printVectorBCs(ind%B%RF(1)%b,'Bx','By','Bz')
         ! if (cleanB)         call printAllBoundaries(ind%phi%RF(1)%b,'phi')
         ! if (solveInduction) call writeVectorBCs(ind%B_bcs,dir//'parameters/','Bx','By','Bz')
         ! if (cleanB)         call writeAllBoundaries(ind%phi%RF(1)%b,dir//'parameters/','phi')
       end subroutine

       subroutine inductionExportTransient(ind,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(solverSettings),intent(in) :: ss_MHD
         call export(ind%mB)
         call export(ind%mJ)
       end subroutine

       subroutine inductionExportRaw(ind,g,dir)
         implicit none
         type(induction),intent(in) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         if (restartB.and.(.not.solveInduction)) then
           ! This preserves the initial data
         else
           if (solveInduction) then
             write(*,*) 'Exporting RAW Solutions for B'
             call export_3C_VF(g,ind%B0   ,dir//'Bfield/','B0ct',0)
             call export_3C_VF(g,ind%B    ,dir//'Bfield/','Bct',0)
             call export_3C_VF(g,ind%J_cc ,dir//'Jfield/','Jct',0)
             call export_1C_SF(g,ind%sigma,dir//'material/','sigmac',0)
             call export_1C_SF(g,ind%sigma,dir//'Bfield/','divBct',0)
             call export_1C_SF(g,ind%sigma,dir//'Jfield/','divJct',0)
             write(*,*) '     finished'
           endif
         endif
       end subroutine

       subroutine inductionExport(ind,g,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         type(SF) :: tempN,tempCC
         type(VF) :: tempVFn,tempVFn2

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           write(*,*) 'Exporting PROCESSED solutions for B'

           ! B
           call init_Node(tempVFn,g)
           call init_Node(tempVFn2,g)
           call cellCenter2Node(tempVFn,ind%B,g,ind%temp_F,ind%temp_E)
           call export_3C_VF(g,tempVFn,dir//'Bfield/','Bnt',0)

           ! call cellCenter2Node(tempVFn,ind%B0,g,ind%temp_F,ind%temp_E)
           ! call export_3C_VF(g,tempVFn,dir//'Bfield/','B0nt',0)

           ! B0
           call cellCenter2Node(tempVFn,ind%B,g,ind%temp_F,ind%temp_E)
           call cellCenter2Node(tempVFn2,ind%B0,g,ind%temp_F,ind%temp_E)
           call add(tempVFn,tempVFn2)
           call export_3C_VF(g,tempVFn,dir//'Bfield/','Btotnt',0)

           ! J
           call cellCenter2Node(tempVFn,ind%J_cc,g,ind%temp_F,ind%temp_E)
           call export_3C_VF(g,tempVFn,dir//'Jfield/','Jtotnt_phys',0)
           call delete(tempVFn)
           call delete(tempVFn2)

           ! JxB
           ! call export_1C_SF(g,ind%jCrossB_F%x,dir//'Jfield/','jCrossB_Fx',0)
           ! call export_1C_SF(g,ind%jCrossB_F%y,dir//'Jfield/','jCrossB_Fy',0)
           ! call export_1C_SF(g,ind%jCrossB_F%z,dir//'Jfield/','jCrossB_Fz',0)

           ! U_induction
           call init_CC(tempCC,g)
           call div(tempCC,ind%U_cct,g)
           call export_3C_VF(g,ind%U_cct,dir//'Ufield/','Ucct',0)
           call export_1C_SF(g,tempCC,dir//'Ufield/','divUct',0)
           call delete(tempCC)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine inductionExportMaterial(ind,dir)
         implicit none
         type(induction),intent(inout) :: ind
         character(len=*),intent(in) :: dir
         type(SF) :: tempN
         if (solveInduction) then
           call init_Node(tempN,ind%g)
           call cellCenter2Node(tempN,ind%sigma,ind%g,ind%temp_F%x,ind%temp_E%z)
           call treatInterface(tempN)
           call export_1C_SF(ind%g,tempN,dir//'material/','sigman',0)
           call delete(tempN)
         endif
       end subroutine

       subroutine inductionInfo(ind,un)
         ! Use un = 6 to print to screen
         implicit none
         type(induction),intent(in) :: ind
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MAGNETIC **************************'
         write(un,*) '**************************************************************'
         write(un,*) '(Rem) = ',ind%Rem
         write(un,*) '(t,dt) = ',ind%t,ind%dTime
         write(un,*) ''
         call print(ind%g)
         write(un,*) ''
         call printPhysicalMinMax(ind%B,'B')
         call printPhysicalMinMax(ind%B0,'B0')
         call printPhysicalMinMax(ind%divB,'divB')
         call printPhysicalMinMax(ind%divJ,'divJ')
       end subroutine

       subroutine inductionExportTransientFull(ind,g,dir)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: dir
         type(VF) :: tempVFn,tempVFn2

         ! -------------------------- B/J FIELD AT NODES --------------------------
         if (solveInduction) then
           call init_Node(tempVFn,g)
           call init_Node(tempVFn2,g)

           ! call cellCenter2Node(tempVFn,ind%B,g)

           call cellCenter2Node(tempVFn,ind%B,g,ind%temp_F,ind%temp_E)
           call cellCenter2Node(tempVFn2,ind%B0,g,ind%temp_F,ind%temp_E)
           call add(tempVFn,tempVFn2)

           ! call cellCenter2Node(tempVFn,ind%J_cc,g)

           ! call cellCenter2Node(tempVFn,ind%J_cc,g)

           call delete(tempVFn)
           call delete(tempVFn2)
         endif
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine inductionSolver(ind,U,g_mom,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U
         type(grid),intent(in) :: g_mom
         type(solverSettings),intent(inout) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow
         ! ********************** LOCAL VARIABLES ***********************
         ! ind%B0%x = exp(-ind%omega*ind%t)
         ! ind%B0%y = exp(-ind%omega*ind%t)
         ! ind%B0%z = 1.0_cp

         ! ind%B0%x = exp(-ind%omega*ind%t)
         ! ind%B0%y = 0.0_cp
         ! ind%B0%z = 0.0_cp

         ! ind%B0%x = 0.0_cp
         ! ind%B0%y = 0.0_cp
         ! ind%B0%z = exp(-ind%omega*ind%t)


         call embedVelocity(ind,U,g_mom)
         call solve(CTM)

         ! call assign(ind%dB0dt,0.0_cp)
         ! call assignZ(ind%dB0dt,ind%omega*exp(-ind%omega*ind%t))

         select case (solveBMethod)
         ! case (1); call lowRemPoisson(ind,ind%U_cct,ind%g,ss_MHD)
         ! case (2); call lowRemPseudoTimeStepUniform(ind,ind%U_cct,ind%g)
         ! case (3); call lowRemPseudoTimeStep(ind,ind%U_cct,ind%g)
         case (4); call lowRemCTmethod(ind,ind%g)
         ! case (5); call finiteRemCTmethod(ind,ind%dB0dt,ind%g)
         case (5); call finiteRemCTmethod(ind,ind%g)
         ! case (6); call lowRem_ADI(ind,ind%U_cct,ind%g,ss_MHD)
         ! case (7); call lowRemMultigrid(ind,ind%U_cct,ind%g)
         end select

         if (cleanB) call cleanBSolution(ind,ind%g,ss_MHD)

         ind%nstep = ind%nstep + 1
         ind%t = ind%t + ind%dTime ! This only makes sense for finite Rem

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call computeCurrent(ind)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         ! call computeTotalMagneticEnergy(ind,ss_MHD)
         ! call computeTotalMagneticEnergyFluid(ind,g_mom,ss_MHD)
         call exportTransient(ind,ss_MHD)

         ! call inductionExportTransientFull(ind,ind%g,dir) ! VERY Expensive

         if (getExportErrors(ss_MHD)) call computeDivergence(ind,ind%g)
         if (getExportErrors(ss_MHD)) call exportTransientFull(ind,ind%g,dir)

         if (getPrintParams(ss_MHD)) then
           call inductionInfo(ind,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowB')
         else; exportNow = .false.
         endif

         if (getExportRawSolution(ss_MHD).or.exportNow) then
           call exportRaw(ind,ind%g,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(ind,ind%g,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowB')
         endif
       end subroutine


       ! ********************* COMPUTE *****************************

       subroutine computeAddJCrossB(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! addJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the applied or total mangetic
         ! field, depending on the solveBMethod.
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem
         type(VF) :: temp
         call init(temp,jcrossB)
         call assign(temp,0.0_cp)
         call computeJCrossB(temp,ind,g_mom,Ha,Re,Rem)
         call add(jcrossB,temp)
         call delete(temp)
       end subroutine

       subroutine computeJCrossB(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_Bface(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J,ind%B,ind%g)
           ! call edge2Face(ind%temp_F,ind%J,ind%g)

           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeJCrossB_new(jcrossB,ind,g_mom,Ha,Re,Rem)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g_mom
         real(cp),intent(in) :: Ha,Re,Rem

         ! Magnetic Pressure (not yet done)
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call square(ind%Bstar)
           call divide(ind%Bstar,2.0_cp)
           ! call grad(ind%jCrossB_F,ind%Bstar,ind%g)
           call multiply(ind%jCrossB_F,-1.0_cp)
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select

         ! Magnetic Stress (not yet done)
         select case (solveBMethod)
         case (5,6) ! Finite Rem
           call add(ind%Bstar,ind%B,ind%B0)
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%Bstar)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         case default ! Low Rem
           call curl(ind%J_cc,ind%B,ind%g)
           call cross(ind%temp_CC,ind%J_cc,ind%B0)
           call cellCenter2Face(ind%jCrossB_F,ind%temp_CC,ind%g)
           call extractFace(jcrossB,ind%jCrossB_F,ind%SD,g_mom)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         end select
       end subroutine

       subroutine computeDivergenceInduction(ind,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         if (solveInduction) then
           select case (solveBMethod)
           case (4:5)
             ! CT method enforces div(b) = 0, (result is in CC), 
             ! when computed from FACE-centered data:
             call div(ind%divB,ind%temp_F,g)
           case default
             call div(ind%divB,ind%B,g)
           end select
         endif
         call div(ind%divJ,ind%J_cc,g)
       end subroutine

       subroutine computeCurrent(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call add(ind%Bstar,ind%B0,ind%B)
         call curl(ind%J_cc,ind%Bstar,ind%g)
       end subroutine


       ! ********************* AUX *****************************

       subroutine embedVelocity(ind,U_E,g)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U_E ! Momentum edge velocity
         type(grid),intent(in) :: g ! Momentum grid
         call embedEdge(ind%U_E%x,U_E%x,ind%SD,g)
         call embedEdge(ind%U_E%y,U_E%y,ind%SD,g)
         call embedEdge(ind%U_E%z,U_E%z,ind%SD,g)
       end subroutine

       ! subroutine embedVelocity_old(ind,U_fi,g)
       !   implicit none
       !   type(induction),intent(inout) :: ind
       !   type(VF),intent(in) :: U_fi ! Raw momentum velocity
       !   type(grid),intent(in) :: g ! Momentum grid
       !   type(VF) :: temp
       !   logical,dimension(4) :: usedVelocity
       !   usedVelocity = (/.true.,.false.,.false.,.false./)
       !   if (usedVelocity(1)) then ! Edge - 2 interpolations
       !     call init_Edge(temp,g)
       !     call face2Edge(temp%x,U_fi%x,g,1,1)
       !     call face2Edge(temp%y,U_fi%x,g,1,2)
       !     call face2Edge(temp%z,U_fi%x,g,1,3)
       !     call embedEdge(ind%U_E,temp,ind%SD,g)
       !     call face2Edge(temp%x,U_fi%y,g,2,1)
       !     call face2Edge(temp%y,U_fi%y,g,2,2)
       !     call face2Edge(temp%z,U_fi%y,g,2,3)
       !     call embedEdge(ind%V_E,temp,ind%SD,g)
       !     call face2Edge(temp%x,U_fi%z,g,3,1)
       !     call face2Edge(temp%y,U_fi%z,g,3,2)
       !     call face2Edge(temp%z,U_fi%z,g,3,3)
       !     call embedEdge(ind%W_E,temp,ind%SD,g)
       !     call delete(temp)
       !     ! call printPhysicalMinMax(ind%U_E,'U_E')
       !     ! call printPhysicalMinMax(ind%V_E,'V_E')
       !     ! call printPhysicalMinMax(ind%W_E,'W_E')
       !   endif
       !   if (usedVelocity(2)) then ! CC - 1 interpolation
       !     call init_CC(temp,g)
       !     call face2CellCenter(temp%x,U_fi%x,g,1)
       !     call face2CellCenter(temp%y,U_fi%y,g,2)
       !     call face2CellCenter(temp%z,U_fi%z,g,3)
       !     call embedCC(ind%U_cct,temp,ind%SD,g)
       !     call delete(temp)
       !   endif
       !   if (usedVelocity(3)) then ! Face - no interpolations
       !     call embedFace(ind%U_Ft,U_Fi,ind%SD,g)
       !   endif
       !   ! if (usedVelocity(4)) then ! Node - 3 interpolations (not needed for any solvers)
       !     ! call allocateX(temp,g%c(1)%sc,g%c(2)%sn,g%c(3)%sn)
       !     ! call allocateY(temp,g%c(1)%sn,g%c(2)%sc,g%c(3)%sn)
       !     ! call allocateZ(temp,g%c(1)%sn,g%c(2)%sn,g%c(3)%sc)
       !     ! call face2Node(temp%x,U_fi%x,g,1)
       !     ! call face2Node(temp%y,U_fi%x,g,1)
       !     ! call face2Node(temp%z,U_fi%x,g,1)
       !     ! call embedNode(ind%U_N,temp,Nin1,Nin2,g)
       !     ! call face2Node(temp%x,U_fi%y,g,2)
       !     ! call face2Node(temp%y,U_fi%y,g,2)
       !     ! call face2Node(temp%z,U_fi%y,g,2)
       !     ! call embedNode(ind%V_N,temp,Nin1,Nin2,g)
       !     ! call face2Node(temp%x,U_fi%z,g,3)
       !     ! call face2Node(temp%y,U_fi%z,g,3)
       !     ! call face2Node(temp%z,U_fi%z,g,3)
       !     ! call embedNode(ind%W_N,temp,Nin1,Nin2,g)
       !     ! call delete(temp)
       !   ! endif
       ! end subroutine


       end module