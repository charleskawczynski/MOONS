       module momentum_export_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod

       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod

       use norms_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_physics_mod

       use probe_base_mod
       use probe_transient_mod
       use probe_derived_mod
       
       implicit none
       private
       
       public :: export_raw

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

       subroutine export_SF_raw(m,x,dir,name)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         call export_3D_1C(m,x,dir,name,0)
       end subroutine

       subroutine export_VF_raw(m,x,dir,name)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         if (x%is_CC.or.x%is_Node) then
           call export_3D_3C(m,x,dir,name,0)
         elseif (x%is_Face.or.x%is_Edge) then
           call export_3D_1C(m,x,dir,name//'_x',0)
           call export_3D_1C(m,x,dir,name//'_y',0)
           call export_3D_1C(m,x,dir,name//'_z',0)
         endif
       end subroutine

       subroutine export_SF_raw(m,x,dir,name)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         type(SF) :: temp_1,temp_2,temp_N
         if (x%is_Node) then
           call export_3D_1C(m,x,dir,name,0)
         elseif (x%is_CC) then
         call init_Face(temp_1,m,1); call init_Edge(temp_2,m,2)
         call cellCenter2Node(temp_N,x,m,temp_1,temp_2)
         elseif (x%is_Face) then
         elseif (x%is_Edge) then
         endif
       end subroutine



       subroutine export_VF_processed(m,x,dir,name)
         implicit none
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: x
         character(len=*),intent(in) :: dir,name
         if (x%is_Node) then
           call export_3D_3C(m,x,dir,name,1)
         elseif (x%is_Face.or.x%is_Edge) then
           call export_3D_1C(m,x,dir,name//'_x',0)
           call export_3D_1C(m,x,dir,name//'_y',0)
           call export_3D_1C(m,x,dir,name//'_z',0)
         endif
       end subroutine

       subroutine momentumExportTransient(mom,ss_MHD,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir

         if (solveMomentum.and.(getExportTransient(ss_MHD))) then
           call apply(mom%u_center,mom%nstep,mom%U%x%RF(1)%f)
           call apply(mom%v_center,mom%nstep,mom%U%y%RF(1)%f)
           call apply(mom%w_center,mom%nstep,mom%U%z%RF(1)%f)
         endif

         if (solveMomentum.and.getExportErrors(ss_MHD)) then
           call set(mom%transient_ppe,mom%nstep,mom%err_PPE%L2)
           call apply(mom%transient_ppe)
           ! call export(mom%e_KE_terms,dir//'Ufield/energy/','e_KE_terms'//int2str(mom%nstep),1)
           call apply(mom%transient_divU,mom%nstep,mom%divU,mom%m)
           call apply(mom%u_symmetry,mom%nstep,mom%U%z)
         endif
       end subroutine

       subroutine export_fields(mom,m,F,dir)
         implicit none
         type(momentum),intent(in) :: mom
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: F
         character(len=*),intent(in) :: dir
         if (restartU.and.(.not.solveMomentum)) then
           ! This preserves the initial data
         else
           write(*,*) 'Exporting Solutions for U'
           call export_raw(m,mom%U,dir//'Ufield/','u')
           call export_raw(m,mom%p,dir//'Ufield/','p')
           ! call export_raw(m,F,dir//'Ufield/','jCrossB')
           call export_raw(m,mom%divU,dir//'Ufield/','divU')
           call export_processed(m,mom%U,dir//'Ufield/','u')
           call export_processed(m,mom%p,dir//'Ufield/','p')
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine momentumInfo(mom,un)
         ! Use un = 6 to print to screen
         implicit none
         type(momentum),intent(in) :: mom
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************** MOMENTUM **************************'
         write(un,*) '**************************************************************'
         write(un,*) '(Re,Ha) = ',mom%Re,mom%Ha
         write(un,*) '(Gr,Fr) = ',mom%Gr,mom%Fr
         write(un,*) '(t,dt) = ',mom%t,mom%dTime
         write(un,*) '(nstep) = ',mom%nstep
         write(un,*) 'Kolmogorov Length = ',mom%L_eta
         write(un,*) 'Kolmogorov Velocity = ',mom%U_eta
         write(un,*) 'Kolmogorov Time = ',mom%t_eta
         write(un,*) ''
         call export(mom%m,un)
         write(un,*) ''
         call printPhysicalMinMax(mom%U,'u')
         call printPhysicalMinMax(mom%p,'p')
         call printPhysicalMinMax(mom%divU,'divU')
         ! call printPhysicalMinMax(mom%Fo_grid,'Fo_grid')
         ! call printPhysicalMinMax(mom%Co_grid,'Co_grid')
         ! call printPhysicalMinMax(mom%Re_grid,'Re_grid')
         write(*,*) ''
       end subroutine

       subroutine momentumExportTransientFull(mom,m,dir)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(VF) :: tempNVF

         call init_Node(tempNVF,m)
         call face2Node(tempNVF,mom%U,m,mom%temp_E1)
         call export_2D_2C(mom%m,tempNVF,dir//'Ufield/transient/','Uni_phys',1,mom%nstep)
         call delete(tempNVF)
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solveMomentumEquation(mom,F,ss_MHD,dir)
         implicit none
         ! ********************** INPUT / OUTPUT ************************
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         type(solverSettings),intent(in) :: ss_MHD
         character(len=*),intent(in) :: dir
         logical :: exportNow

         select case(solveUMethod)
         case (1); call explicitEuler(mom,F,mom%m,ss_MHD)
         ! case (2); call semi_implicit_ADI(mom,F,mom%m,ss_MHD)
         case default
         stop 'Error: solveUMethod must = 1,2 in solveMomentumEquation.'
         end select
         mom%t = mom%t + mom%dTime
         mom%nstep = mom%nstep + 1

         ! ********************* POST SOLUTION COMPUTATIONS *********************
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge(mom%U_E,mom%U,mom%m,mom%temp_CC,mom%temp_F)

         ! ********************* POST SOLUTION PRINT/EXPORT *********************
         call computeTotalKineticEnergy(mom,ss_MHD)

         ! call computeKineticEnergy(mom,mom%m,F)
         ! call computeMomentumStability(mom,ss_MHD)
         if (getExportErrors(ss_MHD)) call computeDivergence(mom,mom%m)
         ! if (getExportErrors(ss_MHD)) call exportTransientFull(mom,mom%m,dir)
         if (getExportTransient(ss_MHD)) call exportTransient(mom,ss_MHD,dir)

         if (getPrintParams(ss_MHD)) then
           call momentumInfo(mom,6)
           exportNow = readSwitchFromFile(dir//'parameters/','exportNowU')
         else; exportNow = .false.
         endif

         if (getExportRawSolution(ss_MHD).or.exportNow) then
           call exportRaw(mom,mom%m,F,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowU')
         endif
         if (getExportSolution(ss_MHD).or.exportNow) then
           call export(mom,mom%m,dir)
           call writeSwitchToFile(.false.,dir//'parameters/','exportNowU')
         endif
       end subroutine


       ! ********************* COMPUTE **************************

!        subroutine computeKineticEnergy(mom,m,F)
!          implicit none
!          type(momentum),intent(inout) :: mom
!          type(mesh),intent(in) :: m
!          type(VF),intent(in) :: F
!          real(cp) :: Re,dt
!          dt = mom%dTime
!          Re = mom%Re

!          ! Advection Terms -----------------------------------------
!          select case (advectiveUFormulation)
!          case (1);call faceAdvectDonor(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,m)
!          case (2);call faceAdvect(mom%temp_F,mom%U,mom%U,m)
!          case (3);call faceAdvectHybrid(mom%temp_F,mom%U,mom%U,mom%temp_E1,mom%temp_E2,mom%U_CC,m)
!          end select

!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_adv,mom%U_CC)

!          ! Laplacian Terms -----------------------------------------
!          call lap(mom%temp_F,mom%U,m)
!          call divide(mom%temp_F,Re)
!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_diff,mom%U_CC)

!          ! Source Terms (e.m. N j x B) -----------------------------
!          call assign(mom%temp_F,F)
!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_jCrossB,mom%U_CC)

!          ! Solve with explicit Euler --------------------
!          call assign(mom%temp_F,mom%U)
!          call square(mom%temp_F)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_transient,mom%U_CC)

!          call assign(mom%temp_F,mom%Unm1)
!          call square(mom%temp_F)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_pres,mom%U_CC)
!          call subtract(mom%KE_transient,mom%KE_pres)

!          call divide(mom%KE_transient,2.0_cp*mom%dTime)

!          ! Pressure Correction -------------------------------------
!          call grad(mom%temp_F,mom%p,m)
!          call multiply(mom%temp_F,mom%U)
!          call face2CellCenter(mom%U_CC,mom%temp_F,m)
!          call sum(mom%KE_pres,mom%U_CC)
!          ! Norms
!          call compute(mom%e_KE_terms(1),mom%KE_adv)
!          call compute(mom%e_KE_terms(2),mom%KE_diff)
!          call compute(mom%e_KE_terms(3),mom%KE_jCrossB)
!          call compute(mom%e_KE_terms(4),mom%KE_pres)
!          call compute(mom%e_KE_terms(5),mom%KE_transient)
!        end subroutine

       subroutine computeTotalKineticEnergy(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
         real(cp) :: K_energy
         if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
          call totalEnergy(K_energy,mom%U_CC,mom%m)
          call set(mom%KU_energy,mom%nstep,K_energy)
          call apply(mom%KU_energy)
         endif
       end subroutine

       subroutine computeMomentumStability(mom,ss_MHD)
         implicit none
         type(momentum),intent(inout) :: mom
         type(solverSettings),intent(in) :: ss_MHD
          if (computeKU.and.getExportTransient(ss_MHD).or.mom%nstep.eq.0) then
           call face2CellCenter(mom%U_CC,mom%U,mom%m)

           call stabilityTerms(mom%Co_grid,mom%U_CC,mom%m,1)
           call multiply(mom%Co_grid,mom%dTime)

           call stabilityTerms(mom%Fo_grid,mom%U_CC,mom%m,2)
           call multiply(mom%Fo_grid,mom%dTime)

           call stabilityTerms(mom%Re_grid,mom%U_CC,mom%m,-1)
           call multiply(mom%Re_grid,mom%Re)
          endif
       end subroutine

       subroutine computeDivergenceMomentum(mom,m)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         call div(mom%divU,mom%U,m)
         call zeroGhostPoints(mom%divU)
       end subroutine

       ! ********************* AUX *****************************

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
         stop 'Error: dir must = 1,2,3 in addMeanPressureGrad in momentumSolver.f90'
         end select
       end subroutine

       end module