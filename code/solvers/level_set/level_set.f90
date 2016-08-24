       module level_set_mod
       use current_precision_mod
       
       use BCs_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use domain_mod
       use string_mod
       use path_mod
       use dir_tree_mod

       use level_set_solver_mod
       use init_LSBCs_mod
       use init_LSField_mod

       use IO_tools_mod
       use IO_auxiliary_mod
       use IO_SF_mod
       use IO_VF_mod
       use export_raw_processed_mod
       use print_export_mod

       use norms_mod
       use ops_norms_mod
       use ops_discrete_mod
       use ops_del_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_embedExtract_mod

       use apply_BCs_mod
       use apply_stitches_mod

       implicit none
       private
       
       public :: level_set,init,delete,solve
       public :: export,exportTransient

       type level_set
         ! phi is a scalar field, but it must live on the cell face (like sigma on the cell edge)
         type(SF) :: phi
         type(mesh) :: m
         integer :: N_PPE,N_LS
         real(cp) :: tol_PPE,tol_LS
         integer :: nstep
         real(cp) :: dTime,t
       end type

       interface init;                module procedure initlevel_set;               end interface
       interface delete;              module procedure deletelevel_set;             end interface
       interface solve;               module procedure solve_level_set;             end interface
       interface export;              module procedure export_level_set;            end interface
       interface exportTransient;     module procedure level_setExportTransient;    end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initlevel_set(LS,m,N_LS,tol_LS,dTime,DT)
         implicit none
         type(level_set),intent(inout) :: LS
         type(mesh),intent(in) :: m
         integer,intent(in) :: N_LS
         real(cp),intent(in) :: tol_LS
         real(cp),intent(in) :: dTime
         type(dir_tree),intent(in) :: DT
         integer :: temp_unit
         write(*,*) 'Initializing level_set:'

         LS%dTime = dTime
         LS%N_LS = N_LS
         LS%tol_LS = tol_LS

         call init(LS%m,m)
         call init_CC(LS%phi,m,0.0_cp)

         write(*,*) '     Fields allocated'

         call init_LSBCs(LS%phi,m)
         write(*,*) '     BCs initialized'
         if (solvelevel_set) call print_BCs(LS%phi,'LS')
         if (solvelevel_set) call export_BCs(LS%phi,str(DT%params),'LS')

         call init_LSfield(LS%phi,m,str(DT%LS))
         write(*,*) '     Field initialized'

         call apply_BCs(LS%phi,m)
         call apply_stitches(LS%phi,m)
         write(*,*) '     phi BCs applied'

         if (restartPhi) then
         call readLastStepFromFile(LS%nstep,str(DT%params),'nstep_LS')
         else; LS%nstep = 0
         endif

         temp_unit = newAndOpen(str(DT%params),'info_LS')
         call level_setInfo(LS,temp_unit)
         close(temp_unit)
         LS%t = 0.0_cp
         write(*,*) '     Solver settings initialized'
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine deletelevel_set(LS)
         implicit none
         type(level_set),intent(inout) :: LS
         call delete(LS%phi)
         write(*,*) 'level_set object deleted'
       end subroutine

       ! ******************* EXPORT ****************************

       subroutine level_setExportTransient(LS)
         implicit none
         type(level_set),intent(inout) :: LS
       end subroutine

       subroutine export_level_set(LS,m,F,DT)
         implicit none
         type(level_set),intent(in) :: LS
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: F
         type(dir_tree),intent(in) :: DT
         if (.not.(restartPhi.and.(.not.solvelevel_set))) then
           write(*,*) 'Exporting Solutions for U at LS%nstep = ',LS%nstep
           call export_processed(m,LS%phi,str(DT%LS),'phi',1)
           call export_raw(m,LS%phi,str(DT%LS),'phi',0)
           write(*,*) '     finished'
         endif
       end subroutine

       subroutine level_setInfo(LS,un) ! Use un = 6 to print to screen
         implicit none
         type(level_set),intent(in) :: LS
         integer,intent(in) :: un
         write(un,*) '**************************************************************'
         write(un,*) '************************* Level Set **************************'
         write(un,*) '**************************************************************'
         write(un,*) 't,dt = ',LS%t,LS%dTime
         write(un,*) 'solveLSMethod,N_LS = ',solveLSMethod,LS%N_LS
         write(un,*) 'tol_LS,tol_PPE = ',LS%tol_LS,LS%tol_PPE
         write(un,*) 'nstep = ',LS%nstep
         write(un,*) ''
         call export(LS%m,un)
         write(*,*) ''
       end subroutine

       ! ******************* SOLVER ****************************

       subroutine solve_level_set(LS,F,PE,DT)
         implicit none
         type(level_set),intent(inout) :: LS
         type(VF),intent(in) :: F
         type(print_export),intent(in) :: PE
         type(dir_tree),intent(in) :: DT
         logical :: exportNow,exportNowLS

         select case(solveLSMethod)
         case (1); call Euler_LS(LS%phi,U,m,dt,temp_F,temp_CC)
         case default; stop 'Error: solveLSMethod must = 1,2 in level_set.f90.'
         end select
         LS%t = LS%t + LS%dTime
         LS%nstep = LS%nstep + 1

         ! ********************* POST SOLUTION PRINT/EXPORT *********************

         if (PE%info) then
           call level_setInfo(LS,6)
           exportNow = readSwitchFromFile(str(DT%params),'exportNow')
           exportNowLS = readSwitchFromFile(str(DT%params),'exportNowLS')
           write(*,*) ''
         else; exportNow = .false.; exportNowLS = .false.
         endif

         if (PE%solution.or.exportNowLS.or.exportNow) then
           ! call curl(LS%temp_E,LS%U,m)
           call export(LS,LS%m,F,DT)
           call writeSwitchToFile(.false.,str(DT%params),'exportNowLS')
         endif
       end subroutine

       end module