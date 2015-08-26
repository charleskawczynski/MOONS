       module inductionSolver_mod
       use simParams_mod
       use SF_mod
       use VF_mod
       use ops_embedExtract_mod
       use grid_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use ops_physics_mod
       use applyBCs_mod
       use solverSettings_mod
       use SOR_mod
       ! use MG_mod

       implicit none

       private
       public :: induction,init,delete,solve
       public :: setDTime,setNmaxB,setNmaxCleanB

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
         type(VF) :: B,B0            ! CC data
         type(VF) :: temp_CC         ! CC data
         type(SF) :: phi,temp               ! CC data

         ! Solver settings
         type(solverSettings) :: ss_ind,ss_cleanB
         type(SORSolver) :: SOR_B,SOR_cleanB
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
       end type

       interface init;                 module procedure initInduction;                 end interface
       interface setPiGroups;          module procedure setPiGroupsInduction;          end interface
       interface delete;               module procedure deleteInduction;               end interface
       interface solve;                module procedure inductionSolver;               end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initInduction(ind,g,SD,NmaxB)
         implicit none
         type(induction),intent(inout) :: ind
         type(grid),intent(in) :: g
         integer,intent(in) :: NmaxB
         write(*,*) 'Initializing induction:'

         ind%g = g
         call init_CC(ind%B,g)
         call init_CC(ind%B0,g)
         call init_CC(ind%temp_CC,g)
         call init_CC(ind%temp,g)

         ! Initialize solver settings
         call init(ind%ss_ind)
         call setName(ind%ss_ind,'SS B equation       ')
         call setMaxIterations(ind%ss_ind,NmaxB)
         write(*,*) '     Solver settings for B initialized'

         write(*,*) '     Finished'
       end subroutine

       subroutine deleteInduction(ind)
         implicit none
         type(induction),intent(inout) :: ind
         call delete(ind%B)
         call delete(ind%B0)
         call delete(ind%temp_CC)
         call delete(ind%temp)
         call delete(ind%g)

         call delete(ind%SOR_B)
         ! if (cleanB) call delete(ind%MG)

         write(*,*) 'Induction object deleted'
       end subroutine


       subroutine lowRemPoisson_CC(ind,U,g,ss_MHD)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

         call poisson(ind%SOR_B,ind%B%x,ind%temp_CC%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%y,ind%temp_CC%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR_B,ind%B%z,ind%temp_CC%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       ! subroutine lowRemMultigrid(ind,U,g)
       !   implicit none
       !   type(induction),intent(inout) :: ind
       !   type(VF),intent(in) :: U
       !   type(grid),intent(in) :: g
       !   type(multiGrid),dimension(2) :: MG
       !   call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)
       !   call poisson(MG,ind%B%x,ind%temp_CC%x,ind%B_bcs%x,g,ind%ss_ind,&
       !   ind%err_residual,.false.)
       !   call poisson(MG,ind%B%y,ind%temp_CC%y,ind%B_bcs%y,g,ind%ss_ind,&
       !   ind%err_residual,.false.)
       !   call poisson(MG,ind%B%z,ind%temp_CC%z,ind%B_bcs%z,g,ind%ss_ind,&
       !   ind%err_residual,.false.)
       ! end subroutine


       end module