       module ind_SOR_mod
       use simParams_mod
       use SF_mod
       use VF_mod
       use grid_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use ops_physics_mod
       use applyBCs_mod
       use solverSettings_mod
       use SOR_mod

       implicit none

       private
       public :: ind_SOR,init,delete,solve
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

       type ind_SOR
         type(VF) :: B,B0            ! CC data
         type(VF) :: temp_CC         ! CC data

         ! Solver settings
         type(solverSettings) :: ss_ind
         type(SORSolver) :: SOR
         integer :: NmaxB             ! Maximum number iterations in solving B (if iterative)
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
       end type

       interface init;                 module procedure initind_SOR;                 end interface
       interface setPiGroups;          module procedure setPiGroupsind_SOR;          end interface
       interface delete;               module procedure deleteind_SOR;               end interface
       interface solve;                module procedure ind_SORSolver;               end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine initind_SOR(ind,g,SD,NmaxB)
         implicit none
         type(ind_SOR),intent(inout) :: ind
         type(grid),intent(in) :: g
         integer,intent(in) :: NmaxB
         write(*,*) 'Initializing ind_SOR:'

         ind%g = g
         call init_CC(ind%B,g)
         call init_CC(ind%B0,g)
         call init_CC(ind%temp_CC,g)

         call init(ind%SOR,ind%B%RF(1)%s,g)

         ! Initialize solver settings
         call init(ind%ss_ind)
         call setName(ind%ss_ind,'SS B equation       ')
         call setMaxIterations(ind%ss_ind,NmaxB)
         write(*,*) '     Solver settings for B initialized'

         write(*,*) '     Finished'
       end subroutine

       subroutine deleteind_SOR(ind)
         implicit none
         type(ind_SOR),intent(inout) :: ind
         call delete(ind%B)
         call delete(ind%B0)
         call delete(ind%temp_CC)
         call delete(ind%g)

         call delete(ind%SOR)
         write(*,*) 'ind_SOR object deleted'
       end subroutine


       subroutine lowRemPoisson_CC(ind,U,g,ss_MHD)
         implicit none
         type(ind_SOR),intent(inout) :: ind
         type(VF),intent(in) :: U
         type(grid),intent(in) :: g
         type(solverSettings),intent(inout) :: ss_MHD

         call CCBfieldAdvect(ind%temp_CC,U,ind%B0,g)

         call poisson(ind%SOR,ind%B%x,ind%temp_CC%x,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR,ind%B%y,ind%temp_CC%y,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))

         call poisson(ind%SOR,ind%B%z,ind%temp_CC%z,g,ind%ss_ind,&
         ind%err_residual,getExportErrors(ss_MHD))
       end subroutine

       end module