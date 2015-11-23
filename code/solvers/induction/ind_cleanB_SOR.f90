       module ind_cleanB_SORSolver_mod
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

       implicit none

       private
       public :: ind_cleanB_SOR,init,delete,clean
       public :: setNmaxCleanB

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type ind_cleanB_SOR
         type(SF) :: phi,temp               ! CC data
         type(solverSettings) :: ss_cleanB
         type(SORSolver) :: SOR_cleanB
         type(norms) :: err_cleanB
         integer :: NmaxCleanB        ! Maximum number iterations to clean B
       end type

       interface init;        module procedure initind_cleanB_SOR;       end interface
       interface delete;      module procedure deleteind_cleanB_SOR;     end interface
       interface clean;       module procedure ind_cleanB_SORclean;      end interface

       contains

       subroutine initind_cleanB_SOR(ind,g)
         implicit none
         type(ind_cleanB_SOR),intent(inout) :: ind
         type(grid),intent(in) :: g
         write(*,*) 'Initializing ind_cleanB_SOR:'
         call init_CC(ind%phi,g)
         call init_CC(ind%temp,g)

         call init(ind%ss_cleanB)
         call setName(ind%ss_cleanB,'cleaning B          ')
         call setMaxIterations(ind%ss_cleanB,ind%NmaxCleanB)
         write(*,*) '     Finished'
       end subroutine

       subroutine deleteind_cleanB_SOR(ind)
         implicit none
         type(ind_cleanB_SOR),intent(inout) :: ind
         call delete(ind%temp)
         call delete(ind%phi)
         call delete(ind%SOR_cleanB)
         write(*,*) 'ind_cleanB_SOR object deleted'
       end subroutine

       subroutine ind_cleanB_SORclean(ind,B,divB,gradPhi,g,ss_MHD)
         implicit none
         type(ind_cleanB_SOR),intent(inout) :: ind
         type(VF),intent(inout) :: B,gradPhi
         type(SF),intent(inout) :: divB
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         call div(divB,B,g)
         call poisson(ind%SOR_cleanB,ind%phi,divB,g,ind%ss_cleanB,&
          ind%err_cleanB,getExportErrors(ss_MHD))
         call grad(gradPhi,ind%phi,g)
         call subtract(B,gradPhi)
       end subroutine

       end module