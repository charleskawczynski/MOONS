       module clean_B_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use applyBCs_mod

       implicit none

       private
       public :: clean_B,init,delete,clean

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type clean_B
         type(SF) :: phi,divB
         type(VF) :: temp_F
       end type

       interface init;    module procedure init_clean_B;      end interface
       interface delete;  module procedure delete_clean_B;    end interface
       interface clean;   module procedure apply_cleaning;    end interface

       contains

       subroutine init_clean_B(CB,B,m)
         implicit none
         type(clean_B),intent(inout) :: CB
         type(VF),intent(in) :: B
         type(mesh),intent(in) :: m
         call init_CC(CB%phi,m)
         call init_CC(CB%divB,m)
         call init(CB%temp_F,B)
         call init_Neumann(CB%phi)
         call assign(CB%phi,0.0_cp)
         call init_BCs(CB%phi,0.0_cp)
       end subroutine

       subroutine delete_clean_B(CB)
         implicit none
         type(clean_B),intent(inout) :: CB
         call delete(CB%phi)
         call delete(CB%divB)
         call delete(CB%temp_F)
       end subroutine

       subroutine apply_cleaning(CB,B,m,n,compute_norms)
         implicit none
         type(clean_B),intent(inout) :: CB
         type(VF),intent(inout) :: B
         integer,intent(in) :: n
         logical,intent(in) :: compute_norms
         call div(CB%divB,B,m)
         call solve(CB%CG,CB%phi,CB%divB,m,n,compute_norms,CB%tmp_F,CB%tmp_CC,tmp_CC_VF)
         call grad(CB%temp_F,CB%phi,g)
         call subtract(B,CB%temp_F)
         call applyAllBCs(B,m)
       end subroutine

       end module