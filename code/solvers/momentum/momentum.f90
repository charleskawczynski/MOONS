       module momentum2_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use IO_tools_mod
       use init_PBCs_mod
       use init_UBCs_mod
       use init_UField_mod
       use init_PField_mod
       use momentum_solver_mod
       use monitor_mod
       use SOR_mod
       use CG_mod
       use PCG_mod
       use norms_mod
       implicit none

       private
       public :: momentum
       public :: init,delete,solve

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type momentum
         type(mesh) :: m
         type(VF) :: U,Unm1,Ustar,U_CC,temp_F,temp_E1,temp_E2
         type(TF) :: U_E
         type(SF) :: p,temp_CC,divU
         type(SORSolver) :: SOR_p
         type(CG_solver_SF) :: CG_p
         integer :: nstep,N_PPE,N_mom
         real(cp) :: dTime,t
         real(cp) :: Re
         type(norms) :: norm
         type(monitor) :: mon_TKE,mon_divU
       end type

       interface init;           module procedure init_momentum;        end interface
       interface delete;         module procedure delete_momentum;      end interface
       interface solve;          module procedure solve_momentum;       end interface
       interface export;         module procedure export_momentum;      end interface

       contains

       subroutine init_momentum(mom,m,dir,N_PPE,N_mom,dTime,Re,solveMomentum)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime,Re
         integer,intent(in) :: N_PPE,N_mom
         character(len=*),intent(in) :: dir
         logical,intent(in) :: solveMomentum
         write(*,*) 'Initializing momentum:'
         mom%dTime = dTime
         mom%Re = Re
         mom%N_PPE = N_PPE
         mom%N_mom = N_mom

         call init(mom%m,m)
         call init_Face(mom%U,m,0.0_cp)
         call init_Face(mom%Unm1,m,0.0_cp)
         call init_Face(mom%Ustar,m,0.0_cp)
         call init_Face(mom%temp_F,m,0.0_cp)
         call init_Edge(mom%temp_E1,m,0.0_cp)
         call init_Edge(mom%temp_E2,m,0.0_cp)
         call init_CC(mom%p,m,0.0_cp)
         call init_CC(mom%U_CC,m,0.0_cp)
         call init_CC(mom%temp_CC,m,0.0_cp)

         call init_UBCs(mom%U,m)
         call init_PBCs(mom%p,m)

         call init_Ufield(mom%U,m,dir)
         call init_Pfield(mom%p,m,dir)

         if (solveMomentum) call apply_BCs(mom%U,m)
         if (solveMomentum) call apply_BCs(mom%p,m)
         call init(mom%mon_TKE,dir,'TKE',.false.)
         call init(mom%mon_divU,dir,'divU',.true.)

         call init(mom%SOR_p,mom%p,mom%m)  ! Initialize interior solvers

         call momentumInfo(mom,newAndOpen(dir//'parameters/','info_mom'))
         mom%t = 0.0_cp
         mom%nstep = 0
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine delete_momentum(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U)
         call delete(mom%Unm1)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%temp_E1)
         call delete(mom%temp_E2)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%divU)
         call delete(mom%U_CC)
         call delete(mom%U_E)
         call delete(mom%m)
         call delete(mom%SOR_p)
         call delete(mom%mon_TKE)
         call delete(mom%mon_divU)
       end subroutine

       subroutine solve_momentum(mom,dir,F,expensive,cheap)
         implicit none
         type(momentum),intent(inout) :: mom
         character(len=*),intent(in) :: dir
         type(VF),intent(in) :: F
         logical,intent(in) :: expensive,cheap
         
         call Euler_SOR_Donor(mom%SOR_p,mom%U,mom%p,F,mom%U_CC,&
         mom%m,mom%Re,mom%dTime,mom%N_PPE,mom%Ustar,mom%temp_F,&
         mom%temp_CC,mom%temp_E1,mom%temp_E2,cheap)

         call Crank_AB2_AB2_CG_PPE(mom%SOR_p,mom%U,mom%p,F,mom%U_CC,&
         mom%m,mom%Re,mom%dTime,mom%N_PPE,mom%Ustar,mom%temp_F,&
         mom%temp_CC,mom%temp_E1,mom%temp_E2,cheap)

         mom%t = mom%t + mom%dTime
         mom%nstep = mom%nstep + 1

         ! Post-processing
         call face2CellCenter(mom%U_CC,mom%U,mom%m)
         call face2edge(mom%U_E,mom%U,mom%m,mom%temp_CC,mom%temp_F)

         ! Exporting
         call export(mom,mom%m,dir,F,expensive,cheap,cheap)
       end subroutine

       subroutine export_momentum(mom,m,dir,F,exp_solution,exp_div,exp_TKE)
         implicit none
         type(momentum),intent(in) :: mom
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         type(VF),intent(in) :: F
         logical,intent(in) :: exp_solution,exp_div,exp_TKE
         real(cp) :: TKE
         if (exp_div) then
           call div(mom%divU,mom%U,mom%m)
           call compute(mom%norm,mom%divU,mom%m)
           call apply(mom%mon_divU,mom%nstep,mom%norm)
         endif
         if (exp_solution) then
           write(*,*) 'Exporting U solution'
           call export_raw(mom%m,mom%U,dir,'U',0)
           call export_raw(mom%m,mom%p,dir,'p',0)
           call export_raw(mom%m,F,dir,'jCrossB',0)
           if (.not.exp_div) call div(mom%divU,mom%U,mom%m)
           call export_raw(mom%m,mom%divU,dir,'divU',0)
           call export_processed(mom%m,mom%U,dir,'U',1)
           call export_processed(mom%m,mom%p,dir,'p',1)
           write(*,*) '     finished'
         endif
         if (exp_TKE) then
           call totalEnergy(TKE,mom%U_CC,mom%m)
           call apply(mom%mon_TKE,TKE,mom%m,mom%nstep)
         endif
       end subroutine

       end module