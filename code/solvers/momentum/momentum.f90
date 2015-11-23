       module momentum_mod
       use simParams_mod
       
       use mesh_mod
       use SF_mod
       use VF_mod

       use init_PBCs_mod
       use init_UBCs_mod
       use init_UField_mod
       use init_PField_mod

       use momentum_solver_mod
       use SOR_mod
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
         type(VF) :: U,Ustar,U_CC,temp_F,temp_E1,temp_E2
         type(SF) :: p,temp_CC
         type(SORSolver) :: SOR_p
         integer :: nstep,N_PPE
         real(cp) :: dTime,t
         real(cp) :: Re
       end type

       interface init;                module procedure init_momentum;              end interface
       interface delete;              module procedure delete_momentum;            end interface
       interface solve;               module procedure solve_momentum;             end interface

       contains

       subroutine init_momentum(mom,m,dir,N_PPE,dTime,Re)
         implicit none
         type(momentum),intent(inout) :: mom
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: dir
         write(*,*) 'Initializing momentum:'
         mom%Re = Re
         mom%N_PPE = N_PPE

         call init(mom%m,m)
         
         call init_Face(mom%U,m);          call assign(mom%U,0.0_cp)        ! Vector Fields
         call init_Face(mom%Ustar,m);      call assign(mom%Ustar,0.0_cp)
         call init_Face(mom%temp_F,m);     call assign(mom%temp_F,0.0_cp)
         call init_Edge(mom%temp_E1,m);    call assign(mom%temp_E1,0.0_cp)
         call init_Edge(mom%temp_E2,m);    call assign(mom%temp_E2,0.0_cp)

         call init_CC(mom%p,m);            call assign(mom%p,0.0_cp)         ! Scalar Fields
         call init_CC(mom%U_CC,m);         call assign(mom%U_CC,0.0_cp)
         call init_CC(mom%temp_CC,m);      call assign(mom%temp_CC,0.0_cp)

         call init_UBCs(mom%U,m)
         call init_PBCs(mom%p,m)

         call init_Ufield(mom%U,m,dir)
         call init_Pfield(mom%p,m,dir)

         if (solveMomentum) call apply_BCs(mom%U,m)
         if (solveMomentum) call apply_BCs(mom%p,m)

         call init(mom%SOR_p,mom%p,mom%m)  ! Initialize interior solvers

         call momentumInfo(mom,newAndOpen(dir//'parameters/','info_mom'))
         mom%t = 0.0_cp
         write(*,*) '     Finished'
         write(*,*) ''
       end subroutine

       subroutine solve_momentum(mom,F,norm_PPE,compute_norms)
         implicit none
         type(momentum),intent(inout) :: mom
         type(VF),intent(in) :: F
         logical,intent(in) :: compute_norms
         type(norm),intent(inout) :: norm_PPE
         
         call Euler_SOR_Donor(mom%SOR_p,mom%U,mom%p,F,mom%U_CC,&
         mom%m,mom%Re,mom%dTime,mom%N_PPE,mom%Ustar,mom%temp_F,&
         mom%temp_CC,mom%temp_E1,mom%temp_E2,norm_PPE,compute_norms)

         mom%t = mom%t + mom%dTime
         mom%nstep = mom%nstep + 1
         call face2cellcenter(mom%U_CC,mom%U,mom%m)
       end subroutine

       subroutine deleteMomentum(mom)
         implicit none
         type(momentum),intent(inout) :: mom
         call delete(mom%U)
         call delete(mom%Ustar)
         call delete(mom%temp_F)
         call delete(mom%temp_E1)
         call delete(mom%temp_E2)
         call delete(mom%p)
         call delete(mom%temp_CC)
         call delete(mom%U_CC)
         call delete(mom%m)
         call delete(mom%SOR_p)
       end subroutine

       end module