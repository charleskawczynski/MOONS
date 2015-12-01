      module CG_mod
      use IO_SF_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      use matrix_mod
      use CG_solver_mod
      use CG_operators_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      public :: CG_solver
      public :: init,solve,delete
      
      interface init;    module procedure init_CG_VF;  end interface
      interface solve;   module procedure solve_CG_VF; end interface
      interface delete;  module procedure delete_CG;   end interface

      type CG_solver
        type(VF) :: r,p,tempx,Ax
        type(VF) :: vol
        type(VF) :: tempk,k
      end type

      contains

      subroutine init_CG_VF(CG,m,x,k)
        implicit none
        type(CG_solver),intent(inout) :: CG
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x,k
        logical :: analyzeA
        real(cp) :: dt,Rem
        call init(CG%r,x)
        call init(CG%p,x)
        call init(CG%tempx,x)
        call init(CG%Ax,x)

        call init(CG%vol,x)

        call init(CG%k,k)
        call init(CG%tempk,k)

        call assign(CG%k,k)

        call volume(CG%vol,m)
        analyzeA = .false.
        if (analyzeA) then
          dt = 0.01_cp; Rem = 1.0_cp
          write(*,*) '     Starting symmetry test'
          ! call test_symmetry_VF(operator,name,x,vol,m,tempk,c,k)
          call test_symmetry(B_diff_nat_stag,'CG_VF',x,CG%vol,m,CG%tempk,dt/Rem,CG%k)
          write(*,*) '     Finished symmetry test'
          ! call export_operator_VF(operator,name,dir,x,vol,m,tempk,c,k)
          call export_operator(B_diff_nat_stag,'CG_VF','out/LDC/',x,CG%vol,m,CG%tempk,dt/Rem,CG%k)
          write(*,*) '     Finished exporting operator'
          stop 'Done'
        endif
      end subroutine

      subroutine solve_CG_VF(CG,x,b,dt,Rem,m,n,norm,compute_norms)
        implicit none
        type(CG_solver),intent(inout) :: CG
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: dt,Rem
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_CG(B_diff_nat_stag,x,b,CG%vol,dt/Rem,CG%k,m,n,norm,&
        compute_norms,CG%tempx,CG%tempk,CG%Ax,CG%r,CG%p)
      end subroutine

      subroutine delete_CG(CG)
        implicit none
        type(CG_solver),intent(inout) :: CG
        call delete(CG%r)
        call delete(CG%p)
        call delete(CG%tempx)
        call delete(CG%Ax)
        call delete(CG%vol)
        call delete(CG%k)
        call delete(CG%tempk)
      end subroutine

      end module