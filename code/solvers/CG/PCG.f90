      module PCG_mod
      use IO_SF_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      use matrix_mod
      use PCG_solver_mod
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
      public :: PCG_solver
      public :: init,solve,delete

      logical :: visualize_operator = .false.
      logical :: test_symmetry_TF = .false.
      logical :: precondition = .false.
      
      interface init;    module procedure init_PCG_VF;  end interface
      interface solve;   module procedure solve_PCG_VF; end interface
      interface delete;  module procedure delete_PCG;   end interface

      type PCG_solver
        type(VF) :: r,p,tempx,Ax,z,Minv
        type(VF) :: vol
        type(VF) :: tempk,k
      end type

      contains

      subroutine init_PCG_VF(PCG,m,x,k,dt,Rem)
        implicit none
        type(PCG_solver),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x,k
        real(cp),intent(in) :: dt,Rem
        ! type(VF) :: temp_CC,temp_N,temp_F,temp_E

        ! call init_CC(temp_CC,m)
        ! call init_Node(temp_N,m)
        ! call init_Face(temp_F,m)
        ! call init_Edge(temp_E,m)
        ! write(*,*) 'Reached before tests'
        ! call assign(temp_F,0.0_cp)
        ! call add(temp_F,1.0_cp)
        ! call export_3D_1C(m,temp_F%x,'out/LDC/','temp_F(x)',0)
        ! call export_3D_1C(m,temp_F%y,'out/LDC/','temp_F(y)',0)
        ! call export_3D_1C(m,temp_F%z,'out/LDC/','temp_F(z)',0)
        ! call assign(temp_E,0.0_cp)
        ! call add(temp_E,1.0_cp)
        ! call export_3D_1C(m,temp_E%x,'out/LDC/','temp_E(x)',0)
        ! call export_3D_1C(m,temp_E%y,'out/LDC/','temp_E(y)',0)
        ! call export_3D_1C(m,temp_E%z,'out/LDC/','temp_E(z)',0)
        ! call delete(temp_CC)
        ! call delete(temp_N)
        ! call delete(temp_F)
        ! call delete(temp_E)
        ! stop 'Done testing'

        call init(PCG%r,x)
        call init(PCG%p,x)
        call init(PCG%tempx,x)
        call init(PCG%Ax,x)
        call init(PCG%z,x)
        call init(PCG%Minv,x)

        call init(PCG%vol,x)

        call init(PCG%k,k)
        call init(PCG%tempk,k)

        call assign(PCG%k,k)

        call volume(PCG%vol,m)

        ! This seems to be working, but takes very long (3 hrs for 67^3 size problem)
        ! call get_diagonal(B_diff_nat_stag,PCG%Minv,PCG%vol,m,PCG%tempk,dt/Rem,PCG%k)

        ! The complicated part of the diagonal is the curl-curl, so
        ! let's compute that automatically and then modify the rest after:
        if (precondition) then
          call get_diagonal(B_diff_nat_stag_diag,PCG%Minv,PCG%vol,m,PCG%tempk,dt/Rem,PCG%k)
          call multiply(PCG%Minv,dt/Rem)
          call add(PCG%Minv,1.0_cp)
          call multiply(PCG%Minv,PCG%vol)
        else; call assign(PCG%Minv,1.0_cp)
        endif

        if (test_symmetry_TF) call test_symmetry(B_diff_nat_stag,'PCG_VF',x,PCG%vol,m,PCG%tempk,dt/Rem,PCG%k)

        if (visualize_operator) then
          ! call export_operator_VF(operator,name,dir,x,vol,m,tempk,c,k)
          call export_operator(B_diff_nat_stag,'PCG_VF','out/LDC/',x,PCG%vol,m,PCG%tempk,dt/Rem,PCG%k)
          write(*,*) '     Finished exporting operator'

          call export_matrix(PCG%Minv,m,'out/LDC/','diag(PCG_VF)')
          write(*,*) '     Finished exporting diagonal'
          stop 'Done'
        endif
        call invert(PCG%Minv)
        call zeroGhostPoints(PCG%Minv)
      end subroutine

      subroutine solve_PCG_VF(PCG,x,b,dt,Rem,m,n,norm,compute_norms)
        implicit none
        type(PCG_solver),intent(inout) :: PCG
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: dt,Rem
        type(norms),dimension(3),intent(inout) :: norm
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_PCG(B_diff_nat_stag,x,b,PCG%vol,dt/Rem,PCG%k,m,n,norm,&
        compute_norms,PCG%tempx,PCG%tempk,PCG%Ax,PCG%r,PCG%p,PCG%z,PCG%Minv)
      end subroutine

      subroutine delete_PCG(PCG)
        implicit none
        type(PCG_solver),intent(inout) :: PCG
        call delete(PCG%r)
        call delete(PCG%p)
        call delete(PCG%tempx)
        call delete(PCG%Ax)
        call delete(PCG%vol)
        call delete(PCG%k)
        call delete(PCG%tempk)
        call delete(PCG%z)
        call delete(PCG%Minv)
      end subroutine

      end module