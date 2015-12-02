      module Jacobi_mod
      use mesh_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_SF_mod
      use Jacobi_operators_mod
      use Jacobi_solver_mod
      use matrix_mod

      implicit none

      private
      public :: Jacobi
      public :: init,delete
      public :: solve
      logical :: visualize_operator = .false.

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      type Jacobi
        type(mesh) :: m
        type(VF) :: Ax,res,Dinv,D,k,tempk,vol
      end type
      
      interface init;        module procedure init_Jacobi;       end interface
      interface solve;       module procedure solve_Jacobi;      end interface
      interface delete;      module procedure delete_Jacobi;     end interface

      contains

      subroutine init_Jacobi(JAC,x,k,m,dt,Rem)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: dt,Rem
        call init(JAC%m,m)
        call init(JAC%Ax,x)
        call init(JAC%res,x)
        call init(JAC%Dinv,x)
        call init(JAC%D,x)
        call init_Face(JAC%k,m)
        call init(JAC%tempk,JAC%k)
        call assign(JAC%k,k)
        call init(JAC%vol,x)
        call volume(JAC%vol,m)

        call get_diagonal(B_diff_nat_stag_diag,JAC%D,JAC%vol,m,JAC%tempk,dt/Rem,JAC%k)
        call multiply(JAC%D,dt/Rem)
        call add(JAC%D,1.0_cp)

        call assign(JAC%Dinv,JAC%D)
        call invert(JAC%Dinv)
      end subroutine

      subroutine solve_Jacobi(JAC,x,f,n,norm,displayTF)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: f
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        ! call solve_VF(operator,x,f,k,Dinv,D,m,n,norm,displayTF,Ax,res)
        call solve(B_diff_nat_stag,x,f,JAC%k,JAC%Dinv,JAC%D,JAC%m,n,norm,displayTF,JAC%Ax,JAC%res)
      end subroutine

      subroutine delete_Jacobi(JAC)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        call delete(JAC%m)
        call delete(JAC%Ax)
        call delete(JAC%res)
        call delete(JAC%Dinv)
        call delete(JAC%D)
        call delete(JAC%k)
      end subroutine

      end module