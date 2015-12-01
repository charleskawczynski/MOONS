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
        type(VF) :: Au,res,Dinv,D,sigma,tempk,vol
      end type
      
      interface init;        module procedure init_Jacobi;       end interface
      interface solve;       module procedure solve_Jacobi;      end interface
      interface delete;      module procedure delete_Jacobi;     end interface

      contains

      subroutine init_Jacobi(JAC,u,sigma,m)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(in) :: u
        type(VF),intent(in) :: sigma
        type(mesh),intent(in) :: m
        call init(JAC%m,m)
        call init(JAC%Au,u)
        call init(JAC%res,u)
        call init(JAC%Dinv,u)
        call init(JAC%D,u)
        call init_Face(JAC%sigma,m)
        call init(JAC%tempk,JAC%sigma)
        call assign(JAC%sigma,sigma)
        call init(JAC%vol,u)
        call volume(JAC%vol,m)
        ! call get_diagonal_VF(operator,D,vol,m,tempk,c,k)
        call get_diagonal(compute_A,JAC%D,JAC%vol,m,JAC%tempk,1.0_cp,JAC%sigma)
        call assign(JAC%Dinv,JAC%D)
        call invert(JAC%Dinv)
      end subroutine

      subroutine solve_Jacobi(JAC,u,f,n,norm,displayTF)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(inout) :: u
        type(VF),intent(in) :: f
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        ! call solve_VF(operator,u,f,k,Dinv,D,m,n,norm,displayTF,Au,res)
        call solve(compute_A,u,f,JAC%sigma,JAC%Dinv,JAC%D,JAC%m,n,norm,displayTF,JAC%Au,JAC%res)
      end subroutine

      subroutine delete_Jacobi(JAC)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        call delete(JAC%m)
        call delete(JAC%Au)
        call delete(JAC%res)
        call delete(JAC%Dinv)
        call delete(JAC%D)
        call delete(JAC%sigma)
      end subroutine

      subroutine compute_A(Au,u,k,m,temp)
        implicit none
        type(VF),intent(inout) :: Au
        type(VF),intent(inout) :: temp
        type(VF),intent(in) :: k
        type(VF),intent(in) :: u
        type(mesh),intent(in) :: m
        call compute_diffusion(Au,u,k,m,temp)
      end subroutine

      subroutine compute_diffusion(Au,u,k,m,temp) ! ∇•(∇u)
        implicit none
        type(VF),intent(inout) :: Au
        type(VF),intent(inout) :: temp
        type(VF),intent(in) :: k
        type(VF),intent(in) :: u
        type(mesh),intent(in) :: m
        call curl(temp,u,m)
        call multiply(temp,k)
        call curl(Au,temp,m)
      end subroutine

      end module