      module Jacobi_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_stitches_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use IO_SF_mod
      use ops_del_mod
      use VF_mod

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
        type(VF) :: Au,res,Dinv,D,sigma
      end type
      
      interface init;        module procedure init_Jacobi;       end interface
      interface delete;      module procedure delete_Jacobi;     end interface
      interface solve;       module procedure solve_Jacobi;      end interface

      contains

      subroutine initJAC(JAC,u,sigma,operator,m)
        implicit none
        external :: operator
        type(Jacobi),intent(inout) :: JAC
        type(SF),intent(in) :: u
        type(mesh),intent(in) :: m
        call init(JAC%m,m)
        call init(JAC%Au,u)
        call init(JAC%res,u)
        call init(JAC%Dinv,u)
        call init(JAC%D,u)
        call init_Face(JAC%sigma,m)
        call assign(JAC%sigma,sigma)
        call get_diagonal(JAC%D,operator,m)
        call divide(JAC%Dinv,1.0_cp,JAC%D)
      end subroutine

      subroutine solve_Jacobi(JAC,operator,u,f,n,norm,displayTF)
        implicit none
        type(Jacobi),intent(in) :: JAC
        external :: operator
        type(VF),intent(inout) :: u
        type(VF),intent(in) :: f
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        call solve_VF(operator,u,f,JAC%sigma,JAC%Dinv,JAC%D,JAC%m,n,norm,displayTF,JAC%Au,JAC%res)
      end subroutine

      subroutine deleteJAC(JAC)
        implicit none
        type(JACSolver),intent(inout) :: JAC
        call delete(JAC%m)
        call delete(JAC%Au)
        call delete(JAC%res)
        call delete(JAC%Dinv)
        call delete(JAC%D)
        call delete(JAC%sigma)
      end subroutine

      end module