      module Jacobi_mod
      use current_precision_mod
      use mesh_extend_mod
      use mesh_domain_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use string_mod
      use SF_extend_mod
      use VF_mod
      use TF_mod
      use IO_export_mod
      use IO_tools_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      use Jacobi_solver_mod
      use matrix_mod
      use preconditioners_mod
      use diagonals_mod

      implicit none

      private
      public :: Jacobi
      public :: init,delete
      public :: solve

      type Jacobi
        type(mesh) :: m
        type(VF) :: Ax,res,Dinv,D,vol,x_interior
        type(TF) :: k,tempk
        type(norms) :: norm
        type(mesh_domain) :: D_interior
        type(string) :: name
        type(matrix_free_params) :: MFP
        integer :: un,n_skip_check_res,N_iter ! unit to export norm
        real(cp) :: tol
        procedure(op_VF),pointer,nopass :: operator
      end type

      interface init;        module procedure init_Jacobi;       end interface
      interface solve;       module procedure solve_Jacobi_VF;   end interface
      interface delete;      module procedure delete_Jacobi;     end interface

      contains

      subroutine init_Jacobi(JAC,operator,x,x_interior,k,m,D_interior,&
        MFP,n_skip_check_res,tol,dir,name,vizualizeOperator)
        implicit none
        procedure(op_VF_explicit) :: operator
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(in) :: x,x_interior
        type(TF),intent(in) :: k
        type(mesh),intent(in) :: m
        type(mesh_domain),intent(in) :: D_interior
        type(matrix_free_params),intent(in) :: MFP
        real(cp),intent(in) :: tol
        integer,intent(in) :: n_skip_check_res
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: vizualizeOperator
        call init(JAC%m,m)
        call init(JAC%Ax,x)
        call init(JAC%res,x)
        call init(JAC%Dinv,x)
        call init(JAC%D,x)
        call init(JAC%tempk,k)
        call init(JAC%k,k)
        call assign(JAC%k,k)
        call init(JAC%vol,x)
        call init(JAC%x_interior,x_interior)
        call assign(JAC%x_interior,x_interior)
        call volume(JAC%vol,m)
        call init(JAC%name,name)
        call init(JAC%D_interior,D_interior)
        JAC%un = new_and_open(dir,'norm_JAC_VF_'//name)
        call tecHeader(name,JAC%un,.true.)
        JAC%operator => operator
        call init(JAC%norm)
        call init(JAC%MFP,MFP)
        JAC%tol = tol
        JAC%N_iter = 1
        JAC%n_skip_check_res = n_skip_check_res

        ! call get_diagonal(operator,JAC%D,x,JAC%k,JAC%vol,m,MFP,JAC%tempk)

        if (vizualizeOperator) then
          call export_operator(operator,dir,'JAC_VF_'//name,x,JAC%k,JAC%vol,m,MFP,JAC%tempk)
          call export_matrix(JAC%D,dir,'JAC_VF_diag_'//name)
        endif

        call diag_Lap(JAC%D,m)
        call assign(JAC%Dinv,JAC%D)
        call invert(JAC%Dinv)
        call assign_ghost_XPeriodic(JAC%Dinv,0.0_cp)
      end subroutine

      subroutine solve_Jacobi_VF(JAC,x,f,m,n,compute_norm)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norm
        call solve_Jacobi(JAC%operator,x,JAC%x_interior,f,JAC%vol,JAC%k,JAC%Dinv,&
        JAC%D,m,JAC%D_interior,JAC%MFP,n,JAC%N_iter,JAC%norm,compute_norm,JAC%un,&
        JAC%n_skip_check_res,JAC%tol,str(JAC%name),JAC%Ax,JAC%res,JAC%tempk)
      end subroutine

      subroutine delete_Jacobi(JAC)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        call delete(JAC%D_interior)
        call delete(JAC%m)
        call delete(JAC%vol)
        call delete(JAC%Ax)
        call delete(JAC%res)
        call delete(JAC%Dinv)
        call delete(JAC%D)
        call delete(JAC%k)
        call delete(JAC%tempk)
        call init(JAC%norm)
        JAC%un = 0
        JAC%N_iter = 1
      end subroutine

      subroutine tecHeader(name,un,VF)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: un
        logical,intent(in) :: VF
        if (VF) then; write(un,*) 'TITLE = "Jacobi_VF residuals for '//name//'"'
        else;         write(un,*) 'TITLE = "Jacobi_SF residuals for '//name//'"'
        endif
        write(un,*) 'VARIABLES = N_iter,L1,L2,Linf,L1_0,L2_0,Linf_0,i-1+i_earlyExit'
        write(un,*) 'ZONE DATAPACKING = POINT'
        flush(un)
      end subroutine


      end module