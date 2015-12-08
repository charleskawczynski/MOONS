      module Jacobi_mod
      use mesh_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_SF_mod
      use IO_tools_mod
      use matrix_free_operators_mod
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
        type(norms) :: norm
        integer,dimension(3) :: un ! unit to export norm
      end type
      
      interface init;        module procedure init_Jacobi;       end interface
      interface solve;       module procedure solve_Jacobi;      end interface
      interface delete;      module procedure delete_Jacobi;     end interface

      contains

      subroutine init_Jacobi(JAC,x,k,m,dt,Rem,dir,name)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(in) :: x
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: dt,Rem
        character(len=*),intent(in) :: dir,name
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
        JAC%un(1) = newAndOpen(dir,'norm_JAC_x_'//name)
        JAC%un(2) = newAndOpen(dir,'norm_JAC_y_'//name)
        JAC%un(3) = newAndOpen(dir,'norm_JAC_z_'//name)

        call init(JAC%norm)

        call get_diagonal(B_diff_nat_stag_diag,JAC%D,JAC%vol,m,JAC%tempk,dt/Rem,JAC%k)
        call multiply(JAC%D,dt/Rem)
        call add(JAC%D,1.0_cp)

        if (visualize_operator) then
          ! call export_operator_VF(operator,name,dir,x,vol,m,tempk,c,k)
          call export_operator(B_diff_nat_stag,'JAC_VF','out/LDC/',x,JAC%vol,m,JAC%tempk,dt/Rem,JAC%k)
          write(*,*) '     Finished exporting operator'

          call export_matrix(JAC%D,'out/LDC/','diag(JAC_VF)')
          write(*,*) '     Finished exporting diagonal'
          stop 'Done'
        endif

        call assign(JAC%Dinv,JAC%D)
        call invert(JAC%Dinv)
      end subroutine

      subroutine solve_Jacobi(JAC,x,f,m,n,compute_norm)
        implicit none
        type(Jacobi),intent(inout) :: JAC
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norm
        ! call solve(operator,x,f,vol,k,c,Dinv,D,m,n,norm,compute_norm,un,Ax,res,tempk)
        call solve(B_diff_nat_stag,x,f,JAC%vol,JAC%k,1.0_cp,JAC%Dinv,&
        JAC%D,m,n,JAC%norm,compute_norm,JAC%un,JAC%Ax,JAC%res,JAC%tempk)
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
        call init(JAC%norm)
        JAC%un = 0
      end subroutine

      end module