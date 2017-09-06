      module PSE_mod
      use current_precision_mod
      use IO_export_mod
      use mesh_extend_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_extend_mod
      use VF_mod
      use TF_mod
      use IO_tools_mod

      use matrix_mod
      use PSE_solver_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: PSE_solver_SF
      public :: PSE_solver_VF
      public :: init,solve,delete

      interface init;    module procedure init_PSE_SF;   end interface
      interface init;    module procedure init_PSE_VF;   end interface

      interface solve;   module procedure solve_PSE_SF;  end interface
      interface solve;   module procedure solve_PSE_VF;  end interface

      interface delete;  module procedure delete_PSE_SF; end interface
      interface delete;  module procedure delete_PSE_VF; end interface

      type PSE_solver_SF
        type(matrix_free_params) :: MFP
        type(TF) :: tempk,k
        type(norms) :: norm
        type(SF) :: r,Ax,vol
        integer :: un,N_iter
        procedure(op_SF),pointer,nopass :: operator
      end type

      type PSE_solver_VF
        type(matrix_free_params) :: MFP
        type(TF) :: tempk,k
        type(norms) :: norm
        type(VF) :: r,Ax,vol
        integer :: un,N_iter
        procedure(op_VF),pointer,nopass :: operator
      end type

      contains

      subroutine init_PSE_SF(PSE,operator,m,MFP,x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        procedure(op_SF) :: operator
        type(PSE_solver_SF),intent(inout) :: PSE
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        type(TF),intent(in) :: k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,exportOperator
        type(matrix_free_params),intent(in) :: MFP
        call init(PSE%r,x)
        call init(PSE%Ax,x)
        call init(PSE%vol,x)
        call init(PSE%k,k)
        call init(PSE%tempk,k)
        call init(PSE%norm)
        call assign(PSE%k,k)
        call init(PSE%MFP,MFP)
        call volume(PSE%vol,m)
        PSE%un = new_and_open(dir,'norm_PSE_'//name)
        call tecHeader(name,PSE%un,.false.)
        PSE%N_iter = 1
        PSE%operator => operator
        if (testSymmetry) then
          call test_symmetry(operator,'PSE_SF_'//name,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,dir,'PSE_SF_'//name,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
      end subroutine

      subroutine init_PSE_VF(PSE,operator,m,MFP,x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        procedure(op_VF) :: operator
        type(PSE_solver_VF),intent(inout) :: PSE
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x
        type(TF),intent(in) :: k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,exportOperator
        type(matrix_free_params),intent(in) :: MFP
        call init(PSE%r,x)
        call init(PSE%Ax,x)
        call init(PSE%vol,x)
        call init(PSE%k,k)
        call init(PSE%tempk,k)
        call init(PSE%norm)
        call assign(PSE%k,k)
        call init(PSE%MFP,MFP)
        call volume(PSE%vol,m)
        PSE%un = new_and_open(dir,'norm_PSE_'//name)
        call tecHeader(name,PSE%un,.true.)
        PSE%N_iter = 1
        PSE%operator => operator
        if (testSymmetry) then
          call test_symmetry(operator,'PSE_VF_'//name,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,dir,'PSE_VF_'//name,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
      end subroutine

      subroutine solve_PSE_SF(PSE,x,b,m,n,ds,compute_norms)
        implicit none
        type(PSE_solver_SF),intent(inout) :: PSE
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        real(cp),intent(in) :: ds
        logical,intent(in) :: compute_norms
        call solve_PSE(PSE%operator,x,b,PSE%vol,PSE%k,m,PSE%MFP,n,ds,PSE%norm,&
        compute_norms,PSE%un,PSE%tempk,PSE%Ax,PSE%r,PSE%N_iter)
      end subroutine

      subroutine solve_PSE_VF(PSE,x,b,m,n,ds,compute_norms)
        implicit none
        type(PSE_solver_VF),intent(inout) :: PSE
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        real(cp),intent(in) :: ds
        logical,intent(in) :: compute_norms
        call solve_PSE(PSE%operator,x,b,PSE%vol,PSE%k,m,PSE%MFP,n,ds,PSE%norm,&
        compute_norms,PSE%un,PSE%tempk,PSE%Ax,PSE%r,PSE%N_iter)
      end subroutine

      subroutine delete_PSE_SF(PSE)
        implicit none
        type(PSE_solver_SF),intent(inout) :: PSE
        call delete(PSE%r)
        call delete(PSE%Ax)
        call delete(PSE%vol)
        call delete(PSE%k)
        call delete(PSE%tempk)
        call delete(PSE%MFP)
        PSE%N_iter = 1
      end subroutine

      subroutine delete_PSE_VF(PSE)
        implicit none
        type(PSE_solver_VF),intent(inout) :: PSE
        call delete(PSE%r)
        call delete(PSE%Ax)
        call delete(PSE%vol)
        call delete(PSE%k)
        call delete(PSE%tempk)
        call delete(PSE%MFP)
        PSE%N_iter = 1
      end subroutine

      subroutine tecHeader(name,un,VF)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: un
        logical,intent(in) :: VF
        if (VF) then; write(un,*) 'TITLE = "PSE_VF residuals for '//name//'"'
        else;         write(un,*) 'TITLE = "PSE_SF residuals for '//name//'"'
        endif
        write(un,*) 'VARIABLES = N,stop_criteria,L1,L2,Linf,norm_r0_L1,norm_r0_L2,norm_r0_Linf,iter_used'
        write(un,*) 'ZONE DATAPACKING = POINT'
        flush(un)
      end subroutine

      end module