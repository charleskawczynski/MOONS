      module CG_mod
      use IO_SF_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_tools_mod
      use export_raw_processed_mod

      use matrix_mod
      use CG_solver_mod
      use matrix_free_params_mod
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
      public :: CG_solver_SF
      public :: CG_solver_VF
      public :: init,solve,delete
      
      interface init;    module procedure init_CG_SF;   end interface
      interface init;    module procedure init_CG_VF;   end interface

      interface solve;   module procedure solve_CG_SF;  end interface
      interface solve;   module procedure solve_CG_VF;  end interface

      interface delete;  module procedure delete_CG_SF; end interface
      interface delete;  module procedure delete_CG_VF; end interface

      type CG_solver_SF
        type(matrix_free_params) :: MFP
        type(VF) :: tempk,k
        type(norms) :: norm
        type(SF) :: r,p,tempx,Ax,vol
        integer :: un,N_iter
        procedure(),pointer,nopass :: operator
        procedure(),pointer,nopass :: operator_explicit
      end type

      type CG_solver_VF
        type(matrix_free_params) :: MFP
        type(VF) :: tempk,k
        type(norms) :: norm
        type(VF) :: r,p,tempx,Ax,vol
        integer,dimension(3) :: un
        integer :: N_iter
        procedure(),pointer,nopass :: operator
        procedure(),pointer,nopass :: operator_explicit
      end type

      contains

      subroutine init_CG_SF(CG,operator,operator_explicit,m,MFP,x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        external :: operator,operator_explicit
        type(CG_solver_SF),intent(inout) :: CG
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        type(VF),intent(in) :: k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,exportOperator
        type(matrix_free_params),intent(in) :: MFP
        call init(CG%r,x)
        call init(CG%p,x)
        call init_BCs(CG%p,x)
        call init(CG%tempx,x)
        call init(CG%Ax,x)
        call init(CG%vol,x)
        call init(CG%k,k)
        call init(CG%tempk,k)
        call init(CG%norm)
        call assign(CG%k,k)
        call init(CG%MFP,MFP)
        call volume(CG%vol,m)
        CG%un = newAndOpen(dir,'norm_CG_'//name)
        call tecHeader(name,CG%un,.false.)
        CG%N_iter = 1
        CG%operator => operator
        CG%operator_explicit => operator_explicit
        if (testSymmetry) then
          call test_symmetry(operator,'CG_SF_'//name,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,'CG_SF_'//name,dir,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
      end subroutine

      subroutine init_CG_VF(CG,operator,operator_explicit,m,MFP,x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        external :: operator,operator_explicit
        type(CG_solver_VF),intent(inout) :: CG
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x,k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,exportOperator
        type(matrix_free_params),intent(in) :: MFP
        call init(CG%r,x)
        call init(CG%p,x)
        call init_BCs(CG%p,x)
        call init(CG%tempx,x)
        call init(CG%Ax,x)
        call init(CG%vol,x)
        call init(CG%k,k)
        call init(CG%tempk,k)
        call init(CG%norm)
        call assign(CG%k,k)
        call init(CG%MFP,MFP)
        call volume(CG%vol,m)
        CG%un(1) = newAndOpen(dir,'norm_CG_x_'//name)
        CG%un(2) = newAndOpen(dir,'norm_CG_y_'//name)
        CG%un(3) = newAndOpen(dir,'norm_CG_z_'//name)
        call tecHeader(name,CG%un(1),.true.)
        call tecHeader(name,CG%un(2),.true.)
        call tecHeader(name,CG%un(3),.true.)
        CG%N_iter = 1
        CG%operator => operator
        CG%operator_explicit => operator_explicit
        if (testSymmetry) then
          call test_symmetry(operator,'CG_VF_'//name,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,'CG_VF_'//name,dir,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
      end subroutine

      subroutine solve_CG_SF(CG,x,b,m,n,compute_norms)
        implicit none
        type(CG_solver_SF),intent(inout) :: CG
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_CG(CG%operator,CG%operator_explicit,x,b,CG%vol,CG%k,m,CG%MFP,n,CG%norm,&
        compute_norms,CG%un,CG%tempx,CG%tempk,CG%Ax,CG%r,CG%p,CG%N_iter)
      end subroutine

      subroutine solve_CG_VF(CG,x,b,m,n,compute_norms)
        implicit none
        type(CG_solver_VF),intent(inout) :: CG
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_CG(CG%operator,CG%operator_explicit,x,b,CG%vol,CG%k,m,CG%MFP,n,CG%norm,&
        compute_norms,CG%un,CG%tempx,CG%tempk,CG%Ax,CG%r,CG%p,CG%N_iter)
      end subroutine

      subroutine delete_CG_SF(CG)
        implicit none
        type(CG_solver_SF),intent(inout) :: CG
        call delete(CG%r)
        call delete(CG%p)
        call delete(CG%tempx)
        call delete(CG%Ax)
        call delete(CG%vol)
        call delete(CG%k)
        call delete(CG%tempk)
        call delete(CG%MFP)
        CG%N_iter = 1
      end subroutine

      subroutine delete_CG_VF(CG)
        implicit none
        type(CG_solver_VF),intent(inout) :: CG
        call delete(CG%r)
        call delete(CG%p)
        call delete(CG%tempx)
        call delete(CG%Ax)
        call delete(CG%vol)
        call delete(CG%k)
        call delete(CG%tempk)
        call delete(CG%MFP)
        CG%N_iter = 1
      end subroutine

      subroutine tecHeader(name,un,VF)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: un
        logical,intent(in) :: VF
        if (VF) then; write(un,*) 'TITLE = "CG_VF residuals for '//name//'"'
        else;         write(un,*) 'TITLE = "CG_SF residuals for '//name//'"'
        endif
        write(un,*) 'VARIABLES = N,L1,L2,Linf'
        write(un,*) 'ZONE DATAPACKING = POINT'
      end subroutine

      end module