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
        type(SF) :: r,p,tempx,Ax,vol
        type(VF) :: tempk,k
        type(norms) :: norm
        integer :: un
      end type

      type CG_solver_VF
        type(matrix_free_params) :: MFP
        type(VF) :: r,p,tempx,Ax,vol
        type(VF) :: tempk,k
        type(norms) :: norm
        integer,dimension(3) :: un
      end type

      contains

      subroutine init_CG_SF(CG,operator,m,MFP,x,k,dir,name,testSymmetry,vizualizeOperator)
        implicit none
        external :: operator
        type(CG_solver_SF),intent(inout) :: CG
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        type(VF),intent(in) :: k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,vizualizeOperator
        type(matrix_free_params),intent(in) :: MFP
        call init(CG%r,x)
        call init(CG%p,x)
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
        if (testSymmetry) then
          call test_symmetry(operator,'CG_SF_'//name,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
        if (vizualizeOperator) then
          call export_operator(operator,'CG_SF_'//name,dir,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
      end subroutine

      subroutine init_CG_VF(CG,operator,m,MFP,x,k,dir,name,testSymmetry,vizualizeOperator)
        implicit none
        external :: operator
        type(CG_solver_VF),intent(inout) :: CG
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x,k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,vizualizeOperator
        type(matrix_free_params),intent(in) :: MFP
        call init(CG%r,x)
        call init(CG%p,x)
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
        if (testSymmetry) then
          call test_symmetry(operator,'CG_VF_'//name,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
        if (vizualizeOperator) then
          call export_operator(operator,'CG_VF_'//name,dir,x,CG%k,CG%vol,m,MFP,CG%tempk)
        endif
      end subroutine

      subroutine solve_CG_SF(CG,operator,x,b,m,n,compute_norms)
        implicit none
        external :: operator
        type(CG_solver_SF),intent(inout) :: CG
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_CG(operator,x,b,CG%vol,CG%k,m,CG%MFP,n,CG%norm,&
        compute_norms,CG%un,CG%tempx,CG%tempk,CG%Ax,CG%r,CG%p)
      end subroutine

      subroutine solve_CG_VF(CG,operator,x,b,m,n,compute_norms)
        implicit none
        external :: operator
        type(CG_solver_VF),intent(inout) :: CG
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_CG(operator,x,b,CG%vol,CG%k,m,CG%MFP,n,CG%norm,&
        compute_norms,CG%un,CG%tempx,CG%tempk,CG%Ax,CG%r,CG%p)
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
      end subroutine

      end module