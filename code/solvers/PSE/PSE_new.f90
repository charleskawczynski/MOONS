      module PSE_mod
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
      use PSE_solver_mod
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
        type(VF) :: tempk,k
        type(norms) :: norm
        type(SF) :: r,Ax,vol
        integer :: un
        procedure(),pointer,nopass :: operator
      end type

      type PSE_solver_VF
        type(matrix_free_params) :: MFP
        type(VF) :: tempk,k
        type(norms) :: norm
        type(VF) :: r,Ax,vol
        integer,dimension(3) :: un
        procedure(),pointer,nopass :: operator
      end type

      contains

      subroutine init_PSE_SF(PSE,operator,m,MFP,x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        external :: operator
        type(PSE_solver_SF),intent(inout) :: PSE
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        type(VF),intent(in) :: k
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
        PSE%un = newAndOpen(dir,'norm_PSE_'//name)
        PSE%operator => operator
        if (testSymmetry) then
          call test_symmetry(operator,'PSE_SF_'//name,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,'PSE_SF_'//name,dir,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
      end subroutine

      subroutine init_PSE_VF(PSE,operator,m,MFP,x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        external :: operator
        type(PSE_solver_VF),intent(inout) :: PSE
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x,k
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
        PSE%un(1) = newAndOpen(dir,'norm_PSE_x_'//name)
        PSE%un(2) = newAndOpen(dir,'norm_PSE_y_'//name)
        PSE%un(3) = newAndOpen(dir,'norm_PSE_z_'//name)
        PSE%operator => operator
        if (testSymmetry) then
          call test_symmetry(operator,'PSE_VF_'//name,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,'PSE_VF_'//name,dir,x,PSE%k,PSE%vol,m,MFP,PSE%tempk)
        endif
      end subroutine

      subroutine solve_PSE_SF(PSE,x,b,m,n,compute_norms)
        implicit none
        type(PSE_solver_SF),intent(inout) :: PSE
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_PSE(PSE%operator,x,b,PSE%vol,PSE%k,m,PSE%MFP,n,PSE%norm,&
        compute_norms,PSE%un,PSE%tempk,PSE%Ax,PSE%r)
      end subroutine

      subroutine solve_PSE_VF(PSE,x,b,m,n,compute_norms)
        implicit none
        type(PSE_solver_VF),intent(inout) :: PSE
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        logical,intent(in) :: compute_norms
        call solve_PSE(PSE%operator,x,b,PSE%vol,PSE%k,m,PSE%MFP,n,PSE%norm,&
        compute_norms,PSE%un,PSE%tempk,PSE%Ax,PSE%r)
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
      end subroutine

      end module