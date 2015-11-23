      module Jacobi_solver_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_Stitches_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_SF_mod

      implicit none

      private
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

      interface solve;   module procedure solve_SF_uniform; end interface
      interface solve;   module procedure solve_SF;         end interface
      interface solve;   module procedure solve_VF_uniform; end interface
      interface solve;   module procedure solve_VF;         end interface

      interface body;    module procedure body_SF;          end interface
      interface body;    module procedure body_VF;          end interface

      interface post;    module procedure post_SF;          end interface
      interface post;    module procedure post_VF;          end interface

      contains

      subroutine solve_SF_uniform(operator,u,f,Dinv,D,m,n,norm,displayTF,Au,res)
        implicit none
        external :: operator
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,Dinv,D
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(SF),intent(inout) :: Au,res
        integer :: i
        call apply_BCs(u,m) ! Boundaries
        do i=1,n
          call operator(Au,u,m)
          call body(u,f,Dinv,D,m,Au,res)
        enddo
        if (u%all_neumann) call subtract(u,mean(u))
        if (displayTF) then
          call operator(Au,u,m)
          call post(u,f,m,n,norm,Au,res)
        endif
      end subroutine

      subroutine solve_SF(operator,u,f,k,Dinv,D,m,n,norm,displayTF,Au,res)
        implicit none
        external :: operator
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,Dinv,D
        type(VF),intent(in) :: k
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(SF),intent(inout) :: Au,res
        integer :: i
        call apply_BCs(u,m) ! Boundaries
        do i=1,n
          call operator(Au,u,k,m)
          call body(u,f,Dinv,D,m,Au,res)
        enddo
        if (u%all_neumann) call subtract(u,mean(u))
        if (displayTF) then
          call operator(Au,u,k,m)
          call post(u,f,m,n,norm,Au,res)
        endif
      end subroutine

      subroutine solve_VF_uniform(operator,u,f,Dinv,D,m,n,norm,displayTF,Au,res)
        implicit none
        external :: operator
        type(VF),intent(inout) :: u
        type(VF),intent(in) :: f,Dinv,D
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: Au,res
        integer :: i
        call apply_BCs(u,m) ! Boundaries
        do i=1,n
          call operator(Au,u,m)
          call body(u,f,Dinv,D,m,Au,res)
        enddo
        if (u%x%all_neumann) call subtract(u%x,mean(u%x))
        if (u%y%all_neumann) call subtract(u%y,mean(u%y))
        if (u%z%all_neumann) call subtract(u%z,mean(u%z))
        if (displayTF) then
          call operator(Au,u,m)
          call post(u,f,m,n,norm,Au,res)
        endif
      end subroutine

      subroutine solve_VF(operator,u,f,k,Dinv,D,m,n,norm,displayTF,Au,res)
        implicit none
        external :: operator
        type(VF),intent(inout) :: u
        type(VF),intent(in) :: f,k,Dinv,D
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(VF),intent(inout) :: Au,res
        integer :: i
        call apply_BCs(u,m) ! Boundaries
        do i=1,n
          call operator(Au,u,k,m)
          call body(u,f,Dinv,D,m,Au,res)
        enddo
        if (u%x%all_neumann) call subtract(u%x,mean(u%x))
        if (u%y%all_neumann) call subtract(u%y,mean(u%y))
        if (u%z%all_neumann) call subtract(u%z,mean(u%z))
        if (displayTF) then
          call operator(Au,u,k,m)
          call post(u,f,m,n,norm,Au,res)
        endif
      end subroutine

      subroutine body_SF(u,f,Dinv,D,m,Au,res)
        implicit none
        type(SF),intent(inout) :: u,Au,res
        type(SF),intent(in) :: f,Dinv,D
        type(mesh),intent(in) :: m
        call subtract(Au,D) ! LU = Au - D
        call subtract(res,f,Au)
        call multiply(u,Dinv,res)
        call apply_Stitches(u,m)
        call apply_BCs(u,m)
      end subroutine

      subroutine body_VF(u,f,Dinv,D,m,Au,res)
        implicit none
        type(VF),intent(inout) :: u,Au,res
        type(VF),intent(in) :: f,Dinv,D
        type(mesh),intent(in) :: m
        call subtract(Au,D) ! LU = Au - D
        call subtract(res,f,Au)
        call multiply(u,Dinv,res)
        call apply_Stitches(u,m)
        call apply_BCs(u,m)
      end subroutine

      subroutine post_SF(u,f,m,n,norm,Au,res)
        implicit none
        type(SF),intent(inout) :: u,Au,res
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        write(*,*) 'Jacobi iterations = ',n
        call subtract(res,Au,f)
        call zeroGhostPoints(res)
        call compute(norm,res,m)
        call print(norm,'Jacobi residuals')
      end subroutine

      subroutine post_VF(u,f,m,n,norm,Au,res)
        implicit none
        type(VF),intent(inout) :: u,Au,res
        type(VF),intent(in) :: f
        type(mesh),intent(in) :: m
        integer,intent(inout) :: n
        type(norms),intent(inout) :: norm
        write(*,*) 'Jacobi iterations = ',n
        call subtract(res,Au,f)
        call zeroGhostPoints(res)
        call compute(norm,res%x,m); call print(norm,'Jacobi residuals')
        call compute(norm,res%y,m); call print(norm,'Jacobi residuals')
        call compute(norm,res%z,m); call print(norm,'Jacobi residuals')
      end subroutine

      end module