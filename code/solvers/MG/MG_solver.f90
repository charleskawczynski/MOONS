      module MG_solver_mod
      ! type(mg),dimension(n_levels) :: mg
      !
      ! call solve(operator,mg,x,b,m,n_levels,n,norm,compute_norms)
      ! solves Ax = b using multigrid method.
      !
      ! A is described by matrix-free
      ! operations defined in routine "operator"
      !
      ! Smoother      = Jacobi method
      ! Direct solver = Conjugate Gradient Method

      use mesh_mod
      use SF_mod
      use boundary_conditions_mod
      use apply_BCs_mod
      use norms_mod
      use MG_tools_mod
      use ops_aux_mod
      use IO_tools_mod
      use IO_SF_mod
      use IO_VF_mod
      use MG_mod
      use Jacobi_mod
      use CG_mod
      implicit none

      private

      public :: solve_SF,solve_VF
      interface solve;      module procedure solve_SF;     end interface
      interface solve;      module procedure solve_VF;     end interface

      contains

      subroutine solve_SF(mg,x,b,m,n,compute_norms,norm,n_levels)
        implicit none
        type(multiGrid),dimension(n_levels),intent(inout) :: mg
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        integer,intent(in) :: n,n_levels
        logical,intent(in) :: compute_norms
        type(norms),intent(inout) :: norm
        logical :: continueLoop,L
        integer :: i_MG

        call assign(mg(1)%smooth%b,b)
        call assign(mg(1)%smooth%x,x)

        do i_MG = 1,n

          ! 1) Smooth on finest level
          ! Improvement on efficiency: Pass back residual from smooth:
          ! call solve(operator,x,b,Dinv,D,m,n,norm,compute_norms,Au,res)
          call solve(mg(1)%smooth%operator,mg(1)%smooth%x,mg(1)%smooth%b,mg(1)%smooth%Dinv,&
          mg(1)%smooth%D,mg(1)%smooth%m,n,mg(1)%smooth%norm,mg(1)%compute_norms,&
          mg(1)%smooth%Ax,mg(1)%smooth%res)

          ! 2) Get residual on finest level
          call mg(1)%smooth%operator(mg(1)%smooth%Ax,mg(1)%smooth%x,mg(1)%smooth%m)
          call subtract(mg(1)%smooth%res,mg(1)%smooth%b,mg(1)%smooth%Ax)

          ! Zero boundary values
          call zeroGhostPoints(mg(1)%smooth%res)
          ! call zeroWall_conditional(mg(1)%smooth%res)

          ! 3) Begin decending into coarser grids, starting at level 2
          ! V-Cycle: Given whatever is needed, find, "exactly" the error
          ! on level 2.
          call Vcycle(mg(1)%smooth%operator,mg,1,n_levels)

          ! 4) Prolong correction back to mesh level 1
          call prolongate(mg(1)%e,mg(2)%e,mg(1)%m,mg(1)%temp_rxy,mg(1)%temp_rx)
          call add(mg(1)%x,mg(1)%e)

          ! 5) Final smoothing sweeps
          call solve(mg(1)%smooth%operator,mg(1)%smooth%x,mg(1)%smooth%b,mg(1)%smooth%Dinv,&
          mg(1)%smooth%D,mg(1)%smooth%m,n,mg(1)%smooth%norm,mg(1)%compute_norms,&
          mg(1)%smooth%Ax,mg(1)%smooth%res)

        enddo
        call assign(x,mg(1)%smooth%x)
        call init(norm,mg(1)%smooth%norm)

        if (compute_norms) then
          write(*,*) 'Number of V-Cycles = ',i_MG-1
          call mg(1)%smooth%operator(mg(1)%smooth%Ax,mg(1)%smooth%x,mg(1)%smooth%m)
          call init(norm,mg(1)%smooth%norm)
        endif
      end subroutine

      recursive subroutine Vcycle(operator,mg,j,n_levels)
        implicit none
        external :: operator
        type(multigrid),dimension(:),intent(inout) :: mg
        integer,intent(in) :: j,n_levels

        ! 1) Restrict the residual onto the coarse mesh
        !    and use it as the source term of the error
        call restrict(mg%smooth(j+1)%res,mg%smooth(j)%res,mg%smooth(j)%m,mg(j)%temp_rx,mg(j)%temp_rxy)

        call assign(mg%smooth(j+1)%b,mg%smooth(j+1)%res)

        if (j+1 .lt. n_levels) then
          ! j+1 is not the last level, so we are certain to have
          ! access to j+1 AND j+2

          ! Have not reached coarsest level, prepare to
          ! solve for error, and restrict residual to coarse
          ! mesh

          ! 2) Smooth
          call solve(operator,mg(j+1)%smooth%x,mg(j+1)%smooth%b,mg(j+1)%smooth%Dinv,&
          mg(j+1)%smooth%D,mg(j+1)%smooth%m,n,mg(j+1)%smooth%norm,mg(j+1)%compute_norms,&
          mg(j+1)%smooth%Ax,mg(j+1)%smooth%res)


          ! 3) Get residual
          call operator(mg(j+1)%smooth%Ax,mg(j+1)%smooth%x,mg(j+1)%smooth%m)
          call subtract(mg(j+1)%smooth%res,mg(j+1)%smooth%b,mg(j+1)%smooth%Ax)

          ! Zero boundary values
          call zeroGhostPoints(mg(j+1)%smooth%res)
          ! call zeroWall_conditional(mg(j+1)%smooth%res)

          ! 4) Decend to coarser level
          call Vcycle(operator,mg,j+1,n_levels)

          ! Finished Vcycle decent. Prepare to ascend back to level 1
          ! 5) Prolongate the error

          call prolongate(mg(j+1)%e,mg(j+2)%e,mg(j+1)%m,mg(j+1)%temp_rxy,mg(j+1)%temp_rx)
          call add(mg(j+1)%x,mg(j+1)%e)

          ! 6) Final smoothing sweeps
          call solve(operator,mg(j+1)%smooth%x,mg(j+1)%smooth%b,mg(j+1)%smooth%Dinv,&
          mg(j+1)%smooth%D,mg(j+1)%smooth%m,n,mg(j+1)%smooth%norm,mg(j+1)%compute_norms,&
          mg(j+1)%smooth%Ax,mg(j+1)%smooth%res)

          ! The solution on any mesh above the
          ! base mesh is the error!
          call assign(mg(j+1)%e,mg(j+1)%x)

        else ! At coarsest level. Solve exactly.

          ! call setMaxIterations(mg(j+1)%ss,5) ! Fixed
          call solve(operator,mg(j+1)%direct%x,mg(j+1)%direct%b,mg(j+1)%direct%Dinv,&
          mg(j+1)%direct%D,mg(j+1)%direct%m,n,mg(j+1)%direct%norm,mg(j+1)%compute_norms,&
          mg(j+1)%direct%Ax,mg(j+1)%direct%res)

          ! The solution on a given mesh is the correction on the finer mesh
          call assign(mg(j+1)%e,mg(j+1)%direct%x)
        endif
      end subroutine

      end module