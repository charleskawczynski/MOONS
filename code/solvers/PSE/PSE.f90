      module PSE_mod
      use mesh_mod
      use apply_BCs_mod
      use apply_stitches_mod
      use BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod

      implicit none

      private
      public :: solve_PSE

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      contains

      subroutine solve_PSE(u,f,vol,m,n,ds,norm,displayTF,res,lapu)
        implicit none
        type(SF),intent(inout) :: u
        type(SF),intent(in) :: f,vol
        type(mesh),intent(in) :: m
        integer,intent(in) :: n
        real(cp),intent(in) :: ds
        type(norms),intent(inout) :: norm
        logical,intent(in) :: displayTF
        type(SF),intent(inout) :: res,lapu
        integer :: i
        call apply_BCs(u,m) ! Necessary with ghost nodes
        do i=1,minval((/n,m%N_cells_tot/))
          call lap(lapu,u,m)
          call subtract(res,lapu,f)
          call multiply(res,ds)
          call add(u,res)
          call apply_BCs(u,m)
          call apply_stitches(u,m)
        enddo
        if (u%all_Neumann) call subtract(u,mean(u))

        if (displayTF) then
          call lap(lapu,u,m)
          call subtract(res,lapu,f)
          call zeroGhostPoints(res)
          call zeroWall(res,m)
          call compute(norm,res,vol)
          write(*,*) 'Number of PSE iterations = ',n
          write(*,*) 'Iterations (input/max) = ',(/n,m%N_cells_tot/)
          call print(norm,'PSE Residuals')
        endif

      end subroutine

      end module