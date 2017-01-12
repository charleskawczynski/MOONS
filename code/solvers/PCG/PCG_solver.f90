      module PCG_solver_mod
      use current_precision_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use boundary_conditions_mod
      use export_raw_processed_mod
      use iter_solver_params_mod
      use is_nan_mod
      use SF_mod
      use VF_mod
      use PCG_aux_mod
      use ops_norms_mod
      use IO_tools_mod
      use IO_export_mod

      use iter_solver_params_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: solve_PCG
      interface solve_PCG;      module procedure solve_PCG_SF;   end interface
      interface solve_PCG;      module procedure solve_PCG_VF;   end interface

      contains

      subroutine solve_PCG_SF(operator,operator_explicit,name,x,b,vol,k,m,&
        MFP,ISP,res_norm,compute_norms,un,un_convergence,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        procedure(op_SF) :: operator
        procedure(op_SF_explicit) :: operator_explicit
        character(len=*),intent(in) :: name
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: res_norm
        integer,intent(in) :: un,un_convergence
        type(iter_solver_params),intent(inout) :: ISP
        integer,intent(inout) :: N_iter
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p,z
        type(norms) :: res_norm0
        logical :: skip_loop
        integer :: i,i_earlyExit
        real(cp) :: alpha,rhok,rhokp1 ! betak = rhokp1/rhok
        ! ----------------------- MODIFY RHS -----------------------
        call assign(r,b)                              ! r = b
        call multiply_wall_Neumann(r,0.5_cp,x)        ! r = b_mod
        if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call compute_Ax_BC(operator_explicit,tempx,p,x,k,m,MFP,tempk)
        call subtract(r,tempx)                        ! r = (b_mod - Ax_BC - Ax)
        ! if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call operator(Ax,x,k,m,MFP,tempk)
        call multiply_wall_Neumann(Ax,0.5_cp,x)
        call subtract(r,Ax)                           ! r = (b_mod - Ax_BC - Ax_mod)
        call multiply(r,vol)                          ! r = vol*(b_mod - Ax_BC - Ax_mod)
        if (.not.is_CC(x)) call assign_wall_Dirichlet(r,0.0_cp,x)
        ! ----------------------------------------------------------

        ! ********************* START PCG ALGORITHM *********************
        call compute(res_norm0,r)
        call check_nans(res_norm0,res_norm,ISP,i,'check_nans before loop for '//name)
        if (ISP%export_convergence) then
          call compute(res_norm,r)
          res_norm%L2 = dot_product(r,r,x,tempx); i = 0 ; i_earlyExit = 0
          call export_norms(un,res_norm0,res_norm,N_iter,i,i_earlyExit)
        endif
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,x,tempx); res_norm%L2 = abs(rhok); i_earlyExit = 0
        call update_exit_loop(ISP,1,res_norm%L2,res_norm0%L2)
        if (.not.ISP%exit_loop(2)) then ! Only do PCG if necessary!
          do i=1,ISP%iter_max
            call operator(Ax,p,k,m,MFP,tempk)
            call multiply_wall_Neumann(Ax,0.5_cp,x)
            call assign_wall_Dirichlet(Ax,0.0_cp,x)
            call multiply(Ax,vol)
            alpha = rhok/dot_product(p,Ax,x,tempx)
            call add_product(x,p,alpha) ! x = x + alpha p
            call apply_BCs(x) ! Needed for PPE
            N_iter = N_iter + 1
            call add_product(r,Ax,-alpha) ! r = r - alpha Ap

            if (check_res(ISP,i)) then
              res_norm%L2 = dot_product(r,r,x,tempx)
              call update_exit_loop(ISP,i,res_norm%L2,res_norm0%L2)
              if (any(ISP%exit_loop)) then; i_earlyExit=1; exit; endif
            endif
            call update_check_res(ISP,i)
            if (ISP%export_convergence) then
              call compute(res_norm,r)
              call export_norms(un_convergence,res_norm0,res_norm,N_iter,i,i_earlyExit)
            endif

            call multiply(z,Minv,r)
            rhokp1 = dot_product(z,r,x,tempx)
            call product_add(p,rhokp1/rhok,z) ! p = p beta + z
            rhok = rhokp1
            call update_exit_loop(ISP,i)
          enddo
        else; i=1; skip_loop = .true.
        endif
        call update_last_iter(ISP,i)

        call check_nans(res_norm0,res_norm,ISP,i,'check_nans after loop for '//name)

        if (compute_norms) then
          if (.not.skip_loop) then
            call compute(res_norm,r)
            call export_norms(un,res_norm0,res_norm,N_iter,i,i_earlyExit)
            call print_info(name,ISP,res_norm,res_norm0,i,i_earlyExit)
          else
            write(*,*) name//' skip_loop = ',skip_loop
          endif
          write(*,*) ''
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,operator_explicit,name,x,b,vol,k,m,&
        MFP,ISP,res_norm,compute_norms,un,un_convergence,tempx,tempk,Ax,r,p,N_iter,z,Minv)
        implicit none
        procedure(op_VF) :: operator
        procedure(op_VF_explicit) :: operator_explicit
        character(len=*),intent(in) :: name
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b,vol,Minv
        type(VF),intent(in) :: k
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: res_norm
        type(iter_solver_params),intent(inout) :: ISP
        integer,intent(inout) :: N_iter
        integer,intent(in) :: un,un_convergence
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        logical :: skip_loop
        integer :: i,i_earlyExit
        type(norms) :: res_norm0
        real(cp) :: alpha,rhok,rhokp1 ! betak = rhokp1/rhok
        ! ----------------------- MODIFY RHS -----------------------
        call assign(r,b)                              ! r = b
        call multiply_wall_Neumann(r,0.5_cp,x)        ! r = b_mod
        ! if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call compute_Ax_BC(operator_explicit,tempx,p,x,k,m,MFP,tempk)
        call subtract(r,tempx)                        ! r = (b_mod - Ax_BC - Ax)
        ! if (x%all_Neumann) call subtract_physical_mean(r,vol,tempx) ! Not sure correct loc
        call operator(Ax,x,k,m,MFP,tempk)
        call multiply_wall_Neumann(Ax,0.5_cp,x)
        call subtract(r,Ax)                           ! r = (b_mod - Ax_BC - Ax_mod)
        call multiply(r,vol)                          ! r = vol*(b_mod - Ax_BC - Ax_mod)
        if (.not.is_CC(x)) call assign_wall_Dirichlet(r,0.0_cp,x)
        ! ----------------------------------------------------------

        ! ********************* START PCG ALGORITHM *********************
        call compute(res_norm0,r)
        call check_nans(res_norm0,res_norm,ISP,i,'check_nans before loop for '//name)
        if (ISP%export_convergence) then
          call compute(res_norm,r)
          res_norm%L2 = dot_product(r,r,x,tempx); i = 0 ; i_earlyExit = 0
          call export_norms(un,res_norm0,res_norm,N_iter,i,i_earlyExit)
        endif
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,x,tempx); res_norm%L2 = abs(rhok); i_earlyExit = 0
        call update_exit_loop(ISP,1,res_norm%L2,res_norm0%L2)
        if (.not.ISP%exit_loop(2)) then ! Only do PCG if necessary!
          do i=1,ISP%iter_max
            call operator(Ax,p,k,m,MFP,tempk)
            call multiply_wall_Neumann(Ax,0.5_cp,x)
            call assign_wall_Dirichlet(Ax,0.0_cp,x)
            call multiply(Ax,vol)
            alpha = rhok/dot_product(p,Ax,x,tempx)
            call add_product(x,p,alpha) ! x = x + alpha p
            call apply_BCs(x) ! Needed for PPE
            N_iter = N_iter + 1
            call add_product(r,Ax,-alpha) ! r = r - alpha Ap

            if (check_res(ISP,i)) then
              res_norm%L2 = dot_product(r,r,x,tempx)
              call update_exit_loop(ISP,i,res_norm%L2,res_norm0%L2)
              if (any(ISP%exit_loop)) then; i_earlyExit=1; exit; endif
            endif
            call update_check_res(ISP,i)
            if (ISP%export_convergence) then
              call compute(res_norm,r)
              call export_norms(un_convergence,res_norm0,res_norm,N_iter,i,i_earlyExit)
            endif

            call multiply(z,Minv,r)
            rhokp1 = dot_product(z,r,x,tempx)
            call product_add(p,rhokp1/rhok,z) ! p = p beta + z
            rhok = rhokp1
            call update_exit_loop(ISP,i)
          enddo
        else; i=1; skip_loop = .true.
        endif
        call update_last_iter(ISP,i)

        call check_nans(res_norm0,res_norm,ISP,i,'check_nans after loop for '//name)

        if (compute_norms) then
          if (.not.skip_loop) then
            call compute(res_norm,r)
            call export_norms(un,res_norm0,res_norm,N_iter,i,i_earlyExit)
            call print_info(name,ISP,res_norm,res_norm0,i,i_earlyExit)
          else
            write(*,*) name//' skip_loop = ',skip_loop
          endif
          write(*,*) ''
        endif
      end subroutine

      subroutine export_norms(un,res_norm0,res_norm,N_iter,i,i_earlyExit)
        implicit none
        integer,intent(in) :: un,N_iter,i,i_earlyExit
        type(norms),intent(in) :: res_norm0,res_norm
        write(un,*) N_iter,&
                    sqrt(res_norm%L2)/res_norm0%L2,&
                    res_norm%L1,&
                    res_norm%L2,&
                    res_norm%Linf,&
                    res_norm0%L1,&
                    res_norm0%L2,&
                    res_norm0%Linf,&
                    i-1+i_earlyExit
        flush(un)
      end subroutine

      subroutine check_nans(res_norm0,res_norm,ISP,i,location)
        implicit none
        type(norms),intent(in) :: res_norm0,res_norm
        type(iter_solver_params),intent(in) :: ISP
        integer,intent(in) :: i
        character(len=*),intent(in) :: location
        if (is_nan(res_norm%L2).or.is_nan(res_norm%Linf)) then
          write(*,*) 'Error: NaN in ',location
          write(*,*) 'iterations_used = ',i
          write(*,*) 'res_norm%L2 = ',res_norm%L2
          call print(res_norm,'res_norm in '//location)
          call print(res_norm0,'res_norm0 in '//location)
          call print(ISP)
          stop 'Done'
        endif
      end subroutine

      subroutine print_info(name,ISP,res_norm,res_norm0,i,i_earlyExit)
        implicit none
        type(iter_solver_params),intent(in) :: ISP
        type(norms),intent(in) :: res_norm,res_norm0
        integer,intent(in) :: i,i_earlyExit
        character(len=*),intent(in) :: name
        write(*,*) '-------------- '//name//' --------------'
        call print(res_norm0,res_norm,'res_norm0,res_norm')
        call print_exit_loop(ISP)
        write(*,*) 'iterations (executed/max) = ',i-1+i_earlyExit,ISP%iter_max
        write(*,*) 'relative error = ',res_norm%L2/res_norm0%L2
        write(*,*) '----------------------------------------'
      end subroutine

      end module