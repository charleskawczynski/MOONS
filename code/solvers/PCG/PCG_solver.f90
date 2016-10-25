      module PCG_solver_mod
      use current_precision_mod
      use mesh_mod
      use apply_BCs_mod
      use norms_mod
      use ops_discrete_mod
      use ops_aux_mod
      use boundary_conditions_mod
      use export_raw_processed_mod
      use is_nan_mod
      use SF_mod
      use VF_mod
      use ops_norms_mod
      use IO_tools_mod

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
        MFP,ISP,res_norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
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
        integer,intent(in) :: un
        type(iter_solver_params),intent(inout) :: ISP
        integer,intent(inout) :: N_iter
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p,z
        type(norms) :: res_norm0
        logical :: skip_loop
        integer :: i,i_earlyExit
        real(cp) :: alpha,rhok,rhokp1,res_norm_L2 ! betak = rhokp1/rhok

        call assign(r,b)
        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! RHS MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        if (.not.x%is_CC) call assign_wall_Dirichlet(r,0.0_cp,x)
        call assign(p,0.0_cp)
        call apply_BCs(p,m) ! p has BCs for x
        call assign_ghost_N_XPeriodic(p,0.0_cp)
        call operator_explicit(Ax,p,k,m,MFP,tempk)
        call assign_wall_Dirichlet(Ax,0.0_cp,x) ! Does nothing in PPE
        call subtract(r,Ax)
        if (x%all_Neumann) call subtract_physical_mean(r)
        ! ----------------------------------------------------------

        call operator(Ax,x,k,m,MFP,tempk)
        call subtract(r,Ax)
        call multiply_wall_Neumann(r,0.5_cp,x)
        call multiply(r,vol)
        call compute(res_norm0,r)
        call check_nans(res_norm0%L2,res_norm0,name//' PCG_SF res_norm0%L2')

#ifdef _EXPORT_PCG_SF_CONVERGENCE_
          call compute(res_norm,r)
          res_norm_L2 = dot_product(r,r,m,x,tempx); N_iter = 0; i = 0 ; i_earlyExit = 0
          call export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
#endif
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx); res_norm_L2 = abs(rhok); i_earlyExit = 0
        if (.not.sqrt(res_norm0%L2).lt.ISP%tol_abs) then ! Only do PCG if necessary!
          skip_loop = .false.
          do i=1,ISP%iter_max
            call operator(Ax,p,k,m,MFP,tempk)
            call multiply_wall_Neumann(Ax,0.5_cp,x)
            call multiply(Ax,vol)
            alpha = rhok/dot_product(p,Ax,m,x,tempx)
            call assign_ghost_N_XPeriodic(p,0.0_cp)
            call add_product(x,p,alpha) ! x = x + alpha p
            call apply_BCs(x,m) ! Needed for PPE
            N_iter = N_iter + 1
            call add_product(r,Ax,-alpha) ! r = r - alpha Ap

            if (check_res(ISP,i)) then
              res_norm_L2 = dot_product(r,r,m,x,tempx)
              if (exit_loop(ISP,sqrt(res_norm_L2),res_norm0%L2)) then; i_earlyExit=1; exit; endif
            endif
            call update_check_res(ISP,i)

#ifdef _EXPORT_PCG_SF_CONVERGENCE_
            call compute(res_norm,r)
            call export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
#endif
            call multiply(z,Minv,r)
            rhokp1 = dot_product(z,r,m,x,tempx)
            call product_add(p,rhokp1/rhok,z) ! p = p beta + z 
            rhok = rhokp1
          enddo
        else; i=1; skip_loop = .true.
        endif
        call update_last_iter(ISP,i)

        call check_nans(res_norm_L2,res_norm0,name//' PCG_SF res_norm_L2')

        if (compute_norms) then
          if (.not.skip_loop) then
            call operator_explicit(Ax,x,k,m,MFP,tempk)
            call multiply(Ax,vol)
            call multiply(r,b,vol)
            if (x%all_Neumann) call subtract_physical_mean(r)
            call subtract(r,Ax)
            call assign_wall_Dirichlet(r,0.0_cp,x) ! Does nothing in PPE
            call assign_ghost_N_XPeriodic(r,0.0_cp,x)
            call compute(res_norm,r); call print(res_norm,'PCG_SF Residuals for '//name)
            call export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
            write(*,*) 'PCG_SF iterations (executed/max) = ',i-1+i_earlyExit,ISP%iter_max
            write(*,*) 'PCG_SF exit condition = ',sqrt(res_norm_L2)/res_norm0%L2
            write(*,*) 'PCG_SF ISP%n_skip_check_res = ',ISP%n_skip_check_res
          else
            write(*,*) 'PCG_SF skip_loop = ',skip_loop
          endif
          write(*,*) ''
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,operator_explicit,name,x,b,vol,k,m,&
        MFP,ISP,res_norm,compute_norms,un,tempx,tempk,Ax,r,p,N_iter,z,Minv)
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
        integer,intent(in) :: un
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        logical :: skip_loop
        integer :: i,i_earlyExit
        type(norms) :: res_norm0
        real(cp) :: alpha,rhok,rhokp1,res_norm_L2 ! betak = rhokp1/rhok
        call assign(r,b)
        ! ----------------------- MODIFY RHS -----------------------
        ! THE FOLLOWING MODIFICATION SHOULD BE READ VERY CAREFULLY.
        ! MODIFCATIONS ARE EXPLAINED IN DOCUMENTATION.
        if (.not.x%is_CC) call assign_wall_Dirichlet(r,0.0_cp,x)
        call assign(p,0.0_cp)
        call apply_BCs(p,m) ! p has BCs for x
        call assign_ghost_N_XPeriodic(p,0.0_cp)
        call operator_explicit(Ax,p,k,m,MFP,tempk)
        call assign_wall_Dirichlet(Ax,0.0_cp,x) ! Does nothing in PPE
        call subtract(r,Ax)
        ! ----------------------------------------------------------

        call operator(Ax,x,k,m,MFP,tempk)
        call subtract(r,Ax)
        call multiply_wall_Neumann(r,0.5_cp,x)
        call multiply(r,vol)
        call compute(res_norm0,r)
        call check_nans(res_norm0%L2,res_norm0,name//' PCG_VF res_norm0%L2')

#ifdef _EXPORT_PCG_VF_CONVERGENCE_
          call compute(res_norm,r)
          res_norm_L2 = dot_product(r,r,m,x,tempx); N_iter = 0; i = 0 ; i_earlyExit = 0
          call export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
#endif
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,m,x,tempx); res_norm_L2 = abs(rhok); i_earlyExit = 0
        if (.not.sqrt(res_norm0%L2).lt.ISP%tol_abs) then ! Only do PCG if necessary!
          do i=1,ISP%iter_max
            call operator(Ax,p,k,m,MFP,tempk)
            call multiply_wall_Neumann(Ax,0.5_cp,x)
            call multiply(Ax,vol)
            alpha = rhok/dot_product(p,Ax,m,x,tempx)
            call assign_ghost_N_XPeriodic(p,0.0_cp)
            call add_product(x,p,alpha) ! x = x + alpha p
            call apply_BCs(x,m) ! Needed for PPE
            N_iter = N_iter + 1
            call add_product(r,Ax,-alpha) ! r = r - alpha Ap

            if (check_res(ISP,i)) then
              res_norm_L2 = dot_product(r,r,m,x,tempx)
              if (exit_loop(ISP,sqrt(res_norm_L2),res_norm0%L2)) then; i_earlyExit=1; exit; endif
            endif

#ifdef _EXPORT_PCG_VF_CONVERGENCE_
            call compute(res_norm,r)
            call export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
#endif
            call multiply(z,Minv,r)
            rhokp1 = dot_product(z,r,m,x,tempx)
            call product_add(p,rhokp1/rhok,z) ! p = p beta + z 
            rhok = rhokp1
          enddo
        else; i=1; skip_loop = .true.
        endif
        call update_last_iter(ISP,i)

        call check_nans(res_norm_L2,res_norm0,name//' PCG_VF res_norm_L2')
        
        if (compute_norms) then
          if (.not.skip_loop) then
            call operator_explicit(Ax,x,k,m,MFP,tempk)
            call multiply(Ax,vol)
            call multiply(r,b,vol)
            ! if (x%all_Neumann) call subtract_physical_mean(r)
            call subtract(r,Ax)
            call assign_wall_Dirichlet(r,0.0_cp,x)
            call assign_ghost_N_XPeriodic(r,0.0_cp,x)
            call compute(res_norm,r); call print(res_norm,'PCG_VF Residuals for '//name)
            call export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
            write(*,*) 'PCG_VF iterations (executed/max) = ',i-1+i_earlyExit,ISP%iter_max
            write(*,*) 'PCG_VF exit condition = ',sqrt(res_norm_L2)/res_norm0%L2
            write(*,*) 'PCG_VF ISP%n_skip_check_res = ',ISP%n_skip_check_res
          else
            write(*,*) 'PCG_VF skip_loop = ',skip_loop
          endif
          write(*,*) ''
        endif
      end subroutine

      subroutine export_norms(un,res_norm0,res_norm,res_norm_L2,N_iter,i,i_earlyExit)
        implicit none
        integer,intent(in) :: un,N_iter,i,i_earlyExit
        real(cp),intent(in) :: res_norm_L2
        type(norms),intent(in) :: res_norm0,res_norm
        write(un,*) N_iter,&
                    sqrt(res_norm_L2)/res_norm0%L2,&
                    res_norm%L1,&
                    res_norm%L2,&
                    res_norm%Linf,&
                    res_norm0%L1,&
                    res_norm0%L2,&
                    res_norm0%Linf,&
                    i-1+i_earlyExit
        flush(un)
      end subroutine

      subroutine check_nans(f,res_norm0,location)
        implicit none
        real(cp),intent(in) :: f
        type(norms),intent(in) :: res_norm0
        character(len=*),intent(in) :: location
        if (is_nan(f)) then
          write(*,*) 'Error: NaN in ',location
          write(*,*) 'f = ',f
          call print(res_norm0,'res_norm0 in '//location)
          stop 'Done';
        endif
      end subroutine

      end module