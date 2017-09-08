      module PCG_solver_mod
      use current_precision_mod
      use mesh_extend_mod
      use apply_BCs_mod
      use norms_extend_mod
      use ops_discrete_mod
      use ops_aux_mod
      use boundary_conditions_mod
      use export_raw_processed_mod
      use iter_solver_params_mod
      use is_nan_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_extend_mod
      use PCG_aux_mod
      use ops_norms_mod
      use IO_tools_mod
      use IO_export_mod

      use iter_solver_params_mod
      use iter_solver_params_extend_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: solve_PCG
      interface solve_PCG;      module procedure solve_PCG_SF;   end interface
      interface solve_PCG;      module procedure solve_PCG_VF;   end interface

      interface print_info;  module procedure print_info_PCG; end interface
      interface compute_export_norms;  module procedure compute_export_norms_SF; end interface
      interface compute_export_norms;  module procedure compute_export_norms_VF; end interface

      contains

      subroutine solve_PCG_SF(operator,operator_explicit,name,x,x_BC,b,vol,k,m,&
        MFP,ISP,res_norm,compute_norms,un,un_convergence,tempx,tempk,Ax,r,p,z,Minv)
        implicit none
        procedure(op_SF) :: operator
        procedure(op_SF_explicit) :: operator_explicit
        character(len=*),intent(in) :: name
        type(SF),intent(inout) :: x,x_BC
        type(SF),intent(in) :: b,vol,Minv
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: res_norm
        integer,intent(in) :: un,un_convergence
        type(iter_solver_params),intent(inout) :: ISP
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(SF),intent(inout) :: tempx,Ax,r,p,z
        type(norms) :: res_norm0
        real(cp) :: alpha,rhok,rhokp1 ! betak = rhokp1/rhok
        integer :: i
        call modify_RHS(operator,operator_explicit,x,x_BC,b,vol,k,m,MFP,tempx,tempk,Ax,r)

        ! ********************* START PCG ALGORITHM *********************
        call compute(res_norm0,r)
        call init_iter_per_call(ISP)
        call check_nans(res_norm0,res_norm,ISP,'check_nans before loop for '//name)
        if (ISP%export_convergence) call compute_export_norms(un_convergence,res_norm0,res_norm,ISP,r)
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,x,tempx); res_norm%L2 = sqrt(abs(rhok))
        call update_exit_loop(ISP,res_norm%L2,res_norm0%L2)
        if (.not.ISP%exit_loop(2)) then ! Only do PCG if necessary!
          do i=1,ISP%iter_max
            call operator(Ax,p,k,m,MFP,tempk)
            call multiply_wall_Neumann(Ax,0.5_cp,x_BC)
            call assign_wall_Dirichlet(Ax,0.0_cp,x_BC)
            call assign_wall_Periodic_single(Ax,0.0_cp,x_BC)
            call multiply(Ax,vol)
            alpha = rhok/dot_product(p,Ax,x,tempx)
            call add_product(x,p,alpha) ! x = x + alpha p
            call apply_BCs(x) ! Needed for PPE
            call update_iter(ISP)
            call add_product(r,Ax,-alpha) ! r = r - alpha Ap

            if (check_res(ISP)) then
              res_norm%L2 = sqrt(abs(dot_product(r,r,x,tempx)))
              call update_exit_loop(ISP,res_norm%L2,res_norm0%L2)
              if (any(ISP%exit_loop)) exit
            endif
            if (ISP%export_convergence) then
              call check_nans(res_norm0,res_norm,ISP,'check_nans before export for '//name)
              call compute_export_norms(un_convergence,res_norm0,res_norm,ISP,r)
            endif

            call multiply(z,Minv,r)
            rhokp1 = dot_product(z,r,x,tempx)
            call product_add(p,rhokp1/rhok,z) ! p = p beta + z
            rhok = rhokp1
            call update_exit_loop(ISP)
          enddo
        else; call apply_BCs(x); call update_iter(ISP)
        endif

        call check_nans(res_norm0,res_norm,ISP,'check_nans after loop for '//name)

        if (compute_norms) then
          call compute_export_norms(un,res_norm0,res_norm,ISP,r)
          call print_info(name,ISP,res_norm,res_norm0)
        endif
      end subroutine

      subroutine solve_PCG_VF(operator,operator_explicit,name,x,x_BC,b,vol,k,m,&
        MFP,ISP,res_norm,compute_norms,un,un_convergence,tempx,tempk,Ax,r,p,z,Minv)
        implicit none
        procedure(op_VF) :: operator
        procedure(op_VF_explicit) :: operator_explicit
        character(len=*),intent(in) :: name
        type(VF),intent(inout) :: x,x_BC
        type(VF),intent(in) :: b,vol,Minv
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(norms),intent(inout) :: res_norm
        type(iter_solver_params),intent(inout) :: ISP
        integer,intent(in) :: un,un_convergence
        logical,intent(in) :: compute_norms
        type(matrix_free_params),intent(in) :: MFP
        type(VF),intent(inout) :: tempx,Ax,r,p,z
        integer :: i
        type(norms) :: res_norm0
        real(cp) :: alpha,rhok,rhokp1 ! betak = rhokp1/rhok
        call modify_RHS(operator,operator_explicit,x,x_BC,b,vol,k,m,MFP,tempx,tempk,Ax,r)

        ! ********************* START PCG ALGORITHM *********************
        call compute(res_norm0,r)
        call init_iter_per_call(ISP)
        call check_nans(res_norm0,res_norm,ISP,'check_nans before loop for '//name)
        if (ISP%export_convergence) call compute_export_norms(un_convergence,res_norm0,res_norm,ISP,r)
        call multiply(z,Minv,r)
        call assign(p,z)
        rhok = dot_product(r,z,x,tempx); res_norm%L2 = sqrt(abs(rhok))
        call update_exit_loop(ISP,res_norm%L2,res_norm0%L2)
        if (.not.ISP%exit_loop(2)) then ! Only do PCG if necessary!
          do i=1,ISP%iter_max
            call operator(Ax,p,k,m,MFP,tempk)
            call multiply_wall_Neumann(Ax,0.5_cp,x_BC)
            call assign_wall_Dirichlet(Ax,0.0_cp,x_BC)
            call assign_wall_Periodic_single(Ax,0.0_cp,x_BC)
            call multiply(Ax,vol)
            alpha = rhok/dot_product(p,Ax,x,tempx)
            call add_product(x,p,alpha) ! x = x + alpha p
            call apply_BCs(x) ! Needed for PPE
            call update_iter(ISP)
            call add_product(r,Ax,-alpha) ! r = r - alpha Ap

            if (check_res(ISP)) then
              res_norm%L2 = sqrt(abs(dot_product(r,r,x,tempx)))
              call update_exit_loop(ISP,res_norm%L2,res_norm0%L2)
              if (any(ISP%exit_loop)) exit
            endif
            if (ISP%export_convergence) then
              call check_nans(res_norm0,res_norm,ISP,'check_nans before export for '//name)
              call compute_export_norms(un_convergence,res_norm0,res_norm,ISP,r)
            endif

            call multiply(z,Minv,r)
            rhokp1 = dot_product(z,r,x,tempx)
            call product_add(p,rhokp1/rhok,z) ! p = p beta + z
            rhok = rhokp1
            call update_exit_loop(ISP)
          enddo
        else; call apply_BCs(x); call update_iter(ISP)
        endif

        call check_nans(res_norm0,res_norm,ISP,'check_nans after loop for '//name)

        if (compute_norms) then
          call compute_export_norms(un,res_norm0,res_norm,ISP,r)
          call print_info(name,ISP,res_norm,res_norm0)
        endif
      end subroutine

      subroutine compute_export_norms_SF(un,res_norm0,res_norm,ISP,r)
        implicit none
        integer,intent(in) :: un
        type(norms),intent(in) :: res_norm0
        type(norms),intent(inout) :: res_norm
        type(iter_solver_params),intent(in) :: ISP
        type(SF),intent(in) :: r
        call compute(res_norm,r)
        call export_norms(un,res_norm0,res_norm,ISP)
      end subroutine

      subroutine compute_export_norms_VF(un,res_norm0,res_norm,ISP,r)
        implicit none
        integer,intent(in) :: un
        type(norms),intent(in) :: res_norm0
        type(norms),intent(inout) :: res_norm
        type(iter_solver_params),intent(in) :: ISP
        type(VF),intent(in) :: r
        call compute(res_norm,r)
        call export_norms(un,res_norm0,res_norm,ISP)
      end subroutine

      subroutine export_norms(un,res_norm0,res_norm,ISP)
        implicit none
        integer,intent(in) :: un
        type(norms),intent(in) :: res_norm0,res_norm
        type(iter_solver_params),intent(in) :: ISP
        real(cp) :: rel
        call check_nans(res_norm0,res_norm,ISP,'export_norms')
        rel = compute_relative_norm(res_norm,res_norm0)
        write(un,*) ISP%iter_total,&
                    rel,&
                    res_norm%L1,&
                    res_norm%L2,&
                    res_norm%Linf,&
                    res_norm0%L1,&
                    res_norm0%L2,&
                    res_norm0%Linf,&
                    ISP%iter_per_call
        flush(un)
      end subroutine

      subroutine check_nans(res_norm0,res_norm,ISP,location)
        implicit none
        type(norms),intent(in) :: res_norm0,res_norm
        type(iter_solver_params),intent(in) :: ISP
        character(len=*),intent(in) :: location
        logical,dimension(7) :: L
        real(cp) :: rel
        rel = compute_relative_norm(res_norm,res_norm0)
        L(1) = is_nan(res_norm%L1)
        L(2) = is_nan(res_norm%L2)
        L(3) = is_nan(res_norm%Linf)
        L(4) = is_nan(res_norm0%L1)
        L(5) = is_nan(res_norm0%L2)
        L(6) = is_nan(res_norm0%Linf)
        L(7) = is_nan(rel)
        if (any(L)) then
          write(*,*) 'Error: NaN in ',location
          write(*,*) 'iterations_used = ',ISP%iter_per_call
          write(*,*) 'res_norm%L2 = ',res_norm%L2
          call print(res_norm,'res_norm in '//location)
          call print(res_norm0,'res_norm0 in '//location)
          call print(ISP)
          write(*,*) 'L=',L
          stop 'Done'
        endif
      end subroutine

      subroutine print_info_PCG(name,ISP,res_norm,res_norm0)
        implicit none
        type(iter_solver_params),intent(in) :: ISP
        type(norms),intent(in) :: res_norm,res_norm0
        character(len=*),intent(in) :: name
        real(cp) :: rel
        rel = compute_relative_norm(res_norm,res_norm0)
        if (ISP%export_heavy) then
          call print(res_norm0,res_norm,name//' res_norm0,res_norm')
          call print_exit_loop(ISP)
          write(*,*) 'iter_executed,rel error = ',ISP%iter_per_call,rel
          write(*,*) '----------------------------------------'
          write(*,*) ''
        else
          write(*,*) 'L2(res_norm,rel) '//name//' = ',res_norm%L2,rel
        endif
      end subroutine

      function compute_relative_norm(res_norm,res_norm0) result(rel)
        implicit none
        type(norms),intent(in) :: res_norm,res_norm0
        real(cp) :: rel
        if (equal(res_norm0%L2,0.0_cp)) then; rel = res_norm%L2
        else;                                 rel = res_norm%L2/res_norm0%L2
        endif
      end function

      end module