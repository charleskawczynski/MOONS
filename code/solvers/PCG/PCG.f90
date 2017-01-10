      module PCG_mod
      use current_precision_mod
      use string_mod
      use IO_export_mod
      use mesh_mod
      use norms_mod
      use data_location_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use IO_tools_mod
      use export_raw_processed_mod

      use iter_solver_params_mod
      use preconditioners_mod
      use matrix_mod
      use PCG_solver_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: PCG_solver_SF
      public :: PCG_solver_VF
      public :: init,delete
      public :: solve
      public :: prolongate

      logical :: verifyPreconditioner = .false.

      interface init;       module procedure init_PCG_SF;        end interface
      interface init;       module procedure init_PCG_VF;        end interface

      interface solve;      module procedure solve_PCG_SF;       end interface
      interface solve;      module procedure solve_PCG_VF;       end interface

      interface prolongate; module procedure prolongate_PCG_SF;  end interface
      interface prolongate; module procedure prolongate_PCG_VF;  end interface

      interface delete;     module procedure delete_PCG_SF;      end interface
      interface delete;     module procedure delete_PCG_VF;      end interface

      type PCG_solver_SF
        type(matrix_free_params) :: MFP
        type(VF) :: tempk,k
        type(SF) :: r,p,tempx,Ax,vol,z,Minv
        type(norms) :: norm
        integer :: un,un_convergence,N_iter
        type(iter_solver_params) :: ISP
        type(string) :: dir,name
        procedure(preconditioner_SF),pointer,nopass :: prec
        procedure(op_SF),pointer,nopass :: operator
        procedure(op_SF_explicit),pointer,nopass :: operator_explicit
      end type

      type PCG_solver_VF
        type(matrix_free_params) :: MFP
        type(VF) :: tempk,k
        type(VF) :: r,p,tempx,Ax,vol,z,Minv
        type(norms) :: norm
        integer :: un,un_convergence,N_iter
        type(iter_solver_params) :: ISP
        type(string) :: dir,name
        procedure(preconditioner_VF),pointer,nopass :: prec
        procedure(op_VF),pointer,nopass :: operator
        procedure(op_VF_explicit),pointer,nopass :: operator_explicit
      end type

      contains

      subroutine init_PCG_SF(PCG,operator,operator_explicit,prec,m,ISP,MFP,&
        x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        procedure(op_SF) :: operator
        procedure(op_SF_explicit) :: operator_explicit
        procedure(preconditioner_SF) :: prec
        type(PCG_solver_SF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(iter_solver_params),intent(in) :: ISP
        type(SF),intent(in) :: x
        type(VF),intent(in) :: k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,exportOperator
        type(matrix_free_params),intent(in) :: MFP
        type(SF) :: temp_Minv
              if (is_CC(x%DL)) then; call init_CC(PCG%tempx,m)   ! Does not copy BCs of x
        elseif (is_Node(x%DL)) then; call init_Node(PCG%tempx,m) ! Does not copy BCs of x
        elseif (is_Edge(x%DL)) then; call init_Edge(PCG%tempx,m,get_edge(x%DL)) ! Does not copy BCs of x
        elseif (is_Face(x%DL)) then; call init_Face(PCG%tempx,m,get_face(x%DL)) ! Does not copy BCs of x
        else; stop 'Error: bad input type into init_PCG_SF in PCG.f90'
        endif
        call init(PCG%p,x) ! Copies BCs for x
        call init(PCG%r,PCG%tempx)
        call init(PCG%Ax,PCG%tempx)
        call init(PCG%vol,PCG%tempx)
        call init(PCG%k,k)
        call init(PCG%tempk,k)
        call init(PCG%z,PCG%tempx)
        call init(PCG%Minv,PCG%tempx)
        call init(PCG%ISP,ISP)

        call init(PCG%norm)
        call init(PCG%MFP,MFP)
        call volume(PCG%vol,m)
        call init(PCG%dir,dir)
        call init(PCG%name,'norm_PCG_SF_'//name)
        PCG%un = new_and_open(dir,str(PCG%name))
        PCG%un_convergence = new_and_open(dir,str(PCG%name)//'_convergence')
        call tecHeader(str(PCG%name),PCG%un,.false.)
        call tecHeader(str(PCG%name),PCG%un_convergence,.false.)
        PCG%prec => prec
        PCG%operator => operator
        PCG%operator_explicit => operator_explicit
        call assign(PCG%k,k)
        call PCG%prec(PCG%Minv,m,k,MFP%coeff)

        call init(temp_Minv,PCG%Minv)
        call assign(temp_Minv,PCG%Minv)
        if (verifyPreconditioner) then
          call export_raw(m,temp_Minv,dir,'PCG_SF_prec_tec_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_SF_prec_mat_'//str(PCG%name))
          call get_diagonal(operator_explicit,temp_Minv,x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
          call export_raw(m,temp_Minv,dir,'PCG_SF_op_tec_diag_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_SF_op_mat_diag_'//str(PCG%name))
        endif
        call delete(temp_Minv)

        if (testSymmetry) then
          call test_symmetry(operator,'PCG_SF_'//str(PCG%name),x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,'PCG_SF_'//str(PCG%name),dir,x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
          call export_matrix(PCG%Minv,dir,'PCG_SF_diag_'//str(PCG%name))
        endif
        PCG%N_iter = 0
      end subroutine

      subroutine init_PCG_VF(PCG,operator,operator_explicit,prec,m,ISP,MFP,&
        x,k,dir,name,testSymmetry,exportOperator)
        implicit none
        procedure(op_VF) :: operator
        procedure(op_VF_explicit) :: operator_explicit
        procedure(preconditioner_VF) :: prec
        type(PCG_solver_VF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(iter_solver_params),intent(in) :: ISP
        type(VF),intent(in) :: x,k
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: testSymmetry,exportOperator
        type(matrix_free_params),intent(in) :: MFP
        type(VF) :: temp_Minv
            if (is_CC(x)) then;   call init_CC(PCG%tempx,m)   ! Does not copy BCs of x
        elseif (is_Node(x)) then; call init_Node(PCG%tempx,m) ! Does not copy BCs of x
        elseif (is_Edge(x)) then; call init_Edge(PCG%tempx,m) ! Does not copy BCs of x
        elseif (is_Face(x)) then; call init_Face(PCG%tempx,m) ! Does not copy BCs of x
        else; stop 'Error: bad input type into init_PCG_VF in PCG.f90'
        endif
        call init(PCG%p,x) ! Copies BCs for x
        call init(PCG%r,PCG%tempx)
        call init(PCG%Ax,PCG%tempx)
        call init(PCG%vol,PCG%tempx)
        call init(PCG%k,k)
        call init(PCG%tempk,k)
        call init(PCG%z,PCG%tempx)
        call init(PCG%Minv,PCG%tempx)
        call init(PCG%ISP,ISP)
        call init(PCG%norm)
        call init(PCG%MFP,MFP)
        call volume(PCG%vol,m)
        call init(PCG%dir,dir)
        call init(PCG%name,'norm_PCG_VF_'//name)
        PCG%un = new_and_open(dir,str(PCG%name))
        PCG%un_convergence = new_and_open(dir,str(PCG%name)//'_convergence')
        call tecHeader(str(PCG%name),PCG%un,.true.)
        call tecHeader(str(PCG%name),PCG%un_convergence,.true.)
        PCG%prec => prec
        PCG%operator => operator
        PCG%operator_explicit => operator_explicit
        call assign(PCG%k,k)
        call PCG%prec(PCG%Minv,m,k,MFP%coeff)

        call init(temp_Minv,PCG%Minv)
        call assign(temp_Minv,PCG%Minv)
        if (verifyPreconditioner) then
          call export_raw(m,temp_Minv,dir,'PCG_VF_prec_tec_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_VF_prec_mat_'//str(PCG%name))
          call get_diagonal(operator_explicit,temp_Minv,x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
          call export_raw(m,temp_Minv,dir,'PCG_VF_op_tec_diag_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_VF_op_mat_diag_'//str(PCG%name))
        endif
        call delete(temp_Minv)

        if (testSymmetry) then
          call test_symmetry(operator,'PCG_VF_'//str(PCG%name),x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator,'PCG_VF_op_mat_'//str(PCG%name),dir,x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
        PCG%N_iter = 0
      end subroutine

      subroutine solve_PCG_SF(PCG,x,b,m,compute_norms)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        logical,intent(in) :: compute_norms
        call solve_PCG(PCG%operator,PCG%operator_explicit,str(PCG%name),&
        x,b,PCG%vol,PCG%k,m,PCG%MFP,PCG%ISP,PCG%norm,compute_norms,PCG%un,&
        PCG%un_convergence,PCG%tempx,PCG%tempk,PCG%Ax,PCG%r,PCG%p,PCG%N_iter,&
        PCG%z,PCG%Minv)
      end subroutine

      subroutine solve_PCG_VF(PCG,x,b,m,compute_norms)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        type(VF),intent(inout) :: x
        type(VF),intent(in) :: b
        type(mesh),intent(in) :: m
        logical,intent(in) :: compute_norms
        call solve_PCG(PCG%operator,PCG%operator_explicit,str(PCG%name),&
        x,b,PCG%vol,PCG%k,m,PCG%MFP,PCG%ISP,PCG%norm,compute_norms,PCG%un,&
        PCG%un_convergence,PCG%tempx,PCG%tempk,PCG%Ax,PCG%r,PCG%p,PCG%N_iter,&
        PCG%z,PCG%Minv)
      end subroutine

      subroutine delete_PCG_SF(PCG)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        call delete(PCG%r)
        call delete(PCG%p)
        call delete(PCG%tempx)
        call delete(PCG%Ax)
        call delete(PCG%vol)
        call delete(PCG%k)
        call delete(PCG%tempk)
        call delete(PCG%z)
        call delete(PCG%Minv)
        call delete(PCG%MFP)
        PCG%N_iter = 0
        close(PCG%un)
        close(PCG%un_convergence)
        call delete(PCG%dir)
        call delete(PCG%name)
      end subroutine

      subroutine delete_PCG_VF(PCG)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        call delete(PCG%r)
        call delete(PCG%p)
        call delete(PCG%tempx)
        call delete(PCG%Ax)
        call delete(PCG%vol)
        call delete(PCG%k)
        call delete(PCG%tempk)
        call delete(PCG%z)
        call delete(PCG%Minv)
        call delete(PCG%MFP)
        PCG%N_iter = 0
        close(PCG%un)
        close(PCG%un_convergence)
        call delete(PCG%dir)
        call delete(PCG%name)
      end subroutine

      subroutine tecHeader(name,un,VF)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: un
        logical,intent(in) :: VF
        type(string) :: s
        if (VF) then; write(un,*) 'TITLE = "PCG_VF residuals for '//name//'"'
        else;         write(un,*) 'TITLE = "PCG_SF residuals for '//name//'"'
        endif
        call init(s,'VARIABLES = N,stop_criteria')
        call append(s,',res_norm_L1,res_norm_L2,res_norm_Linf')
        call append(s,',res0_norm_L1,res0_norm_L2,res0_norm_Linf')
        call append(s,',iter_used')
        write(un,*) str(s)
        write(un,*) 'ZONE DATAPACKING = POINT'
        call delete(s)
        flush(un)
      end subroutine

      subroutine prolongate_PCG_SF(PCG,m,k,MFP,dir)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: k
        type(matrix_free_params),intent(in) :: MFP
        integer,intent(in) :: dir
        call prolongate(PCG%tempx,m,dir)
        call prolongate(PCG%p,m,dir)
        call prolongate(PCG%r,m,dir)
        call prolongate(PCG%Ax,m,dir)
        call prolongate(PCG%vol,m,dir)
        call prolongate(PCG%k,m,dir); call assign(PCG%k,k)
        call prolongate(PCG%tempk,m,dir)
        call prolongate(PCG%z,m,dir)
        call prolongate(PCG%Minv,m,dir)
        call init(PCG%MFP,MFP)
        call PCG%prec(PCG%Minv,m,PCG%k,PCG%MFP%coeff)
        call volume(PCG%vol,m)
      end subroutine

      subroutine prolongate_PCG_VF(PCG,m,k,MFP,dir)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: k
        type(matrix_free_params),intent(in) :: MFP
        integer,intent(in) :: dir
        call prolongate(PCG%tempx,m,dir)
        call prolongate(PCG%p,m,dir)
        call prolongate(PCG%r,m,dir)
        call prolongate(PCG%Ax,m,dir)
        call prolongate(PCG%vol,m,dir)
        call prolongate(PCG%k,m,dir); call assign(PCG%k,k)
        call prolongate(PCG%tempk,m,dir)
        call prolongate(PCG%z,m,dir)
        call prolongate(PCG%Minv,m,dir)
        call init(PCG%MFP,MFP)
        call PCG%prec(PCG%Minv,m,PCG%k,PCG%MFP%coeff)
        call volume(PCG%vol,m)
      end subroutine

      end module