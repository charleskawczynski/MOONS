      module PCG_mod
      use current_precision_mod
      use string_mod
      use IO_export_mod
      use mesh_extend_mod
      use norms_mod
      use data_location_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_mod
      use IO_tools_mod
      use export_raw_processed_mod

      use iter_solver_params_mod
      use iter_solver_params_extend_mod
      use preconditioners_mod
      use matrix_mod
      use PCG_solver_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private
      public :: PCG_solver_SF
      public :: PCG_solver_VF
      public :: init,delete,export,import
      ! public :: init,delete,display,print,export,import
      public :: solve
      public :: prolongate
      public :: update_MFP

      logical :: verifyPreconditioner = .false.

      interface init;       module procedure init_PCG_SF;       end interface
      interface init;       module procedure init_PCG_VF;       end interface

      interface solve;      module procedure solve_PCG_SF;      end interface
      interface solve;      module procedure solve_PCG_VF;      end interface

      interface prolongate; module procedure prolongate_PCG_SF; end interface
      interface prolongate; module procedure prolongate_PCG_VF; end interface

      interface update_MFP; module procedure update_MFP_PCG_SF; end interface
      interface update_MFP; module procedure update_MFP_PCG_VF; end interface

      interface delete;     module procedure delete_PCG_SF;     end interface
      interface delete;     module procedure delete_PCG_VF;     end interface

      interface export;     module procedure export_PCG_SF;     end interface
      interface export;     module procedure export_PCG_VF;     end interface
      interface import;     module procedure import_PCG_SF;     end interface
      interface import;     module procedure import_PCG_VF;     end interface

      interface export;     module procedure export_PCG_SF_wrapper;     end interface
      interface export;     module procedure export_PCG_VF_wrapper;     end interface
      interface import;     module procedure import_PCG_SF_wrapper;     end interface
      interface import;     module procedure import_PCG_VF_wrapper;     end interface

      type PCG_solver_SF
        type(matrix_free_params) :: MFP
        type(TF) :: tempk,k
        type(SF) :: r,p,tempx,Ax,x_BC,vol,z,Minv
        type(norms) :: norm
        integer :: un,un_convergence
        type(iter_solver_params) :: ISP
        type(string) :: dir,name
        procedure(preconditioner_SF),pointer,nopass :: prec
        procedure(op_SF),pointer,nopass :: operator
        procedure(op_SF_explicit),pointer,nopass :: operator_explicit
      end type

      type PCG_solver_VF
        type(matrix_free_params) :: MFP
        type(TF) :: tempk,k
        type(VF) :: r,p,tempx,Ax,x_BC,vol,z,Minv
        type(norms) :: norm
        integer :: un,un_convergence
        type(iter_solver_params) :: ISP
        type(string) :: dir,name
        procedure(preconditioner_VF),pointer,nopass :: prec
        procedure(op_VF),pointer,nopass :: operator
        procedure(op_VF_explicit),pointer,nopass :: operator_explicit
      end type

      contains

      subroutine init_PCG_SF(PCG,operator,operator_explicit,prec,m,ISP,MFP,&
        x,x_BC,k,dir,name,testSymmetry,exportOperator)
        implicit none
        procedure(op_SF) :: operator
        procedure(op_SF_explicit) :: operator_explicit
        procedure(preconditioner_SF) :: prec
        type(PCG_solver_SF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(iter_solver_params),intent(in) :: ISP
        type(SF),intent(in) :: x,x_BC
        type(TF),intent(in) :: k
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
        call init(PCG%x_BC,x_BC) ! Copies BCs for x
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
        call init(PCG%name,name)
        PCG%un = new_and_open(dir,str(PCG%name))
        PCG%un_convergence = new_and_open(dir,str(PCG%name)//'_convergence')
        call tecHeader(str(PCG%name),PCG%un,.false.)
        call tecHeader(str(PCG%name),PCG%un_convergence,.false.)
        PCG%prec => prec
        PCG%operator => operator
        PCG%operator_explicit => operator_explicit
        call assign(PCG%k,k)
        call PCG%prec(PCG%Minv,m,k,MFP%coeff_implicit,PCG%tempx) ! MFP%coeff_implicit reasonable estimate

        if (verifyPreconditioner) then
          call init(temp_Minv,PCG%Minv)
          call assign(temp_Minv,PCG%Minv)
          call export_raw(m,temp_Minv,dir,'PCG_SF_prec_tec_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_SF_prec_mat_'//str(PCG%name))
          call get_diagonal(operator_explicit,temp_Minv,x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
          call export_raw(m,temp_Minv,dir,'PCG_SF_op_tec_diag_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_SF_op_mat_diag_'//str(PCG%name))
          call delete(temp_Minv)
        endif

        if (testSymmetry) then
          call test_symmetry(operator,'PCG_SF_'//str(PCG%name),x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator_explicit,dir,'PCG_SF_'//str(PCG%name),x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
      end subroutine

      subroutine init_PCG_VF(PCG,operator,operator_explicit,prec,m,ISP,MFP,&
        x,x_BC,k,dir,name,testSymmetry,exportOperator)
        implicit none
        procedure(op_VF) :: operator
        procedure(op_VF_explicit) :: operator_explicit
        procedure(preconditioner_VF) :: prec
        type(PCG_solver_VF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(iter_solver_params),intent(in) :: ISP
        type(VF),intent(in) :: x,x_BC
        type(TF),intent(in) :: k
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
        call init(PCG%x_BC,x_BC) ! Copies BCs for x
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
        call init(PCG%name,name)
        PCG%un = new_and_open(dir,str(PCG%name))
        PCG%un_convergence = new_and_open(dir,str(PCG%name)//'_convergence')
        call tecHeader(str(PCG%name),PCG%un,.true.)
        call tecHeader(str(PCG%name),PCG%un_convergence,.true.)
        PCG%prec => prec
        PCG%operator => operator
        PCG%operator_explicit => operator_explicit
        call assign(PCG%k,k)
        call PCG%prec(PCG%Minv,m,k,MFP%coeff_implicit,PCG%tempx) ! MFP%coeff_implicit reasonable estimate

        if (verifyPreconditioner) then
          call init(temp_Minv,PCG%Minv)
          call assign(temp_Minv,PCG%Minv)
          call export_raw(m,temp_Minv,dir,'PCG_VF_prec_tec_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_VF_prec_mat_'//str(PCG%name))
          call get_diagonal(operator_explicit,temp_Minv,x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
          call export_raw(m,temp_Minv,dir,'PCG_VF_op_tec_diag_'//str(PCG%name),0)
          call export_matrix(temp_Minv,dir,'PCG_VF_op_mat_diag_'//str(PCG%name))
          call delete(temp_Minv)
        endif

        if (testSymmetry) then
          call test_symmetry(operator,'PCG_VF_'//str(PCG%name),x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
        if (exportOperator) then
          call export_operator(operator_explicit,dir,'PCG_VF_'//str(PCG%name),x,PCG%k,PCG%vol,m,MFP,PCG%tempk)
        endif
      end subroutine

      subroutine solve_PCG_SF(PCG,x,b,m,compute_norms)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        type(SF),intent(inout) :: x
        type(SF),intent(in) :: b
        type(mesh),intent(in) :: m
        logical,intent(in) :: compute_norms
        call solve_PCG(PCG%operator,PCG%operator_explicit,str(PCG%name),&
        x,PCG%x_BC,b,PCG%vol,PCG%k,m,PCG%MFP,PCG%ISP,PCG%norm,compute_norms,PCG%un,&
        PCG%un_convergence,PCG%tempx,PCG%tempk,PCG%Ax,PCG%r,PCG%p,&
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
        x,PCG%x_BC,b,PCG%vol,PCG%k,m,PCG%MFP,PCG%ISP,PCG%norm,compute_norms,PCG%un,&
        PCG%un_convergence,PCG%tempx,PCG%tempk,PCG%Ax,PCG%r,PCG%p,&
        PCG%z,PCG%Minv)
      end subroutine

      subroutine delete_PCG_SF(PCG)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        call delete(PCG%r)
        call delete(PCG%p)
        call delete(PCG%tempx)
        call delete(PCG%Ax)
        call delete(PCG%x_BC)
        call delete(PCG%vol)
        call delete(PCG%k)
        call delete(PCG%tempk)
        call delete(PCG%z)
        call delete(PCG%Minv)
        call delete(PCG%MFP)
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
        call delete(PCG%x_BC)
        call delete(PCG%vol)
        call delete(PCG%k)
        call delete(PCG%tempk)
        call delete(PCG%z)
        call delete(PCG%Minv)
        call delete(PCG%MFP)
        close(PCG%un)
        close(PCG%un_convergence)
        call delete(PCG%dir)
        call delete(PCG%name)
      end subroutine

      subroutine export_PCG_SF(PCG,un)
        implicit none
        type(PCG_solver_SF),intent(in) :: PCG
        integer,intent(in) :: un
        write(un,*) PCG%un
        write(un,*) PCG%un_convergence
        call export(PCG%ISP,un)
        call export(PCG%dir,un)
        call export(PCG%name,un)
        call export(PCG%norm,un)
        call export(PCG%r,un)
        call export(PCG%p,un)
        call export(PCG%tempx,un)
        call export(PCG%Ax,un)
        call export(PCG%x_BC,un)
        call export(PCG%vol,un)
        call export(PCG%z,un)
        call export(PCG%Minv,un)
        call export(PCG%k,un)
        call export(PCG%tempk,un)
        call export(PCG%MFP,un)
      end subroutine

      subroutine export_PCG_VF(PCG,un)
        implicit none
        type(PCG_solver_VF),intent(in) :: PCG
        integer,intent(in) :: un
        write(un,*) PCG%un
        write(un,*) PCG%un_convergence
        call export(PCG%ISP,un)
        call export(PCG%dir,un)
        call export(PCG%name,un)
        call export(PCG%norm,un)
        call export(PCG%r,un)
        call export(PCG%p,un)
        call export(PCG%tempx,un)
        call export(PCG%Ax,un)
        call export(PCG%x_BC,un)
        call export(PCG%vol,un)
        call export(PCG%z,un)
        call export(PCG%Minv,un)
        call export(PCG%k,un)
        call export(PCG%tempk,un)
        call export(PCG%MFP,un)
      end subroutine

      subroutine import_PCG_SF(PCG,un)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        integer,intent(in) :: un
        read(un,*) PCG%un
        read(un,*) PCG%un_convergence
        call import(PCG%ISP,un)
        call import(PCG%dir,un)
        call import(PCG%name,un)
        call import(PCG%norm,un)
        call import(PCG%r,un)
        call import(PCG%p,un)
        call import(PCG%tempx,un)
        call import(PCG%Ax,un)
        call import(PCG%x_BC,un)
        call import(PCG%vol,un)
        call import(PCG%z,un)
        call import(PCG%Minv,un)
        call import(PCG%k,un)
        call import(PCG%tempk,un)
        call import(PCG%MFP,un)
      end subroutine

      subroutine import_PCG_VF(PCG,un)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        integer,intent(in) :: un
        read(un,*) PCG%un
        read(un,*) PCG%un_convergence
        call import(PCG%ISP,un)
        call import(PCG%dir,un)
        call import(PCG%name,un)
        call import(PCG%norm,un)
        call import(PCG%r,un)
        call import(PCG%p,un)
        call import(PCG%tempx,un)
        call import(PCG%Ax,un)
        call import(PCG%x_BC,un)
        call import(PCG%vol,un)
        call import(PCG%z,un)
        call import(PCG%Minv,un)
        call import(PCG%k,un)
        call import(PCG%tempk,un)
        call import(PCG%MFP,un)
      end subroutine

      subroutine export_PCG_SF_wrapper(PCG,dir,name)
        implicit none
        type(PCG_solver_SF),intent(in) :: PCG
        character(len=*),intent(in) :: dir,name
        integer :: un
        un = new_and_open(dir,name)
        call export(PCG,un)
        close(un)
      end subroutine

      subroutine export_PCG_VF_wrapper(PCG,dir,name)
        implicit none
        type(PCG_solver_VF),intent(in) :: PCG
        character(len=*),intent(in) :: dir,name
        integer :: un
        un = new_and_open(dir,name)
        call export(PCG,un)
        close(un)
      end subroutine

      subroutine import_PCG_SF_wrapper(PCG,dir,name)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        character(len=*),intent(in) :: dir,name
        integer :: un
        un = open_to_read(dir,name)
        call import(PCG,un)
        close(un)
      end subroutine

      subroutine import_PCG_VF_wrapper(PCG,dir,name)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        character(len=*),intent(in) :: dir,name
        integer :: un
        un = open_to_read(dir,name)
        call import(PCG,un)
        close(un)
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
        call init(s,'VARIABLES = iter_total,stop_criteria')
        call append(s,',res_norm_L1,res_norm_L2,res_norm_Linf')
        call append(s,',res0_norm_L1,res0_norm_L2,res0_norm_Linf')
        call append(s,',iter_per_call')
        write(un,*) str(s)
        write(un,*) 'ZONE DATAPACKING = POINT'
        call delete(s)
        flush(un)
      end subroutine

      subroutine prolongate_PCG_SF(PCG,m,k,MFP,dir)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(TF),intent(in) :: k
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
        call PCG%prec(PCG%Minv,m,PCG%k,PCG%MFP%coeff_implicit_time_split,PCG%tempx)
        call volume(PCG%vol,m)
      end subroutine

      subroutine prolongate_PCG_VF(PCG,m,k,MFP,dir)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        type(TF),intent(in) :: k
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
        call PCG%prec(PCG%Minv,m,PCG%k,PCG%MFP%coeff_implicit_time_split,PCG%tempx)
        call volume(PCG%vol,m)
      end subroutine

      subroutine update_MFP_PCG_SF(PCG,m,coeff_implicit_time_split,update_preconditioner)
        implicit none
        type(PCG_solver_SF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: coeff_implicit_time_split
        logical,intent(in) :: update_preconditioner
        PCG%MFP%coeff_implicit_time_split = coeff_implicit_time_split
        if (update_preconditioner) then
          call PCG%prec(PCG%Minv,m,PCG%k,PCG%MFP%coeff_implicit_time_split,PCG%tempx)
        endif
      end subroutine

      subroutine update_MFP_PCG_VF(PCG,m,coeff_implicit_time_split,update_preconditioner)
        implicit none
        type(PCG_solver_VF),intent(inout) :: PCG
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: coeff_implicit_time_split
        logical,intent(in) :: update_preconditioner
        PCG%MFP%coeff_implicit_time_split = coeff_implicit_time_split
        if (update_preconditioner) then
          call PCG%prec(PCG%Minv,m,PCG%k,PCG%MFP%coeff_implicit_time_split,PCG%tempx)
        endif
      end subroutine

      end module