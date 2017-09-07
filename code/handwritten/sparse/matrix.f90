      module matrix_mod
      use current_precision_mod
      use IO_tools_mod
      use IO_export_mod
      use mesh_extend_mod
      use ops_discrete_mod
      use ops_aux_mod
      use apply_BCs_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_mod
      use index_mapping_mod
      use ops_interp_mod
      use matrix_free_params_mod
      use matrix_free_operators_mod
      implicit none

      private

      integer,parameter :: un_max = 10**5 ! Largest allowable matrix to EXPORT
      integer :: px = 1  ! Include/Exclude ghost points along x = (0,1)
      integer :: py = 1  ! Include/Exclude ghost points along y = (0,1)
      integer :: pz = 1  ! Include/Exclude ghost points along z = (0,1)

      public :: test_symmetry
      interface test_symmetry;    module procedure test_symmetry_SF;    end interface
      interface test_symmetry;    module procedure test_symmetry_VF;    end interface

      public :: export_operator
      interface export_operator;  module procedure export_operator_SF;  end interface
      interface export_operator;  module procedure export_operator_VF;  end interface

      public :: export_matrix
      interface export_matrix;    module procedure export_matrix_SF;    end interface
      interface export_matrix;    module procedure export_matrix_VF;    end interface

      public :: get_diagonal
      interface get_diagonal;     module procedure get_diagonal_SF;     end interface
      interface get_diagonal;     module procedure get_diagonal_VF;     end interface

      interface define_ith_diag;  module procedure define_ith_diag_SF;  end interface
      interface define_ith_diag;  module procedure define_ith_diag_VF;  end interface

      interface export_transpose; module procedure export_transpose_SF; end interface
      interface export_transpose; module procedure export_transpose_VF; end interface

      contains

      ! *********************************************************************
      ! ******************* TEST MATRIX OPERATOR SYMMETRY *******************
      ! *********************************************************************

      subroutine test_symmetry_SF(operator,name,x,k,vol,m,MFP,tempk)
        implicit none
        procedure(op_SF) :: operator
        type(SF),intent(in) :: x,vol
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        character(len=*),intent(in) :: name
        type(SF) :: u,v,Au,Av,temp
        real(cp) :: dot_vAu,dot_uAv
        call init(u,x); call init(Au,x)
        call init(v,x); call init(Av,x)
        call init(temp,x)
        write(*,*) ' ---------------- SYMMETRY TEST --------------- '//name
        write(*,*) 'System size = ',x%numEl
        call random_noise(u); call assign_ghost_XPeriodic(u,0.0_cp)
        call random_noise(v); call assign_ghost_XPeriodic(v,0.0_cp)
        call init_BCs(u,x)
        call init_BCs(v,x)
        call operator(Au,u,k,m,MFP,tempk)
        call multiply(Au,vol)
        call operator(Av,v,k,m,MFP,tempk)
        call multiply(Av,vol)
        dot_vAu = dot_product(v,Au,x,temp)
        dot_uAv = dot_product(u,Av,x,temp)
        write(*,*) 'amax(v),amax(Au) = ',amax(v),amax(Au)
        write(*,*) 'amax(u),amax(Av) = ',amax(u),amax(Av)
        write(*,*) '(v,Au) = ',dot_vAu
        write(*,*) '(u,Av) = ',dot_uAv
        write(*,*) 'Symmetry error = ',abs(dot_vAu-dot_uAv)
        write(*,*) 'Symmetry error/size = ',abs(dot_vAu-dot_uAv)/real(x%numEl,cp)
        write(*,*) ' ---------------------------------------------- '
        call delete(u); call delete(Au)
        call delete(v); call delete(Av)
        call delete(temp)
      end subroutine

      subroutine test_symmetry_VF(operator,name,x,k,vol,m,MFP,tempk)
        implicit none
        procedure(op_VF) :: operator
        type(VF),intent(in) :: x,vol
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        character(len=*),intent(in) :: name
        type(VF) :: u,v,Au,Av,temp
        real(cp) :: dot_vAu,dot_uAv
        call init(u,x); call init(Au,x)
        call init(v,x); call init(Av,x)
        call init(temp,x)
        write(*,*) ' ---------------- SYMMETRY TEST --------------- '//name
        write(*,*) 'System size = ',x%x%numEl+x%y%numEl+x%z%numEl
        call random_noise(u); call assign_ghost_XPeriodic(u,0.0_cp)
        call random_noise(v); call assign_ghost_XPeriodic(v,0.0_cp)
        call init_BCs(u,x)
        call init_BCs(v,x)
        call operator(Au,u,k,m,MFP,tempk)
        call multiply(Au,vol)
        call operator(Av,v,k,m,MFP,tempk)
        call multiply(Av,vol)
        dot_vAu = dot_product(v,Au,x,temp)
        dot_uAv = dot_product(u,Av,x,temp)
        write(*,*) 'amax(v),amax(Au) = ',amax(v),amax(Au)
        write(*,*) 'amax(u),amax(Av) = ',amax(u),amax(Av)
        write(*,*) '(v,Au) = ',dot_vAu
        write(*,*) '(u,Av) = ',dot_uAv
        write(*,*) 'Symmetry error = ',abs(dot_vAu-dot_uAv)
        write(*,*) 'Symmetry error/size = ',abs(dot_vAu-dot_uAv)/real(x%x%numEl,cp)
        write(*,*) ' ---------------------------------------------- '
        call delete(u); call delete(Au)
        call delete(v); call delete(Av)
        call delete(temp)
      end subroutine

      ! *********************************************************************
      ! ********************** EXPORTING GIVEN MATRIX ***********************
      ! *********************************************************************

      subroutine export_matrix_SF(D,dir,name)
        implicit none
        type(SF),intent(in) :: D
        character(len=*),intent(in) :: dir,name
        type(SF) :: un
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,D)
        newU = new_and_open(dir,name)
        write(*,*) ' ------------- EXPORTING SF MATRIX ------------ '//name
        call assign(un,0.0_cp)
        do i=1,un%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un,i)
          if (cycle_TF_SF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/))) cycle
          call unitVector(un,i)
          call multiply(un,D)
          call export_transpose_SF(un,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un,i)
        enddo
        close(newU); call delete(un)
        write(*,*) ' ---------------------------------------------- '
      end subroutine

      subroutine export_matrix_VF(D,dir,name)
        implicit none
        type(VF),intent(in) :: D
        character(len=*),intent(in) :: dir,name
        type(VF) :: un
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,D)
        newU = new_and_open(dir,name)
        write(*,*) ' ------------- EXPORTING VF MATRIX ------------ '//name
        write(*,*) 'System size = ',un%x%numEl + un%y%numEl + un%z%numEl
        call assign(un,0.0_cp)
        do i=1,un%x%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%x,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),1)) cycle
          call unitVector(un%x,i)
          call multiply(un%x,D%x)
          call export_transpose(un,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un%x,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%y%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%y,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),2)) cycle
          call unitVector(un%y,i)
          call multiply(un%y,D%y)
          call export_transpose(un,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un%y,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%z%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%z,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),3)) cycle
          call unitVector(un%z,i)
          call multiply(un%z,D%z)
          call export_transpose(un,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un%z,i)
        enddo
        close(newU); call delete(un)
        write(*,*) ' ---------------------------------------------- '
      end subroutine

      function cycle_TF_SF(U,i,t,p) result(L_any)
        implicit none
        type(SF),intent(in) :: U
        integer,intent(in) :: t
        integer,dimension(3),intent(in) :: i,p
        logical,dimension(3) :: L
        logical :: L_any
        integer :: k
        do k=1,3;L(k)=((p(k).eq.1).and.((i(k).eq.1).or.(i(k).eq.u%BF(t)%GF%s(k)))); enddo
        L_any = any(L)
      end function

      function cycle_TF_VF(U,i,t,p,comp) result(L_any)
        implicit none
        type(VF),intent(in) :: U
        integer,intent(in) :: t,comp
        integer,dimension(3),intent(in) :: i,p
        logical,dimension(3) :: L
        logical :: L_any
        integer :: k
        select case(comp)
        case(1); do k=1,3;L(k)=((p(k).eq.1).and.((i(k).eq.1).or.(i(k).eq.u%x%BF(t)%GF%s(k)))); enddo
        case(2); do k=1,3;L(k)=((p(k).eq.1).and.((i(k).eq.1).or.(i(k).eq.u%y%BF(t)%GF%s(k)))); enddo
        case(3); do k=1,3;L(k)=((p(k).eq.1).and.((i(k).eq.1).or.(i(k).eq.u%z%BF(t)%GF%s(k)))); enddo
        case default; stop 'Error: dir must =1,2,3 in cycle_TF_VF in matrix.f90'
        end select
        L_any = any(L)
      end function

      ! *********************************************************************
      ! ******************** EXPORTING MATRIX OPERATOR **********************
      ! *********************************************************************

      subroutine export_operator_SF(operator,dir,name,x,k,vol,m,MFP,tempk)
        implicit none
        procedure(op_SF) :: operator
        type(SF),intent(in) :: x,vol
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        character(len=*),intent(in) :: dir,name
        type(SF) :: un,Aun
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,x); call init(Aun,x)
        newU = new_and_open(dir,name)
        write(*,*) ' ------------ EXPORTING SF OPERATOR ----------- '//name
        call assign(un,0.0_cp)
        call init_BCs(un,x)
        do i=1,un%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un,i)
          if (cycle_TF_SF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/))) cycle
          call unitVector(un,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call export_transpose(Aun,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un,i)
        enddo
        call delete(un); call delete(Aun)
        close(newU)
      end subroutine

      subroutine export_operator_VF(operator,dir,name,x,k,vol,m,MFP,tempk)
        implicit none
        procedure(op_VF) :: operator
        type(VF),intent(in) :: x,vol
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        character(len=*),intent(in) :: dir,name
        type(VF) :: un,Aun
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,x); call init(Aun,x)
        newU = new_and_open(dir,name)
        write(*,*) ' ------------ EXPORTING VF OPERATOR ----------- '//name
        write(*,*) 'System size = ',un%x%numEl + un%y%numEl + un%z%numEl
        call assign(un,0.0_cp)
        call init_BCs(un,x)
        do i=1,un%x%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%x,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),1)) cycle
          call unitVector(un%x,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call export_transpose(Aun,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un%x,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%y%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%y,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),2)) cycle
          call unitVector(un%y,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call export_transpose(Aun,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un%y,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%z%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%z,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),3)) cycle
          call unitVector(un%z,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call export_transpose(Aun,newU,px,py,pz) ! Export rows of A
          call deleteUnitVector(un%z,i)
        enddo
        close(newU)
        call delete(un); call delete(Aun)
      end subroutine

      subroutine export_transpose_SF(U,un,px,py,pz)
        implicit none
        type(SF),intent(in) :: U
        integer,intent(in) :: un,px,py,pz
        integer :: i,j,k,t
        if (U%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_SF in matrix.f90'
        do t=1,U%s; do k=1+pz,u%BF(t)%GF%s(3)-pz; do j=1+py,u%BF(t)%GF%s(2)-py; do i=1+px,u%BF(t)%GF%s(1)-px
        write(un,'(F20.13,T2)',advance='no') u%BF(t)%GF%f(i,j,k)
        enddo; enddo; enddo; enddo
        write(un,*) ''
      end subroutine

      subroutine export_transpose_VF(U,un,px,py,pz)
        implicit none
        type(VF),intent(in) :: U
        integer,intent(in) :: un,px,py,pz
        integer :: i,j,k,t
        if (U%x%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_VF in matrix.f90'
        if (U%y%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_VF in matrix.f90'
        if (U%z%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_VF in matrix.f90'
        do t=1,U%x%s; do k=1+pz,u%x%BF(t)%GF%s(3)-pz; do j=1+py,u%x%BF(t)%GF%s(2)-py; do i=1+px,u%x%BF(t)%GF%s(1)-px
        write(un,'(F15.8,T2)',advance='no') u%x%BF(t)%GF%f(i,j,k)
        enddo; enddo; enddo; enddo
        do t=1,U%y%s; do k=1+pz,u%y%BF(t)%GF%s(3)-pz; do j=1+py,u%y%BF(t)%GF%s(2)-py; do i=1+px,u%y%BF(t)%GF%s(1)-px
        write(un,'(F15.8,T2)',advance='no') u%y%BF(t)%GF%f(i,j,k)
        enddo; enddo; enddo; enddo
        do t=1,U%z%s; do k=1+pz,u%z%BF(t)%GF%s(3)-pz; do j=1+py,u%z%BF(t)%GF%s(2)-py; do i=1+px,u%z%BF(t)%GF%s(1)-px
        write(un,'(F15.8,T2)',advance='no') u%z%BF(t)%GF%f(i,j,k)
        enddo; enddo; enddo; enddo
        write(un,*) ''
      end subroutine

      ! *********************************************************************
      ! ********************* OBTAINING MATRIX DIAGONAL *********************
      ! *********************************************************************

      subroutine get_diagonal_SF(operator,D,x,k,vol,m,MFP,tempk)
        implicit none
        procedure(op_SF_explicit) :: operator
        type(SF),intent(inout) :: D
        type(SF),intent(in) :: vol,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(SF) :: un,Aun
        integer :: i,n,nmax,nwrite,i_3D,j_3D,k_3D,t_3D
        call init(un,D); call init(Aun,D)
        call assign(un,0.0_cp); call assign(D,0.0_cp)
        n = 0; nmax = un%numEl
        nwrite = 100
        call init_BCs(un,x)
        do i=1,un%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un,i)
          if (cycle_TF_SF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/))) cycle
          call unitVector(un,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call define_ith_diag(D,Aun,i)
          call deleteUnitVector(un,i)
          n = n + 1
          if (mod(n,nwrite).eq.0) then
            write(*,*) 'Percentage complete (computing diagonal) = ',real(n,cp)/real(nmax,cp)*100.0_cp
          endif
        enddo
        call delete(un); call delete(Aun)
      end subroutine

      subroutine define_ith_diag_SF(D,Aun,col)
        implicit none
        type(SF),intent(inout) :: D
        type(SF),intent(in) :: Aun
        integer,intent(in) :: col
        integer :: i,j,k,t
        call get_3D_index(i,j,k,t,D,col)
        D%BF(t)%GF%f(i,j,k) = Aun%BF(t)%GF%f(i,j,k)
      end subroutine

      subroutine get_diagonal_VF(operator,D,x,k,vol,m,MFP,tempk)
        implicit none
        procedure(op_VF_explicit) :: operator
        type(VF),intent(inout) :: D
        type(VF),intent(in) :: vol,x
        type(TF),intent(in) :: k
        type(TF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(matrix_free_params),intent(in) :: MFP
        type(VF) :: un,Aun
        integer :: i,n,nmax,nwrite,i_3D,j_3D,k_3D,t_3D
        call init(un,D); call init(Aun,D)
        call assign(un,0.0_cp); call assign(D,0.0_cp)
        n = 0; nmax = un%x%numEl + un%y%numEl + un%z%numEl
        nwrite = 500
        write(*,*) 'Computing matrix diagonal...'
        call init_BCs(un,x)
        do i=1,un%x%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%x,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),1)) cycle
          call unitVector(un%x,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call define_ith_diag(D,Aun,i,1)
          call deleteUnitVector(un%x,i)
          n = n + 1
          if (mod(n,nwrite).eq.0) then
            write(*,*) 'diag(x) %, un = ',real(n,cp)/real(nmax,cp)*100.0_cp,i
          endif
        enddo
        call assign(un,0.0_cp)
        do i=1,un%y%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%y,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),2)) cycle
          call unitVector(un%y,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call define_ith_diag(D,Aun,i,2)
          call deleteUnitVector(un%y,i)
          n = n + 1
          if (mod(n,nwrite).eq.0) then
            write(*,*) 'diag(y) %, un = ',real(n,cp)/real(nmax,cp)*100.0_cp,i
          endif
        enddo
        call assign(un,0.0_cp)
        do i=1,un%z%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%z,i)
          if (cycle_TF_VF(un,(/i_3D,j_3D,k_3D/),t_3D,(/px,py,pz/),3)) cycle
          call unitVector(un%z,i)
          call operator(Aun,un,k,m,MFP,tempk)
          call multiply(Aun,vol)
          call define_ith_diag(D,Aun,i,3)
          call deleteUnitVector(un%z,i)
          n = n + 1
          if (mod(n,nwrite).eq.0) then
            write(*,*) 'diag(z) %, un = ',real(n,cp)/real(nmax,cp)*100.0_cp,i
          endif
        enddo
        call delete(un); call delete(Aun)
      end subroutine

      subroutine define_ith_diag_VF(D,Aun,col,component)
        implicit none
        type(VF),intent(inout) :: D
        type(VF),intent(in) :: Aun
        integer,intent(in) :: col,component
        integer :: i,j,k,t
        select case (component)
        case (1); call get_3D_index(i,j,k,t,D%x,col); D%x%BF(t)%GF%f(i,j,k) = Aun%x%BF(t)%GF%f(i,j,k)
        case (2); call get_3D_index(i,j,k,t,D%y,col); D%y%BF(t)%GF%f(i,j,k) = Aun%y%BF(t)%GF%f(i,j,k)
        case (3); call get_3D_index(i,j,k,t,D%z,col); D%z%BF(t)%GF%f(i,j,k) = Aun%z%BF(t)%GF%f(i,j,k)
        case default; stop 'Error: dir must = 1,2,3 in define_ith_diag_VF in matrix.f90'
        end select
      end subroutine

      end module