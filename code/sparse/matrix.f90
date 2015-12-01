      module matrix_mod
      use IO_tools_mod
      use IO_SF_mod
      use IO_VF_mod
      use mesh_mod
      use ops_discrete_mod
      use ops_aux_mod
      use SF_mod
      use VF_mod
      use ops_interp_mod
      implicit none

      private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      integer,parameter :: un_max = 10**5 ! Largest allowable matrix to EXPORT

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

      interface zeroSecondIndex;  module procedure zeroSecondIndex_RF;  end interface
      interface zeroSecondIndex;  module procedure zeroSecondIndex_SF;  end interface
      interface zeroSecondIndex;  module procedure zeroSecondIndex_VF;  end interface

      contains

      ! *********************************************************************
      ! ******************* TEST MATRIX OPERATOR SYMMETRY *******************
      ! *********************************************************************

      subroutine test_symmetry_SF(operator,name,x,vol,m,tempk,c,k)
        implicit none
        external :: operator
        type(SF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        real(cp),intent(in) :: c
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: name
        type(SF) :: u,v,Au,Av,temp
        real(cp) :: dot_vAu,dot_uAv
        call init(u,x); call init(Au,x)
        call init(v,x); call init(Av,x)
        call init(temp,x)
        write(*,*) ' ---------------- SYMMETRY TEST --------------- '//name
        write(*,*) 'System size = ',x%numEl
        call noise(u); call zeroGhostPoints(u) ! ; call zeroSecondIndex(u)
        call noise(v); call zeroGhostPoints(v) ! ; call zeroSecondIndex(v)
        call operator(Au,u,vol,m,tempk,c,k)
        call operator(Av,v,vol,m,tempk,c,k)
        dot_vAu = dot_product(v,Au,m,x,temp)
        dot_uAv = dot_product(u,Av,m,x,temp)
        write(*,*) '(v,Au) = ',dot_vAu
        write(*,*) '(u,Av) = ',dot_uAv
        write(*,*) 'Symmetry error = ',abs(dot_vAu-dot_uAv)
        write(*,*) 'Symmetry error/size = ',abs(dot_vAu-dot_uAv)/real(x%numEl,cp)
        write(*,*) ' ---------------------------------------------- '
        call delete(u); call delete(Au)
        call delete(v); call delete(Av)
        call delete(temp)
      end subroutine

      subroutine test_symmetry_VF(operator,name,x,vol,m,tempk,c,k)
        implicit none
        external :: operator
        type(VF),intent(in) :: x,k,vol
        real(cp),intent(in) :: c
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: name
        type(VF) :: u,v,Au,Av,temp
        real(cp) :: dot_vAu,dot_uAv
        call init(u,x); call init(Au,x)
        call init(v,x); call init(Av,x)
        call init(temp,x)
        write(*,*) ' ---------------- SYMMETRY TEST --------------- '//name
        write(*,*) 'System size = ',x%x%numEl+x%y%numEl+x%z%numEl
        call noise(u); call zeroGhostPoints(u) ! ; call zeroSecondIndex(u)
        call noise(v); call zeroGhostPoints(v) ! ; call zeroSecondIndex(v)
        call operator(Au,u,vol,m,tempk,c,k)
        call operator(Av,v,vol,m,tempk,c,k)
        dot_vAu = dot_product(v,Au,m,x,temp)
        dot_uAv = dot_product(u,Av,m,x,temp)
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

      subroutine export_matrix_SF(D,m,dir,name)
        implicit none
        type(SF),intent(in) :: D
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        type(SF) :: un
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,D)
        newU = newAndOpen(dir,name)
        write(*,*) ' ------------- EXPORTING SF MATRIX ------------ '//name
        call assign(un,0.0_cp)
        do i=1,un%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%RF(t_3D)%s(3))) cycle
          call unitVector(un,i)
          call multiply(un,D)
          call export_transpose_SF(un,newU) ! Export rows of A
          call deleteUnitVector(un,i)
        enddo
        close(newU); call delete(un)
        write(*,*) ' ---------------------------------------------- '
      end subroutine

      subroutine export_matrix_VF(D,m,dir,name)
        implicit none
        type(VF),intent(in) :: D
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        type(VF) :: un
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,D)
        newU = newAndOpen(dir,name)
        write(*,*) ' ------------- EXPORTING VF MATRIX ------------ '//name
        write(*,*) 'System size = ',un%x%numEl + un%y%numEl + un%z%numEl
        call assign(un,0.0_cp)
        do i=1,un%x%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%x,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%x%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%x%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%x%RF(t_3D)%s(3))) cycle
          call unitVector(un%x,i)
          call multiply(un%x,D%x)
          call export_transpose(un,newU) ! Export rows of A
          call deleteUnitVector(un%x,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%y%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%y,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%y%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%y%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%y%RF(t_3D)%s(3))) cycle
          call unitVector(un%y,i)
          call multiply(un%y,D%y)
          call export_transpose(un,newU) ! Export rows of A
          call deleteUnitVector(un%y,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%z%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%z,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%z%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%z%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%z%RF(t_3D)%s(3))) cycle
          call unitVector(un%z,i)
          call multiply(un%z,D%z)
          call export_transpose(un,newU) ! Export rows of A
          call deleteUnitVector(un%z,i)
        enddo
        close(newU); call delete(un)
        write(*,*) ' ---------------------------------------------- '
      end subroutine

      ! *********************************************************************
      ! ******************** EXPORTING MATRIX OPERATOR **********************
      ! *********************************************************************

      subroutine export_operator_SF(operator,name,dir,x,vol,m,tempk,c,k)
        implicit none
        external :: operator
        type(SF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        real(cp),intent(in) :: c
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        type(SF) :: un,Aun
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,x); call init(Aun,x)
        newU = newAndOpen(dir,name)
        write(*,*) ' ------------ EXPORTING SF OPERATOR ----------- '//name
        call assign(un,0.0_cp)
        do i=1,un%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%RF(t_3D)%s(3))) cycle
          call unitVector(un,i)
          call operator(Aun,un,vol,m,tempk,c,k)
          call export_transpose_SF(Aun,newU) ! Export rows of A
          call deleteUnitVector(un,i)
        enddo
        call delete(un); call delete(Aun)
        close(newU)
      end subroutine

      subroutine export_operator_VF(operator,name,dir,x,vol,m,tempk,c,k)
        implicit none
        external :: operator
        type(VF),intent(in) :: x,vol
        type(VF),intent(in) :: k
        real(cp),intent(in) :: c
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        character(len=*),intent(in) :: dir,name
        type(VF) :: un,Aun
        integer :: i,newU,i_3D,j_3D,k_3D,t_3D
        call init(un,x); call init(Aun,x)
        newU = newAndOpen(dir,name)
        write(*,*) ' ------------ EXPORTING VF OPERATOR ----------- '//name
        write(*,*) 'System size = ',un%x%numEl + un%y%numEl + un%z%numEl
        call assign(un,0.0_cp)
        do i=1,un%x%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%x,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%x%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%x%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%x%RF(t_3D)%s(3))) cycle
          call unitVector(un%x,i)
          call operator(Aun,un,vol,m,tempk,c,k)
          call export_transpose(Aun,newU) ! Export rows of A
          call deleteUnitVector(un%x,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%y%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%y,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%y%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%y%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%y%RF(t_3D)%s(3))) cycle
          call unitVector(un%y,i)
          call operator(Aun,un,vol,m,tempk,c,k)
          call export_transpose(Aun,newU) ! Export rows of A
          call deleteUnitVector(un%y,i)
        enddo
        call assign(un,0.0_cp)
        do i=1,un%z%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%z,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%z%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%z%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%z%RF(t_3D)%s(3))) cycle
          call unitVector(un%z,i)
          call operator(Aun,un,vol,m,tempk,c,k)
          call export_transpose(Aun,newU) ! Export rows of A
          call deleteUnitVector(un%z,i)
        enddo
        close(newU)
        call delete(un); call delete(Aun)
      end subroutine

      subroutine export_transpose_SF(U,un)
        implicit none
        type(SF),intent(in) :: U
        integer,intent(in) :: un
        integer :: i,j,k,t
        if (U%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_SF in matrix.f90'
        do t=1,U%s; do k=2,U%RF(t)%s(3)-1; do j=2,U%RF(t)%s(2)-1; do i=2,U%RF(t)%s(1)-1
        write(un,'(F20.13,T2)',advance='no') U%RF(t)%f(i,j,k)
        enddo; enddo; enddo; enddo
        write(un,*) ''
      end subroutine

      subroutine export_transpose_VF(U,un)
        implicit none
        type(VF),intent(in) :: U
        integer,intent(in) :: un
        integer :: i,j,k,t
        if (U%x%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_SF in matrix.f90'
        if (U%y%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_SF in matrix.f90'
        if (U%z%numEl.gt.un_max) stop 'Error: trying to export HUGE matrix in export_transpose_SF in matrix.f90'
        do t=1,U%x%s; do k=2,U%x%RF(t)%s(3)-1; do j=2,U%x%RF(t)%s(2)-1; do i=2,U%x%RF(t)%s(1)-1
        write(un,'(F15.8,T2)',advance='no') U%x%RF(t)%f(i,j,k)
        enddo; enddo; enddo; enddo
        do t=1,U%y%s; do k=2,U%y%RF(t)%s(3)-1; do j=2,U%y%RF(t)%s(2)-1; do i=2,U%y%RF(t)%s(1)-1
        write(un,'(F15.8,T2)',advance='no') U%y%RF(t)%f(i,j,k)
        enddo; enddo; enddo; enddo
        do t=1,U%z%s; do k=2,U%z%RF(t)%s(3)-1; do j=2,U%z%RF(t)%s(2)-1; do i=2,U%z%RF(t)%s(1)-1
        write(un,'(F15.8,T2)',advance='no') U%z%RF(t)%f(i,j,k)
        enddo; enddo; enddo; enddo
        write(un,*) ''
      end subroutine

      ! *********************************************************************
      ! ********************* OBTAINING MATRIX DIAGONAL *********************
      ! *********************************************************************

      subroutine get_diagonal_SF(operator,D,vol,m,tempk,c,k)
        implicit none
        external :: operator
        type(SF),intent(inout) :: D
        type(SF),intent(in) :: vol
        type(VF),intent(in) :: k
        real(cp),intent(in) :: c
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(SF) :: un,Aun
        integer :: i,n,nmax,nwrite,i_3D,j_3D,k_3D,t_3D
        call init(un,D); call init(Aun,D)
        call assign(un,0.0_cp); call assign(D,0.0_cp)
        n = 0; nmax = un%numEl
        nwrite = 100
        do i=1,un%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%RF(t_3D)%s(3))) cycle
          call unitVector(un,i)
          call operator(Aun,un,vol,m,tempk,c,k,i)
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
        D%RF(t)%f(i,j,k) = Aun%RF(t)%f(i,j,k)
      end subroutine

      subroutine get_diagonal_VF(operator,D,vol,m,tempk,c,k)
        implicit none
        external :: operator
        type(VF),intent(inout) :: D
        type(VF),intent(in) :: k,vol
        real(cp),intent(in) :: c
        type(VF),intent(inout) :: tempk
        type(mesh),intent(in) :: m
        type(VF) :: un,Aun
        integer :: i,n,nmax,nwrite,i_3D,j_3D,k_3D,t_3D
        call init(un,D); call init(Aun,D)
        call assign(un,0.0_cp); call assign(D,0.0_cp)
        n = 0; nmax = un%x%numEl + un%y%numEl + un%z%numEl
        nwrite = 500
        write(*,*) 'Computing matrix diagonal...'
        do i=1,un%x%numEl
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,un%x,i)
          if ((i_3D.eq.1).or.(i_3D.eq.un%x%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%x%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%x%RF(t_3D)%s(3))) cycle
          call unitVector(un%x,i)
          call operator(Aun,un,vol,m,tempk,c,k,i)
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
          if ((i_3D.eq.1).or.(i_3D.eq.un%y%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%y%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%y%RF(t_3D)%s(3))) cycle
          call unitVector(un%y,i)
          call operator(Aun,un,vol,m,tempk,c,k,i)
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
          if ((i_3D.eq.1).or.(i_3D.eq.un%z%RF(t_3D)%s(1))) cycle
          if ((j_3D.eq.1).or.(j_3D.eq.un%z%RF(t_3D)%s(2))) cycle
          if ((k_3D.eq.1).or.(k_3D.eq.un%z%RF(t_3D)%s(3))) cycle
          call unitVector(un%z,i)
          call operator(Aun,un,vol,m,tempk,c,k,i)
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
        case (1); call get_3D_index(i,j,k,t,D%x,col)
                  D%x%RF(t)%f(i,j,k) = Aun%x%RF(t)%f(i,j,k)
        case (2); call get_3D_index(i,j,k,t,D%y,col)
                  D%y%RF(t)%f(i,j,k) = Aun%y%RF(t)%f(i,j,k)
        case (3); call get_3D_index(i,j,k,t,D%z,col)
                  D%z%RF(t)%f(i,j,k) = Aun%z%RF(t)%f(i,j,k)
        case default; stop 'Error: dir must = 1,2,3 in define_ith_diag_VF in matrix.f90'
        end select
      end subroutine

      subroutine zeroSecondIndex_RF(f,s)
        implicit none
        real(cp),dimension(:,:,:),intent(inout) :: f
        integer,dimension(3),intent(in) :: s
        f(2,:,:) = 0.0_cp; f(s(1)-1,:,:) = 0.0_cp
        f(:,2,:) = 0.0_cp; f(:,s(2)-1,:) = 0.0_cp
        f(:,:,2) = 0.0_cp; f(:,:,s(3)-1) = 0.0_cp
      end subroutine

      subroutine zeroSecondIndex_SF(f)
        implicit none
        type(SF),intent(inout) :: f
        integer :: i
        do i=1,f%s
          call zeroSecondIndex(f%RF(i)%f,f%RF(i)%s)
        enddo
      end subroutine

      subroutine zeroSecondIndex_VF(f)
        implicit none
        type(VF),intent(inout) :: f
        call zeroSecondIndex(f%x); call zeroSecondIndex(f%y); call zeroSecondIndex(f%z)
      end subroutine

      end module