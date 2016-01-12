      module CG_aux_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private

      public :: modify_forcing1
      interface modify_forcing1;             module procedure modify_forcing1_SF;              end interface
      interface modify_forcing1;             module procedure modify_forcing1_VF;              end interface

      public :: zeroGhostPoints_conditional
      interface zeroGhostPoints_conditional; module procedure zeroGhostPoints_conditional_SF;  end interface
      interface zeroGhostPoints_conditional; module procedure zeroGhostPoints_conditional_VF;  end interface

      contains

      subroutine modify_forcing1_SF(f_mod,f,m,x)
        implicit none
        type(SF),intent(inout) :: f_mod
        type(SF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: x
        integer :: i
        if (Node_along(f,1)) then
          do i=1,f%s
            if (m%g(i)%c(1)%N.gt.1) then
              if (x%RF(i)%b%f(1)%b%Neumann) then
                f_mod%RF(i)%f(2,:,:) = f%RF(i)%f(2,:,:)/2.0_cp
              elseif (x%RF(i)%b%f(1)%b%Dirichlet) then
                f_mod%RF(i)%f(2,:,:) = 0.0_cp
              endif
              if (x%RF(i)%b%f(2)%b%Neumann) then
                f_mod%RF(i)%f(f%RF(i)%s(1)-1,:,:) = f%RF(i)%f(f%RF(i)%s(1)-1,:,:)/2.0_cp
              elseif (x%RF(i)%b%f(2)%b%Dirichlet) then
                f_mod%RF(i)%f(f%RF(i)%s(1)-1,:,:) = 0.0_cp
              endif
            endif
          enddo
        endif
        if (Node_along(f,2)) then
          do i=1,f%s
            if (m%g(i)%c(2)%N.gt.1) then
              if (x%RF(i)%b%f(3)%b%Neumann) then
                f_mod%RF(i)%f(:,2,:) = f%RF(i)%f(:,2,:)/2.0_cp
              elseif (x%RF(i)%b%f(3)%b%Dirichlet) then
                f_mod%RF(i)%f(:,2,:) = 0.0_cp
              endif
              if (x%RF(i)%b%f(4)%b%Neumann) then
                f_mod%RF(i)%f(:,f%RF(i)%s(2)-1,:) = f%RF(i)%f(:,f%RF(i)%s(2)-1,:)/2.0_cp
              elseif (x%RF(i)%b%f(4)%b%Dirichlet) then
                f_mod%RF(i)%f(:,f%RF(i)%s(2)-1,:) = 0.0_cp
              endif
            endif
          enddo
        endif
        if (Node_along(f,3)) then
          do i=1,f%s
            if (m%g(i)%c(3)%N.gt.1) then
              if (x%RF(i)%b%f(5)%b%Neumann) then
                f_mod%RF(i)%f(:,:,2) = f%RF(i)%f(:,:,2)/2.0_cp
              elseif (x%RF(i)%b%f(5)%b%Dirichlet) then
                f_mod%RF(i)%f(:,:,2) = 0.0_cp
              endif
              if (x%RF(i)%b%f(6)%b%Neumann) then
                f_mod%RF(i)%f(:,:,f%RF(i)%s(3)-1) = f%RF(i)%f(:,:,f%RF(i)%s(3)-1)/2.0_cp
              elseif (x%RF(i)%b%f(6)%b%Dirichlet) then
                f_mod%RF(i)%f(:,:,f%RF(i)%s(3)-1) = 0.0_cp
              endif
            endif
          enddo
        endif
      end subroutine

      subroutine modify_forcing1_VF(f_mod,f,m,x)
        implicit none
        type(VF),intent(inout) :: f_mod
        type(VF),intent(in) :: f
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: x
        call modify_forcing1_SF(f_mod%x,f%x,m,x%x)
        call modify_forcing1_SF(f_mod%y,f%y,m,x%y)
        call modify_forcing1_SF(f_mod%z,f%z,m,x%z)
      end subroutine

      subroutine zeroGhostPoints_conditional_SF(x)
        implicit none
        type(SF),intent(inout) :: x
        integer :: i
        if (Node_along(x,1)) then
          do i=1,x%s
            x%RF(i)%f(1,:,:) = 0.0_cp; x%RF(i)%f(x%RF(i)%s(1),:,:) = 0.0_cp
          enddo
        endif
        if (Node_along(x,2)) then
          do i=1,x%s
            x%RF(i)%f(:,1,:) = 0.0_cp; x%RF(i)%f(:,x%RF(i)%s(2),:) = 0.0_cp
          enddo
        endif
        if (Node_along(x,3)) then
          do i=1,x%s
            x%RF(i)%f(:,:,1) = 0.0_cp; x%RF(i)%f(:,:,x%RF(i)%s(3)) = 0.0_cp
          enddo
        endif
      end subroutine

      subroutine zeroGhostPoints_conditional_VF(x)
        implicit none
        type(VF),intent(inout) :: x
        call zeroGhostPoints_conditional_SF(x%x)
        call zeroGhostPoints_conditional_SF(x%y)
        call zeroGhostPoints_conditional_SF(x%z)
      end subroutine

      end module