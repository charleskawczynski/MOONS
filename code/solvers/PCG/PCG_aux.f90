      module PCG_aux_mod
      use current_precision_mod
      use bctype_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      implicit none

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
        if (f%N_along(1).and.(.not.m%plane_x)) then
          do i=1,m%s
            ! if (.not.m%B(i)%g%st_faces(1)%TF) then
              if (is_Neumann(x%BF(i)%BCs%bct_f(1))) then
                f_mod%BF(i)%GF%f(2,:,:) = f%BF(i)%GF%f(2,:,:)*0.5_cp
              elseif (is_Dirichlet(x%BF(i)%BCs%bct_f(1))) then
                f_mod%BF(i)%GF%f(2,:,:) = 0.0_cp
              endif
            ! endif
            ! if (.not.m%B(i)%g%st_faces(2)%TF) then
              if (is_Neumann(x%BF(i)%BCs%bct_f(2))) then
                f_mod%BF(i)%GF%f(f%BF(i)%GF%s(1)-1,:,:) = f%BF(i)%GF%f(f%BF(i)%GF%s(1)-1,:,:)*0.5_cp
              elseif (is_Dirichlet(x%BF(i)%BCs%bct_f(2))) then
                f_mod%BF(i)%GF%f(f%BF(i)%GF%s(1)-1,:,:) = 0.0_cp
              endif
            ! endif
          enddo
        endif
        if (f%N_along(2).and.(.not.m%plane_y)) then
          do i=1,m%s
            ! if (.not.m%B(i)%g%st_faces(3)%TF) then
              if (is_Neumann(x%BF(i)%BCs%bct_f(3))) then
                f_mod%BF(i)%GF%f(:,2,:) = f%BF(i)%GF%f(:,2,:)*0.5_cp
              elseif (is_Dirichlet(x%BF(i)%BCs%bct_f(3))) then
                f_mod%BF(i)%GF%f(:,2,:) = 0.0_cp
              endif
            ! endif
            ! if (.not.m%B(i)%g%st_faces(4)%TF) then
              if (is_Neumann(x%BF(i)%BCs%bct_f(4))) then
                f_mod%BF(i)%GF%f(:,f%BF(i)%GF%s(2)-1,:) = f%BF(i)%GF%f(:,f%BF(i)%GF%s(2)-1,:)*0.5_cp
              elseif (is_Dirichlet(x%BF(i)%BCs%bct_f(4))) then
                f_mod%BF(i)%GF%f(:,f%BF(i)%GF%s(2)-1,:) = 0.0_cp
              endif
            ! endif
          enddo
        endif
        if (f%N_along(3).and.(.not.m%plane_z)) then
          do i=1,m%s
            ! if (.not.m%B(i)%g%st_faces(5)%TF) then
              if (is_Neumann(x%BF(i)%BCs%bct_f(5))) then
                f_mod%BF(i)%GF%f(:,:,2) = f%BF(i)%GF%f(:,:,2)*0.5_cp
              elseif (is_Dirichlet(x%BF(i)%BCs%bct_f(5))) then
                f_mod%BF(i)%GF%f(:,:,2) = 0.0_cp
              endif
            ! endif
            ! if (.not.m%B(i)%g%st_faces(6)%TF) then
              if (is_Neumann(x%BF(i)%BCs%bct_f(6))) then
                f_mod%BF(i)%GF%f(:,:,f%BF(i)%GF%s(3)-1) = f%BF(i)%GF%f(:,:,f%BF(i)%GF%s(3)-1)*0.5_cp
              elseif (is_Dirichlet(x%BF(i)%BCs%bct_f(6))) then
                f_mod%BF(i)%GF%f(:,:,f%BF(i)%GF%s(3)-1) = 0.0_cp
              endif
            ! endif
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

      subroutine zeroGhostPoints_conditional_SF(x,m)
        implicit none
        type(SF),intent(inout) :: x
        type(mesh),intent(in) :: m
        integer :: i
        if (x%N_along(1)) then
          do i=1,m%s
            if ((.not.is_periodic(x%BF(i)%BCs%bct_f(1)))) x%BF(i)%GF%f(1,:,:) = 0.0_cp
            if ((.not.is_periodic(x%BF(i)%BCs%bct_f(2)))) x%BF(i)%GF%f(x%BF(i)%GF%s(1),:,:) = 0.0_cp
          enddo
        endif
        if (x%N_along(2)) then
          do i=1,m%s
            if ((.not.is_periodic(x%BF(i)%BCs%bct_f(3)))) x%BF(i)%GF%f(:,1,:) = 0.0_cp
            if ((.not.is_periodic(x%BF(i)%BCs%bct_f(4)))) x%BF(i)%GF%f(:,x%BF(i)%GF%s(2),:) = 0.0_cp
          enddo
        endif
        if (x%N_along(3)) then
          do i=1,m%s
            if ((.not.is_periodic(x%BF(i)%BCs%bct_f(5)))) x%BF(i)%GF%f(:,:,1) = 0.0_cp
            if ((.not.is_periodic(x%BF(i)%BCs%bct_f(6)))) x%BF(i)%GF%f(:,:,x%BF(i)%GF%s(3)) = 0.0_cp
          enddo
        endif
      end subroutine

      subroutine zeroGhostPoints_conditional_VF(x,m)
        implicit none
        type(VF),intent(inout) :: x
        type(mesh),intent(in) :: m
        call zeroGhostPoints_conditional_SF(x%x,m)
        call zeroGhostPoints_conditional_SF(x%y,m)
        call zeroGhostPoints_conditional_SF(x%z,m)
      end subroutine

      end module