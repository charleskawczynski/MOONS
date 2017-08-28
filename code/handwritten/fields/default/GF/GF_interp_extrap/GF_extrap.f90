       module GF_extrap_mod
       ! Compiler flags: (_DEBUG_EXTRAP_)
       use current_precision_mod
       use GF_base_mod
       use face_edge_corner_indexing_mod
       use grid_mod
       implicit none

       private
       public :: extrap
       interface extrap;     module procedure extrap_staggered_GF;   end interface
       interface extrap;     module procedure extrap_self_GF;        end interface

       contains

       subroutine extrap_staggered_GF(N,C,dir)
         ! Formulae are simple because the first 2 cells have equal sizes.
         !         N  C  N  C  N  C  N  C  N
         !         |--o--|--o--|--o--|--o--|    --> dir
         !         *                       *
         !
         ! f_ghost + f_boundary
         ! --------------------  = C    -->  f_ghost = 2g - f_boundary
         !          2
         implicit none
         type(grid_field),intent(inout) :: N
         type(grid_field),intent(in) :: C
         integer,intent(in) :: dir
#ifdef _DEBUG_EXTRAP_
         call insist_shape_match(C,N,dir,'extrap_staggered_GF')
         call insist_shape_staggered(C,N,dir,'extrap_staggered_GF')
#endif
         ! Backward extrapolation
         select case (dir)
         case (1); N%f(1,:,:) = 2.0_cp*C%f(1,:,:) - N%f(2,:,:)
         case (2); N%f(:,1,:) = 2.0_cp*C%f(:,1,:) - N%f(:,2,:)
         case (3); N%f(:,:,1) = 2.0_cp*C%f(:,:,1) - N%f(:,:,2)
         end select
         ! Forward extrapolation
         select case (dir)
         case (1); N%f(N%s(1),:,:) = 2.0_cp*C%f(C%s(1),:,:) - N%f(N%s(1)-1,:,:)
         case (2); N%f(:,N%s(2),:) = 2.0_cp*C%f(:,C%s(2),:) - N%f(:,N%s(2)-1,:)
         case (3); N%f(:,:,N%s(3)) = 2.0_cp*C%f(:,:,C%s(3)) - N%f(:,:,N%s(3)-1)
         end select
       end subroutine

       subroutine extrap_self_GF(N,dir)
         ! Formulae are simple because the first 2 cells have equal sizes.
         implicit none
         type(grid_field),intent(inout) :: N
         integer,intent(in) :: dir
         ! Backward extrapolation
         select case (dir)
         case (1); N%f(1,:,:) = 2.0_cp*N%f(2,:,:) - N%f(3,:,:)
         case (2); N%f(:,1,:) = 2.0_cp*N%f(:,2,:) - N%f(:,3,:)
         case (3); N%f(:,:,1) = 2.0_cp*N%f(:,:,2) - N%f(:,:,3)
         end select
         ! Forward extrapolation
         select case (dir)
         case (1); N%f(N%s(1),:,:) = 2.0_cp*N%f(N%s(1)-1,:,:) - N%f(N%s(1)-2,:,:)
         case (2); N%f(:,N%s(2),:) = 2.0_cp*N%f(:,N%s(2)-1,:) - N%f(:,N%s(2)-2,:)
         case (3); N%f(:,:,N%s(3)) = 2.0_cp*N%f(:,:,N%s(3)-1) - N%f(:,:,N%s(3)-2)
         end select
       end subroutine

       end module