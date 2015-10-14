       module applyStitches_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use mesh_mod
       implicit none

       private
       public :: applyStitches

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface applyStitches;    module procedure applyStitches_VF;     end interface
       interface applyStitches;    module procedure applyStitches_SF;     end interface

       contains

       subroutine applyStitches_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call applyStitches(U%x,m)
         call applyStitches(U%y,m)
         call applyStitches(U%z,m)
       end subroutine

       subroutine applyStitches_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i,k
         if (m%s.gt.1) then
           do i=1,m%s; do k=1,3
             if (m%g(i)%st(k)%hmin) then
              call applyStitch(U%RF(i),U%RF(m%g(i)%st(k)%hmin_id),k)
             endif
           enddo; enddo
         endif
       end subroutine

       subroutine applyStitch(Umin,Umax,dir)
         implicit none
         type(realField),intent(inout) :: Umin,Umax
         integer,intent(in) :: dir
         select case (dir)
         case (1); Umax%f(Umax%s(1),:,:) = Umin%f(2,:,:)
                   Umin%f(1,:,:) = Umax%f(Umax%s(1)-1,:,:)
         case (2); Umax%f(:,Umax%s(2),:) = Umin%f(:,2,:)
                   Umin%f(:,1,:) = Umax%f(:,Umax%s(2)-1,:)
         case (3); Umax%f(:,:,Umax%s(3)) = Umin%f(:,:,2)
                   Umin%f(:,:,1) = Umax%f(:,:,Umax%s(3)-1)
         case default
         stop 'Erorr: dir must = 1,2,3 in applyAllStitches_RF in applyStitches.f90'
         end select
       end subroutine

       end module