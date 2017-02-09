       module apply_periodic_BCs_mod
       use current_precision_mod
       use datatype_conversion_mod
       use check_BCs_mod
       use data_location_mod
       use bctype_mod
       use apply_BCs_faces_bridge_mod
       ! use apply_BCs_edges_bridge_mod
       ! use apply_BCs_corners_bridge_mod
       use face_edge_corner_indexing_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: apply_periodic_BCs
       interface apply_periodic_BCs;  module procedure apply_periodic_BCs_SF;  end interface
       interface apply_periodic_BCs;  module procedure apply_periodic_BCs_VF;  end interface

       contains

       subroutine apply_periodic_BCs_VF(U,X)
         implicit none
         type(VF),intent(inout) :: U
         type(VF),intent(in) :: X
         call apply_periodic_BCs_SF(U%x,X%x)
         call apply_periodic_BCs_SF(U%y,X%y)
         call apply_periodic_BCs_SF(U%z,X%z)
       end subroutine

       subroutine apply_periodic_BCs_SF(U,X)
         implicit none
         type(SF),intent(inout) :: U
         type(SF),intent(in) :: X
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         call apply_periodic_BCs_faces(U,X)
         ! call apply_periodic_BCs_edges(U,X)
         ! call apply_periodic_BCsorners(U,X)
       end subroutine

       subroutine apply_periodic_BCs_faces(U,X)
         implicit none
         type(SF),intent(inout) :: U
         type(SF),intent(in) :: X
         integer :: t,i
         do t=1,U%s
#ifdef _PARALLELIZE_APPLY_BCS_FACES_
         !$OMP PARALLEL DO

#endif
         do i=1,X%BF(t)%BCs%PA_face_BCs%N
         if (is_Periodic(X%BF(t)%BCs%face%bct(X%BF(t)%BCs%PA_face_BCs%SP(i)%ID))) then
           call X%BF(t)%BCs%PA_face_BCs%SP(i)%P(&
           U%BF(t)%GF,&
           X%BF(t)%BCs%face%b(X%BF(t)%BCs%PA_face_BCs%SP(i)%ID),&
           X%BF(t)%BCs%f_BCs,&
           X%BF(t)%BCs%PA_face_BCs%SP(i)%ID)
         endif
         enddo

#ifdef _PARALLELIZE_APPLY_BCS_FACES_
        !$OMP END PARALLEL DO

#endif
         enddo
       end subroutine

       ! subroutine apply_BCs_edges(U)
       !   implicit none
       !   type(SF),intent(inout) :: U
       !   integer :: t,i
       !   do t=1,U%s
       !   do i=1,U%BF(t)%BCs%PA_edge_BCs%N
       !   call U%BF(t)%BCs%PA_edge_BCs%SP(i)%P(&
       !   U%BF(t)%GF,&
       !   U%BF(t)%BCs%edge%b(U%BF(t)%BCs%PA_edge_BCs%SP(i)%ID),&
       !   U%BF(t)%BCs%e_BCs,&
       !   U%BF(t)%BCs%PA_edge_BCs%SP(i)%ID)
       !   enddo
       !   enddo
       ! end subroutine

       ! subroutine apply_BCs_corners(U)
       !   implicit none
       !   type(SF),intent(inout) :: U
       !   integer :: t,i
       !   do t=1,U%s
       !   do i=1,U%BF(t)%BCs%PA_corner_BCs%N
       !   call U%BF(t)%BCs%PA_corner_BCs%SP(i)%P(&
       !   U%BF(t)%GF,&
       !   U%BF(t)%BCs%corner%b(U%BF(t)%BCs%PA_corner_BCs%SP(i)%ID),&
       !   U%BF(t)%BCs%c_BCs,&
       !   U%BF(t)%BCs%PA_corner_BCs%SP(i)%ID)
       !   enddo
       !   enddo
       ! end subroutine

       end module