       module apply_BCs_mod
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
       public :: apply_BCs
       interface apply_BCs;     module procedure apply_BCs_SF;    end interface
       interface apply_BCs;     module procedure apply_BCs_VF;    end interface

       contains

       subroutine apply_BCs_VF(U)
         implicit none
         type(VF),intent(inout) :: U
         call apply_BCs(U%x)
         call apply_BCs(U%y)
         call apply_BCs(U%z)
       end subroutine

       subroutine apply_BCs_SF(U)
         implicit none
         type(SF),intent(inout) :: U
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         call apply_BCs_faces(U)
         ! call apply_BCs_edges(U)
         ! call apply_BCs_corners(U)
       end subroutine

       subroutine apply_BCs_faces(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: t,i
         do t=1,U%s
#ifdef _PARALLELIZE_APPLY_BCS_FACES_
         !$OMP PARALLEL DO

#endif
         do i=1,U%BF(t)%BCs%PA_face_BCs%N
         call U%BF(t)%BCs%PA_face_BCs%SP(i)%P(&
         U%BF(t)%GF,&
         U%BF(t)%BCs%face%SB(U%BF(t)%BCs%PA_face_BCs%SP(i)%ID)%b_total,&
         U%BF(t)%BCs%f_BCs,&
         U%BF(t)%BCs%PA_face_BCs%SP(i)%ID)
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