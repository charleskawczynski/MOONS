       module apply_BCs_implicit_mod
       use current_precision_mod
       use datatype_conversion_mod
       use check_BCs_mod
       use data_location_mod
       use bctype_mod
       use apply_BCs_faces_bridge_implicit_mod
       ! use apply_BCs_edges_bridge_implicit_mod
       ! use apply_BCs_corners_bridge_implicit_mod
       use face_edge_corner_indexing_mod
       use SF_extend_mod
       use VF_extend_mod

       implicit none

       private
       public :: apply_BCs_implicit
       interface apply_BCs_implicit; module procedure apply_BCs_imp_SF; end interface
       interface apply_BCs_implicit; module procedure apply_BCs_imp_VF; end interface

       contains

       subroutine apply_BCs_imp_VF(U)
         implicit none
         type(VF),intent(inout) :: U
         call apply_BCs_implicit(U%x)
         call apply_BCs_implicit(U%y)
         call apply_BCs_implicit(U%z)
       end subroutine

       subroutine apply_BCs_imp_SF(U)
         implicit none
         type(SF),intent(inout) :: U
#ifdef _DEBUG_APPLY_BCS_
         call check_defined(U)
#endif
         call apply_BCs_faces_imp(U)
         ! call apply_BCs_edges_imp(U)
         ! call apply_BCs_corners_imp(U)
       end subroutine

       subroutine apply_BCs_faces_imp(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: t,i
         do t=1,U%s
#ifdef _PARALLELIZE_APPLY_BCS_FACES_
         !$OMP PARALLEL DO

#endif
         do i=1,U%BF(t)%BCs%PA_face_implicit_BCs%N
         call U%BF(t)%BCs%PA_face_implicit_BCs%SP(i)%P(&
         U%BF(t)%GF,&
         U%BF(t)%BCs%face%SB(U%BF(t)%BCs%PA_face_implicit_BCs%SP(i)%ID)%b_total,&
         U%BF(t)%BCs%f_BCs,&
         U%BF(t)%BCs%PA_face_implicit_BCs%SP(i)%ID)
         enddo

#ifdef _PARALLELIZE_APPLY_BCS_FACES_
        !$OMP END PARALLEL DO

#endif
         enddo
       end subroutine

       ! subroutine apply_BCs_edges_imp_SF(U)
       !   implicit none
       !   type(SF),intent(inout) :: U
       !   integer :: t,i
       !   do t=1,U%s
       !   do i=1,U%BF(t)%BCs%PA_edge_implicit_BCs%N
       !   call U%BF(t)%BCs%PA_edge_implicit_BCs%SP(i)%P(&
       !   U%BF(t)%GF,&
       !   U%BF(t)%BCs%edge%b(U%BF(t)%BCs%PA_edge_implicit_BCs%SP(i)%ID),&
       !   U%BF(t)%BCs%e_BCs,&
       !   U%BF(t)%BCs%PA_edge_implicit_BCs%SP(i)%ID)
       !   enddo
       !   enddo
       ! end subroutine

       ! subroutine apply_BCs_corners_imp_SF(U)
       !   implicit none
       !   type(SF),intent(inout) :: U
       !   integer :: t,i
       !   do t=1,U%s
       !   do i=1,U%BF(t)%BCs%PA_corner_implicit_BCs%N
       !   call U%BF(t)%BCs%PA_corner_implicit_BCs%SP(i)%P(&
       !   U%BF(t)%GF,&
       !   U%BF(t)%BCs%corner%b(U%BF(t)%BCs%PA_corner_implicit_BCs%SP(i)%ID),&
       !   U%BF(t)%BCs%c_BCs,&
       !   U%BF(t)%BCs%PA_corner_implicit_BCs%SP(i)%ID)
       !   enddo
       !   enddo
       ! end subroutine

       end module