       module apply_BCs_embed_mod
       use current_precision_mod
       use datatype_conversion_mod
       use check_BCs_mod
       use data_location_mod
       use bctype_mod
       use apply_BCs_faces_bridge_mod
       use face_edge_corner_indexing_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: apply_BCs_faces_em
       interface apply_BCs_faces_em;     module procedure apply_BCs_faces_SF;    end interface
       interface apply_BCs_faces_em;     module procedure apply_BCs_faces_VF;    end interface

       public :: apply_BCs_faces_implicit_em
       interface apply_BCs_faces_implicit_em; module procedure apply_BCs_faces_imp_SF; end interface
       interface apply_BCs_faces_implicit_em; module procedure apply_BCs_faces_imp_VF; end interface

       contains

       subroutine apply_BCs_faces_VF(U)
         implicit none
         type(VF),intent(inout) :: U
         call apply_BCs_faces_SF(U%x)
         call apply_BCs_faces_SF(U%y)
         call apply_BCs_faces_SF(U%z)
       end subroutine

       subroutine apply_BCs_faces_SF(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: t,i,face
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         write(*,*) '************************************************ APPLYING BCS'
         do t=1,U%s
#ifdef _PARALLELIZE_APPLY_BCS_FACES_
         !$OMP PARALLEL DO

#endif
         do i=1,U%BF(t)%BCs%PA_face_BCs%N
         call U%BF(t)%BCs%PA_face_BCs%SP(i)%P(&
         U%BF(t)%GF,&
         U%BF(t)%BCs%f(U%BF(t)%BCs%face_BCs%g%sd(U%BF(t)%BCs%PA_face_BCs%SP(i)%ID)%g_R1_id),&
         U%BF(t)%BCs%face_BCs,&
         U%BF(t)%BCs%PA_face_BCs%SP(i)%ID)
         enddo

#ifdef _PARALLELIZE_APPLY_BCS_FACES_
        !$OMP END PARALLEL DO

#endif
         enddo
         write(*,*) '************************************************ DONE APPLYING BCS'
         ! if (is_Face(U%DL).and.(U%face.eq.2)) stop 'Done in apply_BCs_embed.f90'
       end subroutine

       subroutine apply_BCs_faces_imp_VF(U)
         implicit none
         type(VF),intent(inout) :: U
         call apply_BCs_faces_SF(U%x)
         call apply_BCs_faces_SF(U%y)
         call apply_BCs_faces_SF(U%z)
       end subroutine

       subroutine apply_BCs_faces_imp_SF(U)
         implicit none
         type(SF),intent(inout) :: U
         integer :: t,i
#ifdef _DEBUG_APPLY_BCS_FACES_
         call check_defined(U)
#endif
         do t=1,U%s
         do i=1,U%BF(t)%BCs%PA_face_implicit_BCs%N
         call U%BF(t)%BCs%PA_face_implicit_BCs%SP(i)%P(U%BF(t)%GF,&
                                       U%BF(t)%BCs%f(U%BF(t)%BCs%face_BCs%g%sd(i)%g_R1_id),&
                                       U%BF(t)%BCs%face_BCs,i)
         enddo
         enddo
       end subroutine

       end module