       module ops_BEM_mod
       use current_precision_mod
       use ops_del_mod
       use mesh_mod
       use domain_mod
       use ops_embedExtract_mod
       use VF_mod
       use SF_mod

       implicit none
       private
       public :: boundaryFlux
       interface boundaryFlux;            module procedure boundaryFlux_VF;           end interface
       interface boundaryFlux;            module procedure boundaryFlux_VF_SD;        end interface

       contains

       subroutine boundaryFlux_VF(BF,u,m)
         ! Computes
         ! 
         !   BF = ∫∫ u•n dA
         ! 
         implicit none
         type(VF),intent(in) :: u
         real(cp),intent(inout) :: BF
         type(mesh),intent(in) :: m
         real(cp) :: BFtemp
         integer :: i,j,k,t
         logical :: TF
         BFtemp = 0.0_cp ! temp is necessary for reduction
         BF = 0.0_cp
         do t=1,m%s
           if (.not.m%g(t)%st_face%hmin(1)) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
               BFtemp = BFtemp + u%x%RF(t)%f(2,j,k)*m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_face%hmax(1)) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
               BFtemp = BFtemp + u%x%RF(t)%f(u%x%RF(t)%s(1)-1,j,k)*m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
         do t=1,m%s
           if (.not.m%g(t)%st_face%hmin(2)) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
               BFtemp = BFtemp + u%y%RF(t)%f(i,2,k)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_face%hmax(2)) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
               BFtemp = BFtemp + u%y%RF(t)%f(i,u%y%RF(t)%s(2)-1,k)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
         do t=1,m%s
           if (.not.m%g(t)%st_face%hmin(3)) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
               BFtemp = BFtemp + u%z%RF(t)%f(i,j,2)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(2)%dhn(j)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_face%hmax(3)) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
               BFtemp = BFtemp + u%z%RF(t)%f(i,j,u%z%RF(t)%s(3)-1)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(2)%dhn(j)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
       end subroutine

       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************

       subroutine boundaryFlux_VF_SD(BF,f,D)
         implicit none
         type(VF),intent(in) :: f
         real(cp),intent(inout) :: BF
         type(domain),intent(in) :: D
         type(VF) :: temp
         if (.not.f%is_Face) stop 'Error: Boundary flux must be computed on face in boundaryFlux_VF_SD in ops_aux.f90'
         call init_Face(temp,D%m_in)
         call extractFace(temp,f,D)
         call boundaryFlux(BF,temp,D%m_in)
         call delete(temp)
       end subroutine

       ! is this right???
       ! subroutine boundaryShear_VF(BF,f,m,temp_F,temp_F_TF)
       !   implicit none
       !   type(VF),intent(in) :: f
       !   real(cp),intent(inout) :: BF
       !   type(mesh),intent(in) :: m
       !   type(VF),intent(inout) :: temp_F
       !   type(TF),intent(inout) :: temp_F_TF
       !   type(VF) :: temp
       !   if (.not.f%is_Face) stop 'Error: Boundary flux must be computed on face in boundaryFlux_VF_SD in ops_aux.f90'
       !   call face2face(temp_F_TF,f,m)
       !   call init_Face(temp,m)
       !   call assign(temp%x,temp_F_TF%x%x)
       !   call assign(temp%y,temp_F_TF%y%y)
       !   call assign(temp%z,temp_F_TF%z%z)
       !   call boundaryFlux(BF,temp,m)
       !   call delete(temp)
       ! end subroutine

       end module