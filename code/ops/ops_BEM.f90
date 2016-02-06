       module ops_BEM_mod
       use ops_del_mod
       use mesh_mod
       use domain_mod
       use ops_embedExtract_mod
       use VF_mod
       use SF_mod

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

       public :: boundaryFlux
       interface boundaryFlux;   module procedure boundaryFlux_VF;          end interface
       interface boundaryFlux;   module procedure boundaryFlux_VF_SD;       end interface

       public :: distance
       interface distance;       module procedure distance_from_surf_i_SF;  end interface

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

        subroutine distance_from_surf_i_SF(u,m,index_1D)
          implicit none
          type(SF),intent(inout) :: u
          type(mesh),intent(in) :: m
          integer,intent(in) :: index_1D
          integer :: i_3D,j_3D,k_3D,t_3D,i,j,k,t
          real(cp) :: dx,dy,dz,hx,hy,hz

          call assign(u,0.0_cp)
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,u,index_1D)

          do t=1,m%s
            if (m%g(t)%c(1)%N.eq.1) then; hx=m%g(t_3D)%c(1)%hn(i_3D); else; hx=m%g(t_3D)%c(1)%hc(i_3D); endif
            if (m%g(t)%c(2)%N.eq.1) then; hy=m%g(t_3D)%c(2)%hn(j_3D); else; hy=m%g(t_3D)%c(2)%hc(j_3D); endif
            if (m%g(t)%c(3)%N.eq.1) then; hz=m%g(t_3D)%c(3)%hn(k_3D); else; hz=m%g(t_3D)%c(3)%hc(k_3D); endif

            if (m%g(t)%c(1)%N.eq.1) then
              !$OMP PARALLEL DO SHARED(m) PRIVATE(dx,dy,dz)
              do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              dx = hx - m%g(t)%c(1)%hn(i)
              dy = hy - m%g(t)%c(2)%hc(j)
              dz = hz - m%g(t)%c(3)%hc(k)
              u%RF(t)%f(i,j,k) = sqrt(dx**2.0_cp+dx**2.0_cp+dz**2.0_cp)
              enddo; enddo; enddo
              !$OMP END PARALLEL DO
            elseif (m%g(t)%c(2)%N.eq.1) then
              !$OMP PARALLEL DO SHARED(m) PRIVATE(dx,dy,dz)
              do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              dx = hx - m%g(t)%c(1)%hc(i)
              dy = hy - m%g(t)%c(2)%hn(j)
              dz = hz - m%g(t)%c(3)%hc(k)
              u%RF(t)%f(i,j,k) = sqrt(dx**2.0_cp+dx**2.0_cp+dz**2.0_cp)
              enddo; enddo; enddo
              !$OMP END PARALLEL DO
            elseif (m%g(t)%c(3)%N.eq.1) then
              !$OMP PARALLEL DO SHARED(m) PRIVATE(dx,dy,dz)
              do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
              dx = hx - m%g(t)%c(1)%hc(i)
              dy = hy - m%g(t)%c(2)%hc(j)
              dz = hz - m%g(t)%c(3)%hn(k)
              u%RF(t)%f(i,j,k) = sqrt(dx**2.0_cp+dx**2.0_cp+dz**2.0_cp)
              enddo; enddo; enddo
              !$OMP END PARALLEL DO
            else; stop 'Error: N_cells must = 1 for each surface in BEM in ops_BEM.f90'
            endif
          enddo
        end subroutine

       end module