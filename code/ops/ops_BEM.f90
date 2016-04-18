       module ops_BEM_mod
       use current_precision_mod
       use ops_del_mod
       use mesh_mod
       use domain_mod
       use ops_embedExtract_mod
       use VF_mod
       use SF_mod
       use index_mapping_mod

       implicit none

       private

       public :: boundaryFlux
       interface boundaryFlux;   module procedure boundaryFlux_VF;          end interface
       interface boundaryFlux;   module procedure boundaryFlux_VF_SD;       end interface

       real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp

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
           if (.not.m%g(t)%st_faces(1)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
               BFtemp = BFtemp + u%x%RF(t)%f(2,j,k)*m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_faces(2)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
               BFtemp = BFtemp + u%x%RF(t)%f(u%x%RF(t)%s(1)-1,j,k)*m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
         do t=1,m%s
           if (.not.m%g(t)%st_faces(3)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
               BFtemp = BFtemp + u%y%RF(t)%f(i,2,k)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_faces(4)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
               BFtemp = BFtemp + u%y%RF(t)%f(i,u%y%RF(t)%s(2)-1,k)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
         enddo
         do t=1,m%s
           if (.not.m%g(t)%st_faces(5)%TF) then
             !$OMP PARALLEL DO SHARED(m), REDUCTION(+:BFtemp)
             do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
               BFtemp = BFtemp + u%z%RF(t)%f(i,j,2)*m%g(t)%c(1)%dhn(i)*m%g(t)%c(2)%dhn(j)
             enddo; enddo
             !$OMP END PARALLEL DO
             BF = BF + BFtemp; BFtemp = 0.0_cp
           endif
           if (.not.m%g(t)%st_faces(6)%TF) then
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

        ! subroutine apply_B_tan(B,m,phi,temp_F,temp1,temp2)
        !   ! Computes
        !   !  B_tan from phi and Bn
        !   implicit none
        !   type(SF),intent(inout) :: phi,temp1,temp2
        !   type(VF),intent(inout) :: B,temp_F
        !   type(mesh),intent(in) :: m
        !   integer :: k
        !   call grad(temp_F,phi)
        !   call multiply(temp_F,-1.0_cp)
        !   do k=1,n
        !     call multiply(temp1,phi,(1.0_cp - ds))
        !     call compute_phi(phi,m,B,temp2)
        !     call multiply(phi,ds)
        !     call add(phi,temp1)
        !   enddo
        ! end subroutine

        subroutine compute_phi_pseudo_time_marching(phi,m,B,n,ds,temp1,temp2)
          ! Computes
          !  phi(x_i) = (2π)⁻¹ [ Σ_j φ_j ∫ ∂_n G(x,y) dA + Σ_j Bn_j ∫ G(x,y) dA ]
          ! Where
          !                1
          !   G(x,y) =  -------
          !             |x - y|
          implicit none
          type(SF),intent(inout) :: phi,temp1,temp2
          type(VF),intent(in) :: B
          type(mesh),intent(in) :: m
          real(cp),intent(in) :: ds
          integer,intent(in) :: n
          integer :: k
          do k=1,n
            call multiply(temp1,phi,(1.0_cp - ds))
            call compute_phi(phi,m,B,temp2)
            call multiply(phi,ds)
            call add(phi,temp1)
          enddo
        end subroutine

        subroutine compute_phi(phi,m,B,temp)
          ! Computes
          !  phi(x_i) = (2π)⁻¹ [ Σ_j φ_j ∫ ∂_n G(x,y) dA + Σ_j Bn_j ∫ G(x,y) dA ]
          ! Where
          !                1
          !   G(x,y) =  -------
          !             |x - y|
          implicit none
          type(SF),intent(inout) :: phi,temp
          type(VF),intent(in) :: B
          type(mesh),intent(in) :: m
          integer :: i
          integer :: i_3D,j_3D,k_3D,t_3D
          do i=1,phi%numEl
            call get_3D_index(i_3D,j_3D,k_3D,t_3D,m,i)
            call phi_integral(temp,phi,B,m,i)
            phi%RF(t_3D)%f(i_3D,j_3D,k_3D) = temp%RF(t_3D)%f(i_3D,j_3D,k_3D)
            ! phi%RF(t_3D)%f(i_3D,j_3D,k_3D) = phi_integral_func(phi,B,m,i)
          enddo
        end subroutine

        subroutine phi_integral(phi_i,phi,B,m,i)
          ! Computes
          !  phi(x_i) = (2π)⁻¹ [ Σ_j φ_j ∫ ∂_n G(x,y) dA + Σ_j Bn_j ∫ G(x,y) dA ]
          ! Where
          !                1
          !   G(x,y) =  -------
          !             |x - y|
          implicit none
          type(SF),intent(inout) :: phi_i
          type(SF),intent(in) :: phi
          type(VF),intent(in) :: B
          type(mesh),intent(in) :: m
          integer,intent(in) :: i
          real(cp),dimension(3) :: x,y,Bj
          real(cp) :: G_ij,dA,phij,temp
          integer :: i_3D,j_3D,k_3D,t_3D,j
          temp = 0.0_cp
          do j=1,phi_i%numEl
            call get_3D_index(i_3D,j_3D,k_3D,t_3D,m,j)
            G_ij = get_G_ij(m,i,j)
            x = get_x_m(m,i)
            y = get_x_m(m,j)
            dA = get_dA(m,j)
            phij = get_val(phi,j)
            Bj = get_val(B,j)
            temp = temp + G_ij**(3.0_cp)*dot_n(m,t_3D,x-y)*dA*phij
            temp = temp + G_ij*dA*dot_n(m,t_3D,Bj)
          enddo
          phi_i%RF(t_3D)%f(i_3D,j_3D,k_3D) = temp*0.5_cp*PI
        end subroutine

        function phi_integral_func(phi,B,m,i) result(phi_xi)
          ! Computes
          !  phi(x_i) = (2π)⁻¹ [ Σ_j φ_j ∫ ∂_n G(x,y) dA + Σ_j Bn_j ∫ G(x,y) dA ]
          ! Where
          !                1
          !   G(x,y) =  -------
          !             |x - y|
          implicit none
          type(SF),intent(in) :: phi
          type(VF),intent(in) :: B
          type(mesh),intent(in) :: m
          integer,intent(in) :: i
          real(cp) :: phi_xi
          real(cp),dimension(3) :: x,y,Bj
          real(cp) :: G_ij,dA,phij,temp
          integer :: i_3D,j_3D,k_3D,t_3D,j
          temp = 0.0_cp
          do j=1,phi%numEl
            call get_3D_index(i_3D,j_3D,k_3D,t_3D,m,j)
            G_ij = get_G_ij(m,i,j)
            x = get_x_m(m,i)
            y = get_x_m(m,j)
            dA = get_dA(m,j)
            phij = get_val(phi,j)
            Bj = get_val(B,j)
            temp = temp + G_ij**(3.0_cp)*dot_n(m,t_3D,x-y)*dA*phij
            temp = temp + G_ij*dA*dot_n(m,t_3D,Bj)
          enddo
          phi_xi = temp*0.5_cp*PI
        end function

        function get_x_m(m,index_1D) result(x)
          ! Computes x given 1D index and the mesh
          ! these should be cell centered values
          implicit none
          type(mesh),intent(in) :: m
          integer,intent(in) :: index_1D
          integer :: i_3D,j_3D,k_3D,t_3D
          real(cp),dimension(3) :: x
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,m,index_1D)
          x = get_x_on_surface(m,i_3D,j_3D,k_3D,t_3D)
        end function

        function get_x_i_SF(m,u,index_1D) result(x)
          ! Computes x given 1D index and the mesh
          ! these should be cell centered values
          implicit none
          type(mesh),intent(in) :: m
          type(SF),intent(in) :: u
          integer,intent(in) :: index_1D
          real(cp),dimension(3) :: x
          integer :: i_3D,j_3D,k_3D,t_3D
          call get_3D_index(i_3D,j_3D,k_3D,t_3D,u,index_1D)
          x = get_x_on_surface(m,i_3D,j_3D,k_3D,t_3D)
        end function

        function get_G_ij(m,i_1D,j_1D) result(G_ij)
          ! Computes
          !                1
          !   G(x,y) =  -------
          !             |x - y|
          ! Where x and y are uniquely defined by i_1D and j_1D.
          implicit none
          type(mesh),intent(in) :: m
          integer,intent(in) :: i_1D,j_1D
          real(cp),dimension(3) :: x,y
          real(cp) :: G_ij,distance,tol
          tol = 10.0_cp**(-10.0_cp)
          x = get_x_m(m,i_1D)
          y = get_x_m(m,j_1D)
          distance = sqrt((x(1)-y(1))**2.0_cp+(x(2)-y(2))**2.0_cp+(x(3)-y(3))**2.0_cp)
          if (distance.gt.tol) then; G_ij = 1.0_cp/distance
          else; G_ij = 0.0_cp
          endif
        end function

        function dot_n(m,t_3D,f) result(x)
          ! Computes f n
          implicit none
          type(mesh),intent(in) :: m
          integer,intent(in) :: t_3D
          real(cp),dimension(3),intent(in) :: f
          real(cp) :: x
          integer :: N_s
          N_s = 1
              if (m%g(t_3D)%c(1)%N.eq.N_s) then; x = f(1)
          elseif (m%g(t_3D)%c(2)%N.eq.N_s) then; x = f(2)
          elseif (m%g(t_3D)%c(3)%N.eq.N_s) then; x = f(3)
          else; stop 'Error: bad input to dot_n in ops_BEM.f90'
          endif
        end function

        function get_x_on_surface(m,i_3D,j_3D,k_3D,t_3D) result(x)
          ! Computes x given 1D index and the mesh
          ! these should be cell centered values
          implicit none
          type(mesh),intent(in) :: m
          integer,intent(in) :: i_3D,j_3D,k_3D,t_3D
          real(cp),dimension(3) :: x
          integer :: N_s
          N_s = 1
              if (m%g(t_3D)%c(1)%N.eq.N_s) then; x=(/m%g(t_3D)%c(1)%hn(i_3D),m%g(t_3D)%c(2)%hc(j_3D),m%g(t_3D)%c(3)%hc(k_3D)/)
          elseif (m%g(t_3D)%c(2)%N.eq.N_s) then; x=(/m%g(t_3D)%c(1)%hc(i_3D),m%g(t_3D)%c(2)%hn(j_3D),m%g(t_3D)%c(3)%hc(k_3D)/)
          elseif (m%g(t_3D)%c(3)%N.eq.N_s) then; x=(/m%g(t_3D)%c(1)%hc(i_3D),m%g(t_3D)%c(2)%hc(j_3D),m%g(t_3D)%c(3)%hn(k_3D)/)
          else; stop 'Error: x not found in get_x_on_surface in ops_BEM.f90'
          endif
        end function

       end module