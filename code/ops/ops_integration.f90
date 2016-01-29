       module ops_aux_mod
       ! 
       ! Directions are frequently used in this code. 
       ! For clarity, some diagrams here show how the 
       ! directions are defined.
       ! 
       ! faceDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !      faceDir --->  \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       ! 
       ! 
       ! 
       ! edgeDir = 1 (x)
       ! 
       !                       z
       !                y      |
       !                 \   __|____
       !                  \ |\ |     \
       !                   \| \|______\
       !                    \  |      |
       !                     \ |      |
       !                      \|______|_____ x
       !                        -------> edgeDir
       ! 
       ! 
       use ops_del_mod
       use grid_mod
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

       public :: dot_product
       interface dot_product;             module procedure dot_product_SF;            end interface
       interface dot_product;             module procedure dot_product_VF;            end interface

       public :: totalEnergy
       interface totalEnergy;             module procedure totalEnergy_VF;            end interface
       interface totalEnergy;             module procedure totalEnergy_VF_SD;         end interface

       public :: boundaryFlux
       interface boundaryFlux;            module procedure boundaryFlux_VF;           end interface
       interface boundaryFlux;            module procedure boundaryFlux_VF_SD;        end interface

       contains

       ! *********************************************************************************
       ! *********************************************************************************
       ! ********************************* REAL ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine totalEnergy_VF(e,u,m) ! Finished
         ! Computes
         ! 
         !          1
         !   e = -------- ∫∫∫ ( u_x² + u_y² + u_z² ) dx dy dz
         !        volume
         ! 
         ! Where x,y,z lives in the cell center.
         ! This will yields expected results ONLY
         ! when fluid domains are completely contained
         ! by the total domain (case 1 in define_CE in subdomain.f90).
         implicit none
         type(VF),intent(in) :: u
         real(cp),intent(inout) :: e
         type(mesh),intent(in) :: m
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(m), REDUCTION(+:eTemp)
         do t=1,m%s
           do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + (u%x%RF(t)%f(i,j,k)**2.0_cp +&
                              u%y%RF(t)%f(i,j,k)**2.0_cp +&
                              u%z%RF(t)%f(i,j,k)**2.0_cp)*m%g(t)%c(1)%dhn(i)*&
                                                          m%g(t)%c(2)%dhn(j)*&
                                                          m%g(t)%c(3)%dhn(k)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = etemp/m%volume
       end subroutine

       subroutine boundaryFlux_VF(BF,u,m) ! Finished
         ! Computes
         ! 
         !   BF = ∫∫ u•n dA
         ! 
         implicit none
         type(VF),intent(in) :: u
         real(cp),intent(inout) :: BF
         type(mesh),intent(in) :: m
         real(cp) :: BFtemp,dA
         integer :: i,j,k,t
         BFtemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(m),PRIVATE(dA), REDUCTION(+:BFtemp)
         do t=1,m%s; do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1
           dA = m%g(t)%c(2)%dhn(j)*m%g(t)%c(3)%dhn(k)
           BFtemp = BFtemp + u%x%RF(t)%f(      2         ,j,k)*dA
           BFtemp = BFtemp + u%x%RF(t)%f(u%x%RF(t)%s(1)-1,j,k)*dA
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         BF = BFtemp; BFtemp = 0.0_cp
         !$OMP PARALLEL DO SHARED(m),PRIVATE(dA), REDUCTION(+:BFtemp)
         do t=1,m%s; do k=2,u%y%RF(t)%s(3)-1; do i=2,u%y%RF(t)%s(1)-1
           dA = m%g(t)%c(1)%dhn(i)*m%g(t)%c(3)%dhn(k)
           BFtemp = BFtemp + u%y%RF(t)%f(i,      2         ,k)*dA
           BFtemp = BFtemp + u%y%RF(t)%f(i,u%y%RF(t)%s(2)-1,k)*dA
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         BF = BF + BFtemp; BFtemp = 0.0_cp
         !$OMP PARALLEL DO SHARED(m),PRIVATE(dA), REDUCTION(+:BFtemp)
         do t=1,m%s; do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
           dA = m%g(t)%c(1)%dhn(i)*m%g(t)%c(2)%dhn(j)
           BFtemp = BFtemp + u%z%RF(t)%f(i,j,      2         )*dA
           BFtemp = BFtemp + u%z%RF(t)%f(i,j,u%z%RF(t)%s(3)-1)*dA
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         BF = BF + BFtemp
       end subroutine

       subroutine subtract_physical_mean_SF(u)
         ! Subtracts the physical mean from scalar field u
         ! 
         !      u = u - mean(u)
         ! 
         ! Where this mean operation is only in the interior domain
         implicit none
         type(SF),intent(inout) :: u
         real(cp) :: meanU
         call zeroGhostPoints(u)
         meanU = sum(u)/real(u%numPhysEl,cp)
         call subtract(u,meanU)
         call zeroGhostPoints(u)
       end subroutine

      function dot_product_VF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: A,B,x
        type(VF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp%x) + sum(temp%y) + sum(temp%z)
      end function

      function dot_product_SF(A,B,m,x,temp) result(dot)
        implicit none
        type(mesh),intent(in) :: m
        type(SF),intent(in) :: A,B,x
        type(SF),intent(inout) :: temp
        real(cp) :: dot
        call multiply(temp,A,B)
        call zeroGhostPoints(temp)
        call zeroWall_conditional(temp,m,x)
        dot = sum(temp)
      end function

       ! *********************************************************************************
       ! *********************************************************************************
       ! ******************************* VECTOR ROUTINES *********************************
       ! *********************************************************************************
       ! *********************************************************************************

       subroutine totalEnergy_VF_SD(e,f,D)
         implicit none
         type(VF),intent(in) :: f
         real(cp),intent(inout) :: e
         type(domain),intent(in) :: D
         type(VF) :: temp
         if (.not.f%is_CC) stop 'Error: Total energy must be computed on CC in totalEnergy_VF_SD in ops_aux.f90'
         call init_CC(temp,D%m_in)
         call extractCC(temp,f,D)
         call totalEnergy(e,temp,D%m_in)
         call delete(temp)
       end subroutine

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