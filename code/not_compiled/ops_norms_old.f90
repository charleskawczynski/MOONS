       module norms_mod
       ! Computes L_1,L_2,L_inf norms of a SF or VF.
       !
       ! L-n norm computations:
       !                           NxNyNz
       !               Ln(u)     =  sum  abs( u(i,j,k) )^n
       !                           ijk=1
       ! 
       ! L-infinity norm computations:
       !                             1
       !               Linf(u)   = ------  max( abs( u(i,j,k) ) )
       !                           NxNyNz   
       ! 
       ! Computes
       ! 
       !          1
       !   e = -------- { ∫∫∫ | u(i,j,k)^n | }^(1/n) dx dy dz
       !        volume
       use current_precision_mod
       use IO_tools_mod
       use grid_mod
       use mesh_mod
       use RF_mod
       use SF_mod
       use VF_mod
       implicit none

       private
       public :: norms,init
       public :: print,export
       public :: compute

       type norms
         real(cp) :: L1,L2,Linf
       end type

       interface init;            module procedure init_norms;              end interface
       interface init;            module procedure init_copy;               end interface
       interface export;          module procedure export_norms;            end interface
       interface export;          module procedure export_norms_dir;        end interface
       interface print;           module procedure print_norms;             end interface

       interface compute;         module procedure compute_norms_mesh;     end interface
       interface compute;         module procedure compute_norms;          end interface
       interface compute;         module procedure compute_norms_mesh_VF;  end interface
       interface compute;         module procedure compute_norms_VF;       end interface

       interface compute_Ln;      module procedure compute_Ln_SF;          end interface
       interface compute_Ln;      module procedure compute_Ln_VF;          end interface
       interface compute_Ln_mesh; module procedure compute_Ln_mesh_SF;     end interface
       interface compute_Ln_mesh; module procedure compute_Ln_mesh_VF;     end interface

       contains

       ! **************************************************************
       ! **************************** INIT ****************************
       ! **************************************************************

       subroutine init_norms(e)
         implicit none
         type(norms),intent(inout) :: e
         e%L1 = 0.0_cp; e%L2 = 0.0_cp; e%Linf = 0.0_cp
       end subroutine

       subroutine init_copy(eCopy,e)
         implicit none
         type(norms),intent(inout) :: eCopy
         type(norms),intent(in) :: e
         eCopy%L1 = e%L1; eCopy%L2 = e%L2; eCopy%Linf = e%Linf
       end subroutine

       ! **************************************************************
       ! *********************** PRINT / EXPORT ***********************
       ! **************************************************************

       subroutine print_norms(norm,name)
         implicit none
         type(norms),intent(in) :: norm
         character(len=*),intent(in) :: name
         call export_norms(norm,name,6)
       end subroutine

       subroutine export_norms_dir(norm,dir,name)
         implicit none
         type(norms),intent(in) :: norm
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = newAndOpen(dir,'norms_'//name)
         call export_norms(norm,name,un)
         call closeAndMessage(un,name,dir)
       end subroutine

       subroutine export_norms(norm,name,un)
         implicit none
         type(norms),intent(in) :: norm
         integer,intent(in) :: un
         character(len=*),intent(in) :: name
         write(un,*) '++++++++++++++ '//trim(adjustl(name))//' +++++++++++++++'
         write(un,*) 'L1 = ',norm%L1
         write(un,*) 'L2 = ',norm%L2
         write(un,*) 'Linf = ',norm%Linf
         write(un,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
       end subroutine

       ! **************************************************************
       ! ************************ COMPUTATIONS ************************
       ! **************************************************************

       subroutine compute_Ln_mesh_SF(e,u,m,n) ! Open-MP friendly
         ! Computes
         ! 
         !          1
         !   e = -------- ( ∫∫∫ | u(i,j,k)ⁿ | )⁻ⁿ dx dy dz
         !        volume
         ! 
         ! Where x,y,z lives in the cell center.
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(m), REDUCTION(+:eTemp)
         do t=1,u%s
           do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%RF(t)%f(i,j,k)**n)*m%g(t)%c(1)%dhn(i)*&
                                                      m%g(t)%c(2)%dhn(j)*&
                                                      m%g(t)%c(3)%dhn(k)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = eTemp**(1.0_cp/n)/m%volume
       end subroutine

       subroutine compute_Ln_mesh_VF(e,u,m,n) ! Open-MP friendly
         ! Computes
         ! 
         !          1
         !   e = -------- ( ∫∫∫ | u(i,j,k)ⁿ | )⁻ⁿ dx dy dz
         !        volume
         ! 
         ! Where x,y,z lives in the cell center.
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(m), REDUCTION(+:eTemp)
         do t=1,u%x%s
           do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%x%RF(t)%f(i,j,k)**n)*m%g(t)%c(1)%dhn(i)*&
                                                        m%g(t)%c(2)%dhn(j)*&
                                                        m%g(t)%c(3)%dhn(k)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = eTemp; eTemp = 0.0_cp
         !$OMP PARALLEL DO SHARED(m), REDUCTION(+:eTemp)
         do t=1,u%y%s
           do k=2,u%y%RF(t)%s(3)-1; do j=2,u%y%RF(t)%s(2)-1; do i=2,u%y%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%y%RF(t)%f(i,j,k)**n)*m%g(t)%c(1)%dhn(i)*&
                                                        m%g(t)%c(2)%dhn(j)*&
                                                        m%g(t)%c(3)%dhn(k)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = e+eTemp; eTemp = 0.0_cp
         !$OMP PARALLEL DO SHARED(m), REDUCTION(+:eTemp)
         do t=1,u%z%s
           do k=2,u%z%RF(t)%s(3)-1; do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%z%RF(t)%f(i,j,k)**n)*m%g(t)%c(1)%dhn(i)*&
                                                        m%g(t)%c(2)%dhn(j)*&
                                                        m%g(t)%c(3)%dhn(k)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = e+eTemp
         e = e**(1.0_cp/n)/m%volume
       end subroutine

       subroutine compute_Ln_SF(e,u,n) ! Open-MP friendly
         ! Computes
         ! 
         !   e = ( ∫∫∫ | u(i,j,k)ⁿ | )⁻ⁿ
         ! 
         ! Where x,y,z lives in the cell center.
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%s
           do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%RF(t)%f(i,j,k)**n)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = eTemp**(1.0_cp/n)
       end subroutine

       subroutine compute_Ln_VF(e,u,n) ! Open-MP friendly
         ! Computes
         ! 
         !   e = ( ∫∫∫ | u(i,j,k)ⁿ | )⁻ⁿ
         ! 
         ! Where x,y,z lives in the cell center.
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%x%s
           do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%x%RF(t)%f(i,j,k)**n)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = eTemp; eTemp = 0.0_cp
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%y%s
           do k=2,u%y%RF(t)%s(3)-1; do j=2,u%y%RF(t)%s(2)-1; do i=2,u%y%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%y%RF(t)%f(i,j,k)**n)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = e + eTemp; eTemp = 0.0_cp
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%z%s
           do k=2,u%z%RF(t)%s(3)-1; do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
             eTemp = eTemp + abs(u%z%RF(t)%f(i,j,k)**n)
           enddo; enddo; enddo
         enddo
         !$OMP END PARALLEL DO
         e = e + eTemp
         e = e**(1.0_cp/n)
       end subroutine

       subroutine compute_norms_mesh(e,u,m)
         implicit none
         type(norms),intent(inout) :: e
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: u
         call compute_Ln_mesh(e%L1,u,m,1.0_cp)
         call compute_Ln_mesh(e%L2,u,m,2.0_cp)
         e%Linf = maxabs(u)
       end subroutine

       subroutine compute_norms(e,u)
         implicit none
         type(norms),intent(inout) :: e
         type(SF),intent(in) :: u
         call compute_Ln(e%L1,u,1.0_cp)
         call compute_Ln(e%L2,u,2.0_cp)
         e%Linf = maxabs(u)
       end subroutine

       subroutine compute_norms_mesh_VF(e,u,m)
         implicit none
         type(norms),intent(inout) :: e
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: u
         call compute_Ln_mesh(e%L1,u,m,1.0_cp)
         call compute_Ln_mesh(e%L2,u,m,2.0_cp)
         e%Linf = maxabs(u)
       end subroutine

       subroutine compute_norms_VF(e,u)
         implicit none
         type(norms),intent(inout) :: e
         type(VF),intent(in) :: u
         call compute_Ln(e%L1,u,1.0_cp)
         call compute_Ln(e%L2,u,2.0_cp)
         e%Linf = maxabs(u)
       end subroutine

       ! **************************************************************
       ! **************************************************************
       ! ************************ MPI FRIENDLY ************************
       ! **************************************************************
       ! **************************************************************

       subroutine compute_sum_grid(e,u,g,n) ! Finished
         ! Computes
         ! 
         !          1
         !   e = -------- { ∫∫∫ ( u(i,j,k)^n ) }^(1/n) dx dy dz
         !        volume
         ! 
         ! Where x,y,z lives in the cell center.
         implicit none
         type(realField),intent(in) :: u
         real(cp),intent(inout) :: e
         type(grid),intent(in) :: g
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(g), REDUCTION(+:eTemp)
         do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
           eTemp = eTemp + (u%f(i,j,k)**n)*g%c(1)%dhn(i)*&
                                           g%c(2)%dhn(j)*&
                                           g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       end module