       module norms_mod
       ! Computes L_1,L_2,L_inf norms of a scalar field.
       !
       ! Input / Output:
       !        u  = scalar field
       !        m  = mesh
       !        norm = {L_1,L_2,L_inf}
       ! 
       ! L-n norm computations:
       !                             1     NxNyNz
       !               Ln(u)     = ------   sum  abs( u(i,j,k) )^n
       !                           NxNyNz  ijk=1
       ! 
       ! L-infinity norm computations:
       !                             1
       !               Linf(u)   = ------  max( abs( u(i,j,k) ) )
       !                           NxNyNz   
       ! 

       use IO_tools_mod
       use grid_mod
       use mesh_mod
       use RF_mod
       use SF_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private

       public :: norms
       public :: init,compute,compute_MPI
       public :: print
       public :: export,exportList

       type norms
         real(cp) :: L1,L2,Linf ! Absolute errors:
       end type

       interface init;            module procedure initError;              end interface
       interface init;            module procedure initCopy;               end interface

       interface export;          module procedure exportNorms;            end interface
       interface export;          module procedure exportNorms2;           end interface
       interface exportList;      module procedure exportNormsList;        end interface
       interface exportList;      module procedure exportNormsList2;       end interface
       interface print;           module procedure printNorms;             end interface

       interface compute;         module procedure compute_norms;          end interface
       interface compute_MPI;     module procedure compute_Ln_tot;         end interface

       contains

       ! **************************************************************
       ! **************************************************************
       ! **************************** INIT ****************************
       ! **************************************************************
       ! **************************************************************

       subroutine initError(e)
         implicit none
         type(norms),intent(inout) :: e
         e%L1 = 0.0_cp; e%L2 = 0.0_cp; e%Linf = 0.0_cp
       end subroutine

       subroutine initCopy(eCopy,e)
         implicit none
         type(norms),intent(inout) :: eCopy
         type(norms),intent(in) :: e
         eCopy%L1 = e%L1; eCopy%L2 = e%L2; eCopy%Linf = e%Linf
       end subroutine

       ! **************************************************************
       ! **************************************************************
       ! *********************** PRINT / EXPORT ***********************
       ! **************************************************************
       ! **************************************************************

       subroutine printNorms(err,name)
         implicit none
         type(norms),intent(in) :: err
         character(len=*),intent(in) :: name
         call writeNormsToFileOrScreen(err,name,6)
       end subroutine

       subroutine exportNorms(err,dir,name)
         implicit none
         type(norms),intent(in) :: err
         character(len=*),intent(in) :: dir,name
         integer :: newU
         newU = newAndOpen(dir,'Errors_'//trim(adjustl(name)))
         call writeNormsToFileOrScreen(err,name,newU)
         call closeAndMessage(newU,trim(adjustl(name)),dir)
       end subroutine

       subroutine exportNorms2(e,dir,name,num,u)
         implicit none
         type(norms),dimension(:),intent(in) :: e
         character(len=*),intent(in) :: dir,name
         integer,intent(in) :: num
         integer,optional,intent(in) :: u
         integer :: temp,s,i
         s = size(e)
         if (present(u)) then
               temp = u
         else; temp = newAndOpen(dir,name)
         endif
         select case (num)
         case (1); write(temp,'('//int2str2(s)//arrfmt//')') (/(e(i)%L1,i=1,s)/)
         case (2); write(temp,'('//int2str2(s)//arrfmt//')') (/(e(i)%L2,i=1,s)/)
         case (3); write(temp,'('//int2str2(s)//arrfmt//')') (/(e(i)%Linf,i=1,s)/)
         case default
         stop 'Error: num must = 1:3 in exportNorms2 in norms.f90'
         end select
       end subroutine

       subroutine exportNormsList2(e,dir,name,u)
         implicit none
         type(norms),dimension(:),intent(in) :: e
         character(len=*),intent(in) :: dir,name
         integer,optional,intent(in) :: u
         integer :: temp,s,i
         s = size(e)
         if (present(u)) then
               temp = u
         else; temp = newAndOpen(dir,name)
         endif
         write(temp,'(6(A10))') 'L1','L2','Linf'
         do i=1,s
           call exportList(e(i),dir,name,temp)
         enddo
       end subroutine

       subroutine exportNormsList(e,dir,name,u)
         implicit none
         type(norms),intent(in) :: e
         character(len=*),intent(in) :: dir,name
         integer,optional,intent(in) :: u
         integer :: temp
         if (present(u)) then
               temp = u
         else; temp = newAndOpen(dir,name)
         endif
           write(temp,'(6'//arrfmt//')') e%L1, e%L2, e%Linf
       end subroutine

       subroutine writeNormsToFileOrScreen(err,name,newU)
         implicit none
         type(norms),intent(in) :: err
         integer,intent(in) :: newU
         character(len=*),intent(in) :: name
         write(newU,*) ''
         write(newU,*) '++++++++++++++ '//trim(adjustl(name))//&
                   ' +++++++++++++++'
         write(newU,*) 'L1 = ',err%L1
         write(newU,*) 'L2 = ',err%L2
         write(newU,*) 'Linf = ',err%Linf
         write(newU,*) '++++++++++++++++++++++++++++',&
                    '++++++++++++++++++++++++++++'
         write(newU,*) ''
       end subroutine

       ! **************************************************************
       ! **************************************************************
       ! ************************ COMPUTATIONS ************************
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
         eTemp = real(0.0,cp) ! temp is necessary for reduction
         !$OMP PARALLEL DO SHARED(g), REDUCTION(+:eTemp)
         do k=2,u%s(3)-1; do j=2,u%s(2)-1; do i=2,u%s(1)-1
           eTemp = eTemp + (u%f(i,j,k)**n)*g%c(1)%dhn(i)*&
                                           g%c(2)%dhn(j)*&
                                           g%c(3)%dhn(k)
         enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine compute_Ln_tot(e,u,m,n) ! MPI friendly
         implicit none
         type(SF),intent(in) :: u
         real(cp),intent(inout) :: e
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i
         eTemp = 0.0_cp
         do i=1,u%s
           call compute_sum_grid(eTemp,u%RF(i),m%g(i),n)
           e = e + eTemp
         enddo
         e = e**(1.0_cp/n)/m%volume
       end subroutine

       subroutine compute_Ln(e,u,m,n) ! Open-MP friendly
         ! Computes
         ! 
         !          1
         !   e = -------- { ∫∫∫ | u(i,j,k)^n | }^(1/n) dx dy dz
         !        volume
         ! 
         ! Where x,y,z lives in the cell center.
         implicit none
         type(SF),intent(in) :: u
         real(cp),intent(inout) :: e
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = real(0.0,cp) ! temp is necessary for reduction
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

       subroutine compute_norms(e,u,m)
         implicit none
         type(norms),intent(inout) :: e
         type(mesh),intent(in) :: m
         type(SF),intent(in) :: u
         real(cp) :: n
         call initError(e)
         n = 1.0_cp; call compute_Ln(e%L1,u,m,n)
         n = 2.0_cp; call compute_Ln(e%L2,u,m,n)
         e%Linf = maxabs(u)
       end subroutine

       end module