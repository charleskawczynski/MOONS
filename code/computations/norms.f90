       module norms_mod
       ! This module calculates the L1,L2 and Linfinity norm of a given
       ! 3D exact and approximate matrices.
       !
       ! Input:
       ! exact  = 3D exact solution to compare against
       ! approx = 3D approximate solution
       !
       ! Output:
       !        e%L1 = L1 norm
       !        e%L2 = L2 norm
       !        e%Linf = Linf norm (maximum of abs(exact - approx) )
       !
       !        e%R1 = L1 norm
       !        e%R2 = L2 norm
       !        e%Rinf = Linf norm (maximum of abs(exact - approx) )
       ! 
       ! The L-n norm is computed as:
       ! 
       !                      1     NxNyNz
       !            L-n   = ------   sum  (abs(exact(i,j,k) - approx(i,j,k)))^n
       !                    NxNyNz  ijk=1
       ! 
       ! The L-infinity norm is computed as:
       !                      1      
       !            L-n   = ------  max(abs(exact(i,j,k) - approx(i,j,k)))
       !                    NxNyNz   
       ! The L-n relative norm is computed as:
       ! 
       !                           NxNyNz
       !                            sum  (abs(exact(i,j,k) - approx(i,j,k)))^n
       !                           ijk=1
       !    L-n relative  =       --------------------------------------------
       !                                     NxNyNz
       !                                      sum  (exact(i,j,k))^n
       !                                     ijk=1
       ! 
       ! The L-infinity relative norm is computed as:
       ! 
       !                            max(abs(exact - approx))
       !    L-n relative  =       --------------------------
       !                                max(abs(exact))
       ! 
       ! Note: In the case that the exact solution is zero, the relative norms
       !       are computed as ((exact-approx)+1)/(exact+1) as to avoid division
       !       by zero.
       !

       use IO_tools_mod
       use grid_mod
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

       real(cp),parameter :: zero = 0.0_cp
       real(cp),parameter :: one = 1.0_cp
       real(cp),parameter :: tol = 10.0_cp**(-6.0_cp) ! Minimum number to divide by when computing error.

       private

       public :: norms
       public :: init
       public :: compute
       public :: print
       public :: export,exportList

       type norms
         ! private
         real(cp) :: L1,L2,Linf ! Absolute errors:
         real(cp) :: R1,R2,Rinf ! Relative errors:
       end type

       interface init;            module procedure initError;              end interface
       interface init;            module procedure initCopy;               end interface

       interface export;          module procedure exportNorms;            end interface
       interface export;          module procedure exportNorms2;           end interface
       interface exportList;      module procedure exportNormsList;        end interface
       interface exportList;      module procedure exportNormsList2;       end interface
       interface print;           module procedure printNorms;             end interface

       interface compute;         module procedure Compute_exact_present;  end interface
       interface compute;         module procedure Compute_exact_absent;   end interface

       interface LnError;         module procedure Ln_exact_absent;         end interface
       interface LnError;         module procedure Ln_exact_present;        end interface

       contains

       ! **************************************************************
       ! **************************************************************
       ! **************************** INIT ****************************
       ! **************************************************************
       ! **************************************************************

       subroutine initError(e)
         implicit none
         type(norms),intent(inout) :: e
         e%L1 = zero; e%L2 = zero; e%Linf = zero
         e%R1 = zero; e%R2 = zero; e%Rinf = zero
       end subroutine

       subroutine initCopy(eCopy,e)
         implicit none
         type(norms),intent(inout) :: eCopy
         type(norms),intent(in) :: e
         eCopy%L1 = e%L1; eCopy%L2 = e%L2; eCopy%Linf = e%Linf
         eCopy%R1 = e%R1; eCopy%R2 = e%R2; eCopy%Rinf = e%Rinf
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
         case (4); write(temp,'('//int2str2(s)//arrfmt//')') (/(e(i)%R1,i=1,s)/)
         case (5); write(temp,'('//int2str2(s)//arrfmt//')') (/(e(i)%R2,i=1,s)/)
         case (6); write(temp,'('//int2str2(s)//arrfmt//')') (/(e(i)%Rinf,i=1,s)/)
         case default
         stop 'Error: num must = 1:6 in exportNorms2 in norms.f90'
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
         write(temp,'(6(A10))') 'L1','L2','Linf','R1','R2','Rinf'
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
           write(temp,'(6'//arrfmt//')') e%L1, e%L2, e%Linf, e%R1, e%R2, e%Rinf
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
         write(newU,*) 'R1 = ',err%R1
         write(newU,*) 'R2 = ',err%R2
         write(newU,*) 'Rinf = ',err%Rinf
         write(newU,*) '++++++++++++++++++++++++++++',&
                    '++++++++++++++++++++++++++++'
         write(newU,*) ''
       end subroutine

       ! **************************************************************
       ! **************************************************************
       ! ************************ COMPUTATIONS ************************
       ! **************************************************************
       ! **************************************************************

       subroutine Ln_exact_present(exact,approx,g,n,e,er,denom)
         implicit none
         type(SF) :: approx
         type(SF),intent(in) :: exact
         real(cp),intent(in) :: n
         real(cp),intent(inout) :: e,er,denom
         type(grid),intent(in) :: g
         real(cp) :: vol
         real(cp) :: eTemp,denomTemp
         integer :: i,j,k,t
         e = zero; denom = zero
         eTemp = zero; denomTemp = zero; vol = zero
         do t=1,approx%s
           !$OMP PARALLEL DO SHARED(n), REDUCTION(+:eTemp,denomTemp)
           do k=1,approx%RF(t)%s(3)
             do j=1,approx%RF(t)%s(2)
               do i=1,approx%RF(t)%s(1)
                 eTemp = eTemp + abs(exact%RF(t)%f(i,j,k) - approx%RF(t)%f(i,j,k))**n &
                 *g%c(1)%dhn(i)*&
                  g%c(2)%dhn(j)*&
                  g%c(3)%dhn(k)
                 denomTemp = denomTemp + abs(exact%RF(t)%f(i,j,k))**n
               enddo
             enddo
           enddo
           !$OMP END PARALLEL DO
         enddo
         e = etemp
         denom = denomTemp

         e = e**(one/n)/g%volume
         denom = denom/g%volume
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+one); endif
       end subroutine

       subroutine Ln_exact_absent(approx,g,n,e,er)
         implicit none
         type(SF) :: approx
         real(cp),intent(in) :: n
         real(cp),intent(inout) :: e,er
         type(grid),intent(in) :: g
         real(cp) :: eTemp
         integer :: i,j,k,t
         e = zero; eTemp = zero
         do t=1,approx%s
           !$OMP PARALLEL DO SHARED(n), REDUCTION(+:eTemp)
           do k=1,approx%RF(t)%s(3)
             do j=1,approx%RF(t)%s(2)
               do i=1,approx%RF(t)%s(1)
                 eTemp = eTemp + abs(approx%RF(t)%f(i,j,k))**n
               enddo
             enddo
           enddo
           !$OMP END PARALLEL DO
         enddo
         e = etemp;   e = e**(one/n)/g%volume;   er = e
       end subroutine

       subroutine Compute_exact_present(e,exact,approx,g)
         implicit none
         type(norms),intent(inout) :: e
         type(grid),intent(in) :: g
         type(SF),intent(in) :: exact,approx
         real(cp) :: n,denom

         call initError(e)
         n = 1.0_cp; call Ln_exact_present(exact,approx,g,n,e%L1,e%R1,denom)
         n = 2.0_cp; call Ln_exact_present(exact,approx,g,n,e%L2,e%R2,denom)
         !n = infinity
         e%Linf = maxabsdiff(exact,approx)
         if (denom.gt.tol) then; e%Rinf = e%Linf/maxabs(exact)
         else; e%Rinf = (e%Linf)/(maxabs(exact)+one); endif
       end subroutine

       subroutine Compute_exact_absent(e,approx,g)
         implicit none
         type(norms),intent(inout) :: e
         type(grid),intent(in) :: g
         type(SF),intent(in) :: approx
         real(cp) :: n

         call initError(e)
         n = 1.0_cp; call Ln_exact_absent(approx,g,n,e%L1,e%R1)
         n = 2.0_cp; call Ln_exact_absent(approx,g,n,e%L2,e%R2)
         !n = infinity
         e%Linf = maxabs(approx)
         e%Rinf = e%Linf
       end subroutine

       end module