       module myError_mod
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
       !        e%L1rel = L1 norm
       !        e%L2rel = L2 norm
       !        e%Linfrel = Linf norm (maximum of abs(exact - approx) )
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
       ! Fixes/improvements:
       ! - make set/get functions to make components accessable to myEfficiency

       use constants_mod
       use myTime_mod
       use simParams_mod
       use myIO_mod
       implicit none

       private

       public :: myError
       public :: computeError ! computeError(exact,approx) result(e)
       public :: printMyError ! printMyError(err,name)
       public :: writeMyError ! writeMyError(err,name,dir)
       public :: writeToFile  ! writeErrorsToFile(e,dir[,u])
       public :: initialize

       public :: getL1, getL1Rel
       public :: getL2, getL2Rel
       public :: getLinf, getLinfRel
       public :: copyMyError

       real(dpn),parameter :: tol = 10.0**(-6.0) ! Minimum number to divide by when computing error.

       type myError
         private
         ! Absolute errors:
         real(dpn) :: L1,L2,Linf
         ! Relative errors:
         real(dpn) :: L1rel,L2rel,Linfrel
       end type

       interface initialize;      module procedure initializeError;        end interface
       interface writeToFile;     module procedure writeErrorsToFile;      end interface

       interface computeError;    module procedure computeError1;          end interface
       interface computeError;    module procedure computeError1Uniform;   end interface
       interface computeError;    module procedure computeError2;          end interface
       interface computeError;    module procedure computeError2Uniform;   end interface
       interface computeError;    module procedure computeError3;          end interface
       interface computeError;    module procedure computeError3Uniform;   end interface

       interface LnError;         module procedure LnError1D;              end interface
       interface LnError;         module procedure LnError1DUniform;       end interface
       interface LnError;         module procedure LnError2D;              end interface
       interface LnError;         module procedure LnError2DUniform;       end interface
       interface LnError;         module procedure LnError3D;              end interface
       interface LnError;         module procedure LnError3DUniform;       end interface


       contains

       subroutine printMyError(err,name)
         implicit none
         type(myError),intent(in) :: err
         character(len=*),intent(in) :: name
         call writeMyErrorToFileOrScreen(err,name,6)
       end subroutine

       subroutine writeMyError(err,dir,name)
         implicit none
         type(myError),intent(in) :: err
         character(len=*),intent(in) :: dir
         character(len=*),intent(in) :: name
         integer :: newU
         newU = newAndOpen(dir,'Errors_'//trim(adjustl(name)))
         call writeMyErrorToFileOrScreen(err,name,newU)
         call closeAndMessage(newU,trim(adjustl(name)),dir)
       end subroutine

       subroutine writeMyErrorToFileOrScreen(err,name,newU)
         implicit none
         type(myError),intent(in) :: err
         integer,intent(in) :: newU
         character(len=*),intent(in) :: name
         write(newU,*) ''
         write(newU,*) '++++++++++++++ '//trim(adjustl(name))//&
                   ' +++++++++++++++'
         write(newU,*) ''
         write(newU,*) '------------- Absolute Value -------------'
         write(newU,*) 'L1 = ',err%L1
         write(newU,*) 'L2 = ',err%L2
         write(newU,*) 'Linf = ',err%Linf, ' (largest difference in set)'
         write(newU,*) ''
         write(newU,*) '---------- Normalized by Exact -----------'
         write(newU,*) 'L1_normalized = ',err%L1rel
         write(newU,*) 'L2_normalized = ',err%L2rel
         write(newU,*) 'Linf_normalized = ',err%Linfrel
         write(newU,*) ''
         write(newU,*) '++++++++++++++++++++++++++++',&
                    '++++++++++++++++++++++++++++'
         write(newU,*) ''
       end subroutine

       function getL1(this) result(L1)
         implicit none
         type(myError),intent(in) :: this
         real(dpn) :: L1
         L1 = this%L1
       end function
       
       function getL1Rel(this) result(L1rel)
         implicit none
         type(myError),intent(in) :: this
         real(dpn) :: L1rel
         L1rel = this%L1rel
       end function

       function getL2(this) result(L2)
         implicit none
         type(myError),intent(in) :: this
         real(dpn) :: L2
         L2 = this%L2
       end function
       
       function getL2Rel(this) result(L2rel)
         implicit none
         type(myError),intent(in) :: this
         real(dpn) :: L2rel
         L2rel = this%L2rel
       end function

       function getLinf(this) result(Linf)
         implicit none
         type(myError),intent(in) :: this
         real(dpn) :: Linf
         Linf = this%Linf
       end function
       
       function getLinfRel(this) result(Linfrel)
         implicit none
         type(myError),intent(in) :: this
         real(dpn) :: Linfrel
         Linfrel = this%Linfrel
       end function

       subroutine writeErrorsToFile(e,dir,u)
         implicit none
         type(myError) :: e
         character(len=*),intent(in) :: dir
         integer,optional,intent(in) :: u
         integer :: temp
         if (present(u)) then
           write(u,'(6'//arrfmt//')') e%L1, e%L2, e%Linf, e%L1rel, e%L2rel, e%Linfrel
         else
           temp = newAndOpen(dir,'errors')
           write(temp,'(6'//arrfmt//')') e%L1, e%L2, e%Linf, e%L1rel, e%L2rel, e%Linfrel
         endif
       end subroutine

       subroutine LnError1D(exact,approx,n,e,er,denom)
         implicit none
         real(dpn),intent(in),dimension(:) :: exact,approx
         real(dpn),intent(in) :: n
         real(dpn),intent(inout) :: e,er,denom
         integer,dimension(2) :: s
         real(dpn) :: eTemp,denomTemp
         integer :: i
         s = size(exact); e = 0.0; denom = 0.0
         eTemp = 0.0; denomTemp = 0.0
         !$OMP PARALLEL
         !$OMP DO
         do i=1,s(1)
           eTemp = eTemp + abs(exact(i) - approx(i))**n
           denomTemp = denomTemp + abs(exact(i))**n
         enddo
         !$OMP END DO

         !$OMP ATOMIC
         e = e + etemp
         denom = denom + denomTemp
         !$OMP END PARALLEL

         e = e**(1.0/n)/(dble(s(1)))
         denom = denom/(dble(s(1)))
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+1.0); endif
       end subroutine

       subroutine LnError1DUniform(exact,approx,n,e,er,denom)
         implicit none
         real(dpn),intent(in),dimension(:) :: approx
         real(dpn),intent(in) :: exact
         real(dpn),intent(in) :: n
         real(dpn),intent(inout) :: e,er,denom
         integer,dimension(2) :: s
         real(dpn) :: eTemp,denomTemp
         integer :: i,j
         s = size(approx); e = 0.0; denom = 0.0
         eTemp = 0.0; denomTemp = 0.0
         !$OMP PARALLEL
         !$OMP DO
         do j=1,s(2)
           do i=1,s(1)
             e = e + abs(exact - approx(i))**n
             denom = denom + abs(exact)**n
           enddo
         enddo
         !$OMP END DO
         
         !$OMP ATOMIC
         e = e + etemp
         denom = denom + denomTemp
         !$OMP END PARALLEL

         e = e**(1.0/n)/(dble(s(1)))
         denom = denom/(dble(s(1)))
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+1.0); endif
       end subroutine

       subroutine LnError2D(exact,approx,n,e,er,denom)
         implicit none
         real(dpn),intent(in),dimension(:,:) :: exact,approx
         real(dpn),intent(in) :: n
         real(dpn),intent(inout) :: e,er,denom
         integer,dimension(2) :: s
         real(dpn) :: eTemp,denomTemp
         integer :: i,j
         s = shape(exact); e = 0.0; denom = 0.0
         eTemp = 0.0; denomTemp = 0.0
         !$OMP PARALLEL
         !$OMP DO
         do j=1,s(2)
           do i=1,s(1)
             eTemp = eTemp + abs(exact(i,j) - approx(i,j))**n
             denomTemp = denomTemp + abs(exact(i,j))**n
           enddo
         enddo
         !$OMP END DO

         !$OMP ATOMIC
         e = e + etemp
         denom = denom + denomTemp
         !$OMP END PARALLEL

         e = e**(1.0/n)/(dble(s(1)*s(2)))
         denom = denom/(dble(s(1)*s(2)))
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+1.0); endif
       end subroutine

       subroutine LnError2DUniform(exact,approx,n,e,er,denom)
         implicit none
         real(dpn),intent(in),dimension(:,:) :: approx
         real(dpn),intent(in) :: exact
         real(dpn),intent(in) :: n
         real(dpn),intent(inout) :: e,er,denom
         integer,dimension(2) :: s
         real(dpn) :: eTemp,denomTemp
         integer :: i,j
         s = shape(approx); e = 0.0; denom = 0.0
         eTemp = 0.0; denomTemp = 0.0
         !$OMP PARALLEL
         !$OMP DO
         do j=1,s(2)
           do i=1,s(1)
             e = e + abs(exact - approx(i,j))**n
             denom = denom + abs(exact)**n
           enddo
         enddo
         !$OMP END DO
         
         !$OMP ATOMIC
         e = e + etemp
         denom = denom + denomTemp
         !$OMP END PARALLEL

         e = e**(1.0/n)/(dble(s(1)*s(2)))
         denom = denom/(dble(s(1)*s(2)))
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+1.0); endif
       end subroutine

       subroutine LnError3D(exact,approx,n,e,er,denom)
         implicit none
         real(dpn),intent(in),dimension(:,:,:) :: exact,approx
         real(dpn),intent(in) :: n
         real(dpn),intent(inout) :: e,er,denom
         integer,dimension(3) :: s
         real(dpn) :: eTemp,denomTemp
         integer :: i,j,k
         s = shape(exact); e = 0.0; denom = 0.0
         eTemp = 0.0; denomTemp = 0.0
         !$OMP PARALLEL
         !$OMP DO
         do k=1,s(3)
           do j=1,s(2)
             do i=1,s(1)
               eTemp = eTemp + abs(exact(i,j,k) - approx(i,j,k))**n
               denomTemp = denomTemp + abs(exact(i,j,k))**n
             enddo
           enddo
         enddo
         !$OMP END DO

         !$OMP ATOMIC
         e = e + etemp
         denom = denom + denomTemp
         !$OMP END PARALLEL

         e = e**(1.0/n)/(dble(s(1)*s(2)*s(3)))
         denom = denom/(dble(s(1)*s(2)*s(3)))
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+1.0); endif
       end subroutine

       subroutine LnError3DUniform(exact,approx,n,e,er,denom)
         implicit none
         real(dpn),intent(in),dimension(:,:,:) :: approx
         real(dpn),intent(in) :: exact
         real(dpn),intent(in) :: n
         real(dpn),intent(inout) :: e,er,denom
         integer,dimension(3) :: s
         real(dpn) :: eTemp,denomTemp
         integer :: i,j,k
         s = shape(approx); e = 0.0; denom = 0.0
         eTemp = 0.0; denomTemp = 0.0
         !$OMP PARALLEL
         !$OMP DO
         do k=1,s(3)
           do j=1,s(2)
             do i=1,s(1)
               e = e + abs(exact - approx(i,j,k))**n
               denom = denom + abs(exact)**n
             enddo
           enddo
         enddo
         !$OMP END DO
         
         !$OMP ATOMIC
         e = e + etemp
         denom = denom + denomTemp
         !$OMP END PARALLEL

         e = e**(1.0/n)/(dble(s(1)*s(2)*s(3)))
         denom = denom/(dble(s(1)*s(2)*s(3)))
         if (denom.gt.tol) then; er = e/denom
         else; er = e/(denom+1.0); endif
       end subroutine

       subroutine initializeError(e)
         implicit none
         type(myError),intent(inout) :: e
         e%L1 = 0.0; e%L2 = 0.0; e%Linf = 0.0
         e%L1rel = 0.0; e%L2rel = 0.0; e%Linfrel = 0.0
       end subroutine

       subroutine copyMyError(e1,e2)
         implicit none
         type(myError),intent(in) :: e1
         type(myError),intent(inout) :: e2
         e2%L1 = e1%L1; e2%L2 = e1%L2; e2%Linf = e1%Linf
         e2%L1rel = e1%L1rel; e2%L2rel = e1%L2rel; e2%Linfrel = e1%Linfrel
       end subroutine

       subroutine computeError1(e,exact,approx)
         implicit none
         type(myError),intent(inout) :: e
         real(dpn),intent(in),dimension(:) :: exact,approx
         real(dpn) :: n,denom

         call initializeError(e)
         n = 1.0; call LnError(exact,approx,n,e%L1,e%L1rel,denom)
         n = 2.0; call LnError(exact,approx,n,e%L2,e%L2rel,denom)
         !n = infinity
         e%Linf = maxval(abs(exact-approx))
         if (denom.gt.tol) then; e%Linfrel = e%Linf/maxval(abs(exact))
         else; e%Linfrel = (e%Linf)/maxval(abs(exact)+1.0); endif
       end subroutine

       subroutine computeError1Uniform(e,exact,approx)
         implicit none
         type(myError),intent(inout) :: e
         real(dpn),intent(in),dimension(:) :: approx
         real(dpn),intent(in) :: exact
         real(dpn) :: n,denom

         call initializeError(e)
         n = 1.0; call LnError(exact,approx,n,e%L1,e%L1rel,denom)
         n = 2.0; call LnError(exact,approx,n,e%L2,e%L2rel,denom)
         !n = infinity
         e%Linf = maxval(abs(exact-approx))
         if (denom.gt.tol) then; e%Linfrel = e%Linf/(abs(exact))
         else; e%Linfrel = (e%Linf)/(abs(exact)+1.0); endif
       end subroutine

       subroutine computeError2(e,exact,approx)
         implicit none
         type(myError),intent(inout) :: e
         real(dpn),intent(in),dimension(:,:) :: exact,approx
         real(dpn) :: n,denom

         call initializeError(e)
         n = 1.0; call LnError(exact,approx,n,e%L1,e%L1rel,denom)
         n = 2.0; call LnError(exact,approx,n,e%L2,e%L2rel,denom)
         !n = infinity
         e%Linf = maxval(abs(exact-approx))
         if (denom.gt.tol) then; e%Linfrel = e%Linf/maxval(abs(exact))
         else; e%Linfrel = (e%Linf)/maxval(abs(exact)+1.0); endif
       end subroutine

       subroutine computeError2Uniform(e,exact,approx)
         implicit none
         type(myError),intent(inout) :: e
         real(dpn),intent(in),dimension(:,:) :: approx
         real(dpn),intent(in) :: exact
         real(dpn) :: n,denom

         call initializeError(e)
         n = 1.0; call LnError(exact,approx,n,e%L1,e%L1rel,denom)
         n = 2.0; call LnError(exact,approx,n,e%L2,e%L2rel,denom)
         !n = infinity
         e%Linf = maxval(abs(exact-approx))
         if (denom.gt.tol) then; e%Linfrel = e%Linf/(abs(exact))
         else; e%Linfrel = (e%Linf)/(abs(exact)+1.0); endif
       end subroutine

       subroutine computeError3(e,exact,approx)
         implicit none
         type(myError),intent(inout) :: e
         real(dpn),intent(in),dimension(:,:,:) :: exact,approx
         real(dpn) :: n,denom

         call initializeError(e)
         n = 1.0; call LnError(exact,approx,n,e%L1,e%L1rel,denom)
         n = 2.0; call LnError(exact,approx,n,e%L2,e%L2rel,denom)
         !n = infinity
         e%Linf = maxval(abs(exact-approx))
         if (denom.gt.tol) then; e%Linfrel = e%Linf/maxval(abs(exact))
         else; e%Linfrel = (e%Linf)/maxval(abs(exact)+1.0); endif
       end subroutine

       subroutine computeError3Uniform(e,exact,approx)
         implicit none
         type(myError),intent(inout) :: e
         real(dpn),intent(in),dimension(:,:,:) :: approx
         real(dpn),intent(in) :: exact
         real(dpn) :: n,denom

         call initializeError(e)
         n = 1.0; call LnError(exact,approx,n,e%L1,e%L1rel,denom)
         n = 2.0; call LnError(exact,approx,n,e%L2,e%L2rel,denom)
         !n = infinity
         e%Linf = maxval(abs(exact-approx))
         if (denom.gt.tol) then; e%Linfrel = e%Linf/(abs(exact))
         else; e%Linfrel = (e%Linf)/(abs(exact)+1.0); endif
       end subroutine


       end module