       module richardsonExtrapolation_mod
       ! 
       ! The following analysis closely followed work by
       ! 
       !      Roache, P. J. Quantification of Uncertainty in Computational 
       !      Fluid Dynamics. Annu. Rev. Fluid Mech. 29, 123–160 (1997).
       ! and
       !      De Vahl Davis, G. Natural convection of air in a square cavity: a 
       !      benchmark solution. Int. J. Num. Methods Fluids 3, 249–264 (1983).
       ! 
       ! Index 1 indicates finest grid.
       ! Index Nsims indicates coarsest grid.
       ! 
       ! Very good tutorial for convergence rates:
       ! http://www.grc.nasa.gov/WWW/wind/valid/tutorial/spatconv.html
       ! 
       ! 
       ! NOTE:
       !        It appears that computeGCI_Coarse is supposed to be computed using
       !            GCI = (Fs * abs(eps) * r**p ) / (r**p - real(1.0,cp))
       !        but the example on NASA's site,
       !        http://www.grc.nasa.gov/WWW/wind/valid/tutorial/spatconv.html
       !        shows that this is not the case, and there seem to be
       !        inconsistencies with GCI_23 vs GCI_coarse. So I've adopted the
       !        approach from this example, and doing so seems to yield very good
       !        and sensible results.

       use IO_tools_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use ops_aux_mod

       use grid_mod
       use norms_mod
       use scalarField_mod
       use vectorField_mod

       implicit none

       private
       public :: richardsonExtrapolation
       public :: reportResults
       public :: computeRE
       public :: loadData

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif
      
       interface computeRE;    module procedure computeRE_VF;                 end interface
       ! interface computeRE;    module procedure computeRE_RealfromExisting;   end interface

       type richardsonExtrapolation
         type(norms) :: e_23,GCI_23
         type(norms) :: e_12,GCI_12
         type(norms) :: p
         type(norms) :: AR ! Should be ~ 1 if in asymptotic range
       end type
      
       contains

       subroutine reportResults(RE,name,directory,Nsims,N)
         implicit none
         type(richardsonExtrapolation),dimension(Nsims-2),intent(in) :: RE
         character(len=*),intent(in) :: name,directory
         integer,intent(in) :: Nsims
         integer,dimension(Nsims),intent(in) :: N
         integer :: i,dir
         type(norms),dimension(2*(Nsims-2)) :: etemp,GCItemp
         type(norms),dimension(Nsims-2) :: ptemp,ARtemp

         do i=1,Nsims-2
            call print(RE(i)%e_12,'e_12('//name//'('//int2str2(i)//'))')
            call print(RE(i)%e_23,'e_23('//name//'('//int2str2(i)//'))')

            call print(RE(i)%p,'p('//name//'('//int2str2(i)//'))')
            call print(RE(i)%AR,'AR('//name//'('//int2str2(i)//'))')

            call print(RE(i)%GCI_12,'GCI_12('//name//'('//int2str2(i)//'))')
            call print(RE(i)%GCI_23,'GCI_23('//name//'('//int2str2(i)//'))')
         enddo
         do i=1,Nsims-2
            call init(etemp(2*i-1),RE(i)%e_12)
            call init(etemp(2*i),RE(i)%e_23)

            call init(ptemp(i),RE(i)%p)
            call init(ARtemp(i),RE(i)%AR)

            call init(GCItemp(2*i-1),RE(i)%GCI_12)
            call init(GCItemp(2*i),RE(i)%GCI_23)
         enddo
         call exportREList(etemp,N,directory,'e('//name//'('//int2str2(1)//'_to_'//int2str2(2*(Nsims-2))//'))')
         call exportREList(GCItemp,N,directory,'GCI('//name//'('//int2str2(1)//'_to_'//int2str2(2*(Nsims-2))//'))')
         call exportREList(ptemp,N,directory,'p('//name//'('//int2str2(1)//'_to_'//int2str2(Nsims-2)//'))')
         call exportREList(ARtemp,N,directory,'AR('//name//'('//int2str2(1)//'_to_'//int2str2(Nsims-2)//'))')
       end subroutine

       subroutine exportREList(e,N,dir,name)
         implicit none
         type(norms),dimension(:),intent(in) :: e
         integer,dimension(:),intent(in) :: N
         character(len=*),intent(in) :: dir,name
         integer :: temp,s,sn,i
         s = size(e); sn = size(N)

         temp = newAndOpen(dir,name)

         if (sn.eq.s) then
           write(temp,'(7(A10))') 'N','L1','L2','Linf','R1','R2','Rinf'
           do i=1,s
             write(temp,'(1I5,6'//arrfmt//')') N(i),e(i)%L1, e(i)%L2, e(i)%Linf, &
                                                    e(i)%R1, e(i)%R2, e(i)%Rinf
           enddo
         elseif (sn.eq.s+2) then
           write(temp,'(9(A10))') 'N1','N2','N3','L1','L2','Linf','R1','R2','Rinf'
           do i=1,s
             write(temp,'(3I5,6'//arrfmt//')') N(i),N(i+1),N(i+2),&
                                               e(i)%L1, e(i)%L2, e(i)%Linf,&
                                               e(i)%R1, e(i)%R2, e(i)%Rinf
           enddo
         else
           stop 'Error: unknown size in exportREList in richardsonExtrapolation.f90'
         endif
       end subroutine

       subroutine loadData(f,g,directory,nx,ny,nz)
         implicit none
         type(vectorField),intent(inout) :: f
         type(grid),intent(in) :: g
         character(len=*),intent(in) :: directory,nx,ny,nz
         call readFromFile(g,f,directory,nx,ny,nz)
       end subroutine

       ! *******************************************************************************
       ! *******************************************************************************
       ! ********************************** SINGLE SET *********************************
       ! *******************************************************************************
       ! *******************************************************************************

       function computeRE_VF(f,g,n,r,dir,directory,name) result (RE)
         implicit none
         type(vectorField),dimension(n),intent(in) :: f
         type(grid),dimension(n),intent(in) :: g
         integer,dimension(3),intent(in) :: r
         integer,intent(in) :: n,dir
         character(len=*),intent(in) :: directory,name
         type(richardsonExtrapolation),dimension(n-2) :: RE ! parametric results
         integer :: i
         do i=1,n-2
           select case (dir)
           case (1); RE(i) = computeRE_Real(f(i)%x,f(i+1)%x,f(i+2)%x,g(i+2),r,directory,name//int2str2(i))
           case (2); RE(i) = computeRE_Real(f(i)%y,f(i+1)%y,f(i+2)%y,g(i+2),r,directory,name//int2str2(i))
           case (3); RE(i) = computeRE_Real(f(i)%z,f(i+1)%z,f(i+2)%z,g(i+2),r,directory,name//int2str2(i))
           case default
           stop 'Error: dir must = 1,2,3 in estimateConvergenceRate in convergenceRate.f90'
           end select
         enddo
       end function

       function computeRE_Real(f1,f2,f3,g3,r,dir,name) result (RE)
         ! Computes the properties of RE, which include
         ! 
         !            |f3 - f2|   /
         !    p = log ---------  / log (maxval(r))
         !            |f2 - f1| /
         ! 
         !    e_12 = |f2 - f1|
         !    e_23 = |f3 - f2|
         ! 
         !             |f2 - f1|
         !    GCI_12 = ---------
         !               |f1|
         ! 
         !             |f3 - f2|
         !    GCI_23 = ---------
         !               |f2|
         ! 
         ! Note that 
         !      f1 is the finest grid
         !      f3 is the coarsest grid
         !      grid refinement means that maxval(r) must > 1 for at least one direction
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: f1,f2,f3 ! Cell corner data, (f1 -> finest, f3-> coarsest)
         type(grid),intent(in) :: g3 ! Grid for f3
         integer,dimension(3),intent(in) :: r ! refinement factor
         character(len=*),intent(in) :: dir,name ! directory / name
         type(richardsonExtrapolation) :: RE ! parametric results
         integer,dimension(3) :: r1,r2,r3 ! no refinement compared to coarsest grid
         integer,dimension(3) :: s
         type(norms) :: f3_f2,f2_f1,f1_f0,f2_f0
         type(scalarField) :: f0
         real(cp) :: Fs,r0
         r1 = r*r; r2 = r; r3 = 1; r0 = real(maxval(r),cp)
         call allocateField(f0,shape(f1))
         call assign(f0,real(0.0,cp))

         if (.not.(r0.gt.real(1.0,cp))) stop 'Refinement was not performed in computeRE in convergenceRate.f90'

         s = shape(f3)
         f3_f2 = computeMGError(f2,f3,r2,r3,s,g3,dir,name//'_23')
         f2_f1 = computeMGError(f1,f2,r1,r2,s,g3,dir,name//'_12')
         f1_f0 = computeMGError(f0%phi,f1,r1,r1,s,g3,dir,name//'_1')
         f2_f0 = computeMGError(f0%phi,f2,r1,r2,s,g3,dir,name//'_2')

         RE%p = richardsonExtrap_norms(f3_f2,f2_f1,r0)

         RE%e_12 = f2_f1
         RE%e_23 = f3_f2
         Fs = real(1.25,cp)

         ! Fine GCI
         RE%GCI_12 = computeGCI_norms(f2_f1,f1_f0,RE%p,r0,Fs,.true.)
         ! Coarse GCI
         RE%GCI_23 = computeGCI_norms(f3_f2,f2_f0,RE%p,r0,Fs,.false.)
         
         RE%AR = computeAsymtoticRange_Norms(RE%GCI_12,RE%GCI_23,RE%p,r0)
         call delete(f0)
       end function

       ! *******************************************************************************
       ! *******************************************************************************
       ! ******************************** MULTIGRID ERROR ******************************
       ! *******************************************************************************
       ! *******************************************************************************

       function computeMGError(f1,f2,r1,r2,s,g,dir,name) result(n)
         ! Computes
         ! 
         !    e = f2 - f1
         ! 
         ! On the grid defined by the shape s. The grid taken from 
         ! f1 and f2 depend on the refinement factors r1 and r2.
         ! 
         ! Note that
         ! 
         !     r must >= 1
         !     shape(f1)/r1 should = s
         !     shape(f2)/r2 should = s
         ! 
         !     r1(dir) = 1 --> f1 need not skip grid points along direction dir in grid
         !     r1(dir) = 2 --> f1 needs to skip every other grid point along direction dir in grid
         ! 
         !     r2(dir) = 1 --> f2 need not skip grid points along direction dir in grid
         !     r2(dir) = 2 --> f2 needs to skip every other grid point along direction dir in grid
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: f2,f1
         integer,dimension(3),intent(in) :: r1,r2
         integer,dimension(3),intent(in) :: s
         type(grid),intent(in) :: g ! grid for e
         character(len=*),intent(in) :: dir,name
         type(scalarField) :: e
         type(norms) :: n
         integer :: i,j,k,i1,j1,k1,i2,j2,k2

         call allocateField(e,s)
         !$OMP PARALLEL DO PRIVATE(i1,j1,k1,i2,j2,k2)
         do k=2,s(3)-1
           k1 = 2 + (k-2)*r1(3); k2 = 2 + (k-2)*r2(3)
           do j=2,s(2)-1
            j1 = 2 + (j-2)*r1(2); j2 = 2 + (j-2)*r2(2)
             do i=2,s(1)-1
             i1 = 2 + (i-2)*r1(1); i2 = 2 + (i-2)*r2(1)
           e%phi(i,j,k) = f2(i2,j2,k2) - f1(i1,j1,k1)
         enddo;enddo;enddo
         !$OMP END PARALLEL DO
         call zeroGhostPoints(e%phi)
         ! call writeScalarPhysical(g,e%phi,dir,'MG_Error_'//name)
         call compute( n , e%phi(2:s(1)-1,2:s(2)-1,2:s(3)-1) )
         call delete(e)
       end function

       ! *******************************************************************************
       ! *******************************************************************************
       ! *************************** ORDER OF CONVERGENCE (p) **************************
       ! *******************************************************************************
       ! *******************************************************************************

       function richardsonExtrap_norms(num,denom,r) result(p)
         ! Computes
         ! 
         !            |f3 - f2|   /
         !    p = log ---------  / log (r)
         !            |f2 - f1| /
         ! 
         ! Note that r must > 1
         implicit none
         type(norms),intent(in) :: num,denom
         real(cp),intent(in) :: r
         type(norms) :: p
         p%L1   = richardsonExtrap_Real(getL1(num)  ,getL1(denom),  r)
         p%L2   = richardsonExtrap_Real(getL2(num)  ,getL2(denom),  r)
         p%Linf = richardsonExtrap_Real(getLinf(num),getLinf(denom),r)
         p%R1   = richardsonExtrap_Real(getR1(num)  ,getR1(denom),  r)
         p%R2   = richardsonExtrap_Real(getR2(num)  ,getR2(denom),  r)
         p%Rinf = richardsonExtrap_Real(getRinf(num),getRinf(denom),r)
       end function

       function richardsonExtrap_Real(num,denom,r) result(p)
         ! Computes
         ! 
         !            |f3 - f2|   /
         !    p = log ---------  / log (r)
         !            |f2 - f1| /
         ! 
         ! Note that r must > 1
         implicit none
         real(cp),intent(in) :: num,denom,r
         real(cp) :: p
         p = log( abs(num) / abs(denom) )/log(r)
       end function

       ! *******************************************************************************
       ! *******************************************************************************
       ! ************************* GRID CONVERGECNE INDEX (GCI) ************************
       ! *******************************************************************************
       ! *******************************************************************************

       function computeGCI_norms(num,denom,p,r,Fs,fine) result(GCI)
         implicit none
         type(norms),intent(in) :: num,denom,p
         real(cp),intent(in) :: r,Fs
         logical,intent(in) :: fine
         type(norms) :: GCI
         GCI%L1   = computeGCI_Real(Fs,getL1(   num)/getL1(   denom),getL1(p)  ,r,fine)
         GCI%L2   = computeGCI_Real(Fs,getL2(   num)/getL2(   denom),getL2(p)  ,r,fine)
         GCI%Linf = computeGCI_Real(Fs,getLinf( num)/getLinf( denom),getLinf(p),r,fine)
         GCI%R1   = computeGCI_Real(Fs,getR1(   num)/getR1(   denom),getR1(p)  ,r,fine)
         GCI%R2   = computeGCI_Real(Fs,getR2(   num)/getR2(   denom),getR2(p)  ,r,fine)
         GCI%Rinf = computeGCI_Real(Fs,getRinf( num)/getRinf( denom),getRinf(p),r,fine)
       end function

       function computeGCI_Real(Fs,eps,p,r,fine) result(GCI)
         implicit none
         real(cp),intent(in) :: Fs,eps,p,r
         logical,intent(in) :: fine
         real(cp) :: GCI
         if (fine) then; GCI = computeGCI_Fine(Fs,eps,r,p)
         else;           GCI = computeGCI_Coarse(Fs,eps,r,p)
         endif
       end function

       function computeGCI_Coarse(Fs,eps,p,r) result(GCI)
         ! It appears that this is supposed to be computed using
         !     GCI = (Fs * abs(eps) * r**p ) / (r**p - real(1.0,cp))
         ! but the example on NASA's site,
         ! http://www.grc.nasa.gov/WWW/wind/valid/tutorial/spatconv.html
         ! shows that this is not the case, and there seem to be
         ! inconsistencies with GCI_23 vs GCI_coarse. So I've adopted the
         ! approach from this example, and doing so seems to yield very good
         ! and sensible results.
         implicit none
         real(cp),intent(in) :: Fs,eps,p,r
         real(cp) :: GCI
         GCI = (Fs * abs(eps)) / (r**p - real(1.0,cp))
       end function

       function computeGCI_Fine(Fs,eps,p,r) result(GCI)
         implicit none
         real(cp),intent(in) :: Fs,eps,p,r
         real(cp) :: GCI
         GCI = (Fs * abs(eps)) / (r**p - real(1.0,cp))
       end function

       ! *******************************************************************************
       ! *******************************************************************************
       ! **************************** ASYMPTOTIC RANGE (AR) ****************************
       ! *******************************************************************************
       ! *******************************************************************************

       function computeAsymtoticRange_Norms(GCI_12,GCI_23,p,r) result(AR)
         implicit none
         type(norms),intent(in) :: GCI_12,GCI_23,p
         real(cp),intent(in) :: r
         type(norms) :: AR
         AR%L1   = computeAsymtoticRange_Real(getL1(GCI_12)  ,getL1(GCI_23)  ,getL1(p)  ,r)
         AR%L2   = computeAsymtoticRange_Real(getL2(GCI_12)  ,getL2(GCI_23)  ,getL2(p)  ,r)
         AR%Linf = computeAsymtoticRange_Real(getLinf(GCI_12),getLinf(GCI_23),getLinf(p),r)
         AR%R1   = computeAsymtoticRange_Real(getR1(GCI_12)  ,getR1(GCI_23)  ,getR1(p)  ,r)
         AR%R2   = computeAsymtoticRange_Real(getR2(GCI_12)  ,getR2(GCI_23)  ,getR2(p)  ,r)
         AR%Rinf = computeAsymtoticRange_Real(getRinf(GCI_12),getRinf(GCI_23),getRinf(p),r)
       end function

       function computeAsymtoticRange_Real(GCI_12,GCI_23,p,r) result(AR)
         implicit none
         real(cp),intent(in) :: GCI_12,GCI_23,p,r
         real(cp) :: AR
         AR = GCI_23/((r**p)*GCI_12)
       end function

       end module
