       module richardsonExtrapolation_funcs_mod
       ! 
       ! The following analysis closely followed work by
       ! 
       !      Roache, P. J. Quantification of Uncertainty in Computational 
       !      Fluid Dynamics. Annu. Rev. Fluid Mech. 29, 123–160 (1997).
       ! and
       !      De Vahl Davis, G. Natural convection of air in a square cavity: a 
       !      benchmark solution. Int. J. Num. Methods Fluids 3, 249–264 (1983).
       ! 
       ! Index 1 indicates finest mesh.
       ! Index Nsims indicates coarsest mesh.
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
       use current_precision_mod
       use norms_mod

       implicit none

       private
       public :: richardsonExtrap_norms
       public :: computeGCI_norms
       public :: computeRE
      
       contains

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
         p%L1   = richardsonExtrap_Real(num%L1  ,denom%L1,  r)
         p%L2   = richardsonExtrap_Real(num%L2  ,denom%L2,  r)
         p%Linf = richardsonExtrap_Real(num%Linf,denom%Linf,r)
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
         ! Computes
         ! 
         !     GCI = (Fs * abs(num/denom) ) / (r**p - 1.0_cp)
         ! 
         implicit none
         type(norms),intent(in) :: num,denom,p
         real(cp),intent(in) :: r,Fs
         logical,intent(in) :: fine
         type(norms) :: GCI
         GCI%L1   = computeGCI_Real(Fs,num%L1  /denom%L1  ,p%L1  ,r,fine)
         GCI%L2   = computeGCI_Real(Fs,num%L2  /denom%L2  ,p%L2  ,r,fine)
         GCI%Linf = computeGCI_Real(Fs,num%Linf/denom%Linf,p%Linf,r,fine)
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
         !     GCI = (Fs * abs(eps) * r**p ) / (r**p - 1.0_cp)
         ! but the example on NASA's site,
         ! http://www.grc.nasa.gov/WWW/wind/valid/tutorial/spatconv.html
         ! shows that this is not the case, and there seem to be
         ! inconsistencies with GCI_23 vs GCI_coarse. So I've adopted the
         ! approach from this example, and doing so seems to yield very good
         ! and sensible results.
         implicit none
         real(cp),intent(in) :: Fs,eps,p,r
         real(cp) :: GCI
         GCI = (Fs * abs(eps)) / (r**p - 1.0_cp)
       end function

       function computeGCI_Fine(Fs,eps,p,r) result(GCI)
         implicit none
         real(cp),intent(in) :: Fs,eps,p,r
         real(cp) :: GCI
         GCI = (Fs * abs(eps)) / (r**p - 1.0_cp)
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
         AR%L1   = computeAsymtoticRange_Real(GCI_12%L1  ,GCI_23%L1  ,p%L1  ,r)
         AR%L2   = computeAsymtoticRange_Real(GCI_12%L2  ,GCI_23%L2  ,p%L2  ,r)
         AR%Linf = computeAsymtoticRange_Real(GCI_12%Linf,GCI_23%Linf,p%Linf,r)
       end function

       function computeAsymtoticRange_Real(GCI_12,GCI_23,p,r) result(AR)
         ! Computes: AR = alpha/r^p
         ! Where:    alpha = |eps_23|/|eps_12|
         implicit none
         real(cp),intent(in) :: GCI_12,GCI_23,p,r
         real(cp) :: AR
         AR = GCI_23/((r**p)*GCI_12)
       end function

       end module