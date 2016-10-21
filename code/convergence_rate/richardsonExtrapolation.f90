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
       use richardsonExtrapolation_funcs_mod
       use IO_tools_mod
       use IO_SF_mod
       use IO_VF_mod
       use ops_aux_mod

       use mesh_mod
       use norms_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: richardsonExtrapolation
       public :: reportResults
       public :: computeRE
      
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
         integer,intent(in) :: Nsims
         type(richardsonExtrapolation),dimension(Nsims-2),intent(in) :: RE
         character(len=*),intent(in) :: name,directory
         integer,dimension(Nsims),intent(in) :: N
         integer :: i
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

         temp = new_and_open(dir,name)

         if (sn.eq.s) then
           write(temp,'(7(A10))') 'N','L1','L2','Linf'
           do i=1,s
             write(temp,'(1I5,4'//arrfmt//')') N(i),e(i)%L1, e(i)%L2, e(i)%Linf
           enddo
         elseif (sn.eq.s+2) then
           write(temp,'(6(A10))') 'N1','N2','N3','L1','L2','Linf'
           do i=1,s
             write(temp,'(3I5,3'//arrfmt//')') N(i),N(i+1),N(i+2),&
                                               e(i)%L1, e(i)%L2, e(i)%Linf
           enddo
         else
           stop 'Error: unknown size in exportREList in richardsonExtrapolation.f90'
         endif
       end subroutine

       ! *******************************************************************************
       ! *******************************************************************************
       ! ********************************** SINGLE SET *********************************
       ! *******************************************************************************
       ! *******************************************************************************

       function computeRE_VF(f,m,n,r,dir,directory,name) result (RE)
         implicit none
         integer,intent(in) :: n,dir
         type(VF),dimension(n),intent(in) :: f
         type(mesh),dimension(n),intent(in) :: m
         integer,dimension(3),intent(in) :: r
         character(len=*),intent(in) :: directory,name
         type(richardsonExtrapolation),dimension(n-2) :: RE ! parametric results
         integer :: i
         do i=1,n-2
           select case (dir)
           case (1); RE(i) = computeRE_Real(f(i)%x,f(i+1)%x,f(i+2)%x,m(i),m(i+2),r,directory,name//int2str2(i))
           case (2); RE(i) = computeRE_Real(f(i)%y,f(i+1)%y,f(i+2)%y,m(i),m(i+2),r,directory,name//int2str2(i))
           case (3); RE(i) = computeRE_Real(f(i)%z,f(i+1)%z,f(i+2)%z,m(i),m(i+2),r,directory,name//int2str2(i))
           case default
           stop 'Error: dir must = 1,2,3 in computeRE_VF in convergenceRate.f90'
           end select
         enddo
       end function

       function computeRE_Real(f1,f2,f3,m1,m3,r,dir,name) result (RE)
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
         !      f1 is the finest mesh
         !      f3 is the coarsest mesh
         !      mesh refinement means that maxval(r) must > 1 for at least one direction
         ! 
         implicit none
         type(SF),intent(in) :: f1,f2,f3 ! Cell corner data, (f1 -> finest, f3-> coarsest)
         type(mesh),intent(in) :: m1,m3 ! Mesh for f3
         integer,dimension(3),intent(in) :: r ! refinement factor
         character(len=*),intent(in) :: dir,name ! directory / name
         type(richardsonExtrapolation) :: RE ! parametric results
         integer,dimension(3) :: r1,r2,r3 ! no refinement compared to coarsest mesh
         type(norms) :: f3_f2,f2_f1,f1_f0,f2_f0
         type(SF) :: f0
         real(cp) :: Fs,r0
         r1 = r*r; r2 = r; r3 = 1; r0 = real(maxval(r),cp)
         call init_Node(f0,m1)
         call assign(f0,0.0_cp)

         if (.not.(r0.gt.1.0_cp)) stop 'Refinement was not performed in computeRE_Real in convergenceRate.f90'

         f3_f2 = computeMGError(f2,f3,r2,r3,m3,dir,name//'_23',.false.)    ! |f_3 - f_2|
         f2_f1 = computeMGError(f1,f2,r1,r2,m3,dir,name//'_12',.false.)    ! |f_2 - f_1|
         f1_f0 = computeMGError(f0,f1,r1,r1,m3,dir,name//'_1',.false.)     ! |   f_1   |
         f2_f0 = computeMGError(f0,f2,r1,r2,m3,dir,name//'_2',.false.)     ! |   f_2   |

         RE%p = richardsonExtrap_norms(f3_f2,f2_f1,r0)

         RE%e_12 = f2_f1
         RE%e_23 = f3_f2
         Fs = 1.25_cp

         ! Fine GCI
         RE%GCI_12 = computeGCI_norms(f2_f1,f1_f0,RE%p,r0,Fs,.true.) ! GCI = Fs*|f_2 - f_1|/|f_1|
         ! Coarse GCI
         RE%GCI_23 = computeGCI_norms(f3_f2,f2_f0,RE%p,r0,Fs,.false.) ! GCI = Fs*|f_3 - f_2|/|f_2|
         
         RE%AR = computeAsymtoticRange_Norms(RE%GCI_12,RE%GCI_23,RE%p,r0)
         call delete(f0)
       end function

       ! *******************************************************************************
       ! *******************************************************************************
       ! ******************************** MULTIGRID ERROR ******************************
       ! *******************************************************************************
       ! *******************************************************************************

       function computeMGError(f1,f2,r1,r2,m,dir,name,plotTF) result(n)
         ! Computes
         ! 
         !    e = f2 - f1
         ! 
         ! On the mesh defined by the shape s. The mesh taken from 
         ! f1 and f2 depend on the refinement factors r1 and r2.
         ! 
         ! Note that
         ! 
         !     r must >= 1
         !     shape(f1)/r1 should = s
         !     shape(f2)/r2 should = s
         ! 
         !     r1(dir) = 1 --> f1 need not skip mesh points along direction dir in mesh
         !     r1(dir) = 2 --> f1 needs to skip every other mesh point along direction dir in mesh
         ! 
         !     r2(dir) = 1 --> f2 need not skip mesh points along direction dir in mesh
         !     r2(dir) = 2 --> f2 needs to skip every other mesh point along direction dir in mesh
         ! 
         implicit none
         type(SF),intent(in) :: f2,f1
         integer,dimension(3),intent(in) :: r1,r2
         type(mesh),intent(in) :: m ! mesh for e
         character(len=*),intent(in) :: dir,name
         logical,intent(in) :: plotTF
         type(SF) :: e,vol
         type(norms) :: n
         integer :: i,j,k,i1,j1,k1,i2,j2,k2,t

         call init_Node(e,m)
         do t = 1,e%s
#ifdef _PARALLELIZE_RICHARDSONEXTRAPOLATION_
           !$OMP PARALLEL DO PRIVATE(i1,j1,k1,i2,j2,k2)

#endif

           do k=2,e%BF(t)%GF%s(3)-1
             k1 = 2 + (k-2)*r1(3); k2 = 2 + (k-2)*r2(3)
             do j=2,e%BF(t)%GF%s(2)-1
              j1 = 2 + (j-2)*r1(2); j2 = 2 + (j-2)*r2(2)
               do i=2,e%BF(t)%GF%s(1)-1
               i1 = 2 + (i-2)*r1(1); i2 = 2 + (i-2)*r2(1)
             e%BF(t)%GF%f(i,j,k) = f2%BF(t)%GF%f(i2,j2,k2) - f1%BF(t)%GF%f(i1,j1,k1)
           enddo;enddo;enddo
#ifdef _PARALLELIZE_RICHARDSONEXTRAPOLATION_
           !$OMP END PARALLEL DO

#endif
         enddo
         call zeroGhostPoints(e)
         if (plotTF) call export_3D_1C(m,e,dir,'MG_Error_'//name,0)

         call init(vol,e)
         call volume(vol,m)
         call compute(n,e,vol)
         call delete(vol)
         call delete(e)
       end function

       end module
