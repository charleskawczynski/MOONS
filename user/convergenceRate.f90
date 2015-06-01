       module convergenceRate_mod
       use simParams_mod
       use IO_tools_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use IO_auxiliary_mod
       use ops_aux_mod

       use grid_mod
       use norms_mod
       use scalarField_mod
       use vectorField_mod

       use MOONS_mod

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

       type parametricResults
         type(norms) :: e,p
       end type
      
       contains

       subroutine convergenceRateTest(directory)
         ! Convergence rate tests begin at the finest grid
         ! and progress towards coarser grids. This way,
         ! the longest simulation time is known shortly after
         ! the simulation starts.
         ! 
         ! 
         ! 
         ! For convenience
         !                            2^2  = 4
         !                            2^3  = 8
         !                            2^4  = 16
         !                            2^5  = 32
         !                            2^6  = 64
         !                            2^7  = 128
         !                            2^8  = 256
         !                            2^9  = 512
         !                            2^10 = 1024
         implicit none
         character(len=*),intent(in) :: directory
         integer,parameter :: Nstart = 5
         integer,parameter :: Nsims = 3
         integer,parameter :: r = 2 ! Refinement factor
         integer :: i,dir
         integer,dimension(Nsims) :: N = (/(r**i,i=Nstart,Nstart+Nsims-1)/)
         type(parametricResults),dimension(Nsims-1) :: PR
         type(vectorField),dimension(Nsims) :: U,B
         type(grid),dimension(Nsims) :: g
         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'N = ',N
         if (Nsims.lt.3) stop 'Error: size(f) must > 3 for convergence rate test.'

         do i=Nsims,1,-1 ! Start with finest grid
           call allocateVectorField(U(i),N(i),N(i),N(i))
           call allocateVectorField(B(i),N(i),N(i),N(i))
           call MOONS_Parametric(U(i),B(i),g(i),N(i),directory//'N_'//trim(adjustl(int2str2(N(i))))//'\')
         enddo

         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'N = ',N

         PR = estimateConvergenceRate(U,g,Nsims,r,1)
         call reportResults(PR,'U',directory,Nsims)
         PR = estimateConvergenceRate(U,g,Nsims,r,2)
         call reportResults(PR,'V',directory,Nsims)
         PR = estimateConvergenceRate(U,g,Nsims,r,3)
         call reportResults(PR,'W',directory,Nsims)
         do i=1,Nsims; call delete(U(i)); enddo
          
         if (solveInduction) then
           PR = estimateConvergenceRate(B,g,Nsims,r,1)
           call reportResults(PR,'Bx',directory,Nsims)
           PR = estimateConvergenceRate(B,g,Nsims,r,2)
           call reportResults(PR,'By',directory,Nsims)
           PR = estimateConvergenceRate(B,g,Nsims,r,3)
           call reportResults(PR,'Bz',directory,Nsims)
           do i=1,Nsims; call delete(B(i)); enddo
         endif
          
         write(*,*) '***************************************************'
         write(*,*) '********** CONVERGENCE TEST COMPLETE **************'
         write(*,*) '***************************************************'
       end subroutine

       subroutine reportResults(PR,name,directory,Nsims)
        implicit none
        integer,intent(in) :: Nsims
        type(parametricResults),dimension(Nsims-1),intent(in) :: PR
        character(len=*),intent(in) :: name,directory
        integer :: i,dir
        type(norms),dimension(Nsims-1) :: etemp
        type(norms),dimension(Nsims-2) :: ptemp

        do i=1,Nsims-1
           call print(PR(i)%e,'e('//name//'('//int2str2(i)//'))')
        enddo
        do i=1,Nsims-1; call init(etemp(i),PR(i)%e); enddo
        call exportList(etemp,directory,'e('//name//'('//int2str2(1)//'_to_'//int2str2(Nsims-1)//'))')

        do i=1,Nsims-2
           call print(PR(i)%p,'p('//name//'('//int2str2(i)//'))')
        enddo
        do i=1,Nsims-2; call init(ptemp(i),PR(i)%p); enddo
        call exportList(ptemp,directory,'p('//name//'('//int2str2(1)//'_to_'//int2str2(Nsims-2)//'))')
       end subroutine

       function estimateConvergenceRate(f,g,n,r,dir) result (PR)
         implicit none
         type(vectorField),dimension(n),intent(in) :: f ! Cell corner data, (1->size(f)) = (coarse->fine)
         type(grid),dimension(n),intent(in) :: g
         integer,intent(in) :: n,r,dir ! Number of simulations / refinement factor / direction
         type(parametricResults),dimension(n-1) :: PR ! parametric results
         integer :: i
         integer,dimension(3) :: s
         type(norms) :: e_num,e_denom

         select case (dir)
         case (1)
           do i=1,n-2
             s = shape(f(i)%x)
             e_num   = computeMultigridError(f(i+1)%x,f(i+2)%x,r,r*r,s)
             e_denom = computeMultigridError(f(i)%x,f(i+1)%x,1,r,s)
             PR(i)%p = richardsonExtrapolation(e_num,e_denom,r)
             PR(i)%e = e_denom
           enddo
         case (2)
           do i=1,n-2
             s = shape(f(i)%y)
             e_num   = computeMultigridError(f(i+1)%y,f(i+2)%y,r,r*r,s)
             e_denom = computeMultigridError(f(i)%y,f(i+1)%y,1,r,s)
             PR(i)%p = richardsonExtrapolation(e_num,e_denom,r)
             PR(i)%e = e_denom
           enddo
         case (3)
           do i=1,n-2
             s = shape(f(i)%z)
             e_num   = computeMultigridError(f(i+1)%z,f(i+2)%z,r,r*r,s)
             e_denom = computeMultigridError(f(i)%z,f(i+1)%z,1,r,s)
             PR(i)%p = richardsonExtrapolation(e_num,e_denom,r)
             PR(i)%e = e_denom
           enddo
         case default
         stop 'Error: dir must = 1,2,3 in estimateConvergenceRate in convergenceRate.f90'
         end select
         PR(n-1)%e   = e_num
         call init(PR(n-1)%p) ! undefined here
       end function

       function computeMultigridError(f1,f2,r1,r2,s) result(n)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: f2,f1
         integer,intent(in) :: r1,r2
         integer,dimension(3),intent(in) :: s
         type(scalarField) :: e
         type(norms) :: n
         integer :: i,j,k,i1,j1,k1,i2,j2,k2

         call allocateField(e,s(1),s(2),s(3))
         !$OMP PARALLEL DO PRIVATE(i1,j1,k1,i2,j2,k2)
         do k=2,s(3)-1
           k1 = 2 + (k-2)*r1; k2 = 2 + (k-2)*r2
           do j=2,s(2)-1
            j1 = 2 + (j-2)*r1; j2 = 2 + (j-2)*r2
             do i=2,s(1)-1
             i1 = 2 + (i-2)*r1; i2 = 2 + (i-2)*r2
           e%phi(i,j,k) = f2(i2,j2,k2) - f1(i1,j1,k1)
         enddo;enddo;enddo
         !$OMP END PARALLEL DO
         call zeroGhostPoints(e%phi)
         call compute( n , e%phi(2:s(1)-1,2:s(2)-1,2:s(3)-1) )
         call delete(e)
       end function

       function richardsonExtrapolation(num,denom,r) result(p)
         implicit none
         type(norms),intent(in) :: num,denom
         integer,intent(in) :: r
         type(norms) :: p
         p%L1   = abs( log( getL1(   num) / getL1(   denom)) )/log(real(r,cp))
         p%L2   = abs( log( getL2(   num) / getL2(   denom)) )/log(real(r,cp))
         p%Linf = abs( log( getLinf( num) / getLinf( denom)) )/log(real(r,cp))
         p%R1   = abs( log( getR1(   num) / getR1(   denom)) )/log(real(r,cp))
         p%R2   = abs( log( getR2(   num) / getR2(   denom)) )/log(real(r,cp))
         p%Rinf = abs( log( getRinf( num) / getRinf( denom)) )/log(real(r,cp))
       end function

       end module
