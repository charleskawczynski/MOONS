       module convergenceRate_mod
       use simParams_mod
       use IO_tools_mod
       use IO_scalarFields_mod
       use IO_vectorFields_mod
       use IO_auxiliary_mod
       use ops_aux_mod

       use grid_mod
       use norms_mod
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

       type norms_vec
         type(norms) :: x,y,z,e
       end type
      
       contains

       subroutine convergenceRateTest(dir)
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
         character(len=*),intent(in) :: dir
         integer,parameter :: Nstart = 5
         integer,parameter :: Nsims = 4
         integer,parameter :: r = 2 ! Refinement factor
         integer :: i
         integer,dimension(Nsims) :: N = (/(r**i,i=Nstart,Nstart+Nsims-1)/)
         type(norms_vec),dimension(Nsims-2) :: p_U,p_B
         type(vectorField),dimension(Nsims) :: U,B
         type(grid),dimension(Nsims) :: g
         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'N = ',N
         if (Nsims.lt.3) stop 'Error: size(f) must > 3 for convergence rate test.'

         do i=1,Nsims
           call allocateVectorField(U(i),N(i),N(i),N(i))
           call allocateVectorField(B(i),N(i),N(i),N(i))
           call MOONS_Parametric(U(i),B(i),g(i),N(i),dir//'N_'//trim(adjustl(int2str2(N(i))))//'\')
         enddo
         p_U = estimateConvergenceRate(U,g,Nsims,r,dir)
         ! p_B = estimateConvergenceRate(B,g,Nsims,r,dir)
         do i=1,Nsims
           call delete(U(i))
           ! call delete(B(i))
         enddo
         write(*,*) '***************************************************'
         write(*,*) '**************** CONVERGENCE RATES ****************'
         write(*,*) '***************************************************'
         write(*,*) 'N = ',N
         do i=1,Nsims-2
           call print(p_U(i)%x,'p(Ux('//int2str2(i)//')')
           call print(p_U(i)%y,'p(Uy('//int2str2(i)//')')
           call print(p_U(i)%z,'p(Uz('//int2str2(i)//')')
           ! call print(p_B(i),'p(B('//int2str2(i)//')')
         enddo
         ! do i=1,Nsims-2
         !   call export(p_U(i),'p(U('//int2str2(i)//')')
         !   call export(p_B(i),'p(B('//int2str2(i)//')')
         ! enddo
         write(*,*) '***************************************************'
         write(*,*) '********** CONVERGENCE TEST COMPLETE **************'
         write(*,*) '***************************************************'
       end subroutine

       function estimateConvergenceRate(f,g,n,r,dir) result (p)
         implicit none
         type(vectorField),dimension(n),intent(in) :: f ! Cell corner data, (1->size(f)) = (coarse->fine)
         type(grid),dimension(n),intent(in) :: g
         integer,intent(in) :: n,r ! Number of simulations / refinement factor
         type(norms_vec),dimension(n-2) :: p ! order of accuracy
         character(len=*),intent(in) :: dir
         type(vectorField),dimension(n-2) :: num,denom
         integer :: i,t
         integer,dimension(3) :: Ni
         type(norms) :: e_num,e_denom

         do t=1,n-2
           call allocateVectorField(num(t),f(t))
           call allocateVectorField(denom(t),f(t))

           call computeNumDenom(num(t)%x,denom(t)%x,f(t)%x,f(t+1)%x,f(t+2)%x,r)
           call computeNumDenom(num(t)%y,denom(t)%y,f(t)%y,f(t+1)%y,f(t+2)%y,r)
           call computeNumDenom(num(t)%z,denom(t)%z,f(t)%z,f(t+1)%z,f(t+2)%z,r)

           call zeroGhostPoints(num(t))
           call zeroGhostPoints(denom(t))

           Ni = (/(num(t)%sx(i),i=1,3)/)-1
           call compute( e_num , num(t)%x(2:Ni(1),2:Ni(2),2:Ni(3)) )
           Ni = (/(denom(t)%sx(i),i=1,3)/)-1
           call compute( e_denom , denom(t)%x(2:Ni(1),2:Ni(2),2:Ni(3)) )
           call richardsonExtrapolation(p(t)%x,e_num,e_denom,r)

           Ni = (/(num(t)%sy(i),i=1,3)/)-1
           call compute( e_num , num(t)%y(2:Ni(1),2:Ni(2),2:Ni(3)) )
           Ni = (/(denom(t)%sy(i),i=1,3)/)-1
           call compute( e_denom , denom(t)%y(2:Ni(1),2:Ni(2),2:Ni(3)) )
           call richardsonExtrapolation(p(t)%y,e_num,e_denom,r)

           Ni = (/(num(t)%sz(i),i=1,3)/)-1
           call compute( e_num , num(t)%z(2:Ni(1),2:Ni(2),2:Ni(3)) )
           Ni = (/(denom(t)%sz(i),i=1,3)/)-1
           call compute( e_denom , denom(t)%z(2:Ni(1),2:Ni(2),2:Ni(3)) )
           call richardsonExtrapolation(p(t)%z,e_num,e_denom,r)

           call delete(num(t))
           call delete(denom(t))
         enddo
       end function

       subroutine richardsonExtrapolation(p,num,denom,r)
         implicit none
         type(norms),intent(inout) :: p
         type(norms),intent(in) :: num,denom
         integer,intent(in) :: r
         p%L1   = abs( log( getL1(   num) / getL1(   denom)) )/log(real(r,cp))
         p%L2   = abs( log( getL2(   num) / getL2(   denom)) )/log(real(r,cp))
         p%Linf = abs( log( getLinf( num) / getLinf( denom)) )/log(real(r,cp))
         p%R1   = abs( log( getR1(   num) / getR1(   denom)) )/log(real(r,cp))
         p%R2   = abs( log( getR2(   num) / getR2(   denom)) )/log(real(r,cp))
         p%Rinf = abs( log( getRinf( num) / getRinf( denom)) )/log(real(r,cp))
       end subroutine

       subroutine computeNumDenom(num,denom,f1,f2,f3,r)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: num,denom
         real(cp),dimension(:,:,:),intent(in) :: f1,f2,f3
         integer,intent(in) :: r
         integer :: i,j,k,i2,j2,k2,i3,j3,k3
         integer,dimension(3) :: s
         s = shape(num)
         !$OMP PARALLEL DO PRIVATE(i2,j2,k2,i3,j3,k3)
         do k=2,s(3)-1
           k2 = 2 + (k-2)*r; k3 = 2 + (k-2)*r*r
           do j=2,s(2)-1
            j2 = 2 + (j-2)*r; j3 = 2 + (j-2)*r*r
             do i=2,s(1)-1
             i2 = 2 + (i-2)*r; i3 = 2 + (i-2)*r*r
           num(i,j,k) = f3(i3,j3,k3) - f2(i2,j2,k2)
         enddo;enddo;enddo
         !$OMP END PARALLEL DO
         s = shape(denom)
         !$OMP PARALLEL DO PRIVATE(i2,j2,k2,i3,j3,k3)
         do k=2,s(3)-1
           k2 = 2 + (k-2)*r; k3 = 2 + (k-2)*r*r
           do j=2,s(2)-1
             j2 = 2 + (j-2)*r; j3 = 2 + (j-2)*r*r
             do i=2,s(1)-1
               i2 = 2 + (i-2)*r; i3 = 2 + (i-2)*r*r
           denom(i,j,k) = f2(i2,j2,k2) - f1(i,j,k)
         enddo;enddo;enddo
         !$OMP END PARALLEL DO
       end subroutine

       end module
