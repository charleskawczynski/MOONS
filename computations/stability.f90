       module stability_mod
       use scalarField_mod
       use vectorField_mod

       implicit none

       private

       public :: perturb

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: PI = 3.14159265358979

       contains

       subroutine perturbAll(f,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(3) :: wavenum,eps
         integer,dimension(3) :: s
         integer :: i,j,k
         s = shape(f)
         wavenum = real(0.1,cp)
         eps = real(0.01,cp)
         if (all((/(s(i).eq.g%c(i)%sn,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(real(1.0,cp) + eps(1)*sin(wavenum(1)*PI*g%c(1)%hn(i)) +&
                                                 eps(2)*sin(wavenum(2)*PI*g%c(2)%hn(j)) +&
                                                 eps(3)*sin(wavenum(3)*PI*g%c(3)%hn(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (all((/(s(i).eq.g%c(i)%sc,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(real(1.0,cp) + eps(1)*sin(wavenum(1)*PI*g%c(1)%hc(i)) +&
                                                 eps(2)*sin(wavenum(2)*PI*g%c(2)%hc(j)) +&
                                                 eps(3)*sin(wavenum(3)*PI*g%c(3)%hc(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         else
          stop 'Error: unmatched case in perturb in inductionSolver.f90'
         endif
       end subroutine

       subroutine perturb(f,g)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(3) :: wavenum,eps
         integer,dimension(3) :: s
         integer :: i,j,k
         s = shape(f)
         wavenum = real(10.0,cp)
         eps = real(0.1,cp)
         if (all((/(s(i).eq.g%c(i)%sn,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(real(1.0,cp) + eps(2)*sin(wavenum(2)*PI*g%c(2)%hn(j)) +&
                                                 eps(3)*sin(wavenum(3)*PI*g%c(3)%hn(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (all((/(s(i).eq.g%c(i)%sc,i=1,3)/))) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             f(i,j,k) = f(i,j,k)*(real(1.0,cp) + eps(2)*sin(wavenum(2)*PI*g%c(2)%hc(j)) +&
                                                 eps(3)*sin(wavenum(3)*PI*g%c(3)%hc(k)) )
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         else
          stop 'Error: unmatched case in perturb in inductionSolver.f90'
         endif
       end subroutine

       subroutine perturbAll(f,g,wavenum,eps)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(3),intent(in) :: wavenum,eps
         call perturbDirection(f,g,eps,wavenum(1),eps(1),1)
         call perturbDirection(f,g,eps,wavenum(2),eps(2),2)
         call perturbDirection(f,g,eps,wavenum(3),eps(3),3)
       end subroutine

       subroutine perturbAllExcept(f,g,wavenum,eps,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(3),intent(in) :: wavenum,eps
         select case (dir)
         case (1); call perturbDirection(f,g,eps,wavenum(2),eps(2),2)
                   call perturbDirection(f,g,eps,wavenum(3),eps(3),3)

         case (2); call perturbDirection(f,g,eps,wavenum(1),eps(1),1)
                   call perturbDirection(f,g,eps,wavenum(3),eps(3),3)

         case (3); call perturbDirection(f,g,eps,wavenum(1),eps(1),1)
                   call perturbDirection(f,g,eps,wavenum(2),eps(2),2)
         case default
         stop 'Error: dir must = 1,2,3 in perturbAllExcept in stability.f90'
         end select
       end subroutine

       subroutine perturbDirection(f,g,eps,wavenum,eps,dir)
         ! Modifies f such that
         ! 
         ! f = f * ( 1 + eps * sin( wavenum * PI * h ) )
         ! 
         ! Where h is along direction dir = (1,2,3) = (x,y,z)
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),intent(in) :: wavenum,eps,dir
         integer,dimension(3) :: s
         integer :: i,j,k,x,y,z,t
         s = shape(f)

         select case (dir)
         case (1); x=1;y=0;z=0
         case (2); x=0;y=1;z=0
         case (3); x=0;y=0;z=1
         case default
         stop 'Error: dir must = 1,2,3 in perturbDirection in stability.f90'
         end select

         if (s(dir).eq.g%c(dir)%sn) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             t = i*x + j*y + k*z
             f(i,j,k) = f(i,j,k)*(real(1.0,cp) + eps*sin(wavenum*PI*g%c(dir)%hn(t)))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         elseif (s(dir).eq.g%c(dir)%sc) then
           !$OMP PARALLEL DO
           do k=1,s(3); do j=1,s(2); do i=1,s(1)
             t = i*x + j*y + k*z
             f(i,j,k) = f(i,j,k)*(real(1.0,cp) + eps*sin(wavenum*PI*g%c(dir)%hc(t)))
           enddo; enddo; enddo
           !$OMP END PARALLEL DO
         else
          stop 'Error: unmatched case in perturbDirection in stability.f90'
         endif
       end subroutine


       end module