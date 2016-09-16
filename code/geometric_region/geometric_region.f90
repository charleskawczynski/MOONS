       module geometric_region_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod

       implicit none

       private
       public :: geometric_region
       public :: init
       public :: assign_inside

       interface init;    module procedure init_geometric_region;   end interface

       type geometric_region
         real(cp),dimension(3) :: hmin,hmax
         logical,dimension(3) :: include_hmin,include_hmax
       end type

       contains

       subroutine init_geometric_region(R,hmin,hmax,include_hmin,include_hmax)
         implicit none
         type(geometric_region),intent(inout) :: R
         real(cp),dimension(3),intent(in) :: hmin,hmax
         logical,dimension(3),intent(in) :: include_hmin,include_hmax
         R%hmin = hmin
         R%hmax = hmax
         R%include_hmin = include_hmin
         R%include_hmax = include_hmax
       end subroutine

       subroutine assign_inside(U,m,R,val)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         type(geometric_region),intent(in) :: R
         real(cp),intent(in) :: val
         integer :: i,j,k,t
         real(cp) :: tol
         tol = 10.0_cp**(-12.0_cp)
         if (U%is_CC) then
         !$OMP PARALLEL DO
         do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
         if (inside_CC(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         elseif (U%is_Node) then
         !$OMP PARALLEL DO
         do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
         if (inside_N(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         elseif (U%is_Face) then
           select case (U%face)
           case (1)
           !$OMP PARALLEL DO
           do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
             if (inside_F_x(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           case (2)
           !$OMP PARALLEL DO
           do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
             if (inside_F_y(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           case (3)
           !$OMP PARALLEL DO
           do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
             if (inside_F_z(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           case default; stop 'Error: bad data face input to assign_inside in geometric_define.f90'
           end select
         elseif (U%is_Edge) then
           select case (U%edge)
           case (1)
           !$OMP PARALLEL DO
           do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
             if (inside_E_x(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           case (2)
           !$OMP PARALLEL DO
           do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
             if (inside_E_y(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           case (3)
           !$OMP PARALLEL DO
           do t=1,U%s; do k=1,U%RF(t)%s(3); do j=1,U%RF(t)%s(2); do i=1,U%RF(t)%s(1)
             if (inside_E_z(R,m,i,j,k,t,tol)) U%RF(t)%f(i,j,k) = val
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           case default; stop 'Error: bad data edge input to assign_inside in geometric_define.f90'
           end select
         else; stop 'Error: bad data input to assign_inside in geometric_define.f90'
         endif
       end subroutine


       ! ***********************************************************************
       ! ************************* INCLUDING QUERY FUNCS ***********************
       ! ***********************************************************************

       pure function inside_CC(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hc(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hc(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hc(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hc(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hc(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hc(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_N(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hn(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hn(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hn(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hn(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hn(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hn(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_F_x(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hn(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hc(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hc(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hn(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hc(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hc(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_F_y(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hc(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hn(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hc(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hc(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hn(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hc(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_F_z(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hc(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hc(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hn(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hc(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hc(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hn(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_E_x(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hc(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hn(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hn(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hc(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hc(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hn(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hn(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_E_y(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hn(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hc(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hn(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hn(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hc(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hc(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hn(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hn(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       pure function inside_E_z(R,m,i,j,k,t,tol) result(L)
         implicit none
         type(geometric_region),intent(in) :: R
         type(mesh),intent(in) :: m
         integer,intent(in) :: i,j,k,t
         real(cp),intent(in) :: tol
         logical :: inside_min,inside_max,L
         inside_min=(m%g(t)%c(1)%hn(i).gt.R%hmin(1)+tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmin(1)).lt.&
                     tol).and.R%include_hmin(1))).and.&
                    (m%g(t)%c(2)%hn(j).gt.R%hmin(2)+tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmin(2)).lt.&
                     tol).and.R%include_hmin(2))).and.&
                    (m%g(t)%c(3)%hc(k).gt.R%hmin(3)+tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmin(3)).lt.&
                     tol).and.R%include_hmin(3)))
         inside_max=(m%g(t)%c(1)%hn(i).lt.R%hmax(1)-tol.or.((abs(m%g(t)%c(1)%hn(i)-R%hmax(1)).lt.&
                     tol).and.R%include_hmax(1))).and.&
                    (m%g(t)%c(2)%hn(j).lt.R%hmax(2)-tol.or.((abs(m%g(t)%c(2)%hn(j)-R%hmax(2)).lt.&
                     tol).and.R%include_hmax(2))).and.&
                    (m%g(t)%c(3)%hc(k).lt.R%hmax(3)-tol.or.((abs(m%g(t)%c(3)%hc(k)-R%hmax(3)).lt.&
                     tol).and.R%include_hmax(3)))
         L = inside_min.and.inside_max
       end function

       end module