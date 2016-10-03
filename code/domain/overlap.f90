       module overlap_mod
       use current_precision_mod
       use coordinates_mod
       implicit none

       private
       public :: overlap
       public :: init,delete,display,print,export,import ! Essentials
       public :: is_overlap,inside,init_props

       interface init;          module procedure init_overlap;              end interface
       interface init;          module procedure init_copy_overlap;         end interface
       interface delete;        module procedure delete_overlap;            end interface
       interface print;         module procedure print_overlap;             end interface
       interface display;       module procedure display_overlap;           end interface
       interface export;        module procedure export_overlap;            end interface
       interface import;        module procedure import_overlap;            end interface

       interface is_overlap;    module procedure is_overlap_real;           end interface
       interface is_overlap;    module procedure is_overlap_coordinates;    end interface
       interface inside;        module procedure inside_OL;                 end interface
       interface init_props;    module procedure init_props_OL;             end interface

       type overlap
         ! i1(1) and i1(2) are indexes for start and end of overlapping region 1
         ! i2(1) and i2(2) are indexes for start and end of overlapping region 2
         ! L is a logical of whether overlap exists or not
         ! For example:
         !                           i1(1)     i1(2)
         !                            |----------------R1----------------|
         !     |-------------R2---------------|
         !                           i2(2)     i2(2)
         integer,dimension(2) :: i1,i2
         integer :: iR                 ! = i1(2)-i1(1)+1 = i2(2)-i2(1)+1 (range of indexes)
       end type

       contains

       subroutine init_overlap(OL,i1,i2)
         implicit none
         type(overlap),intent(inout) :: OL
         integer,dimension(2),intent(in) :: i1,i2
         OL%i1 = i1
         OL%i2 = i2
         call init_props(OL)
       end subroutine

       subroutine init_copy_overlap(a,b)
         implicit none
         type(overlap),intent(inout) :: a
         type(overlap),intent(in) :: b
         a%i1 = b%i1
         a%i2 = b%i2
         a%iR = b%iR
       end subroutine

       subroutine init_props_OL(OL)
         implicit none
         type(overlap),intent(inout) :: OL
         OL%iR = OL%i1(2) - OL%i1(1) + 1
       end subroutine

       subroutine delete_overlap(OL)
         implicit none
         type(overlap),intent(inout) :: OL
         OL%i1 = 0
         OL%i2 = 0
         OL%iR = 0
       end subroutine

       subroutine print_overlap(OL)
         implicit none
         type(overlap),intent(in) :: OL
         call display(OL,6)
       end subroutine

       subroutine display_overlap(OL,u)
         implicit none
         type(overlap),intent(in) :: OL
         integer,intent(in) :: u
         write(u,*) 'i1 = ',OL%i1
         write(u,*) 'i2 = ',OL%i2
         write(u,*) 'iR = ',OL%iR
       end subroutine

       subroutine export_overlap(OL,u)
         implicit none
         type(overlap),intent(in) :: OL
         integer,intent(in) :: u
         write(u,*) 'i1'; write(u,*) OL%i1
         write(u,*) 'i2'; write(u,*) OL%i2
         write(u,*) 'iR'; write(u,*) OL%iR
       end subroutine

       subroutine import_overlap(OL,u)
         implicit none
         type(overlap),intent(inout) :: OL
         integer,intent(in) :: u
         read(u,*) ; read(u,*) OL%i1
         read(u,*) ; read(u,*) OL%i2
         read(u,*) ; read(u,*) OL%iR
       end subroutine

       function is_overlap_coordinates(R1,R2,tol) result(L)
         type(coordinates),intent(in) :: R1,R2
         real(cp),intent(in) :: tol
         logical :: L
         L = is_overlap_real(R1%hmin,R1%hmax,R2%hmin,R2%hmax,tol)
       end function

       function is_overlap_real(R1_hmin,R1_hmax,R2_hmin,R2_hmax,tol) result(L_any)
         ! L = overlapp
         ! 6 possibilities:
         ! 
         !       1)
         !                 |-----R1-----|
         !             |-----R2-----|
         !       2)
         !         |-----R1-----|
         !             |-----R2-----|
         !       3)
         !             |---R1---|      |-------R1-------|
         !             |-----R2-----|  |-----R2-----|
         !       4)
         !                 |---R1---|  |-------R1-------|
         !             |-----R2-----|      |-----R2-----|
         !       5)
         !               |---R1---|      |-------R1-------|
         !             |-----R2-----|      |-----R2-----|
         !       6)
         !               |---R2---|      |-------R2-------|
         !             |-----R1-----|      |-----R1-----|
         real(cp),intent(in) :: R1_hmin,R1_hmax,R2_hmin,R2_hmax
         real(cp),intent(in) :: tol
         logical,dimension(6) :: L
         logical :: L_any
         L(1) = (R1_hmin.gt.R2_hmin).and.(R1_hmin.lt.R2_hmax)
         L(2) = (R1_hmax.gt.R2_hmin).and.(R1_hmax.lt.R2_hmax)
         L(3) = abs(R2_hmin-R1_hmin).lt.tol
         L(4) = abs(R2_hmax-R1_hmax).lt.tol
         L(5) = (R1_hmin.gt.R2_hmin).and.(R1_hmax.lt.R2_hmax)
         L(6) = (R2_hmin.gt.R1_hmin).and.(R2_hmax.lt.R1_hmax)
         L_any = any(L)
       end function

       function inside_OL(p,hmin,hmax,tol) result(L_any)
         ! L = point_inside
         ! 3 possibilities:
         ! 
         !       1)
         !                 *
         !             |-----R-----|
         !       2)
         !             *
         !             |-----R-----|
         !       3)
         !                         *
         !             |-----R-----|
         ! Note:
         !       4) not needed since this is a special case of 2 and 3
         !             *     (hmin = hmax)
         !             |
         ! 
         real(cp),intent(in) :: p,hmin,hmax
         real(cp),intent(in) :: tol
         logical,dimension(3) :: L
         logical :: L_any
         L(1) = (p.gt.hmin).and.(p.lt.hmax)
         L(2) = abs(p-hmin).lt.tol
         L(3) = abs(p-hmax).lt.tol
         L_any = any(L)
       end function

       end module