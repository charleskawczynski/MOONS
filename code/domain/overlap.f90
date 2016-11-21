       module overlap_mod
       use current_precision_mod
       use coordinates_mod
       implicit none

       private
       public :: overlap
       public :: init,delete,display,print,export,import ! Essentials

       public :: inside,init_props
       public :: is_overlap_physical
       public :: is_overlap_any
       public :: is_overlap

       public :: pick_extrema_top
       public :: pick_extrema_bot

       public :: valid_range

       public :: get_N_overlap
       public :: get_C_overlap

       public :: get_p_from_boundary_N
       public :: get_p_from_boundary_C

       public :: compare
       public :: same_point

       interface init;                 module procedure init_overlap;                end interface
       interface init;                 module procedure init_copy_overlap;           end interface
       interface delete;               module procedure delete_overlap;              end interface
       interface print;                module procedure print_overlap;               end interface
       interface display;              module procedure display_overlap;             end interface
       interface export;               module procedure export_overlap;              end interface
       interface import;               module procedure import_overlap;              end interface

       interface get_p_from_boundary_N;module procedure get_p_from_boundary_N_OL;    end interface
       interface get_p_from_boundary_C;module procedure get_p_from_boundary_C_OL;    end interface

       interface pick_extrema_bot;     module procedure pick_extrema_bot_OL;         end interface
       interface pick_extrema_top;     module procedure pick_extrema_top_OL;         end interface

       interface get_N_overlap;        module procedure get_N_overlap_OL;            end interface
       interface get_C_overlap;        module procedure get_C_overlap_OL;            end interface

       interface compare;              module procedure compare_overlap;             end interface
       interface same_point;           module procedure same_point_OL;               end interface
       interface same_point;           module procedure same_point_OL_2;             end interface
       interface init_props;           module procedure init_props_OL;               end interface
       interface is_overlap;           module procedure is_overlap_coordinates;      end interface
       interface valid_range;          module procedure valid_range_overlap;         end interface
       interface inside;               module procedure inside_OL;                   end interface

       type overlap
         ! i1(1) and i1(2) are indexes for start and end of overlapping region 1
         ! i2(1) and i2(2) are indexes for start and end of overlapping region 2
         ! L is a logical of whether overlap exists or not
         ! For example:
         !                           i1(1)     i1(2)
         !                            |----------------R1----------------|
         !     |-------------R2---------------|
         !                           i2(2)     i2(2)
         !
         integer,dimension(2) :: i1 = 0 ! index for region 1
         integer,dimension(2) :: i2 = 0 ! index for region 2
         integer :: iR = 0              ! index range
         logical :: success = .false.   ! successful overlap, there are "bad" cases!

         ! Consider R1 is CC and R2 is node data:
         !                       i1(1)=i1(2)
         !                            |      <-- (R1)
         !     |-------------R2---------------|
         !                       i2(1)=i2(2)
         ! Clearly, CI indexes are not valid
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
         a%success = b%success
       end subroutine

       subroutine init_props_OL(OL)
         implicit none
         type(overlap),intent(inout) :: OL
         OL%iR = OL%i1(2) - OL%i1(1) + 1
         OL%success = .not.any((/OL%i1(1).eq.0,OL%i1(2).eq.0,OL%i2(1).eq.0,OL%i2(2).eq.0/))
       end subroutine

       subroutine delete_overlap(OL)
         implicit none
         type(overlap),intent(inout) :: OL
         OL%i1 = 0
         OL%i2 = 0
         OL%iR = 0
         OL%success = .false.
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

       function compare_overlap(a,b) result(L_all)
         implicit none
         type(overlap),intent(in) :: a
         type(overlap),intent(in) :: b
         logical :: L_all
         logical,dimension(6) :: L
         L(1) = a%i1(1) - b%i1(1).eq.0
         L(2) = a%i2(1) - b%i2(1).eq.0
         L(3) = a%i1(2) - b%i1(2).eq.0
         L(4) = a%i2(2) - b%i2(2).eq.0
         L(5) = a%iR - b%iR.eq.0
         L(6) = a%success .eqv. b%success
         L_all = all(L)
       end function

       ! ************************************************************************
       ! *************************** BOUNDARY OVERLAPS **************************
       ! ************************************************************************

       function get_p_from_boundary_N_OL(OL,c,tol,p) result(G)
         implicit none
         type(overlap),intent(in) :: OL
         type(coordinates),dimension(2),intent(in) :: c
         real(cp),intent(in) :: tol
         integer,intent(in) :: p
         type(overlap) :: G
         call init(G,OL)
         if (p.lt.1) stop 'Error: p must be > 1 in get_p_from_boundary_N_overlap in overlap.f90'
         if (G%iR.eq.3) then ! Surface overlap
           if (p.gt.G%iR) stop 'Error: bad input to get_p_from_boundary_N_overlap in overlap.f90'
               if (inside_OL(c(1)%hn(p),c(2)%hn(p),c(2)%hn(p),tol)) then
           G%i2(2) = G%i2(1)-1+p ! move to far left side. Too far! go p back!
           G%i2(1) = G%i2(2) ! copy end
           G%i1(2) = G%i1(1)-1+p ! move to far left side. Too far! go p back!
           G%i1(1) = G%i1(2) ! copy end
           elseif (inside_OL(c(1)%hn(c(1)%sn+1-p),c(2)%hn(c(2)%sn+1-p),c(2)%hn(c(2)%sn+1-p),tol)) then
           G%i2(1) = G%i2(2)+1-p ! move to far right side. Too far! go p back!
           G%i2(2) = G%i2(1) ! copy end
           G%i1(1) = G%i1(2)+1-p ! move to far right side. Too far! go p back!
           G%i1(2) = G%i1(1) ! copy end
           endif
         endif
         call init_props(G)
       end function

       function get_p_from_boundary_C_OL(OL,c,tol,p) result(G)
         implicit none
         type(overlap),intent(in) :: OL
         type(coordinates),dimension(2),intent(in) :: c
         real(cp),intent(in) :: tol
         integer,intent(in) :: p
         type(overlap) :: G
         call init(G,OL)
         if (p.lt.1) stop 'Error: p must be > 1 in get_p_from_boundary_N_overlap in overlap.f90'
         if (G%iR.eq.2) then ! Surface overlap
           if (p.gt.G%iR) stop 'Error: bad input to get_p_from_boundary_N_overlap in overlap.f90'
               if (inside_OL(c(1)%hc(p),c(2)%hc(p),c(2)%hc(p),tol)) then
           G%i2(2) = G%i2(1)-1+p ! move to far left side. Too far! go p back!
           G%i2(1) = G%i2(2) ! copy end
           G%i1(2) = G%i1(1)-1+p ! move to far left side. Too far! go p back!
           G%i1(1) = G%i1(2) ! copy end
           elseif (inside_OL(c(1)%hc(c(1)%sc+1-p),c(2)%hc(c(2)%sc+1-p),c(2)%hc(c(2)%sc+1-p),tol)) then
           G%i2(1) = G%i2(2)+1-p ! move to far right side. Too far! go p back!
           G%i2(2) = G%i2(1) ! copy end
           G%i1(1) = G%i1(2)+1-p ! move to far right side. Too far! go p back!
           G%i1(2) = G%i1(1) ! copy end
           endif
         endif
         call init_props(G)
       end function

       ! ************************************************************************
       ! ******************************** EXTREMA *******************************
       ! ************************************************************************

       subroutine pick_extrema_top_OL(A,B)
         implicit none
         type(overlap),intent(inout) :: A
         type(overlap),intent(in) :: B
         integer,dimension(2) :: i1,i2
         i1(1) = A%i1(1); i1(2) = maxval((/A%i1(2),B%i1(2)/))
         i2(1) = A%i2(1); i2(2) = maxval((/A%i2(2),B%i2(2)/))
         call init(A,i1,i2)
       end subroutine

       subroutine pick_extrema_bot_OL(A,B)
         implicit none
         type(overlap),intent(inout) :: A
         type(overlap),intent(in) :: B
         integer,dimension(2) :: i1,i2
         i1(1) = A%i1(1); i1(2) = minval((/A%i1(2),B%i1(2)/))
         i2(1) = A%i2(1); i2(2) = minval((/A%i2(2),B%i2(2)/))
         call init(A,i1,i2)
       end subroutine

       ! ************************************************************************
       ! ****************************** GET OVERLAP *****************************
       ! ************************************************************************

       function get_N_overlap_OL(c,tol,p) result (OL)
         implicit none
         type(coordinates),dimension(2),intent(in) :: c
         real(cp),intent(in) :: tol
         integer,intent(in) :: p
         type(overlap) :: OL
         if (p.ge.0) then
           if ((c(1)%sn-p.gt.0).and.(c(2)%sn-p.gt.0)) then;OL = get_N_overlap_OL_blind(c,tol,p)
           else;                                           OL = get_N_overlap_OL_blind(c,tol,0)
           endif
         else; stop 'Error: bad input to get_N_overlap_OL in overlap.f90'
         endif
       end function
       function get_N_overlap_OL_blind(c,tol,p) result (OL)
         implicit none
         type(coordinates),dimension(2),intent(in) :: c
         real(cp),intent(in) :: tol
         integer,intent(in) :: p
         type(overlap) :: OL
         integer :: i,j,p1,p2
         OL%i1 = 0; OL%i2 = 0
         p1 = p; p2 = p
         if (p.gt.0) then
         if (c(1)%sc.eq.1) p1 = 0
         if (c(2)%sc.eq.1) p2 = 0
         else; p1 = 0; p2 = 0
         endif
         do i=c(1)%sn-p1,1+p1,-1; do j=1+p2,c(2)%sn-p2
         if (inside_OL(c(2)%hn(j),c(1)%hn(i),c(1)%hn(i),tol)) then
           if (OL%i1(1).eq.0) then; OL%i1(1) = i; else; OL%i1(1) = minval((/OL%i1(1),i/));endif
           if (OL%i1(2).eq.0) then; OL%i1(2) = i; else; OL%i1(2) = maxval((/OL%i1(2),i/));endif
           if (OL%i2(1).eq.0) then; OL%i2(1) = j; else; OL%i2(1) = minval((/OL%i2(1),j/));endif
           if (OL%i2(2).eq.0) then; OL%i2(2) = j; else; OL%i2(2) = maxval((/OL%i2(2),j/));endif
         endif
         enddo; enddo
         call init_props(OL)
       end function

       function get_C_overlap_OL(c,tol,p) result (OL)
         implicit none
         type(coordinates),dimension(2),intent(in) :: c
         real(cp),intent(in) :: tol
         integer,intent(in) :: p
         type(overlap) :: OL
         if (p.ge.0) then
           if ((c(1)%sc-p.gt.0).and.(c(2)%sc-p.gt.0)) then;OL = get_C_overlap_OL_blind(c,tol,p)
           else;                                           OL = get_C_overlap_OL_blind(c,tol,0)
           endif
         else; stop 'Error: bad input to get_C_overlap_OL in overlap.f90'
         endif
       end function
       function get_C_overlap_OL_blind(c,tol,p) result (OL)
         implicit none
         type(coordinates),dimension(2),intent(in) :: c
         real(cp),intent(in) :: tol
         integer,intent(in) :: p
         type(overlap) :: OL
         integer :: i,j,p1,p2
         OL%i1 = 0; OL%i2 = 0
         p1 = p; p2 = p
         if (p.gt.0) then
         if (c(1)%sc.eq.1) p1 = 0
         if (c(2)%sc.eq.1) p2 = 0
         else; p1 = 0; p2 = 0
         endif
         do i=c(1)%sc-p1,1+p1,-1; do j=1+p2,c(2)%sc-p2
         if (inside_OL(c(2)%hc(j),c(1)%hc(i),c(1)%hc(i),tol)) then
           if (OL%i1(1).eq.0) then; OL%i1(1) = i; else; OL%i1(1) = minval((/OL%i1(1),i/));endif
           if (OL%i1(2).eq.0) then; OL%i1(2) = i; else; OL%i1(2) = maxval((/OL%i1(2),i/));endif
           if (OL%i2(1).eq.0) then; OL%i2(1) = j; else; OL%i2(1) = minval((/OL%i2(1),j/));endif
           if (OL%i2(2).eq.0) then; OL%i2(2) = j; else; OL%i2(2) = maxval((/OL%i2(2),j/));endif
         endif
         enddo; enddo
         call init_props(OL)
       end function

       ! ************************* VALID OVERLAP RANGE? *************************

       function valid_range_overlap(OL) result (L)
         implicit none
         type(overlap),intent(in) :: OL
         logical :: L
         L = (OL%i1(2) - OL%i1(1) + 1).eq.(OL%i2(2) - OL%i2(1) + 1)
       end function

       ! ************************* DOES AN OVERLAP EXIST? *************************

       function is_overlap_coordinates(R1,R2,tol) result(L)
         implicit none
         type(coordinates),intent(in) :: R1,R2
         real(cp),intent(in) :: tol
         logical :: L
         L = is_overlap_any(R1,R2,tol)
       end function

       function is_overlap_any(R1,R2,tol) result(L)
         implicit none
         type(coordinates),intent(in) :: R1,R2
         real(cp),intent(in) :: tol
         logical :: L
         L = is_overlap_general(R1%amin,R1%amax,R2%amin,R2%amax,tol)
       end function

       function is_overlap_physical(R1,R2,tol) result(L)
         implicit none
         type(coordinates),intent(in) :: R1,R2
         real(cp),intent(in) :: tol
         logical :: L
         L = is_overlap_general(R1%amin,R1%amax,R2%amin,R2%amax,tol)
       end function

       function is_overlap_general(R1_hmin,R1_hmax,R2_hmin,R2_hmax,tol) result(L_any)
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
         implicit none
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

       function same_point_OL_2(p1,p2) result(L)
         real(cp),intent(in) :: p1,p2
         logical :: L
         L = inside_OL(p1,p2,p2,10.0_cp**(-10.0_cp))
       end function

       function same_point_OL(p1,p2,tol) result(L)
         real(cp),intent(in) :: p1,p2
         real(cp),intent(in) :: tol
         logical :: L
         L = inside_OL(p1,p2,p2,tol)
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