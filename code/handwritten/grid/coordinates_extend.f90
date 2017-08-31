      module coordinates_extend_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
      use coordinates_mod
      use current_precision_mod
      use array_mod
      use array_extend_mod
      use sparse_mod
      use derivative_stencils_mod
      use interpolation_stencils_mod
      use IO_tools_mod
      implicit none

      private
      public :: coordinates
      public :: init

      ! For stitching multi-domains, only
      ! after coordinates has been defined
      public :: stitch_stencils

      ! For multi-grid
      public :: restrict
      public :: prolongate

      public :: pop,snip
      public :: mirror_about_hmin
      public :: mirror_about_hmax

      ! For getting surface / edge corner
      public :: get_GI
      public :: get_boundary
      public :: add_ghost_nodes
      public :: prepend_ghost
      public :: append_ghost

#ifdef _DEBUG_COORDINATES_
      public :: checkCoordinates
#endif

      interface init;              module procedure init_c;                 end interface
      interface init;              module procedure init_array_c;           end interface

      interface restrict;          module procedure restrict_c;             end interface
      interface restrict;          module procedure restrict_reset_c;       end interface
      interface prolongate;        module procedure prolongate_c;           end interface
      interface prolongate;        module procedure prolongate_reset_c;     end interface

      interface mirror_about_hmin; module procedure mirror_about_hmin_c;    end interface
      interface mirror_about_hmax; module procedure mirror_about_hmax_c;    end interface

      interface stitch_stencils;   module procedure stitch_stencils_c;      end interface
      interface init_stencils;     module procedure init_stencils_c;        end interface ! Private

      interface pop;               module procedure pop_c;                  end interface
      interface snip;              module procedure snip_c;                 end interface

      interface get_GI;            module procedure get_GI_c;               end interface
      interface get_boundary;      module procedure get_boundary_c;         end interface

      interface init_props;        module procedure init_props_c;           end interface
      interface add_ghost_nodes;   module procedure add_ghost_nodes_c;      end interface
      interface prepend_ghost;     module procedure prepend_ghost_c;        end interface
      interface append_ghost;      module procedure append_ghost_c;         end interface

      contains

      ! **********************************************************
      ! ********************* ESSENTIALS *************************
      ! **********************************************************

      subroutine init_array_c(c,hn)
        implicit none
        type(coordinates),intent(inout) :: c
        type(array),intent(in) :: hn
        call init(c,hn%f,hn%N)
      end subroutine

      subroutine init_c(c,hn,sn)
        ! Here is a picture how coordinates are initialized for different cases:
        !
        !
        ! ----------------------------- 1) 1 or more interior cells  + 2 ghost (typical):
        !
        !         hn(1)     hn(2)     hn(3)     hn(4)
        !          |-----------------------------|
        !          |    .    |    .    |    .    |
        !          |-----------------------------|
        !          |   hc(1) |   hc(2) |   hc(3) |
        !        amin       hmin      hmax      amax
        !
        ! ----------------------------- 2) 0 interior cells + 2 ghost:
        ! * This case does need special initialization because it is a special case of 1).
        !
        !              hn(1)     hn(2)     hn(3)
        !               |-------------------|
        !               |    .    |    .    |
        !               |-------------------|
        !               |   hc(1) |   hc(2) |
        !             amin    hmin,hmax      amax
        !
        ! ----------------------------- 3) 0 interior cells + 1 ghost:
        !
        !                 hn(1)        hn(2)
        !                  |-------------|
        !                  |      .      |
        !                  |-------------|
        !                  |     hc(1)   |
        !              hmin,amin     hmax,amax
        !
        ! ----------------------------- 4) 0 interior cells + 0 ghost (infinitely thin plane):
        !
        !                    hn(1),hc(1)
        !                         |
        !                         .
        !                         |
        !                         |
        !                hmin,amin,hmax,amax
        !
        ! -----------------------------
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: sn
        real(cp),dimension(sn),intent(in) :: hn
        integer :: i
        call delete(c)
        if (.not.(size(hn).gt.0)) stop 'Error: hn not allocated in init_c in coordinates.f90'
        if (.not.(size(hn).eq.sn)) stop 'Error: sn.ne.size(hn) in init_c in coordinates.f90'

        ! Typical init
        c%sn = size(hn)
        if (c%sn.gt.2) then
          ! Node grid
          call init(c%hn,c%sn)
          call init(c%dhn,c%sn-1)
          c%hn%f = hn
          c%dhn%f = (/(hn(i+1)-hn(i),i=1,c%sn-1)/)
          ! Cell center grid
          c%sc = c%sn-1
          call init(c%hc,c%sc)
          call init(c%dhc,c%sc-1)
          c%hc%f = (/ ((hn(i+1)+hn(i))/2.0_cp,i=1,c%sn-1) /)
          c%dhc%f = (/(c%hc%f(i+1)-c%hc%f(i),i=1,c%sc-1)/)
        elseif (c%sn.eq.2) then
          ! Node grid
          call init(c%hn,c%sn)
          call init(c%dhn,c%sn-1)
          c%hn%f = hn
          c%dhn%f = (/(hn(i+1)-hn(i),i=1,c%sn-1)/)
          ! Cell center grid
          c%sc = c%sn-1
          call init(c%hc,c%sc)
          call init(c%dhc,1)
          c%hc%f = (/ ((hn(i+1)+hn(i))/2.0_cp,i=1,c%sn-1) /)
          c%dhc%f = 0.0_cp
        elseif (c%sn.eq.1) then
          ! Node grid
          call init(c%hn,c%sn)
          call init(c%dhn,1)
          c%hn%f = hn
          c%dhn%f = 0.0_cp
          ! Cell center grid
          c%sc = 1
          call init(c%hc,c%sc)
          call init(c%dhc,1)
          c%hc%f = c%hn%f
          c%dhc%f = 0.0_cp
        endif
        ! Additional information
        call init_props(c)
        call init_stencils(c)
        c%stencils_modified = .false.
        c%defined = .true.
      end subroutine

      subroutine init_props_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
         ! Additional information
         c%dhMin = minval(c%dhn%f)
         c%dhMax = maxval(c%dhn%f)
         c%amin = c%hn%f(1)
         c%amax = c%hn%f(c%sn)
         c%hn_e = c%hn%f(c%sn)
         c%hc_e = c%hc%f(c%sc)
         if (c%sn.gt.2) then ! Typical init
           c%hmin = c%hn%f(2)
           c%hmax = c%hn%f(c%sn-1) ! To account for ghost node
           c%maxRange = c%hmax-c%hmin
           c%N = size(c%hc%f)-2
           c%dhc_e = c%dhc%f(c%sc-1)
           c%dhn_e = c%dhn%f(c%sn-1)
         elseif (c%sn.eq.2) then
           c%hmin = c%hn%f(1)
           c%hmax = c%hn%f(2)
           c%maxRange = c%hmax-c%hmin
           c%N = 0
           c%dhc_e = c%dhc%f(1)
           c%dhn_e = c%dhn%f(1)
         elseif (c%sn.eq.1) then
           c%hmin = c%hn%f(1)
           c%hmax = c%hn%f(1)
           c%maxRange = 0.0_cp
           c%N = 0
           c%dhc_e = c%dhc%f(1)
           c%dhn_e = c%dhn%f(1)
         endif
         c%i_midplane = c%sc/2+1
      end subroutine

      ! *****************************************************************
      ! ************************ STITCH STENCILS ************************
      ! *****************************************************************

      subroutine stitch_stencils_c(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        if (.not.c%defined) then
          stop 'Error: coordinates not defined in stitch_stencils_c in coordinates.f90'
        endif
        if (.not.c%stencils_defined) then
          stop 'Error: coordinate stencils not defined in stitch_stencils_c in coordinates.f90'
        endif
        c%stencils_modified(1) = hmin
        c%stencils_modified(2) = hmax
        call stitch_colCC_1(c,hmin,hmax)
        call stitch_colCC_2(c,hmin,hmax)
        call stitch_colN_1(c,hmin,hmax)
        call stitch_colN_2(c,hmin,hmax)
      end subroutine

      subroutine stitch_colCC_2(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sc.gt.2) then
          s = c%sc; allocate(dh(s-1)); dh = c%dhc%f
          if (hmin) then; i = 2
            c%colCC(2)%L%f(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colCC(2)%D%f(1) = -2.0_cp/(dh(i-1)*dh(i))
            c%colCC(2)%U%f(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          if (hmax) then; i = s-1
            c%colCC(2)%L%f(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colCC(2)%D%f(s-2) = -2.0_cp/(dh(i-1)*dh(i))
            c%colCC(2)%U%f(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine stitch_colCC_1(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        if (c%sc.gt.2) then
          s = c%sc; allocate(dh(s-1)); dh = c%dhc%f
          if (hmin) then; i = 2
            c%colCC(1)%L%f(1) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colCC(1)%D%f(1) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colCC(1)%U%f(1) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
          endif
          if (hmax) then; i = s-1
            c%colCC(1)%L%f(s-2) = (-dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colCC(1)%D%f(s-2) = ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colCC(1)%U%f(s-2) = (dh(i-1)/(dh(i)*(dh(i-1)+dh(i))))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine stitch_colN_2(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        real(cp),dimension(:),allocatable :: dh
        logical,intent(in) :: hmin,hmax
        integer :: i,s
        if (c%sn.gt.2) then
          s = c%sn; allocate(dh(s-1)); dh = c%dhn%f
          if (hmin) then; i = 2
            c%colN(2)%L%f(1) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colN(2)%D%f(1) = -2.0_cp/(dh(i-1)*dh(i))
            c%colN(2)%U%f(1) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          if (hmax) then; i = s-1
            c%colN(2)%L%f(s-2) =  2.0_cp/(dh(i-1)*(dh(i-1)+dh(i)))
            c%colN(2)%D%f(s-2) = -2.0_cp/(dh(i-1)*dh(i))
            c%colN(2)%U%f(s-2) =  2.0_cp/(dh( i )*(dh(i-1)+dh(i)))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine stitch_colN_1(c,hmin,hmax)
        implicit none
        type(coordinates),intent(inout) :: c
        logical,intent(in) :: hmin,hmax
        real(cp),dimension(:),allocatable :: dh
        integer :: i,s
        if (c%sn.gt.2) then
          s = c%sn; allocate(dh(s-1)); dh = c%dhn%f
          if (hmin) then; i = 2
            c%colN(1)%L%f(1) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colN(1)%D%f(1) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colN(1)%U%f(1) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
          endif
          if (hmax) then; i = s-1
            c%colN(1)%L%f(s-2) = -(dh(i)/(dh(i-1)*(dh(i-1)+dh(i))))
            c%colN(1)%D%f(s-2) =  ((-dh(i-1)+dh(i))/(dh(i-1)*dh(i)))
            c%colN(1)%U%f(s-2) =  (dh(i-1)/(dh( i )*(dh(i-1)+dh(i))))
          endif
          deallocate(dh)
        else
          write(*,*) 'Error: cannot stitch domains with single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      ! *****************************************************************
      ! ***************** RESTRICTION (FOR MULTIGRID) *******************
      ! *****************************************************************

      subroutine restrict_c(r,c)
        ! Restriction for bad cases should only be allowed ~1 time
        ! Multiple restrictions of this type can make the cells
        ! blow up in size and overlap.
        ! Consider making a flag to only allow 1 time or
        ! use warnings to the user about the grid
        implicit none
        type(coordinates),intent(inout) :: r
        type(coordinates),intent(in) :: c
        integer :: i
        call init(r,c)
        if (c%sc.gt.3) then
          if (mod(c%sc,2).eq.0) then
            call init(r,(/(c%hn%f(2*i),i=1,c%sc/2)/),c%sc/2)
            call add_ghost_nodes(r)
            else; stop 'Error: coordinates must be even in restrictCoordinates in coordinates.f90'
          endif
        else; call init(r,c) ! return c
        endif
      end subroutine

      subroutine restrict_reset_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        call restrict(temp,c)
        call init(c,temp)
        call delete(temp)
      end subroutine

      ! *****************************************************************
      ! ****************** PROLONGATE (FOR MULTIGRID) *******************
      ! *****************************************************************

      subroutine prolongate_c(p,c)
        implicit none
        type(coordinates),intent(inout) :: p
        type(coordinates),intent(in) :: c
        type(array),dimension(3) :: a
        integer :: i
        call init(p,c)
            if (c%sn.eq.1) then ! Can't prolongate without interior!
        elseif (c%sn.eq.2) then
        elseif (c%sn.eq.3) then
        elseif (c%sn.gt.3) then ! typical case, check to see if should be (c%sn.gt.4)
          call init(a(1),c%hn%f(2:c%sn-1),c%sn-2)
          call init(a(2),c%hc%f(2:c%sc-1),c%sc-2)
          call init(a(3),a(1)%N+a(2)%N)
          ! call init(a(3),2*(a(1)%N-1)-1)
          do i=1,a(1)%N; a(3)%f(2*i-1) = a(1)%f(i); enddo
          do i=1,a(2)%N; a(3)%f(2*i)   = a(2)%f(i); enddo
          call init(p,a(3)%f,a(3)%N)
          call add_ghost_nodes(p)
          do i=1,3; call delete(a(i)); enddo
        endif
      end subroutine

      subroutine prolongate_reset_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        call prolongate(temp,c)
        call init(c,temp)
        call delete(temp)
      end subroutine

      ! *****************************************************************
      ! ******************** MIRROR (FOR SYMMETRY) **********************
      ! *****************************************************************

      subroutine mirror_about_hmin_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(array) :: a,m
        if (c%sn.eq.1) then ! Can't mirror without interior!
        elseif (c%sn.eq.2) then
        elseif (c%sn.eq.3) then
        elseif (c%sn.gt.3) then ! typical case
          call init(m,c%hn%f(2:c%sn),c%sn-1)
          call multiply(m,-1.0_cp)
          call add(m,2.0_cp*c%hn%f(2))
          call reverse(m)
          call init(a,(/m%f,c%hn%f(3:c%sn)/),2*c%sn-1)
          call init(c,a%f,a%N)
          call delete(a)
          call delete(m)
        endif
      end subroutine

      subroutine mirror_about_hmax_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        type(array) :: a,m
        if (c%sn.eq.1) then ! Can't mirror without interior!
        elseif (c%sn.eq.2) then
        elseif (c%sn.eq.3) then
        elseif (c%sn.gt.3) then ! typical case
          call init(m,c%hn%f(1:c%sn-1),c%sn-1)
          call multiply(m,-1.0_cp)
          call add(m,2.0_cp*c%hn%f(c%sn-1))
          call reverse(m)
          call init(a,(/c%hn%f(1:c%sn-2),m%f/),2*c%sn-3)
          call init(c,a%f,a%N)
          call delete(a)
          call delete(m)
        endif
      end subroutine

      ! *****************************************************************
      ! ********************* POP / SNIP ROUTINES ***********************
      ! *****************************************************************

      subroutine snip_c(c) ! Removes the first index from the coordinates
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        if (c%sn.eq.1) stop 'Error: no nodes to snip in snip_c in coordinates.f90'
        call init(temp,c%hn%f(2:c%sn-1),c%sn-1)
        call init(c,temp%hn%f,temp%sn)
        call delete(temp)
      end subroutine

      subroutine pop_c(c) ! Removes the last index from the coordinates
        implicit none
        type(coordinates),intent(inout) :: c
        type(coordinates) :: temp
        if (c%sn.eq.1) stop 'Error: no nodes to snip in pop_c in coordinates.f90'
        call init(temp,c%hn%f(1:c%sn-1),c%sn-1)
        call init(c,temp%hn%f,temp%sn)
        call delete(temp)
      end subroutine

      ! *****************************************************************
      ! ************* GET GHOST / BOUNDARY / FIRST INTERIOR *************
      ! *****************************************************************

      subroutine get_GI_c(c,dir)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: dir
        integer :: i,s
        if ((dir.ne.1).and.(dir.ne.-1)) stop 'Error: dir must = 1,-1 in get_GI_c in coordinates.f90'
        s = c%sn
        if (s.gt.3) then ! 3 or more nodes, remove all but boundary surface
        if (dir.eq.-1) then; do i=1,s-3; call pop(c);  enddo; endif
        if (dir.eq. 1) then; do i=1,s-3; call snip(c); enddo; endif
        elseif ((s.eq.3).or.(s.eq.2).or.(s.eq.1)) then
        else; stop 'Error: bad case in get_GI_c in coordinates.f90'
        endif
      end subroutine

      subroutine get_boundary_c(c,dir)
        implicit none
        type(coordinates),intent(inout) :: c
        integer,intent(in) :: dir
        integer :: i,s
        if ((dir.ne.1).and.(dir.ne.-1)) stop 'Error: dir must = 1,-1 in get_boundary_c in coordinates.f90'
        s = c%sn
        if (s.gt.2) then ! 3 or more nodes, remove all but boundary surface
        if (dir.eq.-1) then; do i=1,s-2; call pop(c);  enddo; call snip(c); endif
        if (dir.eq. 1) then; do i=1,s-2; call snip(c); enddo; call pop(c);  endif
        elseif ((s.eq.2).or.(s.eq.1)) then ! single cell, cannot choose which node to remove
        else; stop 'Error: bad case in get_boundary_c in coordinates.f90'
        endif
      end subroutine

      ! *****************************************************************
      ! ************************** AUXILIARY ****************************
      ! *****************************************************************

      subroutine init_stencils_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        ! Interpolation stencils
        call init(c%theta,interpolation_stencil(c%hc%f,c%hn%f,c%sc,c%sn))
        ! Derivative stencils
        call init(c%stagCC2N,staggered_CC2N(c%dhc%f,c%sc))
        call init(c%stagN2CC,staggered_N2CC(c%dhn%f,c%sn))
        call init(c%colCC_centered(1),collocated_CC_1_centered(c%dhc%f,c%sc))
        call init(c%colCC_centered(2),collocated_CC_2_centered(c%dhc%f,c%sc))
        call init(c%colCC(1),collocated_CC_1(c%dhc%f,c%sc))
        call init(c%colCC(2),collocated_CC_2(c%dhc%f,c%sc))
        call init(c%colN(1),collocated_Node_1(c%dhn%f,c%sn))
        call init(c%colN(2),collocated_Node_2(c%dhn%f,c%sn))

        ! call check(c%stagCC2N)
        ! call check(c%stagN2CC)
        ! call check(c%colCC)
        ! call check(c%colN)
        ! stop 'Done'
        c%stencils_defined = .true.
      end subroutine

      subroutine add_ghost_nodes_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (c%sn.gt.1) then
          call init(c,(/c%hn%f(1)-c%dhn%f(1),c%hn%f,c%hn%f(c%sn)+c%dhn_e/),c%sn+2)
        else
          write(*,*) 'Error: Trying to add ghost point to a single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine prepend_ghost_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (c%sn.gt.1) then
        call init(c,(/c%hn%f(1)-c%dhn%f(1),c%hn%f/),c%sn+1)
        else
          write(*,*) 'Error: Trying to prepend ghost point to a single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine append_ghost_c(c)
        implicit none
        type(coordinates),intent(inout) :: c
        if (c%sn.gt.1) then
        call init(c,(/c%hn%f,c%hn%f(c%sn)+c%dhn_e/),c%sn+1)
        else
          write(*,*) 'Error: Trying to append ghost point to a single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

#ifdef _DEBUG_COORDINATES_

      subroutine checkCoordinates(c)
        implicit none
        type(coordinates),intent(in) :: c
        write(*,*) 'Starting to check coordinates'
        call check(c%colCC(1))
        call check(c%colCC(2))
        call check(c%colCC_centered(1))
        call check(c%colCC_centered(2))
        call check(c%colN(1))
        call check(c%colN(2))
        call check(c%stagCC2N)
        call check(c%stagN2CC)
        write(*,*) 'step 0'; call insist_consecutive_c(c)
        write(*,*) 'step 1'; call check_stencilSymmetry(c,c%stagCC2N,'stagCC2N')
        write(*,*) 'step 2'; call check_stencilSymmetry(c,c%stagN2CC,'stagN2CC')
        write(*,*) 'step 3'; call check_stencilSymmetry(c,c%colCC(1),'colCC(1)')
        write(*,*) 'step 4'; call check_stencilSymmetry(c,c%colCC(2),'colCC(2)')
        write(*,*) 'step 5'; call check_stencilSymmetry(c,c%colCC_centered(1),'colCC_centered(1)')
        write(*,*) 'step 6'; call check_stencilSymmetry(c,c%colCC_centered(2),'colCC_centered(2)')
        write(*,*) 'step 7'; call check_stencilSymmetry(c,c%colN(1),'colN(1)')
        write(*,*) 'step 8'; call check_stencilSymmetry(c,c%colN(2),'colN(2)')
        write(*,*) 'Done checking coordinates'
        ! stop 'Done'
      end subroutine

      subroutine insist_consecutive_c(c)
        implicit none
        type(coordinates),intent(in) :: c
        integer :: i
        real(cp) :: tol
        if (c%sn.gt.1) then
          ! Check if consectutive
          do i=1,c%sn-1
            if (c%hn%f(i+1)-c%hn%f(i).lt.c%dhMin/2.0_cp) then
               write(*,*) 'i,dh',i,c%hn%f(i+1)-c%hn%f(i)
               write(*,*) 'hn = ',c%hn
               stop 'Error: coordinates are not consecutive.'
            endif
          enddo
          ! Check if cell centeres are in cell center
          tol = c%dhMin*1.0_cp**(-10.0_cp)
          do i=1,c%sn-1
            if (abs((c%hc%f(i)-c%hn%f(i))-(c%hn%f(i+1)-c%hc%f(i))).gt.tol) then
               write(*,*) 'Cell centers are not centered'
               write(*,*) 'i = ',i
               write(*,*) 'hn = ',c%hn%f
               write(*,*) 'hc = ',c%hc%f
               stop 'Error: cell centeres are not in cell centers.'
            endif
          enddo
        else
          write(*,*) 'Warning: cannot check consecutive coordinates for a single point'
          stop 'program stopped in coordinates.f90'
        endif
      end subroutine

      subroutine check_stencilSymmetry(c,T,name)
        implicit none
        type(coordinates),intent(in) :: c
        type(triDiag),intent(in) :: T
        character(len=*),intent(in) :: name
        integer :: i
        real(cp) :: tol,temp
        tol = c%dhMin*1.0_cp**(-15.0_cp)
        ! Check L
        ! do i=1,T%sL
        !   if (allocated(T%L)) then
        !    temp = abs(T%L(i)) - abs(T%L(T%s-i+1))
        !    ! write(*,*) 'temp(L(',i,')) = ',temp
        !    if (abs(temp).gt.tol) then
        !      write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
        !      write(*,*) 'for L on stencil '//name
        !      write(*,*) 'T%L = ',T%L
        !      write(*,*) 'error = ',abs(temp)
        !      stop 'Done'
        !    endif
        !   endif
        ! enddo
        ! Check LU
        ! if (allocated(T%L).and.(allocated(T%U))) then
        !   if (T%sL.ne.T%sU) stop 'Error: T%sL must = T%sU in check_stencilSymmetry in coordinates.f90'
        !   do i=1,T%sL
        !      temp = abs(T%L(i)) - abs(T%U(T%s-i+1))
        !      if (abs(temp).gt.tol) then
        !        write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
        !        write(*,*) 'for LU on stencil '//name
        !        write(*,*) 'T%L = ',T%L
        !        write(*,*) 'T%U = ',T%U
        !        write(*,*) 'error = ',abs(temp)
        !        stop 'Done'
        !      endif
        !   enddo
        ! endif
        ! Check D
        do i=1,T%sD
          if (allocated(T%D)) then
          temp = abs(T%D(i)) - abs(T%D(T%s-i+1))
          ! write(*,*) 'temp(D(',i,')) = ',temp
          if (abs(temp).gt.tol) then
            write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
            write(*,*) 'for D on stencil '//name
            write(*,*) 'T%D = ',T%D
            write(*,*) 'error = ',abs(temp)
            stop 'Done'
          endif
          endif
        enddo
        ! Check U
        ! do i=1,T%sU
        !   if (allocated(T%U)) then
        !   temp = abs(T%U(i)) - abs(T%U(T%s-i+1))
        !   ! write(*,*) 'temp(U(',i,')) = ',temp
        !   if (abs(temp).gt.tol) then
        !     write(*,*) 'Error: coordinate stencils are not symmetric at location ',i
        !     write(*,*) 'for U on stencil '//name
        !     write(*,*) 'T%U = ',T%U
        !      write(*,*) 'error = ',abs(temp)
        !     stop 'Done'
        !   endif
        !   endif
        ! enddo
      end subroutine

#endif
      end module