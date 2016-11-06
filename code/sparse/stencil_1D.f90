      module stencil_1D_mod
      use GF_mod
      use grid_mod
      use data_location_mod
      use face_edge_corner_indexing_mod
      use sparse_mod
      use sparse_field_mod
      use current_precision_mod
      implicit none
      private
      public :: stencil_1D
      public :: init,delete,display,print,export,import
      public :: clean

      public :: assign_staggered
      public :: assign_consecutive
      public :: assign_zero

      public :: multiply

      public :: insist_allocated

      type stencil_1D
        type(sparse) :: stag_CC2N ! Sparse 1D stencil_1D
        type(sparse) :: stag_N2CC ! Sparse 1D stencil_1D
        type(sparse_field) :: SF  ! Sparse 1D stencil_1D field
        integer :: dir = 0        ! derivative direction
        type(data_location) :: DL
        logical :: cleaned = .false.
      end type

      interface init;                   module procedure init_S1D;                     end interface
      interface init;                   module procedure init_Copy_S1D;                end interface
      interface delete;                 module procedure delete_S1D;                   end interface
      interface print;                  module procedure print_S1D;                    end interface
      interface display;                module procedure display_S1D;                  end interface
      interface import;                 module procedure import_S1D;                   end interface
      interface export;                 module procedure export_S1D;                   end interface

      interface clean;                  module procedure clean_S1D;                    end interface

      interface assign_staggered;       module procedure assign_staggered_S1D;         end interface
      interface assign_staggered_CC2N;  module procedure assign_staggered_CC2N_S1D;    end interface
      interface assign_staggered_N2CC;  module procedure assign_staggered_N2CC_S1D;    end interface
      interface assign_staggered;       module procedure assign_staggered_S1D_VP;      end interface
      interface assign_staggered_CC2N;  module procedure assign_staggered_CC2N_S1D_VP; end interface
      interface assign_staggered_N2CC;  module procedure assign_staggered_N2CC_S1D_VP; end interface

      interface assign_consecutive;     module procedure assign_consecutive_S1D;       end interface
      interface assign_consecutive_CC;  module procedure assign_consecutive_CC_S1D;    end interface
      interface assign_consecutive_N;   module procedure assign_consecutive_N_S1D;     end interface
      interface assign_consecutive;     module procedure assign_consecutive_S1D_VP;    end interface
      interface assign_consecutive_CC;  module procedure assign_consecutive_CC_S1D_VP; end interface
      interface assign_consecutive_N;   module procedure assign_consecutive_N_S1D_VP;  end interface

      interface assign_zero;            module procedure assign_zero_S1D;              end interface

      interface multiply;               module procedure multiply_S1D;                 end interface

      interface insist_allocated;       module procedure insist_allocated_S1D;         end interface

      contains

      subroutine init_S1D(ST,g,DL,dir)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer,intent(in) :: dir
        call delete(ST)
#ifdef _DEBUG_STENCIL_1D_
        call insist_valid_dir(dir,'init_stencil_1D')
#endif
        ST%dir = dir
        ST%cleaned = .false.
        call init(ST%stag_CC2N,g%c(dir)%stagCC2N)
        call init(ST%stag_N2CC,g%c(dir)%stagN2CC)
        call init(ST%SF,g,DL)
        call init(ST%DL,DL)
      end subroutine

      subroutine init_Copy_S1D(ST,ST_in)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(stencil_1D),intent(in) :: ST_in
        call delete(ST)
        ST%cleaned = ST_in%cleaned
        if (.not.ST_in%cleaned) then
          call init(ST%stag_CC2N,ST_in%stag_CC2N)
          call init(ST%stag_N2CC,ST_in%stag_N2CC)
          call init(ST%DL,ST_in%DL)
        endif
        call init(ST%SF,ST_in%SF)
        ST%dir = ST_in%dir
      end subroutine

      subroutine delete_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        call delete(ST%stag_CC2N)
        call delete(ST%stag_N2CC)
        call delete(ST%SF)
        call delete(ST%DL)
        ST%cleaned = .false.
        ST%dir = 0
      end subroutine

      subroutine print_S1D(ST)
        implicit none
        type(stencil_1D),intent(in) :: ST
        call display(ST,6)
      end subroutine

      subroutine display_S1D(ST,un)
        implicit none
        type(stencil_1D),intent(in) :: ST
        integer,intent(in) :: un
        call display(ST%stag_CC2N,un)
        call display(ST%stag_N2CC,un)
        call display(ST%SF,un)
        call display(ST%DL,un)
        write(un,*) 'dir = ',ST%dir
      end subroutine

      subroutine export_S1D(ST,un)
        implicit none
        type(stencil_1D),intent(in) :: ST
        integer,intent(in) :: un
        call export(ST%stag_CC2N,un)
        call export(ST%stag_N2CC,un)
        call export(ST%SF,un)
        call export(ST%DL,un)
        write(un,*) 'dir = '; write(un,*) ST%dir
      end subroutine

      subroutine import_S1D(ST,un)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        integer,intent(in) :: un
        call import(ST%stag_CC2N,un)
        call import(ST%stag_N2CC,un)
        call import(ST%SF,un)
        call import(ST%DL,un)
        read(un,*); read(un,*) ST%dir
      end subroutine

      ! *********************************************************************
      ! ************************** FREE UP SPACE ****************************
      ! *********************************************************************

      subroutine clean_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        call delete(ST%stag_CC2N)
        call delete(ST%stag_N2CC)
        call delete(ST%DL)
        ST%cleaned = .true.
      end subroutine

      ! *********************************************************************
      ! **************************** MIXED **********************************
      ! *********************************************************************

      subroutine assign_staggered_S1D_VP(ST,sig)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid_field),intent(in) :: sig
        if (CC_along(ST%DL,ST%dir)) then
          call assign_staggered_CC2N(ST,sig)
        elseif (N_along(ST%DL,ST%dir)) then
          call assign_staggered_N2CC(ST,sig)
        else; stop 'Error: bad input to assign_staggered_S1D_VP in stencil_1D.f90'
        endif
      end subroutine

      subroutine assign_staggered_CC2N_S1D_VP(ST,sig)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid_field),intent(in) :: sig
        integer :: i,j,k,t,dir
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_staggered_CC2N_S1D_VP')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          ST%SF%L%f(i,j,k) = 0.0_cp
          ST%SF%D%f(i,j,k) = ST%stag_CC2N%D%f(t-1)*sig%f(i,j,k)
          ST%SF%U%f(i,j,k) = ST%stag_CC2N%U%f(t-1)*sig%f(i+x(1),j+x(2),k+x(3))
        enddo; enddo; enddo
      end subroutine

      subroutine assign_staggered_N2CC_S1D_VP(ST,sig)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid_field),intent(in) :: sig
        integer :: i,j,k,t,dir
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_staggered_N2CC_S1D_VP')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          ST%SF%L%f(i,j,k) = 0.0_cp
          ST%SF%D%f(i,j,k) = ST%stag_N2CC%D%f(t-1)*sig%f(i,j,k)
          ST%SF%U%f(i,j,k) = ST%stag_N2CC%U%f(t-1)*sig%f(i+x(1),j+x(2),k+x(3))
        enddo; enddo; enddo
      end subroutine

      subroutine assign_staggered_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        if (CC_along(ST%DL,ST%dir)) then
          call assign_staggered_CC2N(ST)
        elseif (N_along(ST%DL,ST%dir)) then
          call assign_staggered_N2CC(ST)
        else; stop 'Error: bad input to assign_staggered_stencil_1D in stencil_1D.f90'
        endif
      end subroutine

      subroutine assign_staggered_CC2N_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        integer :: i,j,k,t,dir
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_staggered_CC2N_stencil_1D')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          ST%SF%L%f(i,j,k) = 0.0_cp
          ST%SF%D%f(i,j,k) = ST%stag_CC2N%D%f(t-1)
          ST%SF%U%f(i,j,k) = ST%stag_CC2N%U%f(t-1)
        enddo; enddo; enddo
      end subroutine

      subroutine assign_staggered_N2CC_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        integer :: i,j,k,t,dir
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_staggered_N2CC_stencil_1D')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          ST%SF%L%f(i,j,k) = 0.0_cp
          ST%SF%D%f(i,j,k) = ST%stag_N2CC%D%f(t-1)
          ST%SF%U%f(i,j,k) = ST%stag_N2CC%U%f(t-1)
        enddo; enddo; enddo
      end subroutine

      ! *********************************************************************
      ! *************************** CONSECUTIVE *****************************
      ! *********************************************************************

      subroutine assign_consecutive_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        if (CC_along(ST%DL,ST%dir)) then
          call assign_consecutive_CC(ST)
        elseif (N_along(ST%DL,ST%dir)) then
          call assign_consecutive_N(ST)
        else; stop 'Error: bad input to assign_consecutive_stencil_1D in stencil_1D.f90'
        endif
      end subroutine

      subroutine assign_consecutive_CC_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        integer :: i,j,k,t,dir
        real(cp) :: DC_1,UC_1,k_1
        real(cp) :: DC_2,DN_2,UC_2,UN_2,k_2
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_consecutive_CC_stencil_1D')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          DC_1 = ST%stag_CC2N%D%f(t-1)
          UC_1 = ST%stag_CC2N%U%f(t-1)
          DC_2 = ST%stag_CC2N%D%f(t)
          UC_2 = ST%stag_CC2N%U%f(t)
          DN_2 = ST%stag_N2CC%D%f(t)
          UN_2 = ST%stag_N2CC%U%f(t)
          k_1 = 1.0_cp ! sig%f(i,j,k)
          k_2 = 1.0_cp ! sig%f(i+x(1),j+x(2),k+x(3))
          ST%SF%L%f(i,j,k) = DC_1*DN_2*k_1
          ST%SF%D%f(i,j,k) = DN_2*UC_1*k_1 + DC_2*UN_2*k_2
          ST%SF%U%f(i,j,k) = UC_2*UN_2*k_2
        enddo; enddo; enddo
      end subroutine

      subroutine assign_consecutive_N_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        integer :: i,j,k,t,dir
        real(cp) :: DC_1,DN_1,UC_1,UN_1,k_1
        real(cp) :: DN_2,UN_2,k_2
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_consecutive_N_stencil_1D')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          DC_1 = ST%stag_CC2N%D%f(t-1)
          UC_1 = ST%stag_CC2N%U%f(t-1)
          DN_1 = ST%stag_N2CC%D%f(t-1)
          UN_1 = ST%stag_N2CC%U%f(t-1)
          DN_2 = ST%stag_N2CC%D%f(t)
          UN_2 = ST%stag_N2CC%U%f(t)
          k_1 = 1.0_cp ! sig%f(i,j,k)
          k_2 = 1.0_cp ! sig%f(i+x(1),j+x(2),k+x(3))
          ST%SF%L%f(i,j,k) = DC_1*DN_1*k_1
          ST%SF%D%f(i,j,k) = DN_2*UC_1*k_1 + DC_1*UN_1*k_2
          ST%SF%U%f(i,j,k) = UC_1*UN_2*k_2
        enddo; enddo; enddo
      end subroutine

      subroutine assign_consecutive_S1D_VP(ST,sig)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid_field),intent(in) :: sig
        if (CC_along(ST%DL,ST%dir)) then
          call assign_consecutive_CC(ST,sig)
        elseif (N_along(ST%DL,ST%dir)) then
          call assign_consecutive_N(ST,sig)
        else; stop 'Error: bad input to assign_consecutive_stencil_1D in stencil_1D.f90'
        endif
      end subroutine

      subroutine assign_consecutive_CC_S1D_VP(ST,sig)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid_field),intent(in) :: sig
        integer :: i,j,k,t,dir
        real(cp) :: DC_1,UC_1,k_1
        real(cp) :: DC_2,DN_2,UC_2,UN_2,k_2
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_consecutive_CC_stencil_1D')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          DC_1 = ST%stag_CC2N%D%f(t-1)
          UC_1 = ST%stag_CC2N%U%f(t-1)
          DC_2 = ST%stag_CC2N%D%f(t)
          UC_2 = ST%stag_CC2N%U%f(t)
          DN_2 = ST%stag_N2CC%D%f(t)
          UN_2 = ST%stag_N2CC%U%f(t)
          k_1 = sig%f(i,j,k)
          k_2 = sig%f(i+x(1),j+x(2),k+x(3))
          ST%SF%L%f(i,j,k) = DC_1*DN_2*k_1
          ST%SF%D%f(i,j,k) = DN_2*UC_1*k_1 + DC_2*UN_2*k_2
          ST%SF%U%f(i,j,k) = UC_2*UN_2*k_2
        enddo; enddo; enddo
      end subroutine

      subroutine assign_consecutive_N_S1D_VP(ST,sig)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        type(grid_field),intent(in) :: sig
        integer :: i,j,k,t,dir
        real(cp) :: DC_1,DN_1,UC_1,UN_1,k_1
        real(cp) :: DN_2,UN_2,k_2
        integer,dimension(3) :: x,s
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_consecutive_N_stencil_1D')
#endif
        dir = ST%dir
        x = eye_given_dir(dir)
        s = ST%SF%D%s
        do k=2,s(3)-1; do j=2,s(2)-1; do i=2,s(1)-1
          t = i*x(1)+j*x(2)+k*x(3)
          DC_1 = ST%stag_CC2N%D%f(t-1)
          UC_1 = ST%stag_CC2N%U%f(t-1)
          DN_1 = ST%stag_N2CC%D%f(t-1)
          UN_1 = ST%stag_N2CC%U%f(t-1)
          DN_2 = ST%stag_N2CC%D%f(t)
          UN_2 = ST%stag_N2CC%U%f(t)
          k_1 = sig%f(i,j,k)
          k_2 = sig%f(i+x(1),j+x(2),k+x(3))
          ST%SF%L%f(i,j,k) = DC_1*DN_1*k_1
          ST%SF%D%f(i,j,k) = DN_2*UC_1*k_1 + DC_1*UN_1*k_2
          ST%SF%U%f(i,j,k) = UC_1*UN_2*k_2
        enddo; enddo; enddo
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine assign_zero_S1D(ST)
        implicit none
        type(stencil_1D),intent(inout) :: ST
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'assign_zero_stencil_1D')
#endif
        call assign(ST%SF,0.0_cp)
      end subroutine

      subroutine multiply_S1D(ST,v)
        implicit none
        type(stencil_1D),intent(inout) :: ST
        real(cp),intent(in) :: v
#ifdef _DEBUG_STENCIL_1D_
        call insist_allocated(ST,'multiply_S1D')
#endif
        call multiply(ST%SF,v)
      end subroutine

      ! *********************************************************************
      ! *********************************************************************
      ! *********************************************************************

      subroutine insist_allocated_S1D(ST,caller)
        implicit none
        type(stencil_1D),intent(in) :: ST
        character(len=*),intent(in) :: caller
        call insist_allocated(ST%stag_CC2N,caller//' ST(1)')
        call insist_allocated(ST%stag_N2CC,caller//' ST(2)')
        call insist_allocated(ST%SF,caller//' ST(3)')
      end subroutine

      end module